%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%% 
%%%  Created : 15 Feb 2001 by Nicolas Niclausse <nniclausse@IDEALX.com>

-module(ts_client).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').
-modified_by('jflecomte@IDEALX.com').

-behaviour(gen_server).

-include("../include/ts_profile.hrl").

%% External exports
-export([start/1, next/1, close/1]).

%% gen_server callbacks
-export([init/1, init/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {rcvpid, % pid of receiving process
				server, % name (or IP) of server
				port,   % server port
				protocol,   % gen_tcp, gen_udp or ssl
				socket,     % Socket descriptor
				clienttype, % type of client (ts_http11, jabber_online, etc.)
				mestype,    % type of messages (dynamic or static)
				profile,    % list of requests parameters
				persistent, % if true, don't exit when connexion is closed
				lasttimeout,% value of the last timeout
				timestamp,  % previous message date
				starttime,  % date of the beginning of the session
				dyndata =[],    % dynamic data (only used by parsing clients)
				count,      % number of requests waiting to be sent
				monitor     % type of monitoring
			   }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Start a new session 
start(Opts) ->
	?LOGF("Starting with opts: ~p~n",[Opts],?DEB),
%	fprof:start(),
%	Res = fprof:trace(start, "/tmp/tsunami.fprof"),
%	?LOGF("starting profiler: ~p~n",[Res], ?WARN),
	gen_server:start_link(?MODULE, Opts, []).

%% stop the session 
close({Reason, Pid}) when atom(Reason) -> % close after an error
	gen_server:cast(Pid, {closed, Reason, Pid});
close({Reason, Pid}) when list(Reason) -> % close after an error
	gen_server:cast(Pid, {closed, list_to_atom(Reason), Pid});
close(Pid) -> % normal close
	gen_server:cast(Pid, {closed, Pid}).


%% continue with the next request
next({Pid, []}) ->
	gen_server:cast(Pid, {next_msg});
next({Pid, DynData}) ->
	gen_server:cast(Pid, {next_msg, DynData}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Profile, {CType, PType, static, Persistent}]) ->
	?LOG("Init ... static~n",?DEB),
	init([Profile, {CType, PType, static, Persistent}], length(Profile));

init([Profile, {CType, PType, dynamic, Persistent}]) ->
	?LOG("Init ... dynamic~n",?DEB),
	random:seed(),
	init([Profile, {CType, PType, static, Persistent}],
		 ?config(messages_number) + length(Profile) - 1);

init(Args) ->
	?LOGF("Init ... with unknown args ~p~n",[Args], ?DEB).
	

init([Profile, {CType, PType, MType, Persistent}], Count) ->
	?LOGF("Init ... started with count = ~p  ~n",[Count],?DEB),
	%%init seed
%    random:seed(ts_utils:init_seed()),
%	?LOG("seed OK  ~n",?DEB),

    {ServerName, Port, Protocol} = ts_profile:get_server(), % get server profile
    % open connection
	Opts = protocol_options(Protocol),
	StartTime= now(),
    case Protocol:connect(ServerName, Port, Opts, ?config(connect_timeout)) of
		{ok, Socket} -> 
	    % start a new process for receiving messages from the server
			case ts_client_rcv:start({PType,
									CType, self(),
									Socket,
									Protocol,
									?config(tcp_timeout), 
									?config(messages_ack),
									?config(monitoring)}) of 
				{ok, Pid} ->
					?LOG("rcv server started ~n",?DEB),
					controlling_process(Protocol, Socket, Pid),
					Connected = now(),
					Elapsed = ts_utils:elapsed(StartTime, Connected),
					ts_mon:newclient({self(), Connected}),
					ts_mon:addsample({connect, Elapsed}), % connection time
					{ok, #state{rcvpid = Pid, socket = Socket, port = Port,
								server= ServerName, profile= Profile,
								protocol = Protocol,
								clienttype = CType, mestype = MType,
								persistent = Persistent,
								starttime = StartTime,
								monitor = ?config(monitoring),
								count = Count}, ?short_timeout};
				{error, Reason} ->
					?LOGF("Can't start rcv process ~p~n",
								[Reason],?ERR),
					{stop, Reason}
			end;
		{error, Reason} ->
			?LOGF("Connect Error: ~p~n",[Reason],?ERR),
            CountName="conn_err_" ++ atom_to_list(Reason),
			ts_mon:addcount({ list_to_atom(CountName) }),
			{stop, connfailed}
    end.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({next_msg}, State = #state{lasttimeout = infinity}) ->
	?LOGF("next_msg, count is ~p~n", [State#state.count], ?DEB),
	{noreply, State#state{lasttimeout=1}, ?short_timeout};
handle_cast({next_msg}, State) ->
	?LOG("next_msg (infinite timeout)",?DEB),
	{noreply, State, State#state.lasttimeout};
handle_cast({next_msg, DynData}, State = #state{lasttimeout = infinity}) ->
	?LOGF("next_msg, count is ~p~n", [State#state.count], ?DEB),
	{noreply, State#state{lasttimeout=1, dyndata=DynData}, ?short_timeout};
handle_cast({next_msg, DynData}, State) ->
	?LOG("next_msg (infinite timeout)",?DEB),
	{noreply, State#state{dyndata=DynData}, State#state.lasttimeout};
%% more case to handle ?


%% to be done : what about the current timer ?
handle_cast({add_messages, Messages}, State) ->
	OldProfile = State#state.profile,
	OldCount = State#state.count,
	{noreply, State#state{profile = Messages ++ OldProfile, 
						  count = OldCount + length(Messages)},
	 ?short_timeout};

%% the connexion was closed, but this session is persistent
handle_cast({closed, Pid}, State = #state{persistent = true}) ->
	?LOG("connection closed, stay alive (persistent)",?DEB),
	%% TODO: set the timeout correctly ?
	case State#state.lasttimeout of 
		infinity ->
			Elapsed = 0,
			ThinkTime = 0;
		_ ->
			Elapsed  = ts_utils:elapsed(State#state.timestamp, now()),
			ThinkTime= round(State#state.lasttimeout-Elapsed/1000)
	end,
	if 
		ThinkTime > 0 ->
			?LOGF("setting new thinktime to: ~p~n!", [ThinkTime], ?DEB),
			{noreply,  State#state{socket = none}, ThinkTime};
		true ->
			?LOGF("negative thinktime after connexion closed ~p:~p~n!",
				  [State#state.lasttimeout, Elapsed/1000], ?WARN),
			{noreply,  State#state{socket = none}, ?short_timeout}
	end;
%% the connexion was closed after the last msg was sent, stop quietly
handle_cast({closed, Pid}, State= #state{ count=0 }) ->
	{stop, normal, State};

%% the connexion was closed before the last msg was sent, log this event
handle_cast({closed, Pid}, State) ->
	?LOGF("Closed by server while pending count is: ~p~n!",
		  [State#state.count], ?INFO),
	ts_mon:addcount({ closed_by_server }),
	{stop, normal, State};

handle_cast({closed, Reason, Pid}, State) ->
	?LOGF("Closed after an error while while pending count is: ~p~n!",
		  [State#state.count], ?INFO),
	ts_mon:addcount({ Reason }),
	{stop, normal, State};

handle_cast({timeout, Pid}, State) ->
	{stop, timeoutrcv, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% no more messages to send
handle_info(timeout, State= #state{ count=0 })  ->
	Protocol = State#state.protocol,
	case State#state.socket of 
		none ->
			{stop, normal, State};
		Else ->
			Protocol:close(State#state.socket),
			{stop, normal, State}
	end;

handle_info(timeout, State ) ->
    {Thinktime, Profile, Pending} = set_profile(State#state.count, State#state.profile),
	Count = State#state.count-1,
	Type  = State#state.clienttype,
	case State#state.dyndata of 
		[] ->
			Param = Profile#message.param;
		DynData -> % add dynamic info (cookie for ex.) to request parameters
			Param = ts_profile:add_dynparams(Type, Profile#message.param, DynData)
	end,
	Message = ts_profile:get_message(Type , Param),
	Now = now(),
	%% reconnect if needed
	Protocol = State#state.protocol,
	{ok, Socket} = reconnect(State#state.socket, State#state.server, State#state.port,
					   Protocol, State#state.rcvpid),
	%% warn the receiving side that we are sending a new request
	ts_client_rcv:wait_ack({State#state.rcvpid,Profile#message.ack, Now, 
							Profile#message.endpage, Socket}),
    Timeout = new_timeout(Profile#message.ack, Count, Thinktime),
    case send(Protocol, Socket, Message) of
		ok -> 
			ts_mon:sendmes({State#state.monitor, self(), Message}),
			{noreply, State#state{socket= Socket, count = Count,
								  profile = Pending,
								  timestamp = Now,
								  lasttimeout = Thinktime},
			 Timeout}; 
		{error, closed} -> 
			?LOG("connection close while sending message !~n.", ?WARN),
			case State#state.persistent of 
				true ->
					RetryTimeout = ?config(client_retry_timeout),
					{noreply, State#state{lasttimeout=RetryTimeout}, 
					 RetryTimeout}; % try again in 10ms
				_ ->
					{stop, closed, State}
			end;
		{error, Reason} -> 
			?LOGF("Error: Unable to send data from process ~p, reason: ~p~n.",
				  [self(), Reason], ?ERR),
            CountName="send_err_"++atom_to_list(Reason),
			ts_mon:addcount({ list_to_atom(CountName) }),
			{stop, Reason, State}
	end.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
	?LOGF("Stop, reason= ~p~n",[Reason],?INFO),
	Now = now(),
	Elapsed = ts_utils:elapsed(State#state.starttime, Now),
	ts_mon:endclient({self(), Now}),
	ts_mon:addsample({session, Elapsed}), % session duration
	ts_client_rcv:stop(State#state.rcvpid),
%	fprof:stop(),
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: set_profile/2
%% Args: Count (integer), Profiles (List)
%%----------------------------------------------------------------------
%% static message, get the thinktime from profile
set_profile(Count, [Profile=#message{type = static} | Pending]) ->
    {Profile#message.thinktime, Profile, Pending };
%% dynamic message, last message
set_profile(Count, [Profile | Pending]) when length(Pending)+1 == Count->
    {ts_profile:thinktime(), Profile, Pending };
%% dynamic message, keep the same profiles
set_profile(Count, Profiles) ->
	[Profile | Pending] = Profiles,
    {ts_profile:thinktime(), Profile, Profiles }.
     
%%----------------------------------------------------------------------
%% Func: new_timeout/3
%% Args: Type, Count, Thinktime
%% Puropose: If we need to ack the message, we have to wait first the
%% acknoledgement from the receiving process, therefore, set infinite timeout
%% ----------------------------------------------------------------------
new_timeout(no_ack, Count, Thinktime) -> Thinktime;
new_timeout(_Else,  0,     Thinktime) -> Thinktime; % last message, don't wait
new_timeout(_Else, _Count, Thinktime) -> infinity.

%%----------------------------------------------------------------------
%% Func: reconnect/4
%% Returns: {Socket   }          |
%%          {stop, Reason}
%% purpose: try to reconnect if this is needed (when the socket is set to none)
%%----------------------------------------------------------------------
reconnect(none, ServerName, Port, Protocol, Pid) ->
	?LOGF("Try to reconnect to: ~p (~p)~n",[ServerName, Pid], ?DEB),
	Opts = protocol_options(Protocol),
    case Protocol:connect(ServerName, Port, Opts) of
		{ok, Socket} -> 
			controlling_process(Protocol, Socket, Pid),
			ts_mon:addcount({ reconnect }),
			{ok, Socket};
		{error, Reason} ->
			?LOGF("Error: ~p~n",[Reason],?ERR),
			ts_mon:addcount({ failedreconnect }),
			{stop, connfailed}
    end;
reconnect(Socket, Server, Port, Protocol, Pid) ->
	Socket.

%%----------------------------------------------------------------------
%% Func: send/3
%% Purpose: this fonction is used to avoid the costly M:fun form of function
%% call, see http://www.erlang.org/doc/r9b/doc/efficiency_guide/
%%----------------------------------------------------------------------
send(gen_tcp,Socket,Message) ->
    gen_tcp:send(Socket,Message);
send(ssl,Socket,Message) ->
    ssl:send(Socket,Message);
send(gen_udp,Socket,Message) ->
    gen_udp:send(Socket,Message).

%%----------------------------------------------------------------------
%% Func: controlling_process/3
%% Purpose: this fonction is used to avoid the costly M:fun form of function
%% call, see http://www.erlang.org/doc/r9b/doc/efficiency_guide/
%%----------------------------------------------------------------------
controlling_process(gen_tcp,Socket,Pid) ->
    gen_tcp:controlling_process(Socket,Pid);
controlling_process(ssl,Socket, Pid) ->
    ssl:controlling_process(Socket,Pid);
controlling_process(gen_udp,Socket,Pid) ->
    gen_udp:controlling_process(Socket,Pid).

%%----------------------------------------------------------------------
%% Func: protocol_options/1
%% Purpose: set connection's options for the given protocol
%%----------------------------------------------------------------------
protocol_options(ssl) ->
	[binary, 
	 {active, once},
	 {ciphers, ?config(ssl_ciphers)}
	];
protocol_options(gen_tcp) ->
	[binary, 
	 {active, once},
%	 {packet, http}, % for testing purpose
	 {recbuf, ?config(rcv_size)},
	 {sndbuf, ?config(snd_size)},
	 {keepalive, true}
	];
protocol_options(gen_udp) ->
	[binary, 
	 {active, once},
	 {recbuf, ?config(rcv_size)},
	 {sndbuf, ?config(snd_size)},
	 {keepalive, true}
	].
	
