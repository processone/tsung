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

-behaviour(gen_fsm). % two state: wait_ack | think

-include("ts_profile.hrl").
-include("ts_config.hrl").

%% External exports
-export([start/1, next/1, close/1]).

%% gen_server callbacks
-export([init/1, wait_ack/2, think/2, handle_sync_event/4, handle_event/3,
         handle_info/3, terminate/3, code_change/4]).

-record(state, {rcvpid, % pid of receiving process
				server, % name (or IP) of server
				port,   % server port
				protocol,   % gen_tcp, gen_udp or ssl
				socket,     % Socket descriptor
				ip,         % local ip to bind to
				clienttype, % type of client (ts_http, jabber_online, etc.)
				profile,    % list of requests parameters
				persistent, % if true, don't exit when connexion is closed
				timestamp,  % previous message date
				starttime,  % date of the beginning of the session
				dyndata =[],    % dynamic data (only used by parsing clients)
				maxcount,   % total number of requests to sent
				count,      % number of requests waiting to be sent
				monitor,    % type of monitoring
				transactions=[]% current transactions
			   }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Start a new session 
start(Opts) ->
	?DebugF("Starting with opts: ~p~n",[Opts]),
	gen_fsm:start_link(?MODULE, Opts, []).

%%----------------------------------------------------------------------
%% Func: close/1
%% Purpose: the connection was closed by the server
%%----------------------------------------------------------------------
%% close after an error, send_all_state
close({Reason, Pid}) when is_atom(Reason) ->
	gen_fsm:send_all_state_event(Pid, {closed, Reason, Pid});
close({Reason, Pid}) when is_list(Reason) ->
	gen_fsm:send_all_state_event(Pid, {closed, list_to_atom(Reason), Pid});
close({Reason, Pid}) ->
	gen_fsm:send_all_state_event(Pid, {closed, close_unknown, Pid});
%% normal close, handle by each state of the fsm
close(Pid) ->
	gen_fsm:send_event(Pid, {closed, Pid}).


%%----------------------------------------------------------------------
%% Func: next/1
%% Purpose: continue with the next request
%%----------------------------------------------------------------------
next({Pid}) ->
	gen_fsm:send_event(Pid, {next_msg});

next({Pid, DynData}) ->
	gen_fsm:send_event(Pid, {next_msg, DynData});

next({Pid, DynData, Close}) ->
	gen_fsm:send_event(Pid, {next_msg, DynData, Close}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, State}          |
%%          {ok, StateName, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({Session=#session{id            = Profile,
                        persistent   = Persistent,
                        messages_ack = PType,
                        type         = CType}, Count, IP}) ->
	?DebugF("Init ... started with count = ~p  ~n",[Count]),
	ts_utils:init_seed(),
    {ServerName, Port, Protocol} = get_server_cfg({Profile,1}),
	?DebugF("Get dynparams for ~p  ~n",[CType]),
	DynData = CType:init_dynparams(),
    % open connection
	Opts = protocol_options(Protocol) ++ [{ip, IP}],
	?DebugF("Got first message, connect to ~p with options ~p ~n",
         [{ServerName, Port, Protocol},Opts]),
	StartTime= now(),
    case Protocol:connect(ServerName, Port, Opts, ?config(connect_timeout)) of
		{ok, Socket} -> 
            %% start a new process for receiving messages from the server
			case ts_client_rcv:start({ CType, self(),
                                       Socket,
                                       Protocol,
                                       ServerName,
                                       ?config(tcp_timeout), 
                                       PType,
                                       ?config(monitoring),
                                       DynData }) of 
				{ok, Pid} ->
					?Debug("rcv server started ~n"),
					controlling_process(Protocol, Socket, Pid),
					Connected = now(),
					Elapsed = ts_utils:elapsed(StartTime, Connected),
					ts_mon:newclient({self(), Connected, Elapsed}),
                    set_thinktime(?short_timeout),
					{ok, think, #state{rcvpid=Pid, socket=Socket, port=Port,
								server     = ServerName,
                                profile    = Profile,
								protocol   = Protocol,
								clienttype = CType,
								persistent = Persistent,
								starttime  = StartTime,
								monitor    = ?config(monitoring),
								count      = Count,
								ip         = IP,
								maxcount   = Count,
								dyndata    = DynData
                               }};
				{error, Reason} ->
					?LOGF("Can't start rcv process ~p~n", [Reason],?ERR),
					{stop, Reason}
			end;
		{error, Reason} ->
			?LOGF("Connect Error: ~p~n",[Reason], ?ERR),
            CountName="conn_err_" ++ atom_to_list(Reason),
			ts_mon:add({ count, list_to_atom(CountName) }),
			{stop, normal}
    end.

%%--------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%--------------------------------------------------------------------
handle_event({closed, Reason, Pid}, StateName, State) ->
	?LOGF("Closed reason =~p (in state ~p) after an error while pending count is: ~p~n!",
		  [Reason, StateName, State#state.count], ?WARN),
	ts_mon:add({ count, Reason }),
	{stop, normal, State};
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

    
%%--------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: wait_ack/2
%% Returns: {next_state, NextStateName, State}          |
%%          {next_state, NextStateName, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
wait_ack({next_msg}, State) ->
	?DebugF("next_msg, count is ~p~n", [State#state.count]),
    handle_next_action(State);
wait_ack({next_msg, DynData}, State) ->
	?DebugF("next_msg, count is ~p~n", [State#state.count]),
    NewState = State#state{dyndata=DynData},
    handle_next_action(NewState);
wait_ack({next_msg, DynData, Close}, State) ->
	?DebugF("next_msg, count is ~p, close is ~p~n", [State#state.count, Close]),
    case Close of 
        true ->
            NewState = State#state{dyndata=DynData, socket=none};
        false ->
            NewState = State#state{dyndata=DynData}
    end,
    handle_next_action(NewState);

%% We should not received a closed event in this state, but it was the
%% last request, quit quietly
wait_ack({closed, Pid}, State= #state{ count=0 }) ->
	{stop, normal, State};
%% Oh oh. Received close when state is wait_ack !
wait_ack({closed, Pid}, State) ->
    %% this may happen in ssl when a server close happen
    %% simultaneously with the send; the send call return value is ok,
    %% but the ssl driver has been receiving close in the same
    %% time. To reproduce this case with HTTP, set the timeout equal to 
    %% the KeepAlive timeout in the HTTP server.
    %% FIXME: maybe we should retry instead of stopping ?
	?LOGF("Closed when state is wait_ack (count was ~p)!~n!",[State#state.count], ?WARN),
	ts_mon:add({ count, closed_when_send }),
	{stop, normal, State};

wait_ack(Event, State) ->
	?LOGF("Unknown event ~p in state wait_ack~n", [Event], ?ERR),
    {stop, unknown_event, State}.

%%----------------------------------------------------------------------
%% Func: think/2
%% Returns: {next_state, NextStateName, State}          |
%%          {next_state, NextStateName, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% the connexion was closed, but this session is persistent
think({closed, Pid}, State = #state{persistent = true}) ->
	?LOG("connection closed, stay alive (persistent)",?INFO),
    {next_state, think, State#state{socket = none}};

%% the connexion was closed after the last msg was sent, stop quietly
think({closed, Pid}, State= #state{ count=0 }) ->
	{stop, normal, State};

%% the connexion was closed before the last msg was sent, log this event
think({closed, Pid}, State) ->
	?LOGF("Closed by server while pending count is: ~p~n!",
		  [State#state.count], ?NOTICE),
	ts_mon:add({ count, closed_by_server }),
	{stop, normal, State};

think({timeout, Pid}, State) ->
	{stop, timeoutrcv, State};

think(Event, State) ->
	?LOGF("Unknown event ~p in state think~n", [Event], ?ERR),
    {stop, unknown_event, State}.



%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {next_state, StateName, State}          |
%%          {next_state, StateName, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% no more messages to send
handle_info({timeout, Ref, end_thinktime}, think, State= #state{ count=0 })  ->
    ?LOG("Session ending ~n", ?INFO),
    {stop, normal, State};

%% the timer expires
handle_info({timeout, Ref, end_thinktime}, think, State ) ->
    handle_next_action(State);

handle_info(timeout, StateName, State ) ->
    ?LOGF("Error: timeout receive in state ~p~n",[StateName], ?ERR),
    ts_mon:add({ count, timeout }),
    {stop, normal, State};
handle_info(Msg, StateName, State ) ->
    ?LOGF("Error: Unknown msg ~p receive in state ~p, stop~n", [Msg,StateName], ?ERR),
    ts_mon:add({ count, unknown_msg }),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%--------------------------------------------------------------------
terminate(normal, StateName,State) ->
    finish_session(State);
terminate(Reason, StateName, State) ->
	?LOGF("Stop in state ~p, reason= ~p~n",[StateName,Reason],?NOTICE),
    ts_mon:add({ count, error_unknown }),
    finish_session(State).

%%--------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: handle_next_action/1
%% Purpose: handle next action: thinktime, transaction or #ts_request
%% Args: State
%%----------------------------------------------------------------------
handle_next_action(State=#state{count=0}) ->
    ?LOG("Session ending ~n", ?INFO),
    {stop, normal, State};
handle_next_action(State) ->
	Count = State#state.count-1,
    case set_profile(State#state.maxcount,State#state.count,State#state.profile) of
        {thinktime, Think} ->
            ?DebugF("Starting new thinktime ~p~n", [Think]),
            set_thinktime(Think),
            {next_state, think, State#state{count=Count}};
        {transaction, start, Tname} ->
            ?LOGF("Starting new transaction ~p~n", [Tname], ?INFO),
            TrList = State#state.transactions,
            NewState = State#state{transactions=[{Tname,now()}|TrList],
                                   count=Count}, 
            handle_next_action(NewState);
        {transaction, stop, Tname} ->      
            ?LOGF("Stopping transaction ~p~n", [Tname], ?INFO),
            TrList = State#state.transactions,
            {value, {Key, Tr}} = lists:keysearch(Tname, 1, TrList),
            Now = now(),
            Elapsed = ts_utils:elapsed(Tr, Now),
            ts_mon:add({sample, Tname, Elapsed}),
            NewState = State#state{transactions=lists:keydelete(Tname,1,TrList),
                                   count=Count}, 
            handle_next_action(NewState);
        Profile=#ts_request{} ->                                        
            handle_next_request(Profile, State);
        Other ->
            ?LOGF("Error: set profile return value is ~p (count=~p)~n",[Other,Count],?ERR),
            {stop, set_profile_error, State}
    end.


%%----------------------------------------------------------------------
%% Func: get_server_cfg/1
%% Args: Profile, Id
%%----------------------------------------------------------------------
get_server_cfg({Profile, Id}) ->
    get_server_cfg(ts_session_cache:get_req(Profile, Id), Profile, Id).

%%----------------------------------------------------------------------
%% Func: get_server_cfg/3
%%----------------------------------------------------------------------
get_server_cfg(#ts_request{host=undefined, port=undefined, scheme=undefined},P,_)->
    ?Debug("Server not configured in msg, get global conf ~n"),
    %% get global server profile
    ts_config_server:get_server_config();
get_server_cfg(#ts_request{host=ServerName, port= Port, scheme= Protocol},P,Id) ->
    %% server profile can be overriden in the first URL of the session
    %% curently, the following server modifications in the session are not used.
    ?LOGF("Server setup overriden for this client host=~s port=~p proto=~p~n",
          [ServerName, Port, Protocol], ?INFO),
    {ServerName, Port, Protocol};
get_server_cfg({transaction,Type,Name},Profile,Id) ->
    get_server_cfg(ts_session_cache:get_req(Profile, Id+1), Profile, Id+1);
get_server_cfg({thinktime,_},Profile,Id) ->
    get_server_cfg(ts_session_cache:get_req(Profile, Id+1), Profile, Id+1);
get_server_cfg(Other,P,Id) ->
    ?LOGF("ERROR while getting cfg (~p)! ~n",[Other],?ERR),
    ts_config_server:get_server_config().

%%----------------------------------------------------------------------
%% Func: handle_next_request/2
%% Args: Profile, State
%%----------------------------------------------------------------------
handle_next_request(Profile, State) ->
    Count = State#state.count-1,
	Type  = State#state.clienttype,

	%% does the next message change the server setup ?
    case Profile of 
        #ts_request{host=undefined, port= undefined, scheme= undefined} ->
            {Host,Port,Protocol,Socket} = {State#state.server,State#state.port,
                                           State#state.protocol,State#state.socket};
        #ts_request{host=Host, port= Port, scheme= Protocol} ->
			%% need to reconnect if the server/port/scheme has changed
			case {State#state.server,State#state.port, State#state.protocol} of
				{Host, Port, Protocol} -> % server setup unchanged
					Socket = State#state.socket;
				_ ->
					?Debug("Change server configuration inside a session ~n"),
					ts_utils:close_socket(State#state.protocol, State#state.socket),
					Socket = none
			end
    end,

    Param = Type:add_dynparams(State#state.dyndata,Profile#ts_request.param,Host),
    SubstParam = dyn_substitution(Profile#ts_request.subst, Type, Param),
    Message = Type:get_message(SubstParam),
    Now = now(),

	%% reconnect if needed
	case reconnect(Socket,Host,Port,Protocol,State#state.ip,State#state.rcvpid) of
		{ok, NewSocket} ->
            %% warn the receiving side that we are sending a new request
            ts_client_rcv:wait_ack({State#state.rcvpid,Profile#ts_request.ack,Now,
                                    Profile#ts_request.endpage, NewSocket,
                                    Protocol, Host}),
            case catch send(Protocol, NewSocket, Message) of
                ok -> 
                    ts_mon:sendmes({State#state.monitor, self(), Message}),
                    {next_state, wait_ack, State#state{socket   = NewSocket,
                                                       count    = Count,
                                                       protocol = Protocol,
                                                       server   = Host,
                                                       port     = Port,
                                                       timestamp= Now }};
                {error, closed} -> 
                    ?LOG("connection close while sending message !~n", ?WARN),
                    handle_close_while_sending(State);
                {error, Reason} -> 
                    ?LOGF("Error: Unable to send data, reason: ~p~n",[Reason],?ERR),
                    CountName="error_send_"++atom_to_list(Reason),
                    ts_mon:add({ count, list_to_atom(CountName) }),
                    {stop, normal, State};
                {'EXIT', {noproc, _Rest}} ->
                    handle_close_while_sending(State);
                Exit ->
                    ?LOGF("EXIT Error: Unable to send data, reason: ~p~n",
                          [Exit], ?ERR),
                    ts_mon:add({ count, error_send }),
                    {stop, normal, State}
            end;
		_Error ->
			{stop, normal, State} %% already log in reconnect
	end.

%%----------------------------------------------------------------------
%% Func: finish_session/1
%% Args: State
%%----------------------------------------------------------------------
finish_session(State) ->
	Now = now(),
	Elapsed = ts_utils:elapsed(State#state.starttime, Now),
	ts_mon:endclient({self(), Now, Elapsed}).

%%----------------------------------------------------------------------
%% Func: handle_close_while_sending/1
%% Args: State
%% Purpose: the connection has just be closed a few msec before we
%%          send a message, restart in a few moment (this time we will
%%          reconnect before sending)
%%----------------------------------------------------------------------
handle_close_while_sending(State=#state{persistent=true}) ->
    Think = ?config(client_retry_timeout),
    set_thinktime(Think),
    ?LOGF("Server must have closed connection upon us, waiting ~p msec~n",
          [Think], ?NOTICE),
    {next_state, think, State};
handle_close_while_sending(State) ->
    {stop, error, State}.
    

%%----------------------------------------------------------------------
%% Func: set_profile/2
%% Args: MaxCount, Count (integer), ProfileId (integer)
%%----------------------------------------------------------------------
set_profile(MaxCount, Count, ProfileId) when is_integer(ProfileId) ->
    ts_session_cache:get_req(ProfileId, MaxCount-Count+1).
     
%%----------------------------------------------------------------------
%% Func: reconnect/4
%% Returns: {Socket   }          |
%%          {stop, Reason}
%% purpose: try to reconnect if this is needed (when the socket is set to none)
%%----------------------------------------------------------------------
reconnect(none, ServerName, Port, Protocol, IP, Pid) ->
	?DebugF("Try to reconnect to: ~p (~p)~n",[ServerName, Pid]),
	Opts = protocol_options(Protocol)  ++ [{ip, IP}],
    case Protocol:connect(ServerName, Port, Opts) of
		{ok, Socket} -> 
			controlling_process(Protocol, Socket, Pid),
			ts_mon:add({ count, reconnect }),
			{ok, Socket};
		{error, Reason} ->
			?LOGF("Reconnect Error: ~p~n",[Reason],?ERR),
            CountName="error_reconnect_"++atom_to_list(Reason),
			ts_mon:add({ count, list_to_atom(CountName) }),
			{stop, normal}
    end;
reconnect(Socket, Server, Port, Protocol, IP, Pid) ->
	{ok, Socket}.

%%----------------------------------------------------------------------
%% Func: send/3
%% Purpose: this fonction is used to avoid the costly M:fun form of function
%% call, see http://www.erlang.org/doc/r9b/doc/efficiency_guide/
%% FIXME: is it really faster ? 
%%----------------------------------------------------------------------
send(gen_tcp,Socket,Message) -> gen_tcp:send(Socket,Message);
send(ssl,Socket,Message)     -> ssl:send(Socket,Message);
send(gen_udp,Socket,Message) -> gen_udp:send(Socket,Message).

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
    case ?config(ssl_ciphers) of
        negociate ->
            [binary, {active, once} ];
        Cipher ->
            ?DebugF("cipher is ~p~n",[Cipher]),
            [binary, 
             {active, once},
             {ciphers, Cipher}
            ]
    end;

protocol_options(gen_tcp) ->
	[binary, 
	 {active, once},
	 {recbuf, ?config(rcv_size)},
	 {sndbuf, ?config(snd_size)},
	 {keepalive, true} %% FIXME: should be an option
	];
protocol_options(gen_udp) ->
	[binary, 
	 {active, once},
	 {recbuf, ?config(rcv_size)},
	 {sndbuf, ?config(snd_size)},
	 {keepalive, true} %% FIXME: should be an option
	].
	
%%----------------------------------------------------------------------
%% Func: set_thinktime/1
%% Purpose: set a timer for thinktime if it is not infinite
%%----------------------------------------------------------------------
set_thinktime(infinity) -> ok;
set_thinktime({random, Think}) -> 
	set_thinktime(round(ts_stats:exponential(1/Think)));
set_thinktime(Think) -> 
%% dot not use timer:send_after because it does not scale well:
%% http://www.erlang.org/ml-archive/erlang-questions/200202/msg00024.html
	?DebugF("thinktime of ~p~n",[Think]),
    erlang:start_timer(Think, self(), end_thinktime ).


%%----------------------------------------------------------------------
%% Func: dyn_substitution/3
%% Purpose: If the message is parametered for substitution, call the 
%%          protocole substitution
%%----------------------------------------------------------------------
dyn_substitution(false, Type, Request) ->
    Request;
%% If the subst request attribute in the config file has been set to
%% something: We assume we should use substitution
dyn_substitution(_, Type, Request) ->
    Type:subst(Request).
