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
-export([start/1]).

%% gen_server callbacks

-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3,
         terminate/3, code_change/4]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Start a new session 
start(Opts) ->
	?DebugF("Starting with opts: ~p~n",[Opts]),
	gen_fsm:start_link(?MODULE, Opts, []).

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
init({Session=#session{id           = Profile,
                       persistent   = Persistent,
                       messages_ack = PType,
                       ssl_ciphers  = Ciphers,
                       type         = CType}, Count, IP}) ->
	?DebugF("Init ... started with count = ~p  ~n",[Count]),
	ts_utils:init_seed(),

    {ServerName, Port, Protocol} = get_server_cfg({Profile,1}),
	?DebugF("Get dynparams for ~p  ~n",[CType]),
	DynData = CType:init_dynparams(),
	StartTime= now(),
    ts_mon:newclient({self(), StartTime}),
    set_thinktime(?short_timeout),
    {ok, think, #state_rcv{ port       = Port,
                            host       = ServerName,
                            profile    = Profile,
                            protocol   = Protocol,
                            clienttype = CType,
                            session    = CType:new_session(),
                            persistent = Persistent,
                            starttime  = StartTime,
                            timeout    = ?config(tcp_timeout),
                            dump       = ?config(dump),
                            ssl_ciphers= Ciphers,
                            count      = Count,
                            ip         = IP,
                            maxcount   = Count,
                            dyndata    = DynData
                           }}.

%%--------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%--------------------------------------------------------------------
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
%% Func: handle_info/2
%% Returns: {next_state, StateName, State}          |
%%          {next_state, StateName, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% inet data
handle_info({NetEvent, Socket, Data}, wait_ack, State) when NetEvent==tcp;
                                                            NetEvent==ssl ->
    case handle_data_msg(Data, State) of 
        {NewState=#state_rcv{ack_done=true}, Opts} ->
            NewSocket = inet_setopts(State#state_rcv.protocol, 
                                     NewState#state_rcv.socket,
                                     [{active, once} | Opts]),
            handle_next_action(NewState#state_rcv{socket=NewSocket});
        {NewState, Opts} ->
            NewSocket = inet_setopts(State#state_rcv.protocol,
                                     NewState#state_rcv.socket,
                                     [{active, once} | Opts]),
            {next_state, wait_ack, NewState#state_rcv{socket=NewSocket}, NewState#state_rcv.timeout}
    end;
%% inet close messages; persistent session, waiting for ack
handle_info({NetEvent, Socket}, wait_ack, 
            State = #state_rcv{persistent=true, request=Req,
                               clienttype=Type}) when NetEvent==tcp_closed;
                                                      NetEvent==ssl_closed ->
	?LOG("connection closed while waiting for ack",?INFO),
    {NewState, Opts} = handle_data_msg(closed, State),
    %% socket should be closed in handle_data_msg
    handle_next_action(NewState#state_rcv{socket=none});

%% inet close messages; persistent session
handle_info({NetEvent, Socket}, think, 
            State = #state_rcv{persistent=true}) when NetEvent==tcp_closed;
                                                      NetEvent==ssl_closed ->
	?LOG("connection closed, stay alive (persistent)",?INFO),
    catch ts_utils:close_socket(State#state_rcv.protocol, Socket), % mandatory for ssl
    {next_state, think, State#state_rcv{socket = none}};

%% inet close messages
handle_info({NetEvent, Socket}, StateName, State) when NetEvent==tcp_closed;
                                                       NetEvent==ssl_closed ->
	?LOG("connection closed, abort", ?WARN),
    %% the connexion was closed after the last msg was sent, stop quietly
	ts_mon:add({ count, error_closed }),
    ts_utils:close_socket(State#state_rcv.protocol, Socket), % mandatory for ssl
	{stop, normal, State};

%% inet errors
handle_info({NetError, Socket, Reason}, wait_ack, State)  when NetError==tcp_error;
                                                               NetError==ssl_error ->
	?LOGF("Net error (~p): ~p~n",[NetError, Reason], ?WARN),
    CountName="inet_err_"++atom_to_list(Reason),
	ts_mon:add({ count, list_to_atom(CountName) }),
	{stop, normal, State};

%% timer expires, no more messages to send
handle_info({timeout, Ref, end_thinktime}, think, State= #state_rcv{ count=0 })  ->
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
handle_next_action(State=#state_rcv{count=0}) ->
    ?LOG("Session ending ~n", ?INFO),
    {stop, normal, State};
handle_next_action(State) ->
	Count = State#state_rcv.count-1,
    case set_profile(State#state_rcv.maxcount,State#state_rcv.count,State#state_rcv.profile) of
        {thinktime, Think} ->
            ?DebugF("Starting new thinktime ~p~n", [Think]),
            set_thinktime(Think),
            {next_state, think, State#state_rcv{count=Count}};
        {transaction, start, Tname} ->
            ?LOGF("Starting new transaction ~p~n", [Tname], ?INFO),
            TrList = State#state_rcv.transactions,
            NewState = State#state_rcv{transactions=[{Tname,now()}|TrList],
                                   count=Count}, 
            handle_next_action(NewState);
        {transaction, stop, Tname} ->      
            ?LOGF("Stopping transaction ~p~n", [Tname], ?INFO),
            TrList = State#state_rcv.transactions,
            {value, {Key, Tr}} = lists:keysearch(Tname, 1, TrList),
            Now = now(),
            Elapsed = ts_utils:elapsed(Tr, Now),
            ts_mon:add({sample, Tname, Elapsed}),
            NewState = State#state_rcv{transactions=lists:keydelete(Tname,1,TrList),
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
    Count = State#state_rcv.count-1,
	Type  = State#state_rcv.clienttype,

	%% does the next message change the server setup ?
    case Profile of 
        #ts_request{host=undefined, port= undefined, scheme= undefined} ->
            {Host,Port,Protocol,Socket} = {State#state_rcv.host,State#state_rcv.port,
                                           State#state_rcv.protocol,State#state_rcv.socket};
        #ts_request{host=Host, port= Port, scheme= Protocol} ->
			%% need to reconnect if the server/port/scheme has changed
			Socket = case {State#state_rcv.host,State#state_rcv.port,
                           State#state_rcv.protocol} of
                         {Host, Port, Protocol} -> % server setup unchanged
                             State#state_rcv.socket;
                         _ ->
                             ?Debug("Change server configuration inside a session ~n"),
                             ts_utils:close_socket(State#state_rcv.protocol,
                                                   State#state_rcv.socket),
                             none
                     end
    end,

    Param = Type:add_dynparams(Profile#ts_request.subst,
                               State#state_rcv.dyndata,
                               Profile#ts_request.param, Host),
    Message = Type:get_message(Param),
    Now = now(),

	%% reconnect if needed
    Proto = {Protocol,State#state_rcv.ssl_ciphers},
	case reconnect(Socket,Host,Port,Proto,State#state_rcv.ip) of
		{ok, NewSocket} ->
            case catch send(Protocol, NewSocket, Message) of
                ok -> 
                    PageTimeStamp = case State#state_rcv.page_timestamp of 
                                        0 -> Now; %first request of a page
                                        _ -> %page already started
                                            State#state_rcv.page_timestamp
                                    end,
                    ts_mon:sendmes({State#state_rcv.dump, self(), Message}),
                    %%FIXME: need to set ack_done to true if ack=no_ack ?
                    {next_state, wait_ack, State#state_rcv{socket   = NewSocket,
                                                           count    = Count,
                                                           protocol = Protocol,
                                                           host     = Host,
                                                           request  = Profile,
                                                           port     = Port,
                                                           page_timestamp= PageTimeStamp,
                                                           send_timestamp= Now,
                                                           timestamp= Now }};
                {error, closed} -> 
                    ?LOG("connection close while sending message !~n", ?WARN),
                    handle_close_while_sending(State#state_rcv{socket=NewSocket,
                                                               protocol=Protocol,
                                                               host=Host,
                                                               port=Port});
                {error, Reason} -> 
                    ?LOGF("Error: Unable to send data, reason: ~p~n",[Reason],?ERR),
                    CountName="error_send_"++atom_to_list(Reason),
                    ts_mon:add({ count, list_to_atom(CountName) }),
                    {stop, normal, State};
                {'EXIT', {noproc, _Rest}} ->
                    ?LOG("EXIT from ssl app while sending message !~n", ?WARN),
                    handle_close_while_sending(State#state_rcv{socket=NewSocket,
                                                               protocol=Protocol,
                                                               host=Host,
                                                               port=Port});
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
	Elapsed = ts_utils:elapsed(State#state_rcv.starttime, Now),
	ts_mon:endclient({self(), Now, Elapsed}).

%%----------------------------------------------------------------------
%% Func: handle_close_while_sending/1
%% Args: State
%% Purpose: the connection has just be closed a few msec before we
%%          send a message, restart in a few moment (this time we will
%%          reconnect before sending)
%%----------------------------------------------------------------------
handle_close_while_sending(State=#state_rcv{persistent=true,protocol=Proto})->
    ts_utils:close_socket(Proto, State#state_rcv.socket),
    Think = ?config(client_retry_timeout),
    ?LOGF("Server must have closed connection upon us, waiting ~p msec~n",
          [Think], ?NOTICE),
    set_thinktime(Think),
    {next_state, think, State#state_rcv{socket=none}};
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
reconnect(none, ServerName, Port, {Protocol, Ciphers}, IP) ->
	?DebugF("Try to (re)connect to: ~p:~p using protocol ~p~n",
            [ServerName,Port,Protocol]),
	Opts = protocol_options(Protocol, Ciphers)  ++ [{ip, IP}],
    Before= now(),
    case Protocol:connect(ServerName, Port, Opts) of
		{ok, Socket} -> 
            Elapsed = ts_utils:elapsed(Before, now()),
			ts_mon:add({ sample, connect, Elapsed }),
			?Debug("(Re)connected~n"),
			{ok, Socket};
		{error, Reason} ->
			?LOGF("(Re)connect Error: ~p~n",[Reason],?ERR),
            CountName="error_connect_"++atom_to_list(Reason),
			ts_mon:add({ count, list_to_atom(CountName) }),
			{stop, normal}
    end;
reconnect(Socket, Server, Port, Protocol, IP) ->
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
protocol_options(ssl,negociate) ->
    [binary, {active, once} ];
protocol_options(ssl,Ciphers) ->
    ?DebugF("cipher is ~p~n",[Ciphers]),
    [binary, {active, once}, {ciphers, Ciphers} ];

protocol_options(gen_tcp,_) ->
	[binary, 
	 {active, once},
	 {recbuf, ?config(rcv_size)},
	 {sndbuf, ?config(snd_size)},
	 {keepalive, true} %% FIXME: should be an option
	];
protocol_options(gen_udp,_) ->
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
%% Func: handle_data_msg/2
%% Args: Data (binary), State ('state_rcv' record)
%% Returns: {NewState ('state_rcv' record), Socket options (list)}
%% Purpose: handle data received from a socket
%%----------------------------------------------------------------------
handle_data_msg(Data, State=#state_rcv{request=Req}) when Req#ts_request.ack==no_ack->
	ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    {State, []};

handle_data_msg(Data, State=#state_rcv{request=Req, clienttype=Type}) when Req#ts_request.ack==parse->
	ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
	
    {NewState, Opts, Close} = Type:parse(Data, State),
    NewBuffer = case {Req#ts_request.match,Req#ts_request.dynvar_specs, Data} of 
                    {undefined,undefined,_} -> << >>;
                    {_,_,closed} -> 
                        State#state_rcv.buffer;
                    _ ->
                        ?Debug("Bufferize response~n"),
                        OldBuffer = State#state_rcv.buffer,
                        << OldBuffer/binary, Data/binary >>
                end,
    ?DebugF("Dyndata is now ~p~n",[NewState#state_rcv.dyndata]),
    case NewState#state_rcv.ack_done of
        true ->
            ?DebugF("Response done:~p~n", [NewState#state_rcv.datasize]),
            {PageTimeStamp, DynVars} = update_stats(NewState#state_rcv{buffer=NewBuffer}, Close),
            NewDynData = concat_dynvars(DynVars, NewState#state_rcv.dyndata),
            case Close of
                true ->
                    ?Debug("Close connection required by protocol~n"),
                    ts_utils:close_socket(State#state_rcv.protocol,State#state_rcv.socket),
                    {NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                         socket = none,
                                         dyndata = NewDynData,
                                         buffer = <<>>}, Opts};
                false -> 
                    {NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                         dyndata = NewDynData,
                                         buffer = <<>>}, Opts}
            end;
        _ ->
            ?DebugF("Response: continue:~p~n",[NewState#state_rcv.datasize]),
            {NewState#state_rcv{buffer=NewBuffer}, Opts}
    end;

handle_data_msg(Data, State=#state_rcv{ack_done=true}) ->
    %% still same message, increase size
	ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
	DataSize = size(Data),
    OldSize = State#state_rcv.datasize,
    {State#state_rcv{datasize = OldSize+DataSize}, []};

handle_data_msg(Data, State=#state_rcv{ack_done=false}) ->
	ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
	DataSize = size(Data),
    {PageTimeStamp, _} = update_stats(State, false),
    {State#state_rcv{datasize=DataSize,%ack for a new message, init size
                     ack_done = true, page_timestamp= PageTimeStamp},[]}.



%%----------------------------------------------------------------------
%% Func: update_stats/2
%% Args: State
%% Returns: Page timestamp
%% Purpose: update the statistics
%%----------------------------------------------------------------------
update_stats(State, Close) ->
	Now = now(),
	Elapsed = ts_utils:elapsed(State#state_rcv.send_timestamp, Now),
	Stats= [{ sample, request, Elapsed},
			{ sum, size, State#state_rcv.datasize}],
    Profile = State#state_rcv.request,
    ts_search:match(Profile#ts_request.match, State#state_rcv.buffer),
    DynVars = ts_search:parse_dynvar(Profile#ts_request.dynvar_specs,
                                     State#state_rcv.buffer),
	case Profile#ts_request.endpage of
		true -> % end of a page, compute page reponse time 
			PageElapsed = ts_utils:elapsed(State#state_rcv.page_timestamp, Now),
			ts_mon:add(lists:append([Stats,[{sample, page, PageElapsed}]])),
			{0, DynVars};
		_ ->
			ts_mon:add(Stats),
			{State#state_rcv.page_timestamp, DynVars}
	end.

%%----------------------------------------------------------------------
%% Func: concat_dynvars/2
%%----------------------------------------------------------------------
concat_dynvars(DynData, undefined)  -> #dyndata{dynvars=DynData};
concat_dynvars([], DynData) -> DynData;
concat_dynvars(DynVars, DynData=#dyndata{dynvars=undefined}) ->
    DynData#dyndata{dynvars=DynVars};
concat_dynvars(DynVars, DynData=#dyndata{dynvars=OldDynVars}) ->
    %% FIXME: should we remove duplicate keys ?
    DynData#dyndata{dynvars=lists:keymerge(1,DynVars,OldDynVars)}.
    
%%----------------------------------------------------------------------
%% Func: inet_setopts/4
%% Purpose: set inet options depending on the protocol (gen_tcp, gen_udp,
%%  ssl)
%%----------------------------------------------------------------------
inet_setopts(Protocol, none, Opts) -> %socket was closed before
    none;
inet_setopts(ssl, Socket, Opts) ->
	case ssl:setopts(Socket, Opts) of
		ok ->
			Socket;
		{error, closed} ->
			none;
		Error ->
			?LOGF("Error while setting ssl options ~p ~p ~n", [Opts, Error], ?ERR),
            none
	end;
inet_setopts(gen_tcp, Socket,  Opts)->
	case inet:setopts(Socket, Opts) of
		ok ->
			Socket;
		{error, closed} ->
			none;
		Error ->
			?LOGF("Error while setting inet options ~p ~p ~n", [Opts, Error], ?ERR),
            none
	end;
%% FIXME: UDP not tested
inet_setopts(gen_udp, Socket,  Opts)->
	ok = inet:setopts(Socket, Opts),
    Socket.
