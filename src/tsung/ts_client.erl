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
%%%  Created : 15 Feb 2001 by Nicolas Niclausse <nicolas@niclux.org>

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_client).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').
-modified_by('jflecomte@IDEALX.com').

-behaviour(gen_fsm). % two state: wait_ack | think

%%% if bidi is true (for bidirectional), the server can send data
%%% to the client at anytime (full bidirectional protocol, as jabber
%%% for ex)

-include("ts_config.hrl").
-include("ts_profile.hrl").

%% External exports
-export([start/1, next/1]).

%% gen_server callbacks

-export([init/1, wait_ack/2, think/2,handle_sync_event/4, handle_event/3,
         handle_info/3, terminate/3, code_change/4]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec start(Opts::{Session::#session{},IP::tuple(),Server::#server{},
%%       Id::integer()}) -> {ok, Pid::pid()} | ignore | {error, Error::term()}
%% @doc Start a new session
start(Opts) ->
    ?DebugF("Starting with opts: ~p~n",[Opts]),
    gen_fsm:start_link(?MODULE, Opts, []).

%%----------------------------------------------------------------------
%% @spec next({pid()}) -> ok
%% @doc Purpose: continue with the next request (used for global ack)
%% @end
%%----------------------------------------------------------------------
next({Pid}) ->
    gen_fsm:send_event(Pid, next_msg).

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
init(#session{ id           = SessionId,
               persistent   = Persistent,
               bidi         = Bidi,
               hibernate    = Hibernate,
               rate_limit   = RateLimit,
               proto_opts   = ProtoOpts,
               size         = Count,
               client_ip    = IP,
               userid       = Id,
               dump         = Dump,
               seed         = Seed,
               server       = Server,
               type         = CType}) ->
    ?DebugF("Init ... started with count = ~p~n",[Count]),
    case Seed of
        now ->
            {A, B, C} = now(),
            ts_utils:init_seed({A * Id, B, C});
        SeedVal when is_integer(SeedVal) ->
            %% use a different but fixed seed for each client.
            ts_utils:init_seed({Id,SeedVal})
    end,

    ?DebugF("Get dynparams for ~p~n",[CType]),
    DynVars = ts_dynvars:new(tsung_userid,Id),
    StartTime= ?NOW,
    set_thinktime(?short_timeout),
    ?DebugF("IP param: ~p~n",[IP]),
    NewIP = case IP of
                { TmpIP, -1 } ->
                    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
                    RealIP = case TmpIP of
                                 {scan, Interface} ->
                                     ts_ip_scan:get_ip(Interface);
                                 _ -> TmpIP
                    end,
                    {RealIP, "cport-" ++ MyHostName};
                {{scan, Interface}, PortVal } ->
                    ?DebugF("Must scan interface: ~p~n",[Interface]),
                    { ts_ip_scan:get_ip(Interface), PortVal };
                Val ->
                    Val
            end,
    {RateConf,SizeThresh} = case RateLimit of
                                Token=#token_bucket{} ->
                                    Thresh=lists:min([?size_mon_thresh,Token#token_bucket.burst]),
                                    {Token#token_bucket{last_packet_date=StartTime}, Thresh};
                                undefined ->
                                    {undefined, ?size_mon_thresh}
               end,
    {ok, think, #state_rcv{ port       = Server#server.port,
                            host       = Server#server.host,
                            session_id = SessionId,
                            bidi       = Bidi,
                            protocol   = Server#server.type,
                            clienttype = CType,
                            session    = CType:new_session(),
                            persistent = Persistent,
                            starttime  = StartTime,
                            dump       = Dump,
                            proto_opts = ProtoOpts,
                            size_mon   = SizeThresh,
                            size_mon_thresh = SizeThresh,
                            count      = Count,
                            ip         = NewIP,
                            id         = Id,
                            hibernate  = Hibernate,
                            maxcount   = Count,
                            rate_limit = RateConf,
                            dynvars    = DynVars
                           }}.

%%--------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%--------------------------------------------------------------------
think(next_msg,State=#state_rcv{}) ->
    ?LOG("Global ack received, continue~n", ?DEB),
    NewSocket = (State#state_rcv.protocol):set_opts(State#state_rcv.socket, [{active, once}]),
    handle_next_action(State#state_rcv{socket=NewSocket }).

wait_ack(next_msg,State=#state_rcv{request=R}) when R#ts_request.ack==global->
    NewSocket = (State#state_rcv.protocol):set_opts(State#state_rcv.socket, [{active, once}]),
    {PageTimeStamp, _, _} = update_stats(State),
    handle_next_action(State#state_rcv{socket=NewSocket,
                                       page_timestamp=PageTimeStamp});
wait_ack(timeout,State) ->
    ?LOG("Error: timeout receive in state wait_ack~n", ?ERR),
    ts_mon:add({ count, error_timeout }),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%--------------------------------------------------------------------
handle_event(Event, SName, StateData) ->
    ?LOGF("Unknown event (~p) received in state ~p, abort",[Event,SName],?ERR),
    {stop, unknown_event, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {next_state, StateName, State}          |
%%          {next_state, StateName, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% received data
handle_info({erlang, _Socket, Data}, wait_ack, State) ->
    ?DebugF("erlang function result received: size=~p ~n",[size(term_to_binary(Data))]),
    case handle_data_msg(Data, State) of
        {NewState=#state_rcv{ack_done=true}, _Opts} ->
            handle_next_action(NewState#state_rcv{ack_done=false});
        {NewState, _Opts} ->
            TimeOut = case (NewState#state_rcv.request)#ts_request.ack of
                global ->
                    (NewState#state_rcv.proto_opts)#proto_opts.global_ack_timeout;
                _ ->
                    (NewState#state_rcv.proto_opts)#proto_opts.idle_timeout
            end,
            {next_state, wait_ack, NewState, TimeOut}
    end;

handle_info(Info, StateName, State = #state_rcv{protocol = Transport, socket = Socket}) ->
    handle_info2(Transport:normalize_incomming_data(Socket, Info), StateName, State).

handle_info2({gen_ts_transport, _Socket, Data}, wait_ack, State=#state_rcv{rate_limit=TokenParam}) when is_binary(Data)->
    ?DebugF("data received: size=~p ~n",[size(Data)]),
    NewTokenParam = case TokenParam of
                        undefined ->
                            undefined;
                        #token_bucket{rate=R,burst=Burst,current_size=S0, last_packet_date=T0} ->
                            {S1,_Wait}=token_bucket(R,Burst,S0,T0,size(Data),?NOW,true),
                            TokenParam#token_bucket{current_size=S1, last_packet_date=?NOW}
                    end,
    {NewState, Opts} = handle_data_msg(Data, State),
    NewSocket = (NewState#state_rcv.protocol):set_opts(NewState#state_rcv.socket,
                                                       [{active, once} | Opts]),
    case NewState#state_rcv.ack_done of
        true ->
            handle_next_action(NewState#state_rcv{socket=NewSocket,rate_limit=NewTokenParam,
                                                  ack_done=false});
        false ->
            TimeOut = case (NewState#state_rcv.request)#ts_request.ack of
                global ->
                    (NewState#state_rcv.proto_opts)#proto_opts.global_ack_timeout;
                _ ->
                    (NewState#state_rcv.proto_opts)#proto_opts.idle_timeout
            end,
            {next_state, wait_ack, NewState#state_rcv{socket=NewSocket,rate_limit=NewTokenParam}, TimeOut}
    end;

%% inet close messages; persistent session, waiting for ack
handle_info2({gen_ts_transport, _Socket, closed}, wait_ack,
            State = #state_rcv{persistent=true})  ->
    ?LOG("connection closed while waiting for ack",?INFO),
    set_connected_status(false),
    {NewState, _Opts} = handle_data_msg(closed, State),
    %% socket should be closed in handle_data_msg
    handle_next_action(NewState#state_rcv{socket=none});

%% inet close messages; persistent session
handle_info2({gen_ts_transport, _Socket, closed}, think,
            State = #state_rcv{persistent=true})  ->
    ?LOG("connection closed, stay alive (persistent)",?INFO),
    set_connected_status(false),
    catch (State#state_rcv.protocol):close(State#state_rcv.socket), % mandatory for ssl
    {next_state, think, State#state_rcv{socket = none}};

%% inet close messages
handle_info2({gen_ts_transport, _Socket, closed}, _StateName, State) ->
    ?LOG("connection closed, abort", ?WARN),
    %% the connexion was closed after the last msg was sent, stop quietly
    ts_mon:add({ count, error_closed }),
    set_connected_status(false),
    catch (State#state_rcv.protocol):close(State#state_rcv.socket), % mandatory for ssl
    {stop, normal, State#state_rcv{socket = none}};

%% inet errors
handle_info2({gen_ts_transport, _Socket, error, Reason}, _StateName, State)  ->
    ?LOGF("Net error: ~p~n",[Reason], ?WARN),
    CountName="error_inet_"++atom_to_list(Reason),
    ts_mon:add({ count, list_to_atom(CountName) }),
    set_connected_status(false),
    {stop, normal, State};

%% timer expires, no more messages to send
handle_info2({timeout, _Ref, end_thinktime}, think, State= #state_rcv{ count=0 })  ->
    ?LOG("Session ending ~n", ?INFO),
    {stop, normal, State};

%% the timer expires
handle_info2({timeout, _Ref, end_thinktime}, think, State ) ->
    handle_next_action(State);

handle_info2(timeout, StateName, State ) ->
    ?LOGF("Error: timeout receive in state ~p~n",[StateName], ?ERR),
    ts_mon:add({ count, timeout }),
    {stop, normal, State};
% bidirectional protocol
handle_info2({gen_ts_transport, Socket, Data}, think,State=#state_rcv{
  clienttype=Type, bidi=true,host=Host,port=Port})  ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    ts_mon:add({ sum, size_rcv, size(Data)}),
    Proto = State#state_rcv.protocol,
    ?LOG("Data received from socket (bidi) in state think~n",?INFO),
    NewState = case Type:parse_bidi(Data, State) of
                   {nodata, State2} ->
                       ?LOG("Bidi: no data ~n",?DEB),
                       ts_mon:add({count, async_unknown_data_rcv}),
                       State2;
                   {Data2, State2} ->
                       ts_mon:add([{ sum, size_sent, size(Data2)},{count, async_data_sent}]),
                       ts_mon:sendmes({State#state_rcv.dump, self(), Data2}),
                       ?LOG("Bidi: send data back to server~n",?DEB),
                       send(Proto,Socket,Data2,Host,Port), %FIXME: handle errors ?
                       State2
               end,
    NewSocket = (NewState#state_rcv.protocol):set_opts(NewState#state_rcv.socket, [{active, once}]),
    {next_state, think, NewState#state_rcv{socket=NewSocket}};
% bidi is false, but parse is also false: continue even if we get data
handle_info2({gen_ts_transport, Socket, Data}, think, State = #state_rcv{request=Req} )
  when (Req#ts_request.ack /= parse) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    ts_mon:add({ sum, size_rcv, size(Data)}),
    ?LOGF("Data receive from socket in state think, ack=~p, skip~n",
          [Req#ts_request.ack],?NOTICE),
    ?DebugF("Data was ~p~n",[Data]),
    NewSocket = (State#state_rcv.protocol):set_opts(Socket, [{active, once}]),
    {next_state, think, State#state_rcv{socket=NewSocket}};
handle_info2({gen_ts_transport, _Socket, Data}, think, State) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    ts_mon:add({ count, error_unknown_data }),
    ?LOG("Data receive from socket in state think, stop~n", ?ERR),
    ?DebugF("Data was ~p~n",[Data]),
    {stop, normal, State};
%% pablo TODO:  when this could happen??
handle_info2({inet_reply, _Socket,ok}, StateName, State ) ->
    ?LOGF("inet_reply ok received in state ~p~n",[StateName],?NOTICE),
    {next_state, StateName, State};
%% TODO this would happen in mixed session when previous session was saved and
%% there are data send from the server, and handle_info will NOT normalize
%% these data as {gen_ts_transport, Socket, Data}. Ignore it currently.
handle_info2({tcp, Socket, _Data}, StateName, State ) ->
    ?LOGF("tcp data received in state ~p~n",[StateName],?NOTICE),

    %% we need a set_opts call and update the old socket to the new one,
    %% or if we switch back to the saved session, we can't receive data
    %% from the old socket.
    NewSocket = (State#state_rcv.protocol):set_opts(Socket, [{active, once}]),
    DictList = get(),
    lists:foldl(fun({Key, Value}, Acc) ->
                case {Key, Value} of
                    {{state, _}, {Socket, Session}} ->
                        put(Key, {NewSocket, Session});
                    _ -> ok
                end, Acc end, unused, DictList),
    {next_state, StateName, State};
handle_info2({tcp_closed, Socket, _Data}, StateName, State ) ->
    ?LOGF("tcp_closed received in state ~p~n",[StateName],?NOTICE),

    %% socket closed for the saved session, update the old socket to none.
    %% it's ok if we don't do that: when the old closed socket is used,
    %% tsung will aware the closed state, and do a reconnection.
    DictList = get(),
    lists:foldl(fun({Key, Value}, Acc) ->
                case {Key, Value} of
                    {{state, _}, {Socket, Session}} ->
                        put(Key, {none, Session});
                    _ -> ok
                end, Acc end, unused, DictList),
    {next_state, StateName, State};

handle_info2({ssl_closed, Socket}, StateName, State) ->
    ?LOGF("ssl_closed received in state ~p~n", [StateName], ?NOTICE),

    % set old socket to none, like when receiving tcp_closed
    DictList = get(),
    lists:foldl(fun({Key, Value}, Acc) ->
                case {Key, Value} of
                    {{state, _}, {Socket, Session}} ->
                        put(Key, {none, Session});
                    _ -> ok
                end, Acc end, unused, DictList),
    {next_state, StateName, State};

handle_info2(Msg, StateName, State ) ->
    ?LOGF("Error: Unknown msg ~p receive in state ~p, stop~n", [Msg,StateName], ?ERR),
    ts_mon:add({ count, error_unknown_msg }),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%--------------------------------------------------------------------
terminate(normal, _StateName,State) ->
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
code_change(_OldVsn, StateName, StateData, _Extra) ->
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
handle_next_action(State=#state_rcv{dynvars = DynVars}) ->
    Count = State#state_rcv.count-1,
    case set_profile(State#state_rcv.maxcount,State#state_rcv.count,State#state_rcv.session_id) of
        {thinktime, TmpThink} ->
            Think = case TmpThink of
                        "%%"++_Tail ->
                            Raw=ts_search:subst(TmpThink,DynVars),
                            round(ts_utils:list_to_number(Raw)*1000);
                        Val ->
                            Val
                    end,
            ?DebugF("Starting new thinktime ~p~n", [Think]),
            case (set_thinktime(Think) >= State#state_rcv.hibernate) of
                true ->
                    {next_state, think, State#state_rcv{count=Count},hibernate};
                _ ->
                    {next_state, think, State#state_rcv{count=Count}}
            end;
        {transaction, start, Tname} ->
            Now = ?NOW,
            ?LOGF("Starting new transaction ~p (now~p)~n", [Tname,Now], ?INFO),
            TrList = State#state_rcv.transactions,
            NewState = State#state_rcv{transactions=[{Tname,Now}|TrList],
                                   count=Count},
            handle_next_action(NewState);
        {transaction, stop, Tname} ->
            Now = ?NOW,
            ?LOGF("Stopping transaction ~p (~p)~n", [Tname, Now], ?INFO),
            TrList = State#state_rcv.transactions,
            {value, {_, Tr}} = lists:keysearch(Tname, 1, TrList),
            Elapsed = ts_utils:elapsed(Tr, Now),
            ts_mon:add({sample, Tname, Elapsed}),
            NewState = State#state_rcv{transactions=lists:keydelete(Tname,1,TrList),
                                   count=Count},
            handle_next_action(NewState);
        {setdynvars,SourceType,Args,VarNames} ->
            DynVars=State#state_rcv.dynvars,
            Result = set_dynvars(SourceType,Args,VarNames,DynVars,{State#state_rcv.host,State#state_rcv.port},State#state_rcv.session),
            NewDynVars = ts_dynvars:set(VarNames,Result,DynVars),
            ?DebugF("set dynvars: ~p ~n",[NewDynVars]),
            handle_next_action(State#state_rcv{dynvars = NewDynVars, count=Count});
        {ctrl_struct,CtrlData} ->
            ctrl_struct(CtrlData,State,Count);
        Request=#ts_request{} ->
            handle_next_request(Request, State);
        {change_type, NewCType, Server, Port, PType, Store, Restore, Bidi} ->
            ?DebugF("Change client type, use: ~p ~p~n",[NewCType, [Server , Port, PType, Store, Restore]]),
            NewPort   = case ts_search:subst(Port,DynVars) of
                            I when is_integer(I) ->
                                I;
                            S when is_list(S) ->
                                list_to_integer(S)
                        end,
            NewServer = ts_search:subst(Server,DynVars),
            case Store of
                true -> % keep state
                    put({state, State#state_rcv.clienttype} , {State#state_rcv.socket,State#state_rcv.session});
                false -> % don't keep state of old type, close connection
                    (State#state_rcv.protocol):close(State#state_rcv.socket),
                    set_connected_status(false)
            end,
            {Socket,Session} = case {Restore, get({state,NewCType})} of
                     {true,{OldSocket, OldSession}} -> % restore is true and we have something stored
                         {OldSocket, OldSession};
                     {_,_} -> % nothing to restore, or no restore asked, set new session
                         {none,NewCType:new_session()}
                 end,
            NewState=State#state_rcv{session=Session,socket=Socket,count=Count,clienttype=NewCType,protocol=PType,
                                     port=NewPort,host=NewServer,bidi=Bidi},
            handle_next_action(NewState);
        {set_option, undefined, rate_limit, {Rate, Burst}} ->
            ?LOGF("Set rate limits for client: rate=~p, burst=~p~n",[Rate,Burst],?DEB),
            RateConf=#token_bucket{rate=Rate,burst=Burst,last_packet_date=?NOW},
            Thresh=lists:min([Burst,State#state_rcv.size_mon_thresh]),
            handle_next_action(State#state_rcv{size_mon=Thresh,size_mon_thresh=Thresh,rate_limit=RateConf,count=Count});
        {set_option, undefined, certificate, {Cacert, KeyFile, KeyPass, CertFile}} ->
            ?LOGF("Set client certificate: ~p ~p ~p ~p~n",[Cacert, KeyFile, KeyPass, CertFile],?DEB),
            Opts = ts_utils:filtermap(fun({N,V}) ->
                                           case V of
                                               undefined ->
                                                   false;
                                               B when is_binary(B)->
                                                   {true, {N,ts_search:subst(binary_to_list(B), DynVars)}};
                                               Val ->
                                                   {true, {N,ts_search:subst(Val, DynVars)}}
                                           end
                                   end ,
                                   [{certfile, CertFile},
                                    {keyfile,KeyFile},
                                    {password,KeyPass},
                                    {cacertfile,Cacert}]),
            ?LOGF("SSL options for certificate: ~p~n",[Opts],?DEB),
            OldOpts = State#state_rcv.proto_opts,
            NewOpts = OldOpts#proto_opts{certificate = Opts},
            handle_next_action(State#state_rcv{proto_opts=NewOpts,count=Count});
        {set_option, undefined, connect_timeout, {ConnectTimeout}} ->
            ?LOGF("Set connect timeout: ~p~n", [ConnectTimeout], ?DEB),
            OldOpts = State#state_rcv.proto_opts,
            NewOpts = OldOpts#proto_opts{connect_timeout = ConnectTimeout},
            handle_next_action(State#state_rcv{proto_opts=NewOpts, count=Count});
        {set_option, Type, Name, Args} ->
            NewState=Type:set_option(Name,Args,State),
            handle_next_action(NewState);
        {interaction, send, Id} ->
            ts_interaction_server:send({ts_search:subst(Id, DynVars),?NOW}),
            handle_next_action(State#state_rcv{count=Count});
        {interaction, 'receive', Id} ->
            ts_interaction_server:rcv({ts_search:subst(Id, DynVars),?NOW}),
            handle_next_action(State#state_rcv{count=Count});
        Other ->
            ?LOGF("Error: set profile return value is ~p (count=~p)~n",[Other,Count],?ERR),
            {stop, set_profile_error, State}
    end.


%%----------------------------------------------------------------------
%% @spec set_dynvars (Type::erlang|random|urandom|value|file, Args::tuple(),
%%                    Variables::list(), DynVars::#dynvars{},
%%                    {Server::string(),Port::integer()}, Session::record()) -> integer()|binary()|list()
%% @doc setting the value of several dynamic variables at once.
%% @end
%%----------------------------------------------------------------------
set_dynvars(erlang,{Module,Callback},_Vars,DynVars,_,Session) ->
    Module:Callback({Session,DynVars});
set_dynvars(code,Fun,_Vars,DynVars,_,Session) ->
    Fun({Session,DynVars});
set_dynvars(random,{number,Start,End},Vars,_DynVars,_,_) ->
    lists:map(fun(_) -> ts_stats:uniform(Start,End) end,Vars);
set_dynvars(random,{string,Length},Vars,_DynVars,_,_) ->
    R = fun(_) -> ts_utils:randombinstr(Length) end,
    lists:map(R,Vars);
set_dynvars(urandom,{string,Length},Vars,_DynVars,_,_) ->
    %% not random, but much faster
    R = fun(_) -> ts_utils:urandombinstr(Length) end,
    lists:map(R,Vars);
set_dynvars(value,{string,Value},_Vars,_DynVars,_,_) ->
    [Value];
set_dynvars(file,{random,FileId,Delimiter},_Vars,_DynVars,_,_) ->
    {ok,Line} = ts_file_server:get_random_line(FileId),
    ts_utils:split(Line,Delimiter);
set_dynvars(file,{iter,FileId,Delimiter},_Vars,_DynVars,_,_) ->
    {ok,Line} = ts_file_server:get_next_line(FileId),
    ts_utils:split(Line,Delimiter);
set_dynvars(jsonpath,{JSONPath, From},_Vars,DynVars,_,_) ->
    {ok, Val} = ts_dynvars:lookup(From,DynVars),
    JSON=mochijson2:decode(Val),
    case ts_utils:jsonpath(JSONPath, JSON) of
        undefined -> << >>;
        {struct,S}->  iolist_to_binary(mochijson2:encode({struct,S}));
        V         -> V
    end;
set_dynvars(server,_,_,_,{Host,Port},_) ->
    [Host,Port].


%% @spec ctrl_struct(CtrlData::term(),State::#state_rcv{},Count::integer) ->
%%          {next_state, NextStateName::atom(), NextState::#state_rcv{}} |
%%          {next_state, NextStateName::atom(), NextState::#state_rcv{},
%%                       Timeout::integer() | infinity} |
%%          {stop, Reason::term(), NewState::#state_rcv{}}
%% @doc Common code for flow control actions (repeat,for)
%%      Count is the next action-id, if this action doesn't result
%%      in a jump to another place
%% @end
ctrl_struct(CtrlData,State,Count) ->
    case ctrl_struct_impl(CtrlData,State#state_rcv.dynvars) of
        {next,NewDynVars} ->
            handle_next_action(State#state_rcv{dynvars=NewDynVars,count=Count});
        {jump,Target,NewDynVars} ->
            %%UGLY HACK:
            %% because set_profile/3 works by counting down starting at maxcount,
            %% we need to calculate the correct value to actually make a jump to
            %% the desired target.
            %% In set_profile/3, actionId = MaxCount-Count+1 =>
            %% Count = MaxCount-Target +1
            Next = State#state_rcv.maxcount - Target + 1,
            handle_next_action(State#state_rcv{dynvars=NewDynVars,count=Next})
    end.


%%----------------------------------------------------------------------
%% @spec ctrl_struct_impl(ControlStruct::term(),DynVars::#dynvars{}) ->
%%        {next,NewDynVars::#dynvars{}} |
%%        {jump, Target::integer(), NewDynVars::#dynvars{}}
%% @doc return {next,NewDynVars} to continue with the sequential flow,
%%             {jump,Target,NewDynVars} to jump to action number 'Target'
%% @end
%%----------------------------------------------------------------------
ctrl_struct_impl({for_start,Init="%%_"++_,VarName},DynVars) ->
    InitialValue = list_to_integer(ts_search:subst(Init, DynVars)),
    ?LOGF("Initial value of FOR loop is dynamic: ~p",[InitialValue],?DEB),
    ctrl_struct_impl({for_start,InitialValue,VarName},DynVars);
ctrl_struct_impl({for_start,InitialValue,VarName},DynVars) ->
    NewDynVars = ts_dynvars:set(VarName,InitialValue,DynVars),
    {next,NewDynVars};
ctrl_struct_impl({for_end,VarName,End="%%_"++_,Increment,Target},DynVars) ->
    %% end value is a dynamic variable
    EndValue = list_to_integer(ts_search:subst(End, DynVars)),
    ?LOGF("End value of FOR loop is dynamic: ~p",[EndValue],?DEB),
    ctrl_struct_impl({for_end,VarName,EndValue,Increment,Target},DynVars);
ctrl_struct_impl({for_end,VarName,EndValue,Increment,Target},DynVars) ->
    case ts_dynvars:lookup(VarName,DynVars) of
        {ok,Value}  when Value >= EndValue -> % Reach final value, end loop
            {next,DynVars};
        {ok,Value} ->  % New iteration
            NewValue = Value + Increment,
            NewDynVars = ts_dynvars:set(VarName,NewValue,DynVars),
            {jump,Target,NewDynVars}
    end;


ctrl_struct_impl({if_start,Rel, VarName, Value, Target},DynVars) ->
    case ts_dynvars:lookup(VarName,DynVars) of
        {ok,VarValue} ->
            ?DebugF("If found ~p; value is ~p~n",[VarName,VarValue]),
            ?DebugF("Calling need_jump with args ~p ~p ~p~n",[Rel,Value,VarValue]),
            Jump = need_jump('if',rel(Rel,Value,VarValue)),
            jump_if(Jump,Target,DynVars);
        false ->
            ts_mon:add({ count, 'error_if_undef'}),
            {next,DynVars}
    end;

ctrl_struct_impl({repeat,RepeatName, _,_,_,_,_,_},[]) ->
    Msg= list_to_atom("error_repeat_"++atom_to_list(RepeatName)++"_undef"),
    ts_mon:add({ count, Msg}),
    {next,[]};
ctrl_struct_impl({repeat,RepeatName, While,Rel,VarName,Value,Target,Max},DynVars) ->
    Iteration = case ts_dynvars:lookup(RepeatName,DynVars) of
                    {ok,Val} -> Val;
                    false ->  1
                end,
    ?DebugF("Repeat (name=~p) iteration: ~p~n",[RepeatName,Iteration]),
    case Iteration > Max of
        true ->
            ?LOGF("Max repeat (name=~p) reached ~p~n",[VarName,Iteration],?NOTICE),
            ts_mon:add({ count, max_repeat}),
            {next,DynVars};
        false ->
            case ts_dynvars:lookup(VarName,DynVars) of
                {ok,VarValue} ->
                    ?DebugF("Repeat (name=~p) found; value is ~p~n",[VarName,VarValue]),
                    ?DebugF("Calling need_jump with args ~p ~p ~p ~p~n",[While,Rel,Value,VarValue]),
                    Jump = need_jump(While,rel(Rel,Value,VarValue)),
                    NewValue = 1 + Iteration,
                    NewDynVars = ts_dynvars:set(RepeatName,NewValue,DynVars),
                    jump_if(Jump,Target,NewDynVars);
                false ->
                    Msg= list_to_atom("error_repeat_"++atom_to_list(RepeatName)++"undef"),
                    ts_mon:add({ count, Msg}),
                    {next,DynVars}
            end
    end;

ctrl_struct_impl({foreach_start,ForEachName,VarName,Filter}, DynVars) ->
    case filter(ts_dynvars:lookup(VarName,DynVars),Filter) of
        false ->
            Msg= list_to_atom("error_foreach_"++atom_to_list(VarName)++"undef"),
            ts_mon:add({ count, Msg}),
            {next,DynVars};
        [First|_Tail] ->
            TmpDynVars = ts_dynvars:set(ForEachName,First,DynVars),
            NewDynVars = ts_dynvars:set(ts_utils:concat_atoms([ForEachName,'_iter']),2,TmpDynVars),
            {next,NewDynVars};
        [] ->
            ?LOGF("empty list for ~p (filter is ~p)",[VarName, Filter],?WARN),
            NewDynVars = ts_dynvars:set(ts_utils:concat_atoms([ForEachName,'_iter']),1,DynVars),
            {next,NewDynVars};
        VarValue ->
            ?LOGF("foreach warn:~p is not a list (~p), can't iterate",[VarName, VarValue],?WARN),
            NewDynVars = ts_dynvars:set(ForEachName,VarValue,DynVars),
            {next,NewDynVars}
    end;

ctrl_struct_impl({foreach_end,ForEachName,VarName,Filter,Target}, DynVars) ->
    IterName=ts_utils:concat_atoms([ForEachName,'_iter']),
    {ok,Iteration} = ts_dynvars:lookup(IterName,DynVars),
    ?DebugF("Foreach (var=~p) iteration: ~p~n",[VarName,Iteration]),
    case filter(ts_dynvars:lookup(VarName,DynVars),Filter) of
        false ->
            Msg= list_to_atom("error_foreach_"++atom_to_list(VarName)++"undef"),
            ts_mon:add({ count, Msg}),
            {next,DynVars};
        VarValue when is_list(VarValue)->
            ?DebugF("Foreach list found; value is ~p~n",[VarValue]),
            case catch lists:nth(Iteration,VarValue) of
                {'EXIT',_} -> % out of bounds, exit foreach loop
                    ?LOGF("foreach ~p: last iteration done",[ForEachName],?DEB),
                    {next,DynVars};
                Val ->
                    TmpDynVars = ts_dynvars:set(ForEachName,Val,DynVars),
                    NewDynVars = ts_dynvars:set(IterName,Iteration+1,TmpDynVars),
                    {jump, Target ,NewDynVars}
            end;
        _ ->% not a list, don't loop
            {next,DynVars}
    end.


%%
rel(R,A,B) when is_integer(B) and not is_integer(A)->
    rel(R,A,list_to_binary(integer_to_list(B)));
rel(R,A,B) when is_integer(A) and not is_integer(B)->
    rel(R,list_to_binary(integer_to_list(A)),B);
rel(R,A,B) when is_list(B) ->
    rel(R,A,list_to_binary(B));
rel(R,A,B) when is_list(A) ->
    rel(R,list_to_binary(A),B);
rel(R,A,B) when is_atom(A) ->
    rel(R,atom_to_binary(A,utf8),B);
rel(R,A,B) when is_atom(B) ->
    rel(R,A,atom_to_binary(B,utf8));
rel('eq',A,B) ->
    A == B;
rel('neq',A,B) ->
    A /= B;
rel('gt',A,B) ->
    binary_to_num(B) > binary_to_num(A);
rel('lt',A,B) ->
    binary_to_num(B) < binary_to_num(A);
rel('gte',A,B) ->
    binary_to_num(B) >= binary_to_num(A);
rel('lte',A,B) ->
    binary_to_num(B) =< binary_to_num(A).

need_jump('while',F) -> F;
need_jump('until',F) -> not F;
need_jump('if',F) -> not F.

jump_if(true,Target,DynVars)   -> {jump,Target,DynVars};
jump_if(false,_Target,DynVars) -> {next,DynVars}.

binary_to_num([H|_T]) ->
    binary_to_num(H);
binary_to_num(Value) ->
    case (catch list_to_float(binary_to_list(Value))) of
        {'EXIT', _} -> list_to_integer(binary_to_list(Value));
        Float -> Float
    end.

%%----------------------------------------------------------------------
%% Func: handle_next_request/2
%% Args: Request, State
%%----------------------------------------------------------------------
handle_next_request(Request, State) ->
    Count = State#state_rcv.count-1,
    Type  = State#state_rcv.clienttype,
    {PrevHost, PrevPort, PrevProto} = case Request of
        #ts_request{host=undefined, port=undefined, scheme=undefined} ->
            %% host/port/scheme not defined in request, use the current ones.
            {State#state_rcv.host,State#state_rcv.port, State#state_rcv.protocol};
        #ts_request{host=H1, port=P1, scheme=S1} ->
            {H1,P1,S1}
    end,

    {Param, {Host,Port,Protocol}} =
        case Type:add_dynparams(Request#ts_request.subst,
                                {State#state_rcv.dynvars, State#state_rcv.session},
                                Request#ts_request.param,
                                {PrevHost, PrevPort, PrevProto}) of
            {Par, NewServer} -> % substitution has changed server setup
                ?DebugF("Dynparam, new server:  ~p~n",[NewServer]),
                {Par, NewServer};
            P ->
                {P, {PrevHost, PrevPort, PrevProto}}
        end,
    %% need to reconnect if the server/port/scheme has changed
    Socket = case {State#state_rcv.host,State#state_rcv.port,State#state_rcv.protocol, State#state_rcv.socket} of
                 {Host, Port, Protocol, _} -> % server setup unchanged
                     State#state_rcv.socket;
                 {_,_,_, none} ->
                     ?Debug("Change server configuration inside a session. Socket not opened~n"),
                     set_connected_status(false),
                     none;
                 _ ->
                     ?Debug("Change server configuration inside a session ~n"),
                     (State#state_rcv.protocol):close(State#state_rcv.socket),
                     set_connected_status(false),
                     none
             end,

    {Message, NewSession} = Type:get_message(Param,State),
    Now = ?NOW,

    %% reconnect if needed
    ProtoOpts = State#state_rcv.proto_opts,
    case reconnect(Socket,Host,Port,{Protocol,ProtoOpts},State#state_rcv.ip) of
        {ok, NewSocket} ->
            case catch send(Protocol, NewSocket, Message, Host, Port) of
                ok ->
                    PageTimeStamp = case State#state_rcv.page_timestamp of
                                        0 -> Now; %first request of a page
                                        _ -> %page already started
                                            State#state_rcv.page_timestamp
                                    end,
                    ts_mon:add({ sum, size_sent, size_msg(Message)}),
                    ts_mon:sendmes({State#state_rcv.dump, self(), Message}),
                    NewState = State#state_rcv{socket   = NewSocket,
                                               protocol = Protocol,
                                               host     = Host,
                                               request  = Request,
                                               port     = Port,
                                               count    = Count,
                                               session  = NewSession,
                                               proto_opts = ProtoOpts#proto_opts{is_first_connect = false},
                                               page_timestamp= PageTimeStamp,
                                               send_timestamp= Now,
                                               timestamp= Now },
                    case Request#ts_request.ack of
                        no_ack ->
                            {PTimeStamp, _} = update_stats_noack(NewState),
                            handle_next_action(NewState#state_rcv{ack_done=true, page_timestamp=PTimeStamp});
                        global ->
                            ts_timer:connected(self()),
                            {next_state, wait_ack, NewState};
                        _ ->
                            {next_state, wait_ack, NewState}
                        end;
                {error, closed} when State#state_rcv.retries < ProtoOpts#proto_opts.max_retries ->
                    ?LOG("connection close while sending message!~n", ?NOTICE),
                    ts_mon:add({ count, error_connection_closed }),
                    Retries = State#state_rcv.retries +1,
                    handle_close_while_sending(State#state_rcv{socket=NewSocket,
                                                               protocol=Protocol,
                                                               host=Host,
                                                               session=NewSession,
                                                               retries=Retries,
                                                               port=Port});
                {error, Reason}  when State#state_rcv.retries < ProtoOpts#proto_opts.max_retries ->
                    %% LOG only at INFO level since we report also an error to ts_mon
                    ?LOGF("Error: Unable to send data, reason: ~p~n",[Reason],?INFO),
                    CountName="error_send_"++atom_to_list(Reason),
                    ts_mon:add({ count, list_to_atom(CountName) }),
                    Retries = State#state_rcv.retries +1,
                    handle_timeout_while_sending(State#state_rcv{session=NewSession,retries=Retries});
                {'EXIT', {noproc, _Rest}}  when State#state_rcv.retries < ProtoOpts#proto_opts.max_retries ->
                    ?LOG("EXIT from ssl app while sending message !~n", ?WARN),
                    Retries = State#state_rcv.retries +1,
                    handle_close_while_sending(State#state_rcv{socket=NewSocket,
                                                               protocol=Protocol,
                                                               session=NewSession,
                                                               retries=Retries,
                                                               host=Host,
                                                               port=Port});
                Exit when State#state_rcv.retries < ProtoOpts#proto_opts.max_retries ->
                    ?LOGF("EXIT Error: Unable to send data, reason: ~p~n", [Exit], ?ERR),
                    ts_mon:add({ count, error_send }),
                    {stop, normal, State};
                _Exit ->
                    ?LOGF("EXIT Error: Unable to send data, max_retries reached; reason: ~p~n", [_Exit], ?ERR),
                    ts_mon:add({ count, error_abort_max_send_retries }),
                    {stop, normal, State}
            end;

        {error, timeout} when State#state_rcv.retries < ProtoOpts#proto_opts.max_retries ->
            ts_mon:add({count, error_connect_timeout}),

            handle_reconnect_issue(State#state_rcv{session=NewSession});

        {error, _Reason} when State#state_rcv.retries < ProtoOpts#proto_opts.max_retries ->
            handle_reconnect_issue(State#state_rcv{session=NewSession});

        {error, Reason} ->
            case Reason of
                timeout ->
                    ts_mon:add({count, error_connect_timeout});
                closed ->
                    ts_mon:add({count, error_connect_closed});
                _ ->
                    ok
            end,
            ts_mon:add({ count, error_abort_max_conn_retries }),
            {stop, normal, State}
    end.


%% @spec size_msg(Data::term) -> integer()
size_msg(Data) when is_binary(Data) ->
     size(Data);
size_msg({_Mod,_Fun,_Args,Size}) -> Size.

%%----------------------------------------------------------------------
%% Func: finish_session/1
%% Args: State
%%----------------------------------------------------------------------
finish_session(State) ->
    Now = ?NOW,
    (State#state_rcv.protocol):close(State#state_rcv.socket),
    set_connected_status(false),
    Elapsed = ts_utils:elapsed(State#state_rcv.starttime, Now),
    case State#state_rcv.transactions of
        [] -> % no pending transactions, do nothing
            ok;
        TrList -> % pending transactions (an error has probably occured)
            ?LOGF("Pending transactions: ~p, compute transaction time~n",[TrList],?NOTICE),
            lists:foreach(fun({Tname,StartTime}) ->
                                  ts_mon:add({sample,Tname,ts_utils:elapsed(StartTime,Now)})
                          end,
                          TrList)
    end,
    ts_mon:endclient({State#state_rcv.id, Now, Elapsed}).


%%----------------------------------------------------------------------
%% Func: handle_reconnect_issue/1
%% Args: State
%% Purpose: there was an issue (re)opening the connection. Retry with
%%          backoff in a moment.
%%----------------------------------------------------------------------
handle_reconnect_issue(#state_rcv{proto_opts = PO} = State) ->
    Retries = State#state_rcv.retries + 1,

    % simplified exponential backoff algorithm: we increase
    % the timeout when the number of retries increase, with a
    % simple rule: number of retries * retry_timeout
    set_thinktime(PO#proto_opts.retry_timeout * Retries),
    {next_state, think, State#state_rcv{retries=Retries}}.


%%----------------------------------------------------------------------
%% Func: handle_close_while_sending/1
%% Args: State
%% Purpose: the connection has just be closed a few msec before we
%%          send a message, restart in a few moment (this time we will
%%          reconnect before sending)
%%----------------------------------------------------------------------
handle_close_while_sending(State=#state_rcv{persistent = true,
                                            protocol   = Proto,
                                            proto_opts = PO})->
    Proto:close(State#state_rcv.socket),
    set_connected_status(false),
    Think = PO#proto_opts.retry_timeout,
    %%FIXME: report the error to ts_mon ?
    ?LOGF("Server must have closed connection upon us, waiting ~p msec~n",
          [Think], ?NOTICE),
    set_thinktime(Think),
    {next_state, think, State#state_rcv{socket=none}};
handle_close_while_sending(State) ->
    {stop, error, State}.


%%----------------------------------------------------------------------
%% Func: handle_timeout_while_sending/1
%% Args: State
%% Purpose: retry if a timeout occurs during a send
%%----------------------------------------------------------------------
handle_timeout_while_sending(State=#state_rcv{persistent = true,
                                              proto_opts = PO})->
    Think = PO#proto_opts.retry_timeout,
    set_thinktime(Think),
    {next_state, think, State};
handle_timeout_while_sending(State) ->
    ?LOG("Not persistent, abort client because of send timeout~n", ?INFO),
    {stop, normal, State}.


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
reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,0}) ->
    reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,0,0});
reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,CPort, Try}) when is_integer(CPort)->
    ?DebugF("Try to (re)connect to: ~p:~p from ~p using protocol ~p~n",
            [ServerName,Port,IP,Protocol]),
    Opts = protocol_options(Protocol, Proto_opts)  ++ socket_opts(IP, CPort, Protocol),
    Before= ?NOW,
    case connect(Protocol, ServerName, Port, Opts, Proto_opts#proto_opts.connect_timeout) of
        {ok, Socket} ->
            Elapsed = ts_utils:elapsed(Before, ?NOW),
            ts_mon:add({ sample, connect, Elapsed }),
            set_connected_status(true),
            ?Debug("(Re)connected~n"),
            {ok, Socket};
        {error, timeout} ->
            % don't handle connect timeouts at this level
            {error, timeout};
        {error, Reason} ->
            {A,B,C,D} = IP,
            %% LOG only at INFO level since we report also an error to ts_mon
            ?LOGF("(Re)connect from ~p.~p.~p.~p:~p to ~s:~p, Error: ~p~n",
                  [A,B,C,D, CPort, ServerName, Port , Reason],?INFO),
            case {Reason,CPort,Try}  of
                {eaddrinuse, Val,CPortServer} when Val == 0; CPortServer == undefined ->
                    %% already retry once, don't try again.
                    ts_mon:add({ count, error_connect_eaddrinuse });
                {eaddrinuse, Val,CPortServer} when Val > 0 ->
                    %% retry once when tsung allocates port number
                    NewCPort = case catch ts_cport:get_port(CPortServer,IP) of
                        Data when is_integer(Data) ->
                            Data;
                        Error ->
                            ?LOGF("CPort error (~p), reuse the same port ~p~n",[Error,CPort],?INFO),
                            CPort
                    end,
                    ?LOGF("Connect failed with client port ~p, retry with ~p~n",[CPort, NewCPort],?INFO),
                    reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,NewCPort, undefined});
                _ ->
                    CountName="error_connect_"++atom_to_list(Reason),
                    ts_mon:add({ count, list_to_atom(CountName) })
            end,
            {error, Reason}
    end;
reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,CPortServer}) ->
    CPort = case catch ts_cport:get_port(CPortServer,IP) of
                   Data when is_integer(Data) ->
                       Data;
                   Error ->
                       ?LOGF("CPort error (~p), use random port~n",[Error],?INFO),
                       0
               end,
    reconnect(none, ServerName, Port, {Protocol, Proto_opts}, {IP,CPort,CPortServer});
reconnect(Socket, _Server, _Port, _Protocol, _IP) ->
    {ok, Socket}.


%% set options for local socket ip/ports
socket_opts({0,0,0,0}, CPort, Proto) when Proto==ts_tcp6 orelse Proto==ts_ssl6 orelse Proto==ts_udp6 ->
    %% the config server was not aware if we are using ipv6 or ipv4,
    %% and it set the local IP to be default one; we need to change it
    %% for ipv6
    [{ip, {0,0,0,0,0,0,0,0}},{port,CPort}];
socket_opts(IP, CPort, _)->
    [{ip, IP},{port,CPort}].

%%----------------------------------------------------------------------
%% Func: send/5
%% Purpose: wrapper function for send
%% Return: ok | {error, Reason}
%%----------------------------------------------------------------------
send(Proto, Socket, Message, Host, Port)  ->
    Proto:send(Socket, Message, [{host, Host}, {port, Port}]).

connect(Proto, Server, Port, Opts, ConnectTimeout) ->
    ?LOGF("connect to port ~p",[Port],?DEB),
    Proto:connect(Server, Port, Opts, ConnectTimeout).

%%----------------------------------------------------------------------
%% Func: protocol_options/1
%% Purpose: set connection's options for the given protocol
%%----------------------------------------------------------------------
protocol_options(Proto, #proto_opts{} = ProtoOpts) ->
    Proto:protocol_options(ProtoOpts).

%%----------------------------------------------------------------------
%% Func: set_thinktime/1
%% Purpose: set a timer for thinktime if it is not infinite
%%          returns the chosen thinktime in msec
%%----------------------------------------------------------------------
set_thinktime(infinity) -> infinity;
set_thinktime(wait_global) ->
    ts_timer:connected(self()),
    infinity;
set_thinktime({random, Think}) ->
    set_thinktime(round(ts_stats:exponential(1/Think)));
set_thinktime({range, Min, Max}) ->
    set_thinktime(ts_stats:uniform(Min,Max));
set_thinktime(Think) ->
%% dot not use timer:send_after because it does not scale well:
%% http://www.erlang.org/ml-archive/erlang-questions/200202/msg00024.html
    ?DebugF("thinktime of ~p~n",[Think]),
    erlang:start_timer(Think, self(), end_thinktime ),
    Think.


%%----------------------------------------------------------------------
%% Func: handle_data_msg/2
%% Args: Data (binary), State ('state_rcv' record)
%% Returns: {NewState ('state_rcv' record), Socket options (list)}
%% Purpose: handle data received from a socket
%%----------------------------------------------------------------------
handle_data_msg(Data, State=#state_rcv{request=Req}) when Req#ts_request.ack==no_ack->
    ?Debug("data received while previous msg was no_ack~n"),
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    {State, []};

handle_data_msg(Data,State=#state_rcv{dump=Dump,request=Req,id=Id,clienttype=Type,maxcount=MaxCount,transactions=Transactions})
  when Req#ts_request.ack==parse->
    ts_mon:rcvmes({Dump, self(), Data}),

    {NewState, Opts, Close} = Type:parse(Data, State),
    NewBuffer=set_new_buffer(NewState, Data),

    ?DebugF("Session and dynvars are now ~p ~p~n",[NewState#state_rcv.session, NewState#state_rcv.dynvars]),
    case NewState#state_rcv.ack_done of
        true ->
            ?DebugF("Response done:~p~n", [NewState#state_rcv.datasize]),
            {PageTimeStamp, DynVars,Elapsed} = update_stats(NewState#state_rcv{buffer=NewBuffer}),
            MatchArgs={NewState#state_rcv.count, MaxCount,
                       NewState#state_rcv.session_id, Id},
            NewDynVars=ts_dynvars:merge(DynVars,NewState#state_rcv.dynvars),
            NewCount  =ts_search:match(Req#ts_request.match,NewBuffer,MatchArgs,NewDynVars,Transactions),
            Type:dump(Dump,{Req,NewState#state_rcv.session,Id,
                            NewState#state_rcv.host,NewState#state_rcv.datasize,Elapsed,Transactions}),

            case Close of
                true ->
                    ?Debug("Close connection required by protocol~n"),
                    (State#state_rcv.protocol):close(State#state_rcv.socket),
                    set_connected_status(false),
                    {NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                         socket = none,
                                         datasize = 0,
                                         size_mon = State#state_rcv.size_mon_thresh,
                                         count = NewCount,
                                         dynvars = NewDynVars,
                                         buffer = <<>>}, Opts};
                false ->
                    {NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                         count = NewCount,
                                         size_mon = State#state_rcv.size_mon_thresh,
                                         datasize = 0,
                                         dynvars = NewDynVars,
                                         buffer = <<>>}, Opts}
            end;
        _ ->
            ?DebugF("Response: continue:~p~n",[NewState#state_rcv.datasize]),
            %% For size_rcv stats, we don't want to update this stats
            %% for every packet received (ts_mon will be overloaded),
            %% so we will update the stats at the end of the
            %% request. But this is a problem with very big response
            %% (several megabytes for ex.), because it will create
            %% artificial spikes in the stats (O B/sec for a long time
            %% and lot's of MB/s at the end of the req). So we update
            %% the stats each time a 512Ko threshold is raised.
            case NewState#state_rcv.datasize > NewState#state_rcv.size_mon of
                true ->
                    ?Debug("Threshold raised, update size_rcv stats~n"),
                    ts_mon:add({ sum, size_rcv, NewState#state_rcv.size_mon_thresh}),
                    NewThresh=NewState#state_rcv.size_mon+ NewState#state_rcv.size_mon_thresh,
                    {NewState#state_rcv{buffer=NewBuffer,size_mon=NewThresh}, Opts};
                false->
                    {NewState#state_rcv{buffer=NewBuffer}, Opts}
            end
    end;

handle_data_msg(closed,State) ->
    {State,[]};

%% ack = global
handle_data_msg(Data,State=#state_rcv{request=Req,datasize=OldSize})
  when Req#ts_request.ack==global ->
    %% FIXME: we do not report size now (but after receiving the
    %% global ack), the size stats may be not very accurate.
    %% FIXME: should we set buffer and parse for dynvars ?
    DataSize = size(Data),
    {State#state_rcv{ datasize = OldSize + DataSize},[]};

%% local ack, special case for jabber: skip keepalive msg (single space char)
handle_data_msg(<<32>>, State=#state_rcv{clienttype=ts_jabber}) ->
    {State#state_rcv{ack_done = false},[]};
%% local ack, set ack_done to true
handle_data_msg(Data, State=#state_rcv{request=Req,maxcount=MaxCount,transactions=Transactions}) ->
    ts_mon:rcvmes({State#state_rcv.dump, self(), Data}),
    NewBuffer = set_new_buffer(State, Data),
    DataSize  = size(Data),
    {PageTimeStamp, DynVars,_} = update_stats(State#state_rcv{datasize=DataSize,buffer=NewBuffer}),
    MatchArgs = {State#state_rcv.count,MaxCount,State#state_rcv.session_id,State#state_rcv.id},
    NewDynVars= ts_dynvars:merge(DynVars,State#state_rcv.dynvars),
    NewCount  = ts_search:match(Req#ts_request.match,NewBuffer,MatchArgs,NewDynVars,Transactions),
    {State#state_rcv{ack_done = true, buffer= NewBuffer, dynvars = NewDynVars,
                     page_timestamp= PageTimeStamp, count=NewCount},[]}.


%%----------------------------------------------------------------------
%% Func: set_new_buffer/3
%%----------------------------------------------------------------------
set_new_buffer(#state_rcv{request = #ts_request{match=[], dynvar_specs=[]}} ,_) ->
    << >>;
set_new_buffer(#state_rcv{clienttype=Type, buffer=Buffer, session=Session},closed) ->
    Type:decode_buffer(Buffer,Session);
set_new_buffer(#state_rcv{buffer=OldBuffer,ack_done=false},Data) ->
    ?Debug("Bufferize response~n"),
    << OldBuffer/binary, Data/binary >>;
set_new_buffer(#state_rcv{clienttype=Type, buffer=OldBuffer, session=Session},Data) when is_binary(Data) ->
    ?Debug("decode response~n"),
    Type:decode_buffer(<< OldBuffer/binary, Data/binary >>, Session);
set_new_buffer(#state_rcv{clienttype=Type, buffer=OldBuffer, session=Session}, {_M,_F,_A, Res}) when is_list(Res)->
    %% erlang fun case
    Data=list_to_binary(Res),
    Type:decode_buffer(<< OldBuffer/binary, Data/binary >>, Session);
set_new_buffer(_State, Data) -> % useful ?
    Data.

%%----------------------------------------------------------------------
%% Func: set_connected_status/1
%% Args: true|false
%% Returns: -
%% Purpose: update the statistics for connected users
%%----------------------------------------------------------------------
set_connected_status(S) ->
    set_connected_status(S,get(connected)).
set_connected_status(true, true) ->
    ok;
set_connected_status(true, Old) when Old==undefined; Old==false ->
    put(connected,true),
    ts_mon:add({sum, connected, 1});
set_connected_status(false, true) ->
    put(connected,false),
    ts_mon:add({sum, connected, -1});
set_connected_status(false, Old) when Old==undefined; Old==false ->
    ok.


%%----------------------------------------------------------------------
%% Func: update_stats_noack/1
%% Args: State
%% Returns: {TimeStamp, DynVars}
%% Purpose: update the statistics for no_ack requests
%%----------------------------------------------------------------------
update_stats_noack(#state_rcv{page_timestamp=PageTime,request=Request}) ->
    Now = ?NOW,
    Stats= [{ count, request_noack}], % count and not sample because response time is not defined in this case
    case Request#ts_request.endpage of
        true -> % end of a page, compute page reponse time
            PageElapsed = ts_utils:elapsed(PageTime, Now),
            ts_mon:add(lists:append([Stats,[{sample, page, PageElapsed}]])),
            {0, []};
        _ ->
            ts_mon:add(Stats),
            {PageTime, []}
    end.

%%----------------------------------------------------------------------
%% Func: update_stats/1
%% Args: State
%% Returns: {TimeStamp, DynVars}
%% Purpose: update the statistics
%%----------------------------------------------------------------------
update_stats(S=#state_rcv{size_mon_thresh=T,page_timestamp=PageTime,
                          send_timestamp=SendTime,datasize=Datasize})->
    Now = ?NOW,
    Elapsed = ts_utils:elapsed(SendTime, Now),

    Stats = case S#state_rcv.size_mon > T of
                true ->
                    LastSize=Datasize-S#state_rcv.size_mon+T,
                    [{ sample, request, Elapsed},
                     { sum, size_rcv, LastSize}];
                false->
                    [{ sample, request, Elapsed},
                     { sum, size_rcv, Datasize}]
            end,
    Request = S#state_rcv.request,
    DynVars = ts_search:parse_dynvar(Request#ts_request.dynvar_specs,
                                     S#state_rcv.buffer),
    case Request#ts_request.endpage of
        true -> % end of a page, compute page reponse time
            PageElapsed = ts_utils:elapsed(PageTime, Now),
            ts_mon:add(lists:append([Stats,[{sample, page, PageElapsed}]])),
            {0, DynVars, Elapsed};
        _ ->
            ts_mon:add(Stats),
            {PageTime, DynVars, Elapsed}
    end.

filter(false,undefined) ->
    false;
filter({ok,List},undefined)->
    List;
filter({ok,List},{Include,Re}) when is_list(List)->
    Filter=fun(A) ->
                   case re:run(A,Re) of
                       nomatch -> not Include;
                       {match,_} -> Include
                   end
           end,
    lists:filter(Filter,List);
filter({ok,Data},{Include,Re}) ->
    filter({ok,[Data]},{Include,Re}).

%% @spec token_bucket(R::integer(),Burst::integer(),S0::integer(),T0::tuple(),P1::integer(),
%%                    Now::tuple(),Sleep::boolean()) -> {S1::integer(),Wait::integer()}

%% @doc Implement a token bucket to rate limit the traffic: If the
%%      bucket is full, we wait (if asked) until we can fill the
%%      bucket with the incoming data
%%      R = limit rate in Bytes/millisec, Burst = max burst size in Bytes
%%      T0 arrival date of last packet,
%%      P1 size in bytes of the packet just received
%%      S1: new size of the bucket
%%      Wait: Time to wait
%% @end
token_bucket(R,Burst,S0,T0,P1,Now,Sleep) ->
    S1 = lists:min([S0+R*round(ts_utils:elapsed(T0, Now)),Burst]),
    case P1 < S1 of
        true -> % no need to wait
            {S1-P1,0};
        false -> % the bucket is full, must wait
            Wait=(P1-S1) div R,
            case Sleep of
                true ->
                    timer:sleep(Wait),
                    {0,Wait};
                false->
                    {0,Wait}
            end
    end.
