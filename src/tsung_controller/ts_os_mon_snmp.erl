%%%
%%%  Copyright 2008 © Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas.nniclausse@niclux.org>
%%%  Created: 21 oct 2008 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_os_mon_snmp).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).


-include("ts_macros.hrl").
-include("ts_os_mon.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
          mon_server,        % pid of mon server
          dnscache=[],
          interval,
          pid,               % pid of snmp_mgr
          uri,   % use as an identifier for the snmp manager
          host,
          oids = [], % custom OIDS
          funs, % store fun to be applied in a dict
          version,
          port,
          community,
          addr
         }).

%% SNMP definitions
%% FIXME: make this customizable in the XML config file ?

-define(SNMP_CPU_RAW_USER, [1,3,6,1,4,1,2021,11,50,0]).
-define(SNMP_CPU_RAW_SYSTEM, [1,3,6,1,4,1,2021,11,52,0]).
-define(SNMP_CPU_RAW_IDLE, [1,3,6,1,4,1,2021,11,53,0]).
-define(SNMP_CPU_LOAD1, [1,3,6,1,4,1,2021,10,1,5,1]).
-define(SNMP_MEM_BUFFER, [1,3,6,1,4,1,2021,4,14,0]).
-define(SNMP_MEM_CACHED, [1,3,6,1,4,1,2021,4,15,0]).
-define(SNMP_MEM_AVAIL, [1,3,6,1,4,1,2021,4,6,0]).
-define(SNMP_MEM_TOTAL, [1,3,6,1,4,1,2021,4,5,0]).

-define(SNMP_TIMEOUT,5000).

start(Args) ->
    ?LOGF("starting os_mon_snmp with args ~p",[Args],?NOTICE),
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init({HostStr, {Port, Community, Version, DynOIDS}, Interval, MonServer}) ->
    {ok, IP} = inet:getaddr(HostStr, inet),
    Apps = application:loaded_applications(),
    case proplists:is_defined(snmp,Apps) of
        true ->
            ?LOG("SNMP manager already started~n", ?NOTICE);
        _ ->
            ?LOG("Initialize SNMP application~n", ?NOTICE),
            Res1= application:start(snmp),
            ?LOGF("Initialize SNMP manager: ~p~n", [Res1],?NOTICE),
            Res2=snmpm:start(),
            ?LOGF("Register SNMP manager: ~p~n",[Res2], ?NOTICE),
            Res3=snmpm:register_user("tsung",snmpm_user_default,undefined),
            ?LOGF("SNMP initialization: ~p~n", [Res3],?NOTICE)
    end,
    erlang:start_timer(5, self(), connect ),
    FunsL= [{?SNMP_CPU_RAW_SYSTEM,cpu_system,sample_counter,fun(X)-> X/(Interval/1000) end},
           {?SNMP_CPU_RAW_USER,cpu_user,sample_counter,fun(X)-> X/(Interval/1000) end},
           {?SNMP_MEM_AVAIL,freemem,sample,fun(X)-> X/1000 end},
           {?SNMP_CPU_LOAD1,load,sample,fun(X)-> X*100 end}] ++ DynOIDS,
    OIDS=lists:map(fun({Oid,_Name,_Type,_Fun})-> Oid end,FunsL),
    Funs=lists:foldl(fun({Oid,Name,Type,Fun},Acc)->dict:store(Oid,{Name,Type,Fun},Acc) end,dict:new(),FunsL),
    ?LOGF("Starting SNMP mgr on ~p~n", [IP], ?DEB),
    {ok, #state{ mon_server = MonServer,
                 host       = HostStr,
                 uri        = "snmp://"++HostStr++":" ++ integer_to_list(Port),
                 port       = Port,
                 addr       = IP,
                 oids       = OIDS,
                 funs       = Funs,
                 community  = Community,
                 version    = Version,
                 interval   = Interval}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_message, Msg}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout,_Ref,connect},State=#state{uri=URI, addr=IP,port=Port,community=Community,version=Version}) ->
    ok = snmpm:register_agent("tsung",URI,
                         [{engine_id,"myengine"},
                          {address,IP},
                          {port,Port},
                          {version,Version},
                          {community,Community}]),

    ?LOGF("SNMP mgr started; remote node is ~p~n", [URI],?INFO),
    erlang:start_timer(State#state.interval, self(), send_request ),
    {noreply, State};

handle_info({timeout,_Ref,send_request},State=#state{uri=URI,oids=OIDS}) ->
    ?LOGF("SNMP mgr; get data from host ~p~n", [URI],?DEB),
    snmp_get(URI, OIDS, State),
    erlang:start_timer(State#state.interval, self(), send_request ),
    {noreply,State};

handle_info(Message, State) ->
    {stop, {unknown_message, Message} , State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: analyse_snmp_data/3
%% Returns: any (send msg to ts_mon)
%%--------------------------------------------------------------------
analyse_snmp_data(Args, Host, State) ->
    analyse_snmp_data(Args, Host, [], State).

%% Function: analyse_snmp_data/4
analyse_snmp_data([], _Host, Resp, State) ->
    ts_os_mon:send(State#state.mon_server,Resp);

analyse_snmp_data([Val=#varbind{value='NULL'}| Tail], Host, Stats, State) ->
    ?LOGF("SNMP: Skip void result (~p) ~n", [Val],?DEB),
    analyse_snmp_data(Tail, Host, Stats, State);

%% FIXME: this may not be accurate: if we lost packets (the server is
%% overloaded), the value will be inconsistent, since we assume a
%% constant time across samples ($INTERVAL)

analyse_snmp_data([#varbind{variabletype='NULL'}| Tail], Host, Stats, State) ->
    %% skip bad values
    analyse_snmp_data(Tail, Host, Stats, State);
analyse_snmp_data([#varbind{oid=?SNMP_CPU_RAW_SYSTEM, value=Val}| Tail], Host, Stats, State) ->
    {value, User} = lists:keysearch(?SNMP_CPU_RAW_USER, #varbind.oid, Tail),
    Value = Val + User#varbind.value,
    CountName = {cpu , Host},
    NewValue = Value/(State#state.interval/1000),
    NewTail = lists:keydelete(?SNMP_CPU_RAW_USER, #varbind.oid, Tail),
    analyse_snmp_data(NewTail, Host, [{sample_counter, CountName, NewValue}| Stats], State);

analyse_snmp_data([User=#varbind{oid=?SNMP_CPU_RAW_USER}| Tail], Host, Stats, State) ->
    %%put this entry at the end, this will be used when SYSTEM match
    analyse_snmp_data(Tail ++ [User], Host, Stats, State);

analyse_snmp_data([#varbind{oid=OID, value=Val}| Tail], Host, Stats, State=#state{funs=F}) ->
    {DataName, Type, Fun} = dict:fetch(OID,F),
    Value = Fun(Val),
    Name = {DataName,Host},
    ?LOGF("Analyse SNMP: ~p:~p:~p ~n", [Type, Name, Value],?DEB),
    analyse_snmp_data(Tail, Host, [{Type, Name, Value}| Stats], State).


%%--------------------------------------------------------------------
%% Function: snmp_get/3
%% Description: ask a list of OIDs to the given snmp_mgr
%%--------------------------------------------------------------------
snmp_get(Agent,OIDs,State)->
    snmp_get(Agent,[OIDs],State,?SNMP_TIMEOUT,[]).

snmp_get("snmp://"++Host, [], State, _TimeOut, Results )->
    [Agent|_]=string:tokens(Host,":"),
    analyse_snmp_data(Results,Agent,State);
snmp_get(URI, [OIDs|Tail], State, TimeOut,PrevRes)->
    ?LOGF("Running snmp get ~p ~p~n", [URI,OIDs], ?DEB),
    Res = snmpm:sync_get("tsung",URI,OIDs,TimeOut),
    ?LOGF("Res ~p ~n", [Res], ?DEB),
    case Res of
        {ok,{noError,_,Results},_Remaining} ->
            snmp_get(URI, Tail, State, TimeOut, Results++PrevRes);
        {error, {send_failed,_,tooBig}} ->
            %% split the OID list in two, and retry
            ?LOGF("SNMP: too big packet, split and retry (~p)~n", [URI], ?INFO),
            snmp_get(URI, tuple_to_list(lists:split(length(OIDs) div 2, OIDs)), State, TimeOut, PrevRes);
        Other ->
            ?LOGF("SNMP Error:~p for ~p~n", [Other, URI], ?WARN),
            {error, Other}
    end.

