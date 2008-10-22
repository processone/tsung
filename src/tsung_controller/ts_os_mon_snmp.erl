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
%%%  the two.

-module(ts_os_mon_snmp).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').


-include("ts_profile.hrl").
-include("ts_os_mon.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([init/3, get_data/2, parse/2, restart/3, stop/2]).

%% SNMP definitions
%% FIXME: make this customizable in the XML config file ?

-define(SNMP_CPU_RAW_USER, [1,3,6,1,4,1,2021,11,50,0]).
-define(SNMP_CPU_RAW_SYSTEM, [1,3,6,1,4,1,2021,11,52,0]).
-define(SNMP_CPU_RAW_IDLE, [1,3,6,1,4,1,2021,11,53,0]).

-define(SNMP_MEM_BUFFER, [1,3,6,1,4,1,2021,4,14,0]).
-define(SNMP_MEM_CACHED, [1,3,6,1,4,1,2021,4,15,0]).
-define(SNMP_MEM_AVAIL, [1,3,6,1,4,1,2021,4,6,0]).
-define(SNMP_MEM_TOTAL, [1,3,6,1,4,1,2021,4,5,0]).


%% @spec init(HostStr::string,
%%            Options::[{Port::integer, Community::string, Version::string }]) ->
%%       {ok, Pid} | {error, Reason}
init( HostStr, [{Port, Community, Version }], _State) ->
    {ok, Host} = inet:getaddr(HostStr, inet),
    ?LOGF("Starting SNMP mgr on ~p~n", [Host], ?DEB),
    {ok, Pid} = snmp_mgr:start_link([{agent, Host},
                                     {agent_udp, Port},
                                     {community, Community},
                                     {receive_type, msg},
                                     Version,
                                     quiet
                                    ]),
    %% since snmp_mgr can handle only a single snmp server, change the
    %% registered name to start several smp_mgr at once !
    unregister(snmp_mgr),
    ?LOGF("SNMP mgr started; remote node is ~p~n", [Host],?INFO),
    {ok, { Pid, Host }}.


get_data(Pid, _State) when is_pid(Pid)->
    ?LOGF("SNMP mgr; get data from pid ~p~n", [Pid],?DEB),
    snmp_get(Pid,
             [?SNMP_CPU_RAW_SYSTEM, ?SNMP_CPU_RAW_USER, ?SNMP_MEM_AVAIL ]),
    ok.

parse({snmp_msg, Msg, Ip, _Udp}, State) ->
    PDU = snmp_mgr_misc:get_pdu(Msg),
    case PDU#pdu.type of
        'get-response' ->
            ?LOGF("Got SNMP PDU ~p from ~p~n",[PDU, Ip],?DEB),
            {Hostname, NewCache} = ts_utils:resolve(Ip, State#os_mon.dnscache),
            analyse_snmp_data(PDU#pdu.varbinds, Hostname, State),
            {ok, State#os_mon{dnscache=NewCache}};
        _ ->
            ?LOGF("Got unknown SNMP data ~p from ~p~n",[PDU, Ip],?WARN),
            {ok, State}
    end;
parse(_Data, _State) ->
    ok.

restart(_Node,_Reason,State) ->
    {noreply, State}.

stop(_Node,State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: analyse_snmp_data/3
%% Returns: any (send msg to ts_mon)
%%--------------------------------------------------------------------
analyse_snmp_data(Args, Host, State) ->
    analyse_snmp_data(Args, Host, [], State).

%% Function: analyse_snmp_data/4
analyse_snmp_data([], _Host, Resp, State) ->
    ts_os_mon:send(State#os_mon.mon_server,Resp);

analyse_snmp_data([Val=#varbind{value='NULL'}| Tail], Host, Stats, State) ->
    ?LOGF("SNMP: Skip void result (~p) ~n", [Val],?DEB),
    analyse_snmp_data(Tail, Host, Stats, State);

%% FIXME: this may not be accurate: if we lost packets (the server is
%% overloaded), the value will be inconsistent, since we assume a
%% constant time across samples ($INTERVAL)

analyse_snmp_data([#varbind{oid=?SNMP_CPU_RAW_SYSTEM, value=Val}| Tail], Host, Stats, State) ->
    {value, User} = lists:keysearch(?SNMP_CPU_RAW_USER, #varbind.oid, Tail),
    Value = Val + User#varbind.value,
    CountName = {cpu , Host},
    NewValue = Value/(State#os_mon.interval/1000),
    NewTail = lists:keydelete(?SNMP_CPU_RAW_USER, #varbind.oid, Tail),
    analyse_snmp_data(NewTail, Host, [{sample_counter, CountName, NewValue}| Stats], State);

analyse_snmp_data([User=#varbind{oid=?SNMP_CPU_RAW_USER}| Tail], Host, Stats, State) ->
    %%put this entry at the end, this will be used when SYSTEM match
    analyse_snmp_data(Tail ++ [User], Host, Stats, State);

analyse_snmp_data([#varbind{oid=OID, value=Val}| Tail], Host, Stats, State) ->
    {Type, Name, Value}= oid_to_statname(OID, Host, Val),
    ?LOGF("Analyse SNMP: ~p:~p:~p ~n", [Type, Name, Value],?DEB),
   analyse_snmp_data(Tail, Host, [{Type, Name, Value}| Stats], State).

%%--------------------------------------------------------------------
%% Function: oid_to_statname/3
%%--------------------------------------------------------------------
oid_to_statname(?SNMP_CPU_RAW_IDLE, Name, Value) ->
    CountName = {cpu_idle, Name},
    ?DebugF("Adding counter value for ~p~n",[CountName]),
    {sample_counter, CountName, Value/(?INTERVAL/1000)};
oid_to_statname(?SNMP_MEM_AVAIL, Name, Value)->
    CountName = {freemem, Name},
    ?DebugF("Adding counter value for ~p~n",[CountName]),
    {sample,CountName, Value/1000}.

%%--------------------------------------------------------------------
%% Function: snmp_get/2
%% Description: ask a list of OIDs to the given snmp_mgr
%%--------------------------------------------------------------------
snmp_get(Pid, Oids) ->
    ?DebugF("send snmp get for oid ~p to pid ~p ",[Oids,Pid]),
    Pid ! {get, Oids}, ok.

