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

-module(ts_os_mon_munin).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

%% @doc munin plugin for ts_os_mon

-include("ts_profile.hrl").
-include("ts_os_mon.hrl").

-define(READ_TIMEOUT,1000).

-export([init/3, get_data/2, parse/2, restart/3, stop/2]).


%% @spec init(HostStr::string,
%%            Options::[{Port::integer, Community::string, Version::string }]) ->
%%       {ok, {socket(), Hostname::string()}} | {error, Reason}
init(HostStr, [{Port}], _State) ->
    {ok, Host} = inet:getaddr(HostStr, inet),
    ?LOGF("Starting munin mgr on ~p:~p~n", [Host,Port], ?DEB),
    Opts=[list,
          {active, false},
          {packet, line},
          {keepalive, true}
         ],
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Socket} ->
            case gen_tcp:recv(Socket,0, ?READ_TIMEOUT) of
                {ok, "# munin node at "++ MuninHost} ->
                    ?LOGF("Connected to ~p~n", [MuninHost], ?INFO),
                    %% must fetch some data, otherwise munin-node
                    %% timeout and close the connection
                    gen_tcp:send(Socket,"fetch load\n"),
                    read_munin_data(Socket),
                    {ok, {Socket, ts_utils:chop(MuninHost) }};
                {error, Reason} ->
                    ?LOGF("Error while connecting to munin server: ~p~n", [Reason], ?ERR),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOGF("Can't connect to munin server on ~p, reason:~p~n", [HostStr, Reason], ?ERR),
            {error, Reason}
    end.


get_data({Socket, Hostname}, State) ->
    %% Currenly, fetch only cpu and memory
    %% FIXME: should be customizable in XML config file
    ?LOGF("Fetching munin for cpu on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch cpu\n"),
    AllCPU=read_munin_data(Socket),
    ?LOGF("Fetching munin for memory on host ~p~n", [Hostname], ?DEB),
    gen_tcp:send(Socket,"fetch memory\n"),
    AllMem=read_munin_data(Socket),
    %% sum all cpu types, except idle.
    NonIdle=lists:keydelete('idle.value',1,AllCPU),
    Cpu=lists:foldl(fun({_Key,Val},Acc) when is_integer(Val)->
                            Acc+Val
                    end,0,NonIdle) / (State#os_mon.interval div 1000),
    ?LOGF(" munin cpu on host ~p is  ~p~n", [Hostname,Cpu], ?DEB),
    %% returns free + buffer + cache
    FunFree = fun({Key,Val},Acc) when ((Key=='buffers.value') or
                                       (Key=='free.value')    or
                                       (Key=='cached.value') ) ->
                      Acc+Val;
                 (_, Acc) -> Acc
              end,
    FreeMem=lists:foldl(FunFree,0,AllMem)/1048576, %% megabytes
    ?LOGF(" munin memory on host ~p is ~p~n", [Hostname,FreeMem], ?DEB),
    ts_os_mon:send(State#os_mon.mon_server,[{sample_counter, {cpu, Hostname}, Cpu},
                                            {sample, {freemem, Hostname}, FreeMem}]),
    ok.

parse(_Data, _State) ->
    ok.

restart(_Node,_Reason,State) ->
    {noreply, State}.

stop(_Node,State) ->
    %%TODO: close the socket
    {noreply, State}.

read_munin_data(Socket)->
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT),[]).

read_munin_data(_Socket,{error,Reason}, Acc)->
    ?LOGF("Munin error: ~p~n", [Reason], ?ERR),
    Acc;
read_munin_data(_Socket,{ok,".\n"}, Acc)->
    Acc;
read_munin_data(Socket,{ok, Data}, Acc) when is_list(Acc)->
    NewAcc = case string:tokens(Data," \n") of
                 [Key, Value] ->
                     [{list_to_atom(Key), ts_utils:list_to_number(Value)}|Acc];
                 _ ->
                     ?LOGF("Unknown data received from munin server: ~p~n",[Data],?WARN),
                     Acc
             end,
    read_munin_data(Socket,gen_tcp:recv(Socket,0,?READ_TIMEOUT), NewAcc).
