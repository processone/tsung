%%%  This code was developped by Mickael Remond
%%%  <mickael.remond@erlang-fr.org> and contributors (their names can
%%%  be found in the CONTRIBUTORS file).  Copyright (C) 2003 Mickael
%%%  Remond
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

%%%  Created :  23 Dec 2003 by Mickael Remond <mickael.remond@erlang-fr.org>

%%----------------------------------------------------------------------
%% HEADER ts_os_mon
%% COPYRIGHT Mickael Remond (C) 2003
%% PURPOSE Monitor CPU, memory consumption and network traffic
%%         on a cluster of machines
%% DESCRIPTION
%%   TODO ...
%%----------------------------------------------------------------------
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%% the two.

-module(ts_os_mon).
-author('mickael.remond@erlang-fr.org').
-modifiedby('nicolas@niclux.org').
-vc('$Id$ ').

-behaviour(gen_server).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_os_mon.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/0, start/1, stop/0, activate/0, activated/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ts_os_mon).
-define(OTP_TIMEOUT, infinity).
-define(TIMEOUT, 30000).
-define(OPTIONS, [{timeout,?TIMEOUT}]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: activate/0
%% Purpose: This is used by tsung to start the cluster monitor service
%% It will only be started if there are cluster/monitor@host element
%% in the config file.
%%--------------------------------------------------------------------
activate() ->
    case ts_config_server:get_monitor_hosts() of
        [] ->
           ?LOG("os_mon disabled",?NOTICE),
            ok;
        Hosts ->
            gen_server:cast({global,?SERVER}, {activate, Hosts})
    end.

activated({Id, Type, Node}) ->
    gen_server:cast({global,?SERVER}, {activated, {Id, Type, Node}}).

%%% send data back to the controlling node
send(Mon_Server, Data) when is_pid(Mon_Server) ->
    Mon_Server ! {add, Data};
send(Mon_Server, Data) ->
    gen_server:cast(Mon_Server, {add, Data}).

%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the server, with a list of the hosts in the
%%              cluster to monitor
%%--------------------------------------------------------------------
start() ->
    ?LOG("starting os_mon",?NOTICE),
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], ?OPTIONS).

start(Args) ->
    ?LOGF("starting os_mon with args ~p",[Args],?NOTICE),
    gen_server:start_link({global, ?SERVER}, ?MODULE, Args, ?OPTIONS).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stop the server
%%--------------------------------------------------------------------
stop() ->
    gen_server:call({global,?SERVER}, {stop}, ?OTP_TIMEOUT).




%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init({Mon_Server, Interval}) ->
    ?LOG(" os_mon started",?NOTICE),
    %% to get the EXIT signal from spawn processes on remote nodes
    process_flag(trap_exit,true),
    {ok, #os_mon{mon_server=Mon_Server, pids=dict:new(),interval=Interval}};
init(_) ->
    init({{global, ts_mon},?INTERVAL}).

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
handle_call({stop}, _From, State) ->
    {stop, normal, State};
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
handle_cast({activate, Hosts}, State) ->
    Activate = fun({HostStr, {erlang, Options}}) ->
                       %% spawn a process to start remote beams
                       spawn(ts_os_mon_erlang,init,[HostStr, Options,State]);
                  ({HostStr, {Type, Options}}) ->
                       Module= plugin_module(Type),
                       Module:init(HostStr, Options,State)
               end,
    lists:foreach(Activate,Hosts),
    erlang:start_timer(State#os_mon.interval, self(), send_request ),
    {noreply, State};

handle_cast({activated, {Id,Type,Node}}, State=#os_mon{pids=Ids}) ->
    ?LOGF("os_mon: ~p monitoring activated on ~p (id is ~p)~n",[Type,Node,Id],?NOTICE),
    NewIds=dict:store(Id,{Type, Node},Ids),
    case Type of
        erlang -> link(Id);
        _      -> ok
    end,
    {noreply, State#os_mon{pids=NewIds}};

handle_cast(Msg, State) ->
    ?LOGF("handle cast: unknown msg ~p~n",[Msg],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout, _Ref, send_request},  State ) ->
    Fun = fun(Pid) ->
            {Type, Host} = dict:fetch(Pid,State#os_mon.pids),
            Module=plugin_module(Type),
            Module:get_data({Pid, Host},State)
    end,
    lists:foreach(Fun,  dict:fetch_keys(State#os_mon.pids)),
    erlang:start_timer(State#os_mon.interval, self(), send_request ),
    {noreply, State};

handle_info({'EXIT', From, Reason}, State) ->
    ?LOGF("received exit from ~p with reason ~p~n",[From, Reason],?ERR),
    %% get type  of died pid
    case dict:find(From, State#os_mon.pids) of
        {ok, {Type, Node}} ->
            Module=plugin_module(Type),
            Module:restart({From, Node}, Reason, State);
        error -> % exit when starting
            {noreply, State}
    end;

handle_info(Data, State) ->
    AllPlugins = get_all_plugins(State),
    ?LOGF("Call parse with plugins ~p~n",[AllPlugins],?INFO),
    Res = lists:map(fun(A)->Module=plugin_module(A),
                            Module:parse(Data,State) end,AllPlugins),
    case lists:filter(fun(ok)->false;
                         ({ok, _State})-> true end, Res) of
        [{ok, NewState}| _Tail] -> % there should be a response from a single plugin
            {noreply, NewState#os_mon{plugins=AllPlugins}};
        _ ->
            {noreply, State#os_mon{plugins=AllPlugins}} %% ignore other cases
    end.

get_all_plugins(#os_mon{pids=Dict,plugins=[]}) ->
    lists:usort(lists:map(fun({_,{Type,_}})->Type end, dict:to_list(Dict)) );
get_all_plugins(#os_mon{plugins=Plugins}) ->
    Plugins.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, State) ->
    ?LOG("Terminating ts_os_mon, stop beams (if needed)~n",?NOTICE),
    Ids = dict:fetch_keys(State#os_mon.pids),
    Stop= fun(Id) ->
                  {Type,Node}=dict:fetch(Id,State#os_mon.pids),
                  Module = plugin_module(Type),
                  Module:stop(Node,State)
          end,
    lists:foreach(Stop, Ids),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

plugin_module(Type) -> list_to_atom("ts_os_mon_" ++ atom_to_list(Type)).

