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
-module(ts_os_mon).
-author('mickael.remond@erlang-fr.org').
-vc('$Id: ').

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/0, stop/0, activate/0, updatestats/0]).
-export([client_start/0]).
-export([node_data/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ts_os_mon).
-define(NODE, "os_mon").
-define(OTP_TIMEOUT, infinity).
-define(TIMEOUT, 30000).
-define(OPTIONS, [{timeout,?TIMEOUT}]).
-define(INTERVAL, 5000).

%%====================================================================
%% External functions
%%====================================================================
%% This is used by tsunami to start the cluster monitor service
%% It will only be started if there are cluster/monitor@host element
%% in the config file.
activate() ->
    case ts_config_server:get_monitor_hosts() of
    	[] ->
           ?LOG("os_mon disabled",?NOTICE),
            ok;
        Hosts ->
            gen_server:cast(?SERVER, {activate, Hosts})
    end.

%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the server, with a list of the hosts in the
%%              cluster to monitor
%%--------------------------------------------------------------------
start() ->
    ?LOG("starting os_mon",?NOTICE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], ?OPTIONS).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stop the server
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?SERVER, {stop}, ?OTP_TIMEOUT).

%% Start the monitor tools on the node that you want to spy on
client_start() ->
    application:start(stdlib),
    application:start(sasl),
    application:start(os_mon).

updatestats() ->
    Node = atom_to_list(node()),
    {Cpu, FreeMem, RecvPackets, SentPackets} = node_data(),
    ts_mon:add([{sample, list_to_atom("cpu:" ++ Node), Cpu},
				{sample, list_to_atom("freemem:" ++ Node), FreeMem},
				{sample_counter, list_to_atom("recvpackets:" ++ Node), RecvPackets},
				{sample_counter, list_to_atom("sentpackets:" ++ Node), SentPackets}]),

    timer:sleep(?INTERVAL),
    updatestats().


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
init(_Args) ->
    ?LOG(" os_mon started",?NOTICE),
	process_flag(trap_exit,true), %% to get the EXIT signal from spawn processes on remote nodes
	{ok, []}.

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
handle_call({stop}, From, State) ->
    {stop, normal, State};
handle_call(Request, From, State) ->
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
    start_beam(Hosts),    %% Start an Erlang node on all hosts
    %% TODO: Replace net_adm:world by an explicite ping ?
    net_adm:world_list(lists:map(fun(Host) -> list_to_atom(Host) end,Hosts)),
    
    %% Load ts_os_mon code
    Nodes = lists:map(fun(Host) -> list_to_atom(?NODE ++ "@" ++ Host) end, Hosts),
    load_code(Nodes),
    
	%% because the stats for cpu has to be called from the same
	%% process (otherwise the same value (mean cpu% since the system
	%% last boot)  is returned by cpu_sup:util), we spawn a process
	%% on each node that will do the stats collection and send it to
	%% ts_mon
    PidKeys = lists:map(fun(Node) ->{spawn_link(Node, ?MODULE, updatestats, []),
									 Node}
						end,
					 Nodes),
    {noreply, PidKeys};
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT',From,Reason}, State) ->
	?LOGF("received exit from ~p with reason ~p~n",[From, Reason],?ERR),
	%% get node name of died pid
	case lists:keysearch(From,1,State) of 
		{value, {From, Node}} ->
			%% start a new process on this node
			Pid = spawn_link(Node, ?MODULE, updatestats, []),
			%% replace the pid value
			NewState = lists:keyreplace(From,1,State,{Pid, Node}),
			{noreply, NewState};
		false -> %% the EXIT is not from a stats pid, do nothing 
			?LOGF("unknown exit from ~p !~n",[From],?WARN),
			{noreply, State}
	end;

handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, State_Nodes) ->
    stop_beam(State_Nodes),    
    ok;
terminate(Reason, State_Nodes) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

node_data() ->
    {RecvPackets, SentPackets} = packets(),
    {cpu(), freemem(), RecvPackets, SentPackets}.

%% Return node cpu utilisation
cpu() ->
    cpu_sup:util().

%% Return node cpu average load on 1 minute
cpu1() -> cpu_sup:avg1()/256.

%% Return free memory in bytes
%% Use the result of the free commands on Linux
%%  and os_mon on all other platforms
freemem() ->
    case os:type() of
	{unix, linux} -> freemem_linux();
	Other         -> freemem_all()
    end.

freemem_all() ->
     Data = memsup:get_system_memory_data(),
     {value,{free_memory,FreeMem}} = lists:keysearch(free_memory, 1, Data),
     %% We use Megabytes
     FreeMem/100000.

freemem_linux() ->
    Result = os:cmd("free | grep '\\-/\\+'"),
    [_, _, _, Free] = string:tokens(Result, " \n"),
    list_to_integer(Free)/1024.

packets() ->
	%% linux only !
	%% TODO: handle more than one ethernet interface
    Result = os:cmd("cat /proc/net/dev | grep eth0"), 
    [_, RecvBytes, RecvPackets, _, _, _, _, _, _, SentBytes, SentPackets, _, _, _, _, _,_] = 
        string:tokens(Result, " \n:"),
    {list_to_integer(RecvPackets), list_to_integer(SentPackets)}.

%% Start an Erlang node on every host that you want to spy
start_beam([]) ->
    ok;
start_beam([Host|Hosts]) ->
	Args = ts_utils:erl_system_args(),
    {ok, Node} = slave:start_link(Host, ?NODE, Args),
    ?LOGF("started os_mon newbeam on node ~p~n", [Node], ?INFO),
    start_beam(Hosts).

stop_beam([]) ->
    ok;
stop_beam([Node|Nodes]) ->
    rpc:cast(Node, erlang, halt, []),
    stop_beam(Nodes).

%% Load ts_os_mon code on all Erlang nodes
load_code(Nodes) ->
    ?LOGF("loading tsunami monitor on nodes ~p~n", [Nodes], ?NOTICE),
    {?MODULE, Binary, _File} = code:get_object_code(?MODULE),
    Res1 = rpc:multicall(Nodes, code, load_binary, [?MODULE, ?MODULE, Binary], infinity),
    {ts_mon, Binary2, _File2} = code:get_object_code(ts_mon),
    Res2 = rpc:multicall(Nodes, code, load_binary, [ts_mon, ts_mon, Binary2], infinity),

    Res3 = rpc:multicall(Nodes, ?MODULE, client_start, [], infinity),
    %% first value of load call is garbage
    ?DebugF("load_code - ~p ~p~n", [Res1, Res2, Res3]),
    ok.


%% Config file description: 
%%  <cluster>
%%   <monitor host="f14-1"></monitor>
%%   <monitor host="f14-2"></monitor>
%%   <monitor host="bigfoot-1"></monitor>
%%   <monitor host="bigfoot-2"></monitor>
%%  </cluster>
