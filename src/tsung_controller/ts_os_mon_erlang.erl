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

-module(ts_os_mon_erlang).
-vc('$Id: ts_os_mon_snmp.erl,v 0.0 2008/10/21 12:57:49 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').


-include("ts_macros.hrl").
-include("ts_os_mon.hrl").

-export([start/1, updatestats/3, client_start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(NODE, 'os_mon').
-define(PROCNET, "/proc/net/dev").

-record(state,{
          mon,        % pid of mon server
          interval,   % interval
          node,       % name of node to monitor
          host,       % hostname of server to monitor
          pid,        % remote pid
          options     % updatestats options
         }).

start(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init( {Host, Options, Interval,  MonServer} ) ->
    {ok, LocalHost} = ts_utils:node_to_hostname(node()),
    %% to get the EXIT signal from spawned processes on remote nodes
    process_flag(trap_exit,true),
    %% because the stats for cpu has to be called from the same
    %% process (otherwise the same value (mean cpu% since the system
    %% last boot)  is returned by cpu_sup:util), we must spawn a process
    %% on the remote node that will do the stats collection and send it back
    %% to ts_mon
    case LocalHost of
        Host -> % same host, don't start a new beam
            ?LOG("Running os_mon on the same host as the controller, use the same beam~n",?INFO),
            application:start(sasl),
            application:start(os_mon),
            Pid = spawn_link(?MODULE, updatestats, [Options, Interval, MonServer]),
            {ok, #state{node=node(),mon=MonServer, host=Host, interval=Interval, pid=Pid, options=Options}};
        _ ->
            erlang:start_timer(?INIT_WAIT, self(), start_beam),
            {ok, #state{host=Host, mon=MonServer, interval=Interval, options=Options}}
    end.



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
    ?LOGF("handle cast: unknown msg ~p~n",[Msg],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout,_Ref,start_beam},State=#state{host=Host})->
    case start_beam(Host) of
        {ok, Node} ->
            Pong = net_adm:ping(Node),
            ?LOGF("ping ~p: ~p~n", [Node, Pong],?INFO),
            load_code([Node]),
            Pid = spawn_link(Node, ?MODULE, updatestats,
                             [State#state.options, State#state.interval, State#state.mon]),
            {noreply, State#state{node=Node,pid=Pid}};
        {error,{already_running,Node}} ->
            ?LOGF("Node ~p is already running, start updatestats process~n", [Node],?NOTICE),
            Pid = spawn_link(Node, ?MODULE, updatestats,
                             [State#state.options, State#state.interval, State#state.mon]),
            {noreply, State#state{node=Node,pid=Pid}};
       Error ->
            ?LOGF("Fail to start beam on host ~p (~p)~n", [Host, Error],?ERR),
            {stop, Error, State}
    end.

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

%%--------------------------------------------------------------------
%% Function: updatestats/3
%% Purpose: update stats for erlang monitoring. Executed on the remote host
%%--------------------------------------------------------------------
updatestats(Options, Interval,Mon_Server) ->
    Node = atom_to_list(node()),
    {Cpu, FreeMem, RecvPackets, SentPackets, Load} = node_data(),
    Stats = [{sample, {cpu, Node}, Cpu},
             {sample, {freemem, Node}, FreeMem},
             {sample, {load, Node}, Load},
             {sample_counter, {recvpackets, Node}, RecvPackets},
             {sample_counter, {sentpackets,  Node}, SentPackets}],

    Fun = fun(Option, AccumStats) ->
            case Option of
                {mysqladmin, MysqlOptions} ->
                    {Threads, Questions} = mysqladmin_data(MysqlOptions),
                    lists:append(AccumStats, [{sample, {mysql_threads, Node}, Threads},
                                              {sample_counter, {mysql_questions, Node}, Questions}]);
                _ ->
                    AccumStats
            end
        end,

    ts_os_mon:send(Mon_Server, lists:append(Stats, lists:foldl(Fun, [], Options))),

    timer:sleep(Interval),
    updatestats(Options, Interval,Mon_Server).


%%--------------------------------------------------------------------
%% Function: client_start/0
%% Purpose: Start the monitor tools on the node that you want to spy on
%%--------------------------------------------------------------------
client_start() ->
    application:start(stdlib),
    application:start(sasl),
    application:start(os_mon).


%%--------------------------------------------------------------------
%% Function: load_code/1
%% Purpose: Load ts_os_mon code on all Erlang nodes
%%--------------------------------------------------------------------
load_code(Nodes) ->
    ?LOGF("Loading tsung monitor on nodes ~p~n", [Nodes], ?NOTICE),
    LoadCode = fun(Mod)->
                       {_, Binary, _} = code:get_object_code(Mod),
                       rpc:multicall(Nodes, code, load_binary, [Mod, Mod, Binary], infinity)
               end,
    LoadRes = lists:map(LoadCode, [ts_mon, ?MODULE, ts_os_mon, ts_utils]),
    Res = rpc:multicall(Nodes, ?MODULE, client_start, [], infinity),
    %% first value of load call is garbage
    ?LOGF("load_code: ~p start: ~p ~n", [LoadRes, Res],?DEB),
    ok.


%%--------------------------------------------------------------------
%% Func: node_data/0
%%--------------------------------------------------------------------
node_data() ->
    {RecvPackets, SentPackets} = get_os_data(packets),
    {get_os_data(cpu), get_os_data(freemem), RecvPackets, SentPackets, get_os_data(load)}.

%%--------------------------------------------------------------------
%% Func: mysqladmin_data/1
%%--------------------------------------------------------------------
mysqladmin_data({Port, Username, Password}) ->
    PasswdArg = case Password of
                    false ->
                        "";
                    _ ->
                        io_lib:format("-p\"~s\"", [Password])
                end,
    Cmd = io_lib:format("mysqladmin -u\"~s\" ~s -P~B status", [Username, PasswdArg, Port]),
    Result = os:cmd(Cmd),
    % Uptime: 1146892  Threads: 2  Questions: 15242050  Slow queries: 0  Opens: 176  Flush tables: 1  Open tables: 101  Queries per second avg: 13.290
    [_, _Uptime, _, Threads, _, Questions, _, _, _SlowQueries, _, _Opens, _, _, _FlushTables, _, _, _OpenTables, _, _, _, _, _QPS] = string:tokens(Result, " \n"),
    {list_to_integer(Threads), list_to_integer(Questions)}.


%%--------------------------------------------------------------------
%% Func: get_os_data/1
%%--------------------------------------------------------------------
%% Return node cpu utilisation
get_os_data(cpu) -> cpu_sup:util();

%% Return node cpu average load on 1 minute;
get_os_data(load) -> cpu_sup:avg1()/256;

get_os_data(DataName) -> get_os_data(DataName,os:type()).

%%--------------------------------------------------------------------
%% Func: get_os_data/2
%%--------------------------------------------------------------------
%% Return free memory in bytes.
%% Use the result of the free commands on Linux and os_mon on all
%% other platforms
get_os_data(freemem, {unix, linux}) ->
    case file:open("/proc/meminfo",[raw,read]) of
        {ok, FD} ->
            file:read_line(FD), % skip  MemTotal
            {ok, Data} = file:read_line(FD),
            ["MemFree:",RawFree,_] = string:tokens(Data," \n"),
            case file:read_line(FD) of
                {ok, "MemAvailable:" ++Tail} ->
                    [Avail,_] = string:tokens(Tail," \n"),
                    file:close(FD),
                    list_to_integer(Avail)/1024;
                {ok, NextLine} ->
                    ["Buffers:",Buf,_] = string:tokens(NextLine," \n"),
                    {ok, LastLine} = file:read_line(FD),
                    ["Cached:",Cached,_] = string:tokens(LastLine," \n"),
                    file:close(FD),
                    (list_to_integer(RawFree)+list_to_integer(Buf) + list_to_integer(Cached)) / 1024
            end;
        _ ->
            0
    end;
get_os_data(freemem, {unix, sunos}) ->
    Result = os:cmd("vmstat 1 2 | tail -1"),
    [_, _, _, _, Free | _] = string:tokens(Result, " "),
    list_to_integer(Free)/1024;
get_os_data(freemem, _OS) ->
    Data = memsup:get_system_memory_data(),
    {value,{free_memory,FreeMem}} = lists:keysearch(free_memory, 1, Data),
    %% We use Megabytes
    FreeMem/1048576;

%% Return packets sent/received on network interface
get_os_data(packets, {unix, Val}) ->
    Data=os:cmd("netstat -in"),
    get_os_data(packets, {unix, Val},string:tokens(Data, "\n"));

get_os_data(packets, _OS) ->
    {0, 0 }. % FIXME: not implemented for other arch.

%% %% packets , special case with File as a variable for easy testing
get_os_data(packets, {unix, _}, Data) ->
    %% lists:zf is called lists:filtermap in erlang R16B1 and newer
    Eth=[io_lib:fread("~d~d~d~d~d~d~d~d~d", X) ||
        {E,X}<-ts_utils:filtermap(fun(Y)->
                                   case string:chr(Y, $:) of
                                      0 -> {true, ts_utils:split2(Y,32,strip)};
                                      _ -> false
                                   end
                                  end , Data),
        string:str(E,"eth") /= 0],
    Fun = fun (A, {Rcv, Sent}) ->
                  {ok,[_,_,RcvBytes,_,_,_,SentBytes,_,_],_}=A,
                  {Rcv+RcvBytes,Sent+SentBytes}
          end,
    lists:foldl(Fun, {0,0}, Eth).

%%--------------------------------------------------------------------
%% Function: start_beam/1
%% Purpose: Start an Erlang node on given host
%%--------------------------------------------------------------------
start_beam(Host) ->
    Args = ts_utils:erl_system_args(),
    ?LOGF("Starting os_mon beam on host ~p ~n", [Host], ?NOTICE),
    ?LOGF("~p Args: ~p~n", [Host, Args], ?DEB),
    slave:start(list_to_atom(Host), ?NODE, Args).
