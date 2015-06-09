%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2004 IDEALX
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


%%----------------------------------------------------------------------
%% @copyright 2001 IDEALX
%% @author Nicolas Niclausse <nicolas@niclux.org>
%% @since 8 Feb 2001
%%
%% @doc monitor and log events: arrival and departure of users, new
%% connections, os_mon and send/rcv message (when dump is set to true)
%%----------------------------------------------------------------------

-module(ts_mon).
-author('nicolas@niclux.org').
-vc('$Id$ ').

-behaviour(gen_server).

-include("ts_config.hrl").

%% External exports
-export([start/1, stop/0, newclient/1, endclient/1, sendmes/1, add/2,
         start_clients/1, abort/0, status/0, rcvmes/1, add/1, dumpstats/0,
         add_match/2, dump/1, launcher_is_alive/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-define(DUMP_FILENAME,"tsung.dump").
-define(FULLSTATS_FILENAME,"tsung-fullstats.log").
-define(DELAYED_WRITE_SIZE,524288). % 512KB
-define(DELAYED_WRITE_DELAY,5000).  % 5 sec

%% one global ts_stats_mon procs + 4 dedicated stats procs to share the load
-define(STATSPROCS, [request, connect, page, transaction, ts_stats_mon]).

-record(state, {log,          % log fd
                backend,      % type of backend: text|...
                log_dir,      % log directory
                fullstats,    % fullstats fd
                dump_interval,%
                dumpfile,     % file used when dumptrafic is set light or full
                client=0,     % number of clients currently running
                maxclient=0,  % max of simultaneous clients
                stats,        % record keeping stats info
                stop = false, % true if we should stop
                laststats,    % values of last printed stats
                lastdate,     % date of last printed stats
                type,         % type of logging (none, light, full)
                launchers=0,  % number of launchers started
                timer_ref,    % timer reference (for dumpstats)
                wait_gui=false% wait gui before stopping
               }).

-record(stats, {
          users_count         = 0,
          finish_users_count  = 0,
          os_mon,
          session             = []
          }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% @spec start(LogDir::string())-> {ok, Pid::pid()} | ignore | {error, Error::term()}
%% @doc Start the monitoring process
%% @end
%%----------------------------------------------------------------------
start(LogDir) ->
    ?LOG("starting monitor, global ~n",?NOTICE),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

%% @spec start_clients({Machines::term(), Dump::string(), BackEnd::atom()}) -> ok
start_clients({Machines, Dump, BackEnd}) ->
    gen_server:call({global, ?MODULE}, {start_logger, Machines, Dump, BackEnd},
                    infinity).
stop() ->
    gen_server:cast({global, ?MODULE}, {stop}).

add(nocache,Data) ->
    gen_server:cast({global, ?MODULE}, {add, Data}).


add(Data) ->
    ts_mon_cache:add(Data).

add_match(Data,{UserId,SessionId,RequestId,Tr,Name}) ->
    add_match(Data,{UserId,SessionId,RequestId,[],Tr,Name});
add_match(Data=[Head|_],{UserId,SessionId,RequestId,Bin,Tr,Name}) ->
    TimeStamp=?NOW,
    put(last_match,Head),
    ts_mon_cache:add_match(Data,{UserId,SessionId,RequestId,TimeStamp, Bin,Tr,Name}).

status() ->
    gen_server:call({global, ?MODULE}, {status}).

abort() ->
    gen_server:cast({global, ?MODULE}, {abort}).

dumpstats() ->
    gen_server:cast({global, ?MODULE}, {dumpstats}).

newclient({Who, When}) ->
    gen_server:cast({global, ?MODULE}, {newclient, Who, When}).

endclient({Who, When, Elapsed}) ->
    gen_server:cast({global, ?MODULE}, {endclient, Who, When, Elapsed}).

sendmes({none, _, _})       -> skip;
sendmes({protocol, _, _})   -> skip;
sendmes({protocol_local, _, _})   -> skip;
sendmes({_Type, Who, What}) ->
    gen_server:cast({global, ?MODULE}, {sendmsg, Who, ?NOW, What}).

rcvmes({none, _, _})    -> skip;
rcvmes({protocol, _, _})-> skip;
rcvmes({protocol_local, _, _})-> skip;
rcvmes({_, _, closed})  -> skip;
rcvmes({_Type, Who, What})  ->
    gen_server:cast({global, ?MODULE}, {rcvmsg, Who, ?NOW, What}).

dump({none, _, _})-> skip;
dump({cached, << >> })-> skip;
dump({_Type, Who, What})  ->
    gen_server:cast({global, ?MODULE}, {dump, Who, ?NOW, What});
dump({cached, Data})->
    gen_server:cast({global, ?MODULE}, {dump, cached, Data}).

launcher_is_alive() ->
    gen_server:cast({global, ?MODULE}, {launcher_is_alive}).


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
init([LogDir]) ->
    ?LOGF("Init, log dir is ~p~n",[LogDir],?INFO),
    Stats = #stats{os_mon  = dict:new()},
    State=#state{ dump_interval = ?config(dumpstats_interval),
                  log_dir   = LogDir,
                  stats     = Stats,
                  lastdate  = ?NOW,
                  laststats = Stats
                },
    case ?config(mon_file) of
        "-" ->
            {ok, State#state{log=standard_io}};
        Name ->
            Filename = filename:join(LogDir, Name),
            case file:open(Filename,[write]) of
                {ok, Stream} ->
                    ?LOG("starting monitor~n",?INFO),
                    {ok, State#state{log=Stream}};
                {error, Reason} ->
                    ?LOGF("Can't open mon log file! ~p~n",[Reason], ?ERR),
                    {stop, Reason}
            end
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
handle_call({start_logger, Machines, DumpType, Backend}, From, State) ->
    start_logger({Machines, DumpType, Backend}, From, State);

%%% get status
handle_call({status}, _From, State=#state{stats=Stats}) ->
    {ok, Localhost} = ts_utils:node_to_hostname(node()),
    CpuName         = {{cpu,"tsung_controller@"++Localhost}, sample},
    CPU = case dict:find(CpuName,Stats#stats.os_mon) of
              {ok, [ValCPU|_]} -> ValCPU ;
              _ -> 0
          end,

    Request     = ts_stats_mon:status(request),
    Interval    = ts_utils:elapsed(State#state.lastdate, ?NOW) / 1000,
    Phase       = ts_stats_mon:status(newphase,sum),
    Connected   =  case ts_stats_mon:status(connected,sum) of
                     {ok, Val} -> Val;
                     _ -> 0
                 end,
    Reply = { State#state.client, Request, Connected, Interval, Phase, CPU},
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    ?LOGF("Unknown call ~p !~n",[Request],?ERR),
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({add, _Data}, State=#state{wait_gui=true}) ->
    {noreply,State};
handle_cast({add, Data}, State=#state{stats=Stats}) when is_list(Data) ->
    case State#state.backend of
        fullstats -> io:format(State#state.fullstats,"~p~n",[Data]);
        _Other   -> ok
    end,
    New = lists:foldl(fun ts_stats_mon:add_stats_data/2, Stats#stats.os_mon, Data),
    NewStats = Stats#stats{os_mon=New},
    {noreply,State#state{stats=NewStats}};

handle_cast({add, Data}, State=#state{stats=Stats}) when is_tuple(Data) ->
    case State#state.backend of
        fullstats -> io:format(State#state.fullstats,"~p~n",[Data]);
        _Other   -> ok
    end,
    New = ts_stats_mon:add_stats_data(Data, Stats#stats.os_mon),
    NewStats = Stats#stats{os_mon=New},
    {noreply,State#state{stats=NewStats}};

handle_cast({newclient, Who, When}, State=#state{stats=Stats}) ->
    Clients =  State#state.client+1,
    OldCount = Stats#stats.users_count,
    NewStats = Stats#stats{users_count=OldCount+1},

    case State#state.type of
        none     -> ok;
        protocol -> ok;
        protocol_local -> ok;
        _ ->
            io:format(State#state.dumpfile,"NewClient:~w:~p~n",[ts_utils:time2sec_hires(When), Who]),
            io:format(State#state.dumpfile,"load:~w~n",[Clients])
    end,
    case Clients > State#state.maxclient of
        true ->
            {noreply, State#state{client = Clients, maxclient=Clients, stats=NewStats}};
        false ->
            {noreply, State#state{client = Clients, stats=NewStats}}
    end;

handle_cast({endclient, Who, When, Elapsed}, State=#state{stats=Stats}) ->
    Clients =  State#state.client-1,
    OldSession = Stats#stats.session,
    %% update session sample
    NewSession = ts_stats_mon:update_stats(sample, OldSession, Elapsed),
    OldCount = Stats#stats.finish_users_count,
    NewStats = Stats#stats{finish_users_count=OldCount+1,session= NewSession},

    case State#state.type of
        none ->
            skip;
        protocol ->
            skip;
        protocol_local ->
            skip;
        _Type ->
            io:format(State#state.dumpfile,"EndClient:~w:~p~n",[ts_utils:time2sec_hires(When), Who]),
            io:format(State#state.dumpfile,"load:~w~n",[Clients])
    end,
    {noreply, State#state{client = Clients, stats=NewStats}};

handle_cast({dumpstats}, State=#state{stats=Stats}) ->
    export_stats(State),
    NewSessions = ts_stats_mon:reset_all_stats(Stats#stats.session),
    NewOSmon = ts_stats_mon:reset_all_stats(Stats#stats.os_mon),
    NewStats = Stats#stats{session=NewSessions, os_mon=NewOSmon},
    {noreply, State#state{laststats = Stats, stats=NewStats,lastdate=?NOW}};


handle_cast({sendmsg, _, _, _}, State = #state{type = none}) ->
    {noreply, State};

handle_cast({sendmsg, Who, When, What}, State = #state{type=light,dumpfile=Log}) ->
    io:format(Log,"Send:~w:~w:~-44s~n",[ts_utils:time2sec_hires(When),Who, binary_to_list(What)]),
    {noreply, State};

handle_cast({sendmsg, Who, When, What}, State=#state{type=full,dumpfile=Log}) when is_binary(What)->
    io:format(Log,"Send:~w:~w:~s~n",[ts_utils:time2sec_hires(When),Who,binary_to_list(What)]),
    {noreply, State};

handle_cast({sendmsg, Who, When, What}, State=#state{type=full,dumpfile=Log}) ->
    io:format(Log,"Send:~w:~w:~p~n",[ts_utils:time2sec_hires(When),Who,What]),
    {noreply, State};

handle_cast({dump, Who, When, What}, State=#state{type=protocol,dumpfile=Log}) ->
    io:format(Log,"~w;~w;~s~n",[ts_utils:time2sec_hires(When),Who,What]),
    {noreply, State};

handle_cast({dump, cached, Data}, State=#state{type=protocol,dumpfile=Log}) ->
    file:write(Log,Data),
    {noreply, State};

handle_cast({rcvmsg, _, _, _}, State = #state{type=none}) ->
    {noreply, State};

handle_cast({rcvmsg, Who, When, What}, State = #state{type=light, dumpfile=Log}) when is_binary(What)->
    io:format(Log,"Recv:~w:~w:~-44s~n",[ts_utils:time2sec_hires(When),Who, binary_to_list(What)]),
    {noreply, State};
handle_cast({rcvmsg, Who, When, What}, State = #state{type=light, dumpfile=Log}) ->
    io:format(Log,"Recv:~w:~w:~-44p~n",[ts_utils:time2sec_hires(When),Who, What]),
    {noreply, State};

handle_cast({rcvmsg, Who, When, What}, State=#state{type=full,dumpfile=Log}) when is_binary(What)->
    io:format(Log, "Recv:~w:~w:~s~n",[ts_utils:time2sec_hires(When),Who,binary_to_list(What)]),
    {noreply, State};

handle_cast({rcvmsg, Who, When, What}, State=#state{type=full,dumpfile=Log}) ->
    io:format(Log, "Recv:~w:~w:~p~n",[ts_utils:time2sec_hires(When),Who,What]),
    {noreply, State};

handle_cast({stop}, State = #state{client=0, launchers=1, timer_ref=TRef}) ->
    ?LOG("Stop asked, no more users, last launcher stopped, OK to stop~n", ?INFO),
    case ?config(keep_web_gui) of
        true ->
            io:format(standard_io,"All slaves have stopped; keep controller and web dashboard alive. ~nHit CTRL-C or click Stop on the dashboard to stop.~n",[]),
            timer:cancel(TRef),
            close_stats(State),
            {noreply, State#state{wait_gui=true}};
        _ ->
            {stop, normal, State}
    end;
handle_cast({stop}, State=#state{launchers=L}) -> % we should stop, wait until no more clients are alive
    ?LOG("A launcher has finished, but not all users have finished, wait before stopping~n", ?NOTICE),
    {noreply, State#state{stop = true, launchers=L-1}};

handle_cast({launcher_is_alive}, State=#state{launchers=L}) ->
    ?LOG("A launcher has started~n", ?INFO),
    {noreply, State#state{launchers=L+1}};


handle_cast({abort}, State) -> % stop now !
    ?LOG("Aborting by request !~n", ?EMERG),
    ts_stats_mon:add({ count, error_abort }),
    {stop, abort, State};

handle_cast(Msg, State) ->
    ?LOGF("Unknown msg ~p !~n",[Msg], ?WARN),
    {noreply, State}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


close_stats(State) ->
    export_stats(State),
    % blocking call to all ts_stats_mon; this way, we are
    % sure the last call to dumpstats is finished
    lists:foreach(fun(Name) -> ts_stats_mon:status(Name) end, ?STATSPROCS),
    case State#state.backend of
        json ->
            io:format(State#state.log,"]}]}~n",[]);
        _ ->
            io:format(State#state.log,"EndMonitor:~w~n",[?NOW])
        end,
    case State#state.log of
        standard_io -> ok;
        Dev         -> file:close(Dev)
    end,
    file:close(State#state.fullstats).

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, #state{wait_gui=true}) ->
    ?LOGF("stopping monitor by gui (~p)~n",[Reason],?NOTICE);
terminate(Reason, State) ->
    ?LOGF("stopping monitor (~p)~n",[Reason],?NOTICE),
    close_stats(State),
    slave:stop(node()).

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start_logger/3
%% Purpose: open log files and start timer
%% Returns: {reply, ok, State} | {stop, Reason, State}
%%----------------------------------------------------------------------
%% fulltext backend: open log file with compression enable and delayed_write
start_logger({Machines, DumpType, fullstats}, From, State=#state{fullstats=undefined}) ->
    Filename = filename:join(State#state.log_dir,?FULLSTATS_FILENAME),
    ?LOG("Open file with delayed_write for fullstats backend~n",?NOTICE),
    case file:open(Filename,[write, {delayed_write, ?DELAYED_WRITE_SIZE, ?DELAYED_WRITE_DELAY}]) of
        {ok, Stream} ->
            start_logger({Machines, DumpType, fullstats}, From, State#state{fullstats=Stream});
        {error, Reason} ->
            ?LOGF("Can't open mon log file ~p! ~p~n",[Filename,Reason], ?ERR),
            {stop, Reason, State}
    end;

start_logger({Machines, DumpType, Backend}, _From, State=#state{log=Log,fullstats=FS}) ->
    ?LOGF("Activate clients with ~p backend~n",[Backend],?NOTICE),
    print_headline(Log,Backend),
    start_launchers(Machines),
    {ok, TRef} = timer:apply_interval(State#state.dump_interval, ?MODULE, dumpstats, [] ),
    lists:foreach(fun(Name) -> ts_stats_mon:set_output(Backend,{Log,FS}, Name) end, ?STATSPROCS),
    start_dump(State#state{type=DumpType, backend=Backend, timer_ref=TRef}).

print_headline(Log,json)->
    DateStr = ts_utils:now_sec(),
    io:format(Log,"{~n \"stats\": [~n {\"timestamp\": ~p,  \"samples\": [",[DateStr]);
print_headline(_Log,_Backend)->
    ok.

%% @spec start_dump(State::record(state)) -> {reply, Reply, State}
%% @doc open file for dumping traffic
start_dump(State=#state{type=none}) ->
    {reply, ok, State};
start_dump(State=#state{type=Type}) ->
    Filename = filename:join(State#state.log_dir,?DUMP_FILENAME),
    case file:open(Filename,[write, {delayed_write, ?DELAYED_WRITE_SIZE, ?DELAYED_WRITE_DELAY}]) of
        {ok, Stream} ->
            ?LOG("dump file opened, starting monitor~n",?INFO),
            case Type of
                protocol ->
                    io:format(Stream,"#date;pid;id;http method;host;URL;HTTP status;size;duration;transaction;match;error;tag~n",[]);
                _ ->
                    ok
            end,
            {reply, ok, State#state{dumpfile=Stream}};
        {error, Reason} ->
            ?LOGF("Can't open mon dump file! ~p~n",[Reason], ?ERR),
            {reply, ok, State#state{type=none}}
    end.

%%----------------------------------------------------------------------
%% Func: export_stats/1
%%----------------------------------------------------------------------
export_stats(State=#state{log=Log,stats=Stats,laststats=LastStats, backend=json}) ->
    DateStr = ts_utils:now_sec(),
    io:format(Log,"]},~n {\"timestamp\": ~w,  \"samples\": [",[DateStr]),
    %% print number of simultaneous users
    io:format(Log,"   {\"name\": \"users\", \"value\": ~p, \"max\": ~p}",[State#state.client,State#state.maxclient]),
    export_stats_common(json, Stats,LastStats,Log);

export_stats(State=#state{log=Log,stats=Stats,laststats=LastStats, backend=BackEnd}) ->
    DateStr = ts_utils:now_sec(),
    io:format(Log,"# stats: dump at ~w~n",[DateStr]),
    %% print number of simultaneous users
    io:format(Log,"stats: ~p ~p ~p~n",[users,State#state.client,State#state.maxclient]),
    export_stats_common(BackEnd, Stats,LastStats,Log).

export_stats_common(BackEnd, Stats,LastStats,Log)->
    Param = {BackEnd,LastStats#stats.os_mon,Log},
    dict:fold(fun ts_stats_mon:print_stats/3, Param, Stats#stats.os_mon),
    ts_stats_mon:print_stats({session, sample}, Stats#stats.session,{BackEnd,[],Log}),
    ts_stats_mon:print_stats({users_count, count},
                                 Stats#stats.users_count,
                                 {BackEnd,LastStats#stats.users_count,Log}),
    ts_stats_mon:print_stats({finish_users_count, count},
                             Stats#stats.finish_users_count,
                             {BackEnd,LastStats#stats.finish_users_count,Log}),
    lists:foreach(fun(Name) -> ts_stats_mon:dumpstats(Name) end, ?STATSPROCS).

%%----------------------------------------------------------------------
%% Func: start_launchers/2
%% @doc start the launcher on clients nodes
%%----------------------------------------------------------------------
start_launchers(Machines) ->
    ?LOGF("Tsung clients setup: ~p~n",[Machines],?DEB),
    GetHost = fun(A) -> list_to_atom(A#client.host) end,
    HostList = lists:map(GetHost, Machines),
    ?LOGF("Starting tsung clients on hosts: ~p~n",[HostList],?NOTICE),
    %% starts beam on all client hosts
    ts_config_server:newbeams(HostList).

%% post_process_logs(FileName) ->
%%     {ok, Device} = file:open(FileName, [read]),
%%     post_process_line(io:get_line(Device, ""), Device, []).

%% post_process_line(eof, Device, State) ->
%%     file:close(Device);
%% post_process_line("End "++ _, Device, Logs) ->
%%     post_process_line(io:get_line(Device, ""),Device, Logs);
%% post_process_line("# stats: dump at "++ TimeStamp, D, Logs=#logs{start_time=undefined}) ->
%%     {StartTime,_}=string:to_integer(TimeStamp),
%%     post_process_line(io:get_line(D, ""),D, #logs{start_time=StartTime});
%% post_process_line("# stats: dump at "++ TimeStamp, Dev, Logs) ->
%%     {Time,_}=string:to_integer(TimeStamp),
%%     Current=Time-Logs#logs.start_time,
%%     post_process_line(io:get_line(Dev, ""),Dev, Logs#logs{current_time=Current});
%% post_process_line("# stats:  "++ Stats, Dev, Logs) ->
%%     case string:tokens(Stats," ") of
%%         {"users",Count, GlobalCount} ->
%%             todo;
%%         {Name, Count, Max} ->
%%             todo;
%%         {"tr_" ++ TrName, Count, Mean, StdDev, Max, Min, GMean,GCount} ->
%%             todo;
%%         {"{"++ Name, Count, Mean, StdDev, Max, Min, GMean,GCount} ->
%%             todo;
%%         {Name, Count, Mean, StdDev, Max, Min, GMean,GCount} ->
%%             todo
%%     end,
%%     post_process_line(io:get_line(Dev, ""),Dev, Logs).
