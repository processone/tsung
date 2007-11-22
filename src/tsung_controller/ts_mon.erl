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
%%%  the two.


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

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("rrdtool.hrl").

%% External exports
-export([start/1, stop/0, newclient/1, endclient/1, sendmes/1, add/2,
         start_clients/1, abort/0, status/0, rcvmes/1, add/1, dumpstats/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-define(DUMP_FILENAME,"tsung.dump").

-record(state, {log,          % log fd
                backend,      % type of backend: text|rrdtool
                log_dir,      % log directory
                dump_interval,%
                dumpfile,     % file used when dumptrafic is set light or full
                client=0,     % number of clients currently running
                maxclient=0,  % max of simultaneous clients
                stats,        % dict keeping stats info
                stop = false, % true if we should stop
                laststats,    % values of last printed stats
                lastdate,     % date of last printed stats
                type          % type of logging (none, light, full)
               }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% FUNCTION start/0
%% PURPOSE Start the monitoring process
%% RETURN VALUE ok | throw({error, Reason})
%%----------------------------------------------------------------------
start(LogDir) ->
    ?LOG("starting monitor, global ~n",?NOTICE),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

start_clients({Machines, Dump, BackEnd}) ->
    gen_server:call({global, ?MODULE}, {start_logger, Machines, Dump, BackEnd},
                    infinity).
stop() ->
    gen_server:cast({global, ?MODULE}, {stop}).

add(nocache,Data) ->
    gen_server:cast({global, ?MODULE}, {add, Data}).

add(Data) ->
    ts_session_cache:add(Data).

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

sendmes({none, _, _}) ->
    skip;
sendmes({_Type, Who, What}) ->
    gen_server:cast({global, ?MODULE}, {sendmsg, Who, now(), What}).

rcvmes({none, _, _})-> skip;
rcvmes({_, _, closed}) -> skip;
rcvmes({_Type, Who, What})  ->
    gen_server:cast({global, ?MODULE}, {rcvmsg, Who, now(), What}).


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
    ?LOGF("Init, log dir is ~p~n",[LogDir],?NOTICE),
    Base = filename:basename(?config(log_file)),
    backup_config(LogDir, ?config(config_file)),
    Filename = filename:join(LogDir, Base),
    case file:open(Filename,write) of
        {ok, Stream} ->
            ?LOG("starting monitor~n",?NOTICE),
            Tab = dict:new(),
            {ok, #state{ log     = Stream,
                         dump_interval = ?config(dumpstats_interval),
                         log_dir = LogDir,
                         stats   = Tab,
                         lastdate  = now(),
                         laststats = Tab
                       }};
        {error, Reason} ->
            ?LOGF("Can't open mon log file! ~p~n",[Reason], ?ERR),
            {stop, Reason}
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
handle_call({start_logger, Machines, DumpType, rrdtool}, From, State) ->
    case whereis(rrdtool) of
        undefined ->
            ?LOG("rrdtool port not available, switch to text backend~n",?WARN),
            start_logger({Machines, DumpType, text}, From, State);
        _ ->
            ?LOG("rrdtool port OK !~n",?DEB),
            start_logger({Machines, DumpType, rrdtool}, From, State)
    end;
handle_call({start_logger, Machines, DumpType, text}, From, State) ->
    start_logger({Machines, DumpType, text}, From, State);

%%% get status
handle_call({status}, _From, State ) ->
    Request = ts_stats_mon:status(),
    Interval = ts_utils:elapsed(State#state.lastdate, now()) / 1000,
    Phase = dict:find({newphase, count}, State#state.stats),
    Connected = dict:find({connected, sum}, State#state.stats),
    Reply = { State#state.client, Request,Connected, Interval, Phase},
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
handle_cast({add, Data}, State) when is_list(Data) ->
    NewStats = lists:foldl(fun ts_stats_mon:add_stats_data/2, State#state.stats, Data ),
    {noreply,State#state{stats=NewStats}};

handle_cast({add, Data}, State) when is_tuple(Data) ->
    NewStats = ts_stats_mon:add_stats_data(Data, State#state.stats),
    {noreply,State#state{stats=NewStats}};

handle_cast({newclient, Who, When}, State) ->
    Clients =  State#state.client+1,
    Tab = State#state.stats,
    NewTab = dict:update_counter({users_count, count}, 1, Tab),

    case State#state.type of
        none -> ok;
        _ ->
            io:format(State#state.dumpfile,"NewClient:~w:~w~n",[When, Who]),
            io:format(State#state.dumpfile,"load:~w~n",[Clients])
    end,
    case Clients > State#state.maxclient of
        true ->
            {noreply, State#state{client = Clients, maxclient=Clients, stats=NewTab}};
        false ->
            {noreply, State#state{client = Clients, stats=NewTab}}
    end;

handle_cast({endclient, Who, When, Elapsed}, State) ->
    Clients =  State#state.client-1,
    Tab = State#state.stats,
    New1Tab = dict:update_counter({finish_users_count, count}, 1, Tab),

    %% update session sample
    MyFun = fun (OldV) -> ts_stats_mon:update_stats(sample, OldV, Elapsed) end,
    Init = [Elapsed, 0, Elapsed, Elapsed, 1], % initial value of the sample
    NewTab = dict:update({session, sample}, MyFun, Init, New1Tab),

    case State#state.type of
        none ->
            skip;
        _Type ->
            io:format(State#state.dumpfile,"EndClient:~w:~w~n",[When, Who]),
            io:format(State#state.dumpfile,"load:~w~n",[Clients])
    end,
    case {Clients, State#state.stop} of
        {0, true} ->
            {stop, normal, State};
        _ ->
            {noreply, State#state{client = Clients, stats=NewTab}}
    end;

handle_cast({dumpstats}, State) ->
    export_stats(State),
    NewStats = ts_stats_mon:reset_all_stats(State#state.stats),
    {noreply, State#state{laststats = NewStats, stats=NewStats,lastdate=now()}};


handle_cast({sendmsg, _, _, _}, State = #state{type = none}) ->
    {noreply, State};

handle_cast({sendmsg, Who, When, What}, State = #state{type=light,dumpfile=Log}) ->
    io:format(Log,"Send:~w:~w:~-44s~n",[When,Who, binary_to_list(What)]),
    {noreply, State};

handle_cast({sendmsg, Who, When, What}, State=#state{dumpfile=Log}) ->
    io:format(Log,"Send:~w:~w:~s~n",[When,Who,binary_to_list(What)]),
    {noreply, State};

handle_cast({rcvmsg, _, _, _}, State = #state{type=none}) ->
    {noreply, State};

handle_cast({rcvmsg, Who, When, What}, State = #state{type=light, dumpfile=Log}) ->
    io:format(Log,"Recv:~w:~w:~-44s~n",[When,Who, binary_to_list(What)]),
    {noreply, State};

handle_cast({rcvmsg, Who, When, What}, State=#state{dumpfile=Log}) ->
    io:format(Log, "Recv:~w:~w:~s~n",[When,Who,binary_to_list(What)]),
    {noreply, State};

handle_cast({stop}, State = #state{client = 0}) ->
    ts_os_mon:stop(),
    {stop, normal, State};
handle_cast({stop}, State) -> % we should stop, wait until no more clients are alive
    {noreply, State#state{stop = true}};

handle_cast({abort}, State) -> % stop now !
    ?LOG("Aborting by request !~n", ?EMERG),
    ts_os_mon:stop(),
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

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ?LOGF("stoping monitor (~p)~n",[Reason],?NOTICE),
    export_stats(State),
    io:format(State#state.log,"EndMonitor:~w~n",[now()]),
    file:close(State#state.log),
    slave:stop(node()),
    ok.

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
start_logger({Machines, DumpType, BackEnd}, _From, State) ->
    ?LOGF("Activate clients with ~p backend~n",[BackEnd],?NOTICE),
    timer:apply_interval(State#state.dump_interval, ?MODULE, dumpstats, [] ),
    start_launchers(Machines),
    ts_stats_mon:set_output(BackEnd,State#state.log),
    case DumpType of
        none ->
            {reply, ok, State#state{backend=BackEnd, type=DumpType}};
        _ ->
            Filename = filename:join(State#state.log_dir,?DUMP_FILENAME),
            case file:open(Filename,write) of
                {ok, Stream} ->
                    ?LOG("dump file opened, starting monitor~n",?INFO),
                    {reply, ok, State#state{dumpfile=Stream,
                                            type=DumpType,
                                            backend=BackEnd}};
                {error, Reason} ->
                    ?LOGF("Can't open mon dump file! ~p~n",[Reason], ?ERR),
                    {reply, ok, State#state{type=none, backend=BackEnd}}
            end
    end.

%%----------------------------------------------------------------------
%% Func: export_stats/2
%%----------------------------------------------------------------------
export_stats(State=#state{backend=text}) ->
    DateStr = ts_utils:now_sec(),
    io:format(State#state.log,"# stats: dump at ~w~n",[DateStr]),
    %% print number of simultaneous users
    io:format(State#state.log, "stats: ~p ~p ~p~n", [users,
                                                     State#state.client,
                                                     State#state.maxclient]),
    Param = {State#state.laststats,State#state.log},
    dict:fold(fun ts_stats_mon:print_stats_txt/3, Param, State#state.stats),
    ts_stats_mon:dumpstats().

%%----------------------------------------------------------------------
%% Func: start_launchers/2
%% @doc start the launcher on clients nodes
%%----------------------------------------------------------------------
start_launchers(Machines) ->
    ?DebugF("Need to start tsung client on ~p~n",[Machines]),
    GetHost = fun(A) -> list_to_atom(A#client.host) end,
    HostList = lists:map(GetHost, Machines),
    ?DebugF("Hostlist is ~p~n",[HostList]),
    %% starts beam on all client hosts
    lists:foreach(fun(B) -> ts_config_server:newbeam(B) end, HostList).

%%----------------------------------------------------------------------
%% Func: backup_config/2
%% @doc copy a backup copy of the config file in the log directory
%%   This is useful to have an history of all parameters of a test.
%%----------------------------------------------------------------------
backup_config(Dir, Config) ->
    Backup = filename:basename(Config),
    {ok, _} = file:copy(Config, filename:join(Dir,Backup)).

