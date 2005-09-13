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

%%%  Created :  8 Feb 2001 by Nicolas Niclausse <nniclausse@idealx.com>

%%----------------------------------------------------------------------
%% HEADER ts_mon
%% COPYRIGHT IDEALX (C) 2001
%% PURPOSE monitor and log events and stats
%% DESCRIPTION
%%   TODO ...
%%----------------------------------------------------------------------


-module(ts_mon).
-author('nniclausse@idealx.com').
-vc('$Id$ ').

-behaviour(gen_server).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("rrdtool.hrl").

%% External exports
-export([start/1, stop/0, newclient/1, endclient/1, newclient/1, sendmes/1,
         start_clients/1, abort/0, status/0,
         rcvmes/1, add/1, add/2, dumpstats/0
		]).

-export([update_stats/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-define(DUMP_FILENAME,"idx-tsunami.dump").

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

add(nocache, Data) ->
	gen_server:cast({global, ?MODULE}, {add, Data}).
add(Data) ->
	ts_session_cache:add(Data).

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
    Request = dict:find(request, State#state.stats),
    Interval = ts_utils:elapsed(State#state.lastdate, now()) / 1000,
    Phase = dict:find(newphase, State#state.stats),
    Reply = { State#state.client, Request, Interval, Phase},
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
handle_cast({sendmsg, Who, When, _What}, State = #state{type = none}) ->
	{noreply, State};

handle_cast({sendmsg, Who, When, What}, State = #state{type=light,dumpfile=Log}) ->
	io:format(Log,"Send:~w:~w:~-44s~n",[When,Who, binary_to_list(What)]),
	{noreply, State};

handle_cast({sendmsg, Who, When, What}, State=#state{dumpfile=Log}) ->
	io:format(Log,"Send:~w:~w:~s~n",[When,Who,binary_to_list(What)]),
	{noreply, State};

handle_cast({add, Data}, State) when is_list(Data) ->
	NewStats = lists:foldl(fun add_stats_data/2, State#state.stats, Data ),
	{noreply,State#state{stats=NewStats}};

handle_cast({add, Data}, State) when is_tuple(Data) ->
	NewStats = add_stats_data(Data, State#state.stats),
	{noreply,State#state{stats=NewStats}};

handle_cast({dumpstats}, State) ->
	export_stats(State),
	NewStats = reset_all_stats(State#state.stats),
	{noreply, State#state{laststats = NewStats, stats=NewStats,lastdate=now()}};

handle_cast({rcvmsg, _, _, _}, State = #state{type=none}) ->
	{noreply, State};

handle_cast({rcvmsg, Who, When, What}, State = #state{type=light, dumpfile=Log}) ->
	io:format(Log,"Recv:~w:~w:~-44s~n",[When,Who, binary_to_list(What)]),
	{noreply, State};

handle_cast({rcvmsg, Who, When, What}, State=#state{dumpfile=Log}) ->
	io:format(Log, "Recv:~w:~w:~s~n",[When,Who,binary_to_list(What)]),
	{noreply, State};

handle_cast({newclient, Who, When}, State) ->
	Clients =  State#state.client+1,
	Max= lists:max([Clients,State#state.maxclient]),
	Tab = State#state.stats,
	Add = fun ({count,OldVal}) -> {count,OldVal+1} end,
	NewTab = dict:update(users_count, Add, {count,1}, Tab),

	case State#state.type of 
		none -> ok;
		_ ->
			io:format(State#state.dumpfile,"NewClient:~w:~w~n",[When, Who]),
			io:format(State#state.dumpfile,"load:~w~n",[Clients])
	end,
	{noreply, State#state{client = Clients, maxclient=Max, stats=NewTab}};

handle_cast({endclient, Who, When, Elapsed}, State) ->
	Clients =  State#state.client-1,
	Tab = State#state.stats,
	Add = fun ({count,OldVal}) -> {count,OldVal+1} end,
	New1Tab = dict:update(finish_users_count, Add, {count,1}, Tab),

	%% update session sample
	MyFun = fun (OldV) -> update_stats(OldV, Elapsed) end,
	NewTab = dict:update(session, MyFun, update_stats({sample,[]},Elapsed), New1Tab),

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
%% Func: add_stats_data/2
%% Purpose: update or add value in dictionnary
%% Returns: Dict
%%----------------------------------------------------------------------
add_stats_data({sample, Type, Value}, Stats)  ->
	MyFun = fun (OldVal) -> update_stats(OldVal, Value) end,
	dict:update(Type, MyFun, update_stats({sample, []},Value), Stats);
%% continuous incrementing counters 
add_stats_data({sample_counter, Type, Value}, Stats) ->
	MyFun = fun (OldVal) -> update_stats(OldVal, Value) end,
	dict:update(Type, MyFun, update_stats({sample_counter, []},Value), Stats);
%% increase by one when called
add_stats_data({count, Type}, Stats)  ->
	Add = fun ({Type, OldVal}) -> {Type, OldVal+1} end,
	dict:update(Type, Add, {count, 1}, Stats);
%% cumulative counter
add_stats_data({sum, Type, Val}, Stats)  ->
	Add = fun ({Type, OldVal}) -> {Type, OldVal+Val} end,
	dict:update(Type, Add, {sum, Val}, Stats);
add_stats_data(Data, Stats) ->
    ?LOGF("Wrong stats data ! ~p~n",[Data], ?WARN),
    Stats.

%%----------------------------------------------------------------------
%% Func: export_stats/2
%%----------------------------------------------------------------------
export_stats(State=#state{backend=rrdtool,laststats=Last, stats=Dict}) ->
    New_Keys = dict:fetch_keys(Dict) -- dict:fetch_keys(Last),
    ?DebugF("create new rrd keys [~p]~n",[New_Keys]),
    rrd_create(State#state.log_dir, New_Keys, State#state.dump_interval),
    Res = dict:to_list(State#state.stats),
    rrd_update(State#state.log_dir,Res);
    
export_stats(State=#state{backend=text}) ->
	DateStr = ts_utils:now_sec(),
	io:format(State#state.log,"# stats: dump at ~w~n",[DateStr]),
	Res = dict:to_list(State#state.stats),
	%% print number of simultaneous users
	io:format(State#state.log, "stats: ~p ~p ~p~n", [users, State#state.client,
													 State#state.maxclient]),
	print_dist_list(Res, State#state.laststats , State#state.log).

%%----------------------------------------------------------------------
%% Func: export_stats/2
%% Args: Stats, Last, Logfile
%%----------------------------------------------------------------------
print_dist_list([], _, _) ->
	done;
print_dist_list([{Key,{_Type,[Mean,0,Max,Min,Count|_]}}|Tail],LastRes,Logfile)->
	io:format(Logfile, "stats: ~p ~p ~p ~p ~p ~p~n", 
			  [Key, Count, Mean, 0, Max, Min ]),
	print_dist_list(Tail, LastRes, Logfile);
print_dist_list([{Key,{_Type,[Mean,Var,Max,Min,Count|_]}}|Tail],LastRes,Logfile)->
	StdVar = math:sqrt(Var/Count),
	io:format(Logfile, "stats: ~p ~p ~p ~p ~p ~p~n",
			  [Key, Count, Mean, StdVar, Max, Min ]),
	print_dist_list(Tail, LastRes, Logfile);
print_dist_list([{Key, {_Type, [Sample,Last]}} | Tail], LastRes, Logfile) ->
	io:format(Logfile, "stats: ~p ~p ~p~n", [Key, Sample, Last ]),
    print_dist_list(Tail, LastRes, Logfile);
print_dist_list([{Key, {_Type, Value}} | Tail], LastRes, Logfile) ->
	case dict:find(Key, LastRes) of 
		{ok,  {_, OldVal}} ->
			PrevVal = OldVal ;
		error ->
			PrevVal = 0 
	end,
	io:format(Logfile, "stats: ~p ~p ~p~n", [Key, Value-PrevVal, Value]),
	print_dist_list(Tail, LastRes, Logfile).
	
	
%%----------------------------------------------------------------------
%% Func: update_stats/2
%%----------------------------------------------------------------------
update_stats({sample, []}, New) ->
	{sample, [New, 0, New, New, 1]};
update_stats({sample, [Mean, Var, Max, Min, Count]}, Value) ->
	{NewMean, NewVar, _} = ts_stats:meanvar(Mean, Var, [Value], Count),
	NewMax = lists:max([Max, Value]),
	NewMin = lists:min([Min, Value]),
	{sample, [NewMean, NewVar, NewMax, NewMin, Count+1]};

update_stats({sample_counter,[]}, New) -> %% first call, store the initial value
	{sample_counter, [0, 0, 0, 0, 0, New]};
update_stats({sample_counter, [0, 0, 0, 0, 0, Last]}, Value) ->
    New = Value-Last,
	{sample_counter,[New, 0, New, New, 1, Value]};
update_stats({sample_counter,[Mean, Var, Max, Min, Count, Last]}, Value) ->
	{NewMean, NewVar, _} = ts_stats:meanvar(Mean, Var, [Value-Last], Count),
	NewMax = lists:max([Max, Value]),
	NewMin = lists:min([Min, Value]),
	{sample_counter,[NewMean, NewVar, NewMax, NewMin, Count+1, Value]}.

%%----------------------------------------------------------------------
%% Func: reset_all_stats/2
%%----------------------------------------------------------------------
reset_all_stats(Dict)->
	MyFun = fun (Key, OldVal) -> reset_stats(OldVal) end,
	dict:map(MyFun, Dict).

%%----------------------------------------------------------------------
%% Func: reset_stats/1
%% Purpose: reset all stats except min and max
%%----------------------------------------------------------------------
reset_stats({Type,[]}) ->
	{Type, [0, 0, 0, 0, 0]};
reset_stats({Type, [Mean, Var, Max, Min, _Count, Last]}) ->
	{Type, [0, 0, Max, Min, 0, Last]};
reset_stats({Type, [Mean, Var, Max, Min, _Count]}) ->
	{Type, [0, 0, Max, Min, 0]};
reset_stats({Type, [_Sample, LastValue]}) ->
	{Type, [0, LastValue]};
reset_stats({Type, LastValue}) ->
	{Type, LastValue};
reset_stats(Args) ->
	?LOGF("resetting  unknown stats~p~n",[Args],?WARN),
	Args.
	
%%----------------------------------------------------------------------
%% Func: start_launchers/2
%% start the launcher on clients nodes
%%----------------------------------------------------------------------
start_launchers(Machines) -> 
	?DebugF("Need to start tsunami client on ~p~n",[Machines]),
	GetHost = fun(A) -> list_to_atom(A#client.host) end,
	HostList = lists:map(GetHost, Machines),
	?DebugF("Hostlist is ~p~n",[HostList]),
    %% starts beam on all client hosts
    lists:foreach(fun(B) -> ts_config_server:newbeam(B) end, HostList).
	
%%----------------------------------------------------------------------
%% Func: backup_config/2
%% Purpose: copy a backup copy of the config file in the log directory
%%   This is useful to have an history of all parameters of a test.
%%----------------------------------------------------------------------
backup_config(Dir, Config) ->
    Backup = filename:basename(Config),
    {ok, _} = file:copy(Config, filename:join(Dir,Backup)).

%%----------------------------------------------------------------------
%% Func: rrd_create/2
%% Args: LogDir, List of Names, Interval
%% FIXME: Not yet operationnal
%%----------------------------------------------------------------------
rrd_create(_, [], _) -> ok;
rrd_create(LogDir, [NameI|Rest], Interval) when is_integer(NameI)->
    Name=integer_to_list(NameI),
    rrd_create(LogDir, [Name|Rest], Interval);
rrd_create(LogDir, [NameA|Rest], Interval) when is_atom(NameA)->
    Name=atom_to_list(NameA),
    rrd_create(LogDir, [Name|Rest], Interval);
rrd_create(LogDir, [Name|Rest], Interval) when is_list(Name)->
    ?DebugF("rrd_create called with ~p ~p~n",[Name, Rest]),
    File=filename:join(LogDir, Name  ++ ".rrd"),
    ?LOGF("Adding new rrd database ~p in file ~p~n",[Name, File],?INFO),
    Step = Interval div 1000, % convert in sec
    rrdtool:create(#rrd_create{filename=File,
                                    step=Step,
                                    ds=[#rrd_ds{name="main"}], %FIXME
                                    rra=[#rrd_rra{rows=2880,cf="LAST"}]}),
    rrd_create(LogDir,Rest, Interval).

%%----------------------------------------------------------------------
%% Func: rrd_update/2
%% Args: LogDir, Stats
%% FIXME: Not yet operationnal
%%----------------------------------------------------------------------
rrd_update(_,[]) -> ok;
rrd_update(LogDir,[{Key, Val} | Tail]) when is_integer(Key)->
    KeyStr = integer_to_list(Key),
    rrd_update(LogDir,[{KeyStr, Val} | Tail]);
rrd_update(LogDir,[{Key, Val} | Tail]) when is_atom(Key)->
    KeyStr = atom_to_list(Key),
    rrd_update(LogDir,[{KeyStr, Val} | Tail]);
rrd_update(LogDir,[{Key,{Type, [Mean,Var,Max,Min,_Count|_]}} | Tail]) when is_list(Key),is_integer(Mean) or is_float(Mean)->
    File=filename:join(LogDir, Key  ++ ".rrd"),
    rrdtool:update(#rrd_update{filename=File,
                               updates=[#rrd_supdate{values=[Mean]}]}),
    rrd_update(LogDir, Tail);
rrd_update(LogDir,[{Key,{_Type, Val}} | Tail]) when is_list(Key),
                                                   is_integer(Val) or is_float(Val)->
    File=filename:join(LogDir, Key  ++ ".rrd"),
    rrdtool:update(#rrd_update{filename=File,
                               updates=[#rrd_supdate{values=[Val]}]}),
    rrd_update(LogDir, Tail).
   

