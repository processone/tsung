%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2003 IDEALX
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

%% External exports
-export([start/0, stop/0, newclient/1, endclient/1, newclient/1, sendmes/1,
         start_clients/1,
         rcvmes/1, error/1,
		 addsample/1, get_sample/1,
		 addcount/1,  get_count/1,
		 addsum/1,    get_sum/1,
		 dumpstats/0, addsample_counter/1
		]).

-export([update_stats/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {log,          % log filename
				client=0,     % number of clients currently running
				maxclient=0,  % max of simultaneous clients 
				stats,        % dict keeping stats info
				stop = false, % true if we should stop
				laststats,    % values of last printed stats
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
start() ->
	?LOG("starting monitor, global ~n",?NOTICE),
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

start_clients({Machines, Monitoring}) ->
    gen_server:call({global, ?MODULE}, {start_clients, Machines, Monitoring}, infinity).

stop() ->
	gen_server:cast({global, ?MODULE}, {stop}).

dumpstats() ->
	gen_server:cast({global, ?MODULE}, {dumpstats}).

newclient({Who, When, Connect}) ->
	gen_server:cast({global, ?MODULE}, {newclient, Who, When, Connect}).

endclient({Who, When, Elapsed}) ->
	gen_server:cast({global, ?MODULE}, {endclient, Who, When, Elapsed}).

sendmes({none, Who, What}) ->
	skip;
sendmes({_Type, Who, What}) ->
	gen_server:cast({global, ?MODULE}, {sendmsg, Who, now(), What}).

rcvmes({none, Who, What}) ->
	skip;
rcvmes({_Type, Who, What}) ->
	gen_server:cast({global, ?MODULE}, {rcvmsg, Who, now(), What}).

addsample({Type, Value}) ->
	gen_server:cast({global, ?MODULE}, {sample, Type, Value}).

get_sample(Type) ->
	gen_server:call({global, ?MODULE}, {get_sample, Type}).
get_counter(Type) -> get_sample(Type).
get_count(Type)   -> get_sample(Type).
get_sum(Type)     -> get_sample(Type).

addcount({Type}) ->
	gen_server:cast({global, ?MODULE}, {count, Type}).

%% for continuous incrementing counter
addsample_counter({Type, Value}) -> 
	gen_server:cast({global, ?MODULE}, {sample_counter, Type, Value}).

%% accumulator type of data
addsum({Type, Val}) ->
	gen_server:cast({global, ?MODULE}, {sum, Type, Val}).

error({Who, When, What}) ->
	gen_server:cast({global, ?MODULE}, {error, Who, When, What}).



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
init([]) ->
    Filename = ?config(log_file) ++ ts_utils:datestr(),
    case file:open(Filename,write) of 
		{ok, Stream} ->
			?LOG("starting monitor~n",?NOTICE),
			Tab = dict:new(),
			{ok, #state{ log     = Stream,
                         stats   = Tab,
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
handle_call({start_clients, Machines, Monitoring}, From, State) ->
    timer:apply_interval(?config(dumpstats_interval), ?MODULE, dumpstats, [] ),
    start_launchers(Machines),
	{reply, ok, State#state{type=Monitoring}};

handle_call({get_sample, Type}, From, State) ->
	Reply = dict:find(Type, State#state.stats),
	{reply, Reply, State};

handle_call(Request, From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({sendmsg, Who, When, What}, State = #state{type = none}) ->
	{noreply, State};

handle_cast({sendmsg, Who, When, What}, State = #state{type = light}) ->
	io:format(State#state.log,"Send:~w:~w:~-44s~n",[When,Who,
												 binary_to_list(What)]),
	{noreply, State};

handle_cast({sendmsg, Who, When, What}, State) ->
	io:format(State#state.log,"Send:~w:~w:~s~n",[When,Who,
												 binary_to_list(What)]),
	{noreply, State};

handle_cast({sample, Type, Value}, State)  ->
	Tab = State#state.stats,
	MyFun = fun (OldVal) -> update_stats(OldVal, Value) end,
	?LOGF("Stats: new sample ~p:~p ~n",[Type, Value] ,?DEB),
	NewTab = dict:update(Type, MyFun, update_stats([],Value), Tab),
	{noreply, State#state{stats=NewTab}};

%% increase by one when called
handle_cast({count, Type}, State)  ->
	Tab = State#state.stats,
	Add = fun (OldVal) -> OldVal+1 end,
	NewTab = dict:update(Type, Add, 1, Tab),
	{noreply, State#state{stats=NewTab}};

%% continuous incrementing counters 
handle_cast({sample_counter, Type, Value}, State)  ->
	Tab = State#state.stats,
	MyFun = fun (OldVal) -> update_stats_counter(OldVal, Value) end,
	NewTab = dict:update(Type, MyFun, update_stats_counter([],Value), Tab),
	{noreply, State#state{stats=NewTab}};

%% cumulative counter
handle_cast({sum, Type, Val}, State)  ->
	Tab = State#state.stats,
	Add = fun (OldVal) -> OldVal+Val end,
	NewTab = dict:update(Type, Add, Val, Tab),
	{noreply, State#state{stats=NewTab}};

handle_cast({dumpstats}, State) ->
	print_stats(State),
	NewStats = reset_all_stats(State#state.stats),
	{noreply, State#state{laststats = NewStats, stats= NewStats}};

handle_cast({rcvmsg, Who, When, What}, State = #state{type=none}) ->
	{noreply, State};

handle_cast({rcvmsg, Who, When, What}, State = #state{type=light}) ->
	io:format(State#state.log,"Recv:~w:~w:~-44s~n",[When,Who, 
													binary_to_list(What)]),
	{noreply, State};

handle_cast({rcvmsg, Who, When, What}, State) ->
	io:format(State#state.log,"Recv:~w:~w:~s~n",[When,Who, 
													binary_to_list(What)]),
	{noreply, State};

handle_cast({newclient, Who, When, Connect}, State) ->
	Clients =  State#state.client+1,
	Max= lists:max([Clients,State#state.maxclient]),
	Tab = State#state.stats,
	Add = fun (OldVal) -> OldVal+1 end,
	New1Tab = dict:update(users_count, Add, 1, Tab),

	%% update connect sample
	MyFun = fun (OldV) -> update_stats(OldV, Connect) end,
	NewTab = dict:update(connect, MyFun, update_stats([],Connect), New1Tab),

	case State#state.type of 
		none -> ok;
		_ ->
			io:format(State#state.log,"NewClient:~w:~w~n",[When, Who]),
			io:format(State#state.log,"load:~w~n",[Clients])
	end,
	{noreply, State#state{client = Clients, maxclient=Max, stats=NewTab}};

handle_cast({endclient, Who, When, Elapsed}, State) ->
	Clients =  State#state.client-1,
	Tab = State#state.stats,
	Add = fun (OldVal) -> OldVal+1 end,
	New1Tab = dict:update(finish_users_count, Add, 1, Tab),

	%% update session sample
	MyFun = fun (OldV) -> update_stats(OldV, Elapsed) end,
	NewTab = dict:update(session, MyFun, update_stats([],Elapsed), New1Tab),

	case State#state.type of
		none ->
			skip;
		_Type ->
			io:format(State#state.log,"EndClient:~w:~w~n",[When, Who]),
			io:format(State#state.log,"load:~w~n",[Clients])
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
	{noreply, State#state{stop = true}}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
	?LOGF("stoping monitor (~p)~n",[Reason],?NOTICE),
	print_stats(State),
    io:format(State#state.log,"EndMonitor:~w~n",[now()]),
	file:close(State#state.log),
    slave:stop(node()),
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: print_stats/2
%%----------------------------------------------------------------------
%% TODO: add a function to print stats for gnuplot ?
print_stats(State) ->
	DateStr = ts_utils:now_sec(),
	io:format(State#state.log,"# stats: dump at ~w~n",[DateStr]),
	Res = dict:to_list(State#state.stats),
	%% print number of simultaneous users
	io:format(State#state.log, "stats: ~p ~p ~p~n", [users, State#state.client, State#state.maxclient]),
	print_dist_list(Res, State#state.laststats , State#state.log).
%% Print os_mon figures
%%ts_os_mon:print(State#state.log).

print_dist_list([], Last, Logfile) ->
	done;
print_dist_list([{Key, [Mean, 0, Max, Min, Count]} | Tail], LastRes, Logfile) ->
	io:format(Logfile, "stats: ~s ~p ~p ~p ~p ~p ~p ~n", 
			  [Key, Count, Mean, 0, Max, Min, Count ]),
	print_dist_list(Tail, LastRes, Logfile);
print_dist_list([{Key, [Mean, Var, Max, Min, Count]} | Tail], LastRes, Logfile) ->
	StdVar = math:sqrt(Var/Count),
	io:format(Logfile, "stats: ~s ~p ~p ~p ~p ~p ~p ~n",
			  [Key, Count, Mean, StdVar, Max, Min, Count ]),
	print_dist_list(Tail, LastRes, Logfile);
print_dist_list([{Key, [Sample,Last]} | Tail], LastRes, Logfile) ->
	io:format(Logfile, "stats: ~s ~p ~p~n", [Key, Sample, Last ]),
    print_dist_list(Tail, LastRes, Logfile);
print_dist_list([{Key, Value} | Tail], LastRes, Logfile) ->
	case dict:find(Key, LastRes) of 
		{ok,  _Count} ->
			PrevVal = _Count ;
		error ->
			PrevVal = 0 
	end,
	io:format(Logfile, "stats: ~p ~p ~p~n", [Key, Value-PrevVal, Value]),
	print_dist_list(Tail, LastRes, Logfile).
	
	
%%----------------------------------------------------------------------
%% Func: update_stats/2
%% Returns: List  = [Mean, Variance, Max, Min, Count]
%%----------------------------------------------------------------------
update_stats([], New) ->
	[New, 0, New, New, 1];
update_stats([Mean, Var, Max, Min, Count], Value) ->
	{NewMean, NewVar, _} = ts_stats:meanvar(Mean, Var, [Value], Count),
	NewMax = lists:max([Max, Value]),
	NewMin = lists:min([Min, Value]),
	[NewMean, NewVar, NewMax, NewMin, Count+1];
update_stats(Args, New) -> % ???
	[New, 0, New, New, 1]. 

%%----------------------------------------------------------------------
%% Func: update_stats_counter/2
%% Returns: List  = [Mean, Variance, Max, Min, Count, LastValue]
%%----------------------------------------------------------------------
update_stats_counter([], New) -> %% first call, store the initial value
	[0, New];
update_stats_counter([0, First], New) ->%%second call
    Sample=New-First,
	[Sample, New];
update_stats_counter([PrevVal, Last], New) ->
    Sample=New-Last,
	[Sample, New].
%%
reset_all_stats(Dict)->
	MyFun = fun (Key, OldVal) -> reset_stats(OldVal) end,
	dict:map(MyFun, Dict).

%%----------------------------------------------------------------------
%% Func: reset_stats/1
%% Purpose: reset all stats except min and max
%%----------------------------------------------------------------------
reset_stats([]) ->
	[0, 0, 0, 0, 0];
reset_stats([Mean, Var, Max, Min, Count]) ->
	[0, 0, Max, Min, 0];
reset_stats([Sample, LastValue]) -> %% counter
	[0, LastValue];
reset_stats(Args) ->
	Args.
	
%%----------------------------------------------------------------------
%% Func: start_launchers/2
%% start the launcher on clients nodes
%%----------------------------------------------------------------------

start_launchers(Machines) -> 
	?LOGF("Need to start tsunami client on ~p~n",[Machines], ?DEB),
	GetHost = fun(A) -> list_to_atom(A#client.host) end,
	HostList = lists:map(GetHost, Machines),
	?LOGF("Hostlist is ~p~n",[HostList], ?DEB),
    %% starts beam on all client hosts
    lists:foreach({ts_config_server, newbeam}, HostList).
	
