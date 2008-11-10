%%%
%%%  Copyright (C) 2007 Nicolas Niclausse
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
%% @copyright 2007-2008 Nicolas Niclausse
%% @author Nicolas Niclausse <nicolas@niclux.org>
%% @since 20 Nov 2007
%% @doc computes statistics for request, page, connect, transactions,
%% data size, errors, ... and other stats specific to plugins
%% ----------------------------------------------------------------------

-module(ts_stats_mon).
-author('nicolas@niclux.org').
-vc('$Id: ts_mon.erl 774 2007-11-20 09:36:13Z nniclausse $ ').

-behaviour(gen_server).

-include("ts_profile.hrl").
-include("ts_config.hrl").

%% External exports, API
-export([start/0, stop/0, add/1, dumpstats/0, set_output/2, status/0 ]).

%% More external exports for ts_mon
-export([print_stats_txt/3, update_stats/3, add_stats_data/2, reset_all_stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {log,          % log fd
                backend,      % type of backend: text|rrdtool|fullstats
                dump_interval,%
                fullstats,    % fullstats filename
                stats,        % dict keeping stats info
                laststats     % values of last printed stats
               }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec start() -> ok | throw({error, Reason})
%% @doc Start the monitoring process
%%----------------------------------------------------------------------
start() ->
    ?LOG("starting stats server, global ~n",?NOTICE),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast({global, ?MODULE}, {stop}).

add(Data) ->
    gen_server:cast({global, ?MODULE}, {add, Data}).

status() ->
    gen_server:call({global, ?MODULE}, {status}).

dumpstats() ->
    gen_server:cast({global, ?MODULE}, {dumpstats}).

set_output(BackEnd,Stream) ->
    gen_server:cast({global, ?MODULE}, {set_output, BackEnd, Stream}).

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
    ?LOG("starting stats server~n",?NOTICE),
    Tab = dict:new(),
    {ok, #state{ dump_interval = ?config(dumpstats_interval),
                 stats   = Tab,
                 laststats = Tab
                }}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({status}, _From, State ) ->
    Request = dict:find({request, sample}, State#state.stats),
    {reply, Request, State};

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
    case State#state.backend of
        fullstats -> io:format(State#state.fullstats,"~p~n",[Data]);
        _Other   -> ok
    end,
    NewStats = lists:foldl(fun add_stats_data/2, State#state.stats, Data ),
    {noreply,State#state{stats=NewStats}};

handle_cast({add, Data}, State) when is_tuple(Data) ->
    case State#state.backend of
        fullstats -> io:format(State#state.fullstats,"~p~n",[Data]);
        _Other   -> ok
    end,
    NewStats = add_stats_data(Data, State#state.stats),
    {noreply,State#state{stats=NewStats}};

handle_cast({set_output, BackEnd, {Stream, StreamFull}}, State) ->
    {noreply,State#state{backend=BackEnd, log=Stream, fullstats=StreamFull}};

handle_cast({dumpstats}, State) ->
    export_stats(State),
    NewStats = reset_all_stats(State#state.stats),
    {noreply, State#state{laststats = NewStats, stats=NewStats}};

handle_cast({stop}, State) ->
    {stop, normal, State};

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
    ?LOGF("stoping stats monitor (~p)~n",[Reason],?NOTICE),
    export_stats(State),
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
%% Func: add_stats_data/2
%% Purpose: update or add value in dictionnary
%% Returns: Dict
%%----------------------------------------------------------------------
%% continuous incrementing counters
add_stats_data({Type, Name,Value},Stats) when Type==sample;
                                              Type==sample_counter ->
    MyFun = fun (OldVal) -> update_stats(Type, OldVal, Value) end,
    dict:update({Name,Type}, MyFun, update_stats(Type, [], Value), Stats);
%% increase by one when called
add_stats_data({count, Name}, Stats)  ->
    dict:update_counter({Name, count}, 1, Stats);
%% cumulative counter
add_stats_data({sum, Name, Val}, Stats)  ->
    dict:update_counter({Name, sum}, Val, Stats).

%%----------------------------------------------------------------------
%% Func: export_stats/2
%%----------------------------------------------------------------------
export_stats(State=#state{backend=_Backend}) ->
    %% print number of simultaneous users
    Param = {State#state.laststats,State#state.log},
    dict:fold(fun print_stats_txt/3, Param, State#state.stats).

%%----------------------------------------------------------------------
%% Func: print_dist_list/3
%% @spec (Key::tuple(), Value::List, {Last, Logfile}) -> {Last, Logfile}
%% @doc print statistics in text format in Logfile
%%----------------------------------------------------------------------
print_stats_txt({Name,_Type}, [Mean,0,Max,Min,Count,MeanFB,CountFB|_], {LastRes,Logfile})->
    io:format(Logfile, "stats: ~p ~p ~p ~p ~p ~p ~p ~p~n",
              [Name, Count, Mean, 0, Max, Min,MeanFB,CountFB ]),
    {LastRes, Logfile};
print_stats_txt({Name,_Type},[Mean,Var,Max,Min,Count,MeanFB,CountFB|_],{LastRes,Logfile})->
    StdVar = math:sqrt(Var/Count),
    io:format(Logfile, "stats: ~p ~p ~p ~p ~p ~p ~p ~p~n",
              [Name, Count, Mean, StdVar, Max, Min, MeanFB,CountFB]),
    {LastRes, Logfile};
print_stats_txt({Name, _Type}, [Value,Last], {LastRes, Logfile}) ->
    io:format(Logfile, "stats: ~p ~p ~p~n", [Name, Value, Last ]),
    {LastRes, Logfile};
print_stats_txt({Name, Type}, Value, {LastRes, Logfile}) ->
    PrevVal = case dict:find({Name, Type}, LastRes) of
                  {ok, OldVal} -> OldVal;
                  error        -> 0
              end,
    io:format(Logfile, "stats: ~p ~p ~p~n", [Name, Value-PrevVal, Value]),
    {LastRes, Logfile}.


%%----------------------------------------------------------------------
%% update_stats/3
%% @spec (Type::atom, List, Value::[integer() | float()]) -> List
%% @doc update the mean and variance for the given sample
%%----------------------------------------------------------------------
update_stats(sample, [], New) ->
    [New, 0, New, New, 1,0,0,0];
update_stats(sample, Data, Value) ->
    %% we don't use lastvalue for 'sample', set it to zero
    update_stats2(Data, Value, 0);
update_stats(sample_counter,[], New) -> %% first call, store the initial value
    [0, 0, 0, 0, 0, 0,0,New];
update_stats(sample_counter, Current, 0) -> % skip 0 values
    Current;
update_stats(sample_counter,[Mean,Var,Max,Min,Count,MeanFB,CountFB,Last],Value)
  when Value < Last->
    %% maybe the counter has been restarted, use the new value, but don't update other data
    [Mean,Var,Max,Min,Count,MeanFB,CountFB,Value];
update_stats(sample_counter, [0, 0, 0, 0, 0, MeanFB,CountFB,Last], Value) ->
    New = Value-Last,
    [New, 0, New, New, 1, MeanFB,CountFB,Value];
update_stats(sample_counter,Data, Value) ->
    update_stats2(Data, Value, Value).

update_stats2([Mean, Var, Max, Min, Count, MeanFB,CountFB,Last], Value, NewLast) ->
    New = Value-Last,
    {NewMean, NewVar, _} = ts_stats:meanvar(Mean, Var, [New], Count),
    case New > Max of
        true -> % new max, min unchanged
            [NewMean, NewVar, New, Min, Count+1, MeanFB,CountFB,NewLast];
        false ->
            case New < Min of
                true ->
                    [NewMean, NewVar, Max, New, Count+1, MeanFB,CountFB,NewLast];
                false ->
                    [NewMean, NewVar, Max, Min, Count+1, MeanFB,CountFB,NewLast]
            end
    end.

%%----------------------------------------------------------------------
%% Func: reset_all_stats/1
%%----------------------------------------------------------------------
reset_all_stats(Dict)->
    MyFun = fun (_Key, OldVal) -> reset_stats(OldVal) end,
    dict:map(MyFun, Dict).

%%----------------------------------------------------------------------
%% Func: reset_stats/1
%% @doc reset all stats except min and max and lastvalue.
%%----------------------------------------------------------------------
reset_stats([]) ->  % FIXME: useful ?!
    [0, 0, 0, 0, 0, 0, 0];
reset_stats([_Mean, _Var, Max, Min, 0, _MeanFB,0,Last]) ->
    [0, 0, Max, Min, 0, 0, 0,Last];
reset_stats([Mean, _Var, Max, Min, Count, MeanFB,CountFB,Last]) ->
    NewCount=CountFB+Count,
    NewMean=(CountFB*MeanFB+Count*Mean)/NewCount,
    [0, 0, Max, Min, 0, NewMean,NewCount,Last];
reset_stats([_Sample, LastValue]) ->
    [0, LastValue];
reset_stats(LastValue) ->
    LastValue.

