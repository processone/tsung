%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
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

%%% This module launch clients (ts_client module) given a number of
%%% clients and the intensity of the arrival process (intensity =
%%% inverse of the mean of inter arrival). The arrival process is a
%%% Poisson Process (ie, inter-arrivals are independant and exponential)


-module(ts_launcher).
-created('Date: 2000/10/23 12:09:57 nniclausse ').
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_config.hrl").

% wait up to 10ms after an error
-define(NEXT_AFTER_FAILED_TIMEOUT, 10).
-define(DIE_DELAY, 5000).

-behaviour(gen_fsm). %% a primitive gen_fsm with two state: launcher and wait

%% External exports
-export([start/0, launch/1, set_static_users/1]).
-export([set_warm_timeout/1]).

%% gen_fsm callbacks
-export([init/1, launcher/2,  wait/2, wait_static/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start/0
%%--------------------------------------------------------------------
start() ->
    ?LOG("starting ~n", ?INFO),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: launch/1
%%--------------------------------------------------------------------
%% Start clients with given interarrival (can be empty list)
launch({Node, Arrivals, Seed}) ->
    ?LOGF("starting on node ~p~n",[[Node]], ?INFO),
    gen_fsm:send_event({?MODULE, Node}, {launch, Arrivals, Seed});

% same erlang beam case
launch({Node, Host, Arrivals, Seed}) ->
    ?LOGF("starting on node ~p~n",[[Node]], ?INFO),
    gen_fsm:send_event({?MODULE, Node}, {launch, Arrivals, atom_to_list(Host), Seed}).

%% Start clients with given interarrival (can be empty list)
set_static_users({Node,Value}) ->
    ?LOGF("Substract static users number to max: ~p~n",[Value], ?DEB),
    gen_fsm:send_event({?MODULE, Node}, {static, Value}).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
    ts_launcher_mgr:alive(dynamic),
    {ok, wait, #launcher{myhostname=MyHostName}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

wait({launch, Args, Hostname, Seed}, State) ->
    wait({launch, Args, Seed}, State#launcher{myhostname = Hostname});

%% starting without configuration. We must ask the config server for
%% the configuration of this launcher.
wait({launch, [], Seed}, State=#launcher{static_done=Static_done}) ->
    ts_utils:init_seed(Seed),
    MyHostName = State#launcher.myhostname,
    ?LOGF("Launch msg receive (~p)~n",[MyHostName], ?NOTICE),
    ts_launcher_mgr:check_registered(),
    case ts_config_server:get_client_config(MyHostName) of
        {ok, {[{Intensity, Users, Duration}| Rest], StartDate, Max}} ->
            ?LOGF("Expected duration of first phase: ~p sec (~p users) ~n",[Duration/1000, Users], ?NOTICE),
            check_max_users(Max),
            NewState = State#launcher{phases         = Rest,
                                      nusers         = Users,
                                      phase_nusers   = Users,
                                      start_date     = StartDate,
                                      phase_duration = Duration,
                                      intensity      = Intensity, maxusers=Max },
            case Static_done of
                true  ->
                    wait_static({static, 0}, NewState);
                false ->
                    {next_state,wait_static,NewState}
            end;
        {ok,{[],_,_}} -> % no random users, only static.
            {stop, normal, State}
    end;

%% start with a already known configuration. This case occurs when a
%% beam is started by a launcher (maxclients reached)
wait({launch, {[{Intensity, Users, Duration}| Rest], Max, PhaseId}, Seed}, State) ->
    ?LOGF("Starting with ~p users to do in the current phase (max is ~p)~n",
          [Users, Max],?DEB),
    ts_utils:init_seed(Seed),
    ?LOGF("Expected duration of phase: ~p sec ~n",[Duration/1000], ?NOTICE),
    ts_launcher_mgr:check_registered(),
    {next_state, launcher, State#launcher{phases = Rest, nusers = Users,
                                          phase_nusers = Users,
                                          phase_duration=Duration,
                                          phase_start = ?NOW,
                                          phase_id  = PhaseId,
                                          intensity = Intensity, maxusers=Max},
     State#launcher.short_timeout};
wait({static,0}, State) ->
    %% static launcher has no work to do, do not wait for him.
    ?LOG("Wow, static launcher is already sending me a msg, don't forget it ~n", ?INFO),
    {next_state, wait, State#launcher{static_done=true}}.

wait_static({static, _Static}, State=#launcher{nusers=0})  ->
    %% no users in this phase, next one
    skip_empty_phase(State);

wait_static({static, Static}, State=#launcher{maxusers=Max,intensity=Intensity,
                                           nusers=Users,start_date=StartDate}) when is_integer(Static) ->
    %% add ts_stats:exponential(Intensity) to start time to avoid
    %% simultaneous start of users when a lot of client beams is
    %% used. Also, avoid too long delay, so use a maximum delay
    WarmTimeout = set_warm_timeout(StartDate)+round(ts_stats:exponential(Intensity)),
    Warm = lists:min([WarmTimeout,?config(max_warm_delay)]),
    ?LOGF("Activate launcher (~p users) in ~p msec ~n",[Users, Warm], ?NOTICE),
    PhaseStart = ts_utils:add_time(?NOW, Warm div 1000),
    NewMax = case Max > Static of
               true  ->
                     Max-Static;
               false ->
                   ?LOG("Warning: more static users than maximum users per beam !~n",?WARN),
                   1 % will fork a new beam as soon a one user is started
           end,
    ?LOGF("Set maximum users per beam to ~p~n",[NewMax],?DEB),
    {next_state,launcher,State#launcher{ phase_start = PhaseStart,
                                      maxusers = NewMax }, Warm}.


launcher(_Event, State=#launcher{nusers = 0, phases = [] }) ->
    ?LOG("no more clients to start, stop  ~n",?INFO),
    {stop, normal, State};

launcher(timeout, State=#launcher{nusers        = Users,
                                  phase_nusers  = PhaseUsers,
                                  phases        = Phases,
                                  phase_id      = Id,
                                  started_users = Started,
                                  intensity     = Intensity}) ->
    BeforeLaunch = ?NOW,
    case do_launch({Intensity,State#launcher.myhostname,Id}) of
        {ok, Wait} ->
            case check_max_raised(State) of
                true ->
                    %% let the other beam starts and warns ts_mon
                    timer:sleep(?DIE_DELAY),
                    {stop, normal, State};
                false->
                    Duration = ts_utils:elapsed(State#launcher.phase_start, BeforeLaunch),
                    case change_phase(Users-1, Phases, Duration,
                                      {State#launcher.phase_duration, PhaseUsers}) of
                        {change, 0, _, PhaseLength,Rest} ->
                            %% no users in the next phase
                            skip_empty_phase(State#launcher{phases=Rest,phase_duration=PhaseLength});
                        {change, NewUsers, NewIntensity, PhaseLength,Rest} ->
                            ts_mon:add({ count, newphase }),
                            ?LOGF("Start a new arrival phase (~p users, ~p); expected duration=~p sec~n",
                                  [NewUsers, NewIntensity, PhaseLength/1000], ?NOTICE),
                            {next_state,launcher,State#launcher{phases = Rest,
                                                                nusers = NewUsers,
                                                                phase_nusers = NewUsers,
                                                                phase_duration=PhaseLength,
                                                                phase_start = ?NOW,
                                                                phase_id  = Id+1,
                                                                intensity = NewIntensity},
                             round(Wait)};
                        {stop} ->
                            {stop, normal, State};
                        {continue} ->
                            Now=?NOW,
                            LaunchDuration = ts_utils:elapsed(BeforeLaunch, Now),
                            %% to keep the rate of new users as expected,
                            %% remove the time to launch a client to the next
                            %% wait.
                            NewWait = case Wait > LaunchDuration of
                                          true -> trunc(Wait - LaunchDuration);
                                          false -> 0
                                      end,
                            ?DebugF("Real Wait = ~p (was ~p)~n", [NewWait,Wait]),
                            {next_state,launcher,State#launcher{nusers = Users-1, started_users=Started+1} , NewWait}
                    end
            end;
        error ->
            % retry with the next user, wait randomly a few msec
            RndWait = random:uniform(?NEXT_AFTER_FAILED_TIMEOUT),
            {next_state,launcher,State#launcher{nusers = Users-1} , RndWait}
    end.


%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, _StateData) ->
    ?LOGF("launcher terminating for reason ~p~n",[Reason], ?INFO),
    ts_launcher_mgr:die(dynamic),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%% @spec skip_empty_phase(record(launcher)) -> {next_state, launcher, record(launcher)}
%%% @doc if a phase contains no users, sleep, before trying the next one @end
skip_empty_phase(State=#launcher{phases=Phases,phase_id=Id,phase_duration=Duration})->
    ?LOGF("No user, skip phase (~p ~p)~n",[Phases,Duration],?INFO),
    case change_phase(0, Phases, 0, {Duration, 0}) of
        {stop} ->
            {stop, normal, State};
        {change, 0, _, PhaseLength, Rest} ->
            %% next phase is also empty, loop
            skip_empty_phase(State#launcher{phases=Rest,phase_duration=PhaseLength,phase_id=Id+1});
        {change, NewUsers, NewIntensity, PhaseLength,Rest} ->
            {next_state,launcher,State#launcher{phases = Rest,
                                                nusers = NewUsers,
                                                phase_nusers = NewUsers,
                                                phase_duration=PhaseLength,
                                                phase_start = ?NOW,
                                                phase_id = Id+1,
                                                intensity = NewIntensity}, 1}
    end.

%%%----------------------------------------------------------------------
%%% Func: change_phase/4
%%% Purpose: decide if we need to change phase (if current users is
%%%          reached or if max duration is reached)
%%% ----------------------------------------------------------------------
change_phase(N, [{NewIntensity, NewUsers,NewDuration}|Rest], CurrentDuration, {TotalDuration,_})  when N < 1 andalso CurrentDuration >= TotalDuration ->
    {change, NewUsers, NewIntensity, NewDuration, Rest};
change_phase(N, [{NewIntensity, NewUsers,NewDuration}|Rest], CurrentDuration, {TotalDuration,_})  when N < 1 ->
    %% no more users, check if we need to wait before changing phase (this can happen if maxnumber is set)
    ToWait=round(TotalDuration-CurrentDuration),
    ?LOGF("Need to wait ~p sec before changing phase, going to sleep~n", [ToWait/1000], ?WARN),
    timer:sleep(ToWait),
    ?LOG("Waking up~n", ?NOTICE),
    {change, NewUsers, NewIntensity, NewDuration, Rest};
change_phase(N, [], _, _) when N < 1 ->
    ?LOG("This was the last phase, wait for connected users to finish their session~n",?NOTICE),
    {stop};
change_phase(N,NewPhases,Current,{Total, PhaseUsers}) when Current>Total ->
    ?LOGF("Check phase: ~p ~p~n",[N,PhaseUsers],?DEB),
    Percent = 100*N/PhaseUsers,
    case {Percent > ?MAX_PHASE_EXCEED_PERCENT, N > ?MAX_PHASE_EXCEED_NUSERS} of
        {true,true} ->
            ?LOGF("Phase duration exceeded, more than ~p% (~.1f%) of users were not launched in time (~p users), tsung may be overloaded !~n",
                  [?MAX_PHASE_EXCEED_PERCENT,Percent,N],?WARN);
        {_,_} ->
            ?LOGF("Phase duration exceeded, but not all users were launched (~p users, ~.1f% of phase)~n",
                  [N, Percent],?NOTICE)
    end,
    case NewPhases of
        [{NewIntensity,NewUsers,NewDuration}|Rest] ->
            {change, NewUsers, NewIntensity, NewDuration,Rest};
        [] ->
            ?LOG("This was the last phase, wait for connected users to finish their session~n",?NOTICE),
            {stop}
    end;
change_phase(_N, _, _Current, {_Total, _}) ->
    {continue}.

%%%----------------------------------------------------------------------
%%% Func: check_max_raised/1
%%%----------------------------------------------------------------------
check_max_raised(State=#launcher{phases=Phases,maxusers=Max,nusers=Users, phase_id=Id,
                                 started_users=Started, phase_start=Start, phase_duration=Duration,
                                 intensity=Intensity}) when Started >= Max-1 ->
    PendingDuration = Duration - ts_utils:elapsed(Start, ?NOW),
    ActiveClients =  ts_client_sup:active_clients(),
    ?DebugF("Current active clients on beam: ~p (max is ~p)~n", [ActiveClients, State#launcher.maxusers]),
    case ActiveClients >= Max of
        true ->
            ?LOG("Max number of concurrent clients reached, must start a new beam~n", ?NOTICE),
            Args = case Users of
                       0 ->  Phases;
                       _ -> [{Intensity,Users-1,PendingDuration}|Phases]
                   end,
            ts_config_server:newbeam(list_to_atom(State#launcher.myhostname), {Args, Max, Id}),
            true;
        false ->
            ?DebugF("Current clients on beam: ~p~n", [ActiveClients]),
            false
    end;
check_max_raised(_State) -> % number of started users less than max, no need to check
    ?DebugF("Current started clients on beam: ~p (max is ~p)~n", [_State#launcher.started_users, _State#launcher.maxusers]),
    false.

%%%----------------------------------------------------------------------
%%% Func: do_launch/1
%%%----------------------------------------------------------------------
do_launch({Intensity, MyHostName, PhaseId})->
    %%Get one client
    %%set the profile of the client
    case catch ts_config_server:get_next_session({MyHostName, PhaseId} ) of
        [{'EXIT', {timeout, _ }}] ->
            ?LOG("get_next_session failed (timeout), skip this session !~n", ?ERR),
            ts_mon:add({ count, error_next_session }),
            error;
        {ok, Session} ->
            ts_client_sup:start_child(Session),
            X = ts_stats:exponential(Intensity),
            ?DebugF("client launched, wait ~p ms before launching next client~n",[X]),
            {ok, X};
        Error ->
            ?LOGF("get_next_session failed for unexpected reason [~p], abort !~n", [Error],?ERR),
            ts_mon:add({ count, error_next_session }),
            exit(shutdown)
    end.

set_warm_timeout(StartDate)->
    case ts_utils:elapsed(?NOW, StartDate) of
        WaitBeforeStart when WaitBeforeStart>0 ->
            round(WaitBeforeStart);
        _Neg ->
            ?LOG("Negative Warm timeout !!! Check if client "++
                 " machines are synchronized (ntp ?)~n"++
                 "Anyway, start launcher NOW! ~n", ?WARN),
            1
    end.

check_max_users(Max) ->
    try
        Data = os:cmd("grep \"open files\"  /proc/self/limits"),
        {match,[Val]} = re:run(Data,"Max open files\\s+(\\d+)",[{capture,all_but_first,list}]),
        Limit = list_to_integer(Val),
        case (Max > Limit ) of
            true ->
                ?LOGF("WARNING !!! too few file descriptors available (~w), you should decrease maxusers (currently ~w)",[Limit,Max], ?CRIT);
            false ->
                ?LOGF("maxusers is below file descriptors limit (~p)",[Limit], ?DEB)
        end
    catch
        _Error:_Reason ->
            ?LOG("Can't get file descriptors limit from system, you should verify that 'maxusers' has a good value ",?NOTICE)
    end.
