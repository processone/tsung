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

%%% This module launch clients (ts_client module) given a number of
%%% clients and the intensity of the arrival process (intensity =
%%% inverse of the mean of inter arrival). The arrival process is a
%%% Poisson Process (ie, inter-arrivals are independant and exponential)

-module(ts_launcher).
-created('Date: 2000/10/23 12:09:57 nniclausse ').
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("ts_profile.hrl").

-behaviour(gen_fsm). %% a primitive gen_fsm with two state: launcher and wait

%% External exports
-export([start/0, launch/1]).

%% gen_fsm callbacks
-export([init/1, launcher/2,  wait/2, finish/2, handle_event/3,
		 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {nusers,
                phases =[],
				myhostname,
                intensity,
                maxusers %% if maxusers are currently active, launch a
                         %% new beam to handle the new users
               }).

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
launch({Node, Arrivals}) ->
	?LOGF("starting on node ~p~n",[[Node]], ?INFO),
	gen_fsm:send_event({?MODULE, Node}, {launch, Arrivals}).


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
    ts_utils:init_seed(),
%	ControllerNode = ?config(controller),
%	Res = net_adm:ping(ControllerNode),
%	global:sync(),
%	?LOGF("ping ~p: ~p (nodes=~p)~n",[ControllerNode, Res,nodes()],?NOTICE),

    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
	{ok, wait, #state{myhostname=MyHostName}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
wait({launch, []}, State) ->
	MyHostName = State#state.myhostname,
	?LOGF("Launch msg receive (~p)~n",[MyHostName], ?NOTICE),
    {ok, {[{Intensity, Users}| Rest], StartDate, Max}} = 
        ts_config_server:get_client_config(MyHostName),
	Warm_timeout=case ts_utils:elapsed(now(), StartDate) of 
					 WaitBeforeStart when WaitBeforeStart>0 -> 
						 round(ts_stats:exponential(Intensity) + WaitBeforeStart);
					 Neg -> 
						 ?LOG("Negative Warm timeout !!! Check if client "++
							  " machines are synchronized (ntp ?)~n"++
							  "Anyway, start launcher NOW! ~n", ?WARN),
						 1
					  end,
    Warm = lists:min([Warm_timeout,?config(max_warm_delay)]),
	?LOGF("Activate launcher (~p users) in ~p msec ~n",[Users, Warm], ?NOTICE),
	{next_state,launcher,State#state{phases = Rest, nusers = Users, 
                                     intensity=Intensity,maxusers=Max },Warm};

wait({launch, {[{Intensity, Users}| Rest], Max}}, State) ->
    ?DebugF("Starting with ~p users to do in the current phase (max is ~p)~n",
			[Users, Max]),
	{next_state, launcher, State#state{phases = Rest, nusers = Users, 
                                       intensity = Intensity, maxusers= Max }, ?short_timeout}.

launcher(Event, State=#state{nusers = 0, phases = [] }) ->
	?LOG("no more clients to start, wait  ~n",?INFO),
    {next_state, finish, #state{}, ?check_noclient_timeout};

launcher(timeout, State=#state{nusers    = Users,
                               phases    = Phases,
                               intensity = Intensity}) ->
    Wait = do_launch({Intensity,State#state.myhostname}),
    case check_max_raised(State) of
        true ->
            {next_state, finish, State, ?check_noclient_timeout};
        false->
            case {Users, Phases} of 
                {0, [{NewIntensity, NewUsers}|Rest]} -> % new phase
                    ts_mon:add({ count, newphase }),
                   ?LOGF("Start a new arrival phase (~p ~p) ~n",
                         [NewUsers, NewIntensity], ?NOTICE),
                    {next_state,launcher,State#state{phases = Rest, 
                                                     nusers = NewUsers,
                                                     intensity = NewIntensity},
                     Wait};
                _  ->{next_state,launcher,State#state{nusers = Users-1} , Wait}
            end
    end.
    
finish(timeout, State) ->
    case ts_client_sup:active_clients() of
       0 -> %% no users left, stop
            ?LOG("No more active users, stop beam~n", ?NOTICE),
            ts_mon:stop(),
            slave:stop(node()), %% commit suicide
            {stop, normal, State}; %% should never be executed
        ActiveClients ->
            ?LOGF("Still ~p active client(s)~n", [ActiveClients],?NOTICE),
            {next_state, finish, State, ?check_noclient_timeout}
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
handle_event(Event, StateName, StateData) ->
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
handle_sync_event(Event, From, StateName, StateData) ->
	Reply = ok,
	{reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
	?LOGF("launcher terminating for reason~p~n",[Reason], ?INFO),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

check_max_raised(State=#state{phases=Phases,maxusers=Max,nusers=Users,
							  intensity=Intensity})->
    ActiveClients =  ts_client_sup:active_clients(),
    case ActiveClients >= Max of
        true ->
            ?LOG("Max number of clients reached, must start a new beam~n", ?NOTICE),
            Args = case Users of 
                       0 ->  Phases;
                       _ -> [{Intensity,Users-1}|Phases]
                   end,
            ts_config_server:newbeam(list_to_atom(State#state.myhostname), {Args, Max}),
            true;
        false ->
            ?DebugF("Current clients on beam: ~p~n", [ActiveClients]),
            false
    end.

do_launch({Intensity, MyHostName})->
    %%Get one client
    %%set the profile of the client
    {ok, Profile} = ts_config_server:get_next_session(MyHostName),
    ts_client_sup:start_child(Profile),
    X = round(ts_stats:exponential(Intensity)),
    ?DebugF("client launched, wait ~p ms before launching next client~n",[X]),
    X.
