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

-module(ts_user_server).
-author('jflecomte@IDEALX.com').

-vc('$Id$ ').

-include("ts_profile.hrl").

%%-compile(export_all).
-export([reset/3, 
	 get_id/0,
	 get_idle/0,
	 get_offline/0,
	 add_to_connected/1,
	 remove_connected/1,
	 get_one_connected/1,
	 connect_first/0,
	 disconnect_first/0,
	 get_first/0]).

-behaviour(gen_server).

%% External exports
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {list_offline, list_user, list_connected, first_client, free_users}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start([NDeb, NFin, NClients]) ->
	?LOGF("Starting with args ~p ~n",[[NDeb, NFin, NClients]],?DEB),
    gen_server:start_link({global, ?MODULE}, ?MODULE, {NDeb, NFin, NClients}, []).

reset(NDeb, NFin, NClients)->
    gen_server:call({global, ?MODULE}, {reset, NDeb, NFin, NClients}).

get_id()->
    gen_server:call({global, ?MODULE }, get_id).

get_idle()->
    gen_server:call({global, ?MODULE}, get_idle).


get_one_connected(Id)->
    gen_server:call({global, ?MODULE}, {get_one_connected, Id}). 

get_offline()->
    gen_server:call({global, ?MODULE}, get_offline).


add_to_connected(Id)->
    gen_server:call({global, ?MODULE}, {add_to_connected, Id}). 


connect_first()->
    gen_server:call({global, ?MODULE}, connect_first).

disconnect_first()->
    gen_server:call({global, ?MODULE}, disconnect_first).


get_first()->
    gen_server:call({global, ?MODULE}, get_first).

remove_connected(Id)->
    gen_server:call({global, ?MODULE}, {remove_connected, Id}). 

stop()->
    gen_server:call({global, ?MODULE}, stop).

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
init({NDeb, NFin, NClients}) when  NFin =< NDeb->
	?LOGF("Bad interval ! ~p is not < ~p~n",[NDeb, NFin],?ERR),
	{stop, badinterval}	;

init({NDeb, NFin, NClients}) ->
	?LOG("starting ...",?DEB),
    {Msec, Sec, Nsec} = ts_utils:init_seed(),
    random:seed(Msec,Sec,Nsec),

    {Offline, [H|T]} = build_clients({NDeb, NFin}, NClients+1),
	Free= length(T),
	?LOGF("ok, started with ~p free users~n",[Free], ?DEB),
    State = #state{list_user = T, 
		   list_offline = Offline,
		   list_connected = [],
		   free_users = Free,
		   first_client= {H, not_connected}},
    {ok, State}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%%Get one id in the full list of potential users
handle_call(get_id, From, State) ->
    List = State#state.list_user ++ State#state.list_connected ++ State#state.list_offline,
    Indice = random:uniform( length(List) ) ,
    Element = lists:nth(Indice, List),
    {reply, Element, State};

%%Get one id in the users whos have to be connected
handle_call(get_idle, From, State) ->
    case State#state.free_users of
		0 ->
			?LOG("No more free users ! ~n", ?WARN),
			{reply, [], State};
		_Other ->
			[Element | NewList] = State#state.list_user,
			
			State2 = State#state{list_user = NewList,
								 free_users = State#state.free_users -1,
								 list_connected = [Element | State#state.list_connected]},
			{reply, Element, State2}
    end;

%%Get one offline id 
handle_call(get_offline, From, State) ->
    Indice = random:uniform( length(State#state.list_offline) ) ,
    Element = lists:nth(Indice, State#state.list_offline),
    {reply, Element, State};


handle_call(get_first, From, State) ->
    {reply, State#state.first_client, State};

handle_call(connect_first, From, State) ->
    {Id, Status} = State#state.first_client,
    State2 = State#state{ first_client = {Id, connected} },
    {reply, ok, State2};

handle_call(disconnect_first, From, State) ->
    {Id, Status} = State#state.first_client,
    State2 = State#state{ first_client = {Id, not_connected} },
    {reply, ok, State2};

handle_call({reset, NDeb, NFin, NClients}, From, State) ->
    {Offline, [H|T]} = build_clients({NDeb, NFin}, NClients+1),
    State2 = #state{list_user = T, 
		   list_offline = Offline,
		   free_users = length(T),
		   list_connected = [],
		   first_client= {H, not_connected}},
    {reply, ok, State2};


handle_call({remove_connected, Id}, From, State) ->
    State2 = State#state{list_connected = lists:delete(Id, State#state.list_connected),
						 free_users = State#state.free_users + 1,
						 list_user = [Id | State#state.list_user]},
    {reply, ok, State2};


%%% Get a connected id different from 'Id'
handle_call( {get_one_connected, Id}, From, State) ->
    ?LOGF("free_users=~w, connected= ~w~n",
				[State#state.free_users, State#state.list_connected],?DEB),
	%% First remove Id from the connected list
    Connected = lists:delete(Id, State#state.list_connected),
    case {State#state.free_users, length(Connected)} of
		{0,0} ->
			{reply, undefined, State};
		{_, Length} ->
			Indice = random:uniform(Length+State#state.free_users) ,
			List = State#state.list_user ++ Connected,
			{reply, lists:nth(Indice, List), State}
    end;


    
handle_call(stop, From, State)->
    {stop, normal, ok, State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

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
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Return two lists:
%%% A list of N id's between NDeb and Nfin
%%% A list of (Nfin - NDeb +1) - N  offline Id's (between NDeb and Nfin)
%%%----------------------------------------------------------------------
build_clients({NDeb, NFin}, N)->
	build_clients(lists:seq(NDeb, NFin), N, []).

build_clients(List, 0, L)->
    {List, L};
build_clients(List, N, L)->
    Indice = random:uniform( length(List) ) , %% length is not efficient !
    Element = lists:nth(Indice, List),
    List2 = lists:delete(Element, List),
    build_clients(List2, N-1, [Element|L]).
