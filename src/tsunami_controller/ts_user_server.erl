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
-export([reset/1, 
	 get_id/0,
	 get_idle/0,
	 get_offline/0,
%	 add_to_connected/1,
	 remove_connected/1,
	 get_one_connected/1,
	 get_first/0]).

-behaviour(gen_server).

%% External exports
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {offline,      %ets table 
                online,    %ets table
                first_client, % id (integer)
                userid_max    % max number of ids (starts at 1)
                }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Args) ->
	?LOGF("Starting with args ~p ~n",[Args],?INFO),
    gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

reset(NFin)->
    gen_server:call({global, ?MODULE}, {reset, NFin}).

get_id()->
    gen_server:call({global, ?MODULE }, get_id).

%% get an idle id, and add it to the connected table
get_idle()->
    gen_server:call({global, ?MODULE}, get_idle).

get_one_connected(Id)->
    gen_server:call({global, ?MODULE}, {get_one_connected, Id}). 

get_offline()->
    gen_server:call({global, ?MODULE}, get_offline).


get_first()->
    gen_server:call({global, ?MODULE}, get_first).

remove_connected(Id)->
    gen_server:cast({global, ?MODULE}, {remove_connected, Id}). 

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
init(Args) ->
    ts_utils:init_seed(),
	?LOG("ok, started with unconfigured~n", ?INFO),
    {ok, #state{}}.


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
    Key = random:uniform( State#state.userid_max ) +1,
    {reply, Key, State};

%%Get one id in the users whos have to be connected
handle_call(get_idle, From, State=#state{offline=Offline,online=Online}) ->
    case ets:first(Offline) of 
        '$end_of_table' ->
			?LOG("No more free users !~n", ?WARN),
            {reply, {error, no_free_userid}, State};
        Key ->
            ets:delete(Offline, Key),
            ets:insert(Online, {Key,1}),
            case State#state.first_client of 
                undefined ->
                    {reply, Key, State#state{first_client=Key}};
                Id ->
                    {reply, Key, State}
            end
    end;

%%Get one offline id 
handle_call(get_offline, From, State=#state{offline=Offline,online=Online}) ->
    case ets:first(Offline) of 
        '$end_of_table' ->
            {reply, {error, no_offline}, State};
        Key ->
            {reply, Key, State}
    end;


handle_call(get_first, From, State) ->
    {reply, State#state.first_client, State};

handle_call({reset, NFin}, From, State) ->
    Offline = ets:new(offline,[set, private]),
    Online  = ets:new(online, [set, private]),

    fill_offline(NFin, Offline),
    State2 = #state{offline=Offline, first_client = undefined,
                    online =Online, userid_max=NFin},
    {reply, ok, State2};

%%% Get a connected id different from 'Id'
handle_call( {get_one_connected, Id}, From, State=#state{offline=Offline,online=Online}) ->
    case ets:first(Online) of
        '$end_of_table' ->
            {reply, {error, no_online}, State};
        Id ->
            case ets:next(Online, Id) of
                '$end_of_table' ->
                    {reply, {error, no_online}, State};
                Key ->
                    {reply, Key, State}
            end;
        Key2 ->
            {reply, Key2, State}
    end;
    
handle_call(stop, From, State)->
    {stop, normal, ok, State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({remove_connected, Id}, State=#state{offline=Offline,online=Online}) ->
    ets:delete(Online, Id),
    ets:insert(Offline, {Id,2}),
    {noreply, State};

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
fill_offline(0, Tab)->
    ok;
fill_offline(N, Tab) when is_integer(N) ->
    ets:insert(Tab,{N, 0}),
    fill_offline(N-1, Tab).
