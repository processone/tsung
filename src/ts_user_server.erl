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

-include("../include/ts_profile.hrl").

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
-export([start_link/1, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {list_offline, list_user, list_connected, first_client}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link([NDeb, NFin, NClients]) ->
	?PRINTDEBUG("Starting with args ~p ~n",[[NDeb, NFin, NClients]],?DEB),
    gen_server:start_link({local, ?MODULE}, ?MODULE, {NDeb, NFin, NClients}, []).

start([NDeb, NFin, NClients]) ->
	?PRINTDEBUG("Starting with args ~p ~n",[[NDeb, NFin, NClients]],?DEB),
    gen_server:start({local, ?MODULE}, ?MODULE, {NDeb, NFin, NClients}, []).

reset(NDeb, NFin, NClients)->
    gen_server:call(?MODULE, {reset, NDeb, NFin, NClients}).

get_id()->
    gen_server:call(?MODULE, get_id).

get_idle()->
    gen_server:call(?MODULE, get_idle).


get_one_connected(Id)->
    gen_server:call(?MODULE, {get_one_connected, Id}). 

get_offline()->
    gen_server:call(?MODULE, get_offline).


add_to_connected(Id)->
    gen_server:call(?MODULE, {add_to_connected, Id}). 


connect_first()->
    gen_server:call(?MODULE, connect_first).

disconnect_first()->
    gen_server:call(?MODULE, disconnect_first).


get_first()->
    gen_server:call(?MODULE, get_first).

remove_connected(Id)->
    gen_server:call(?MODULE, {remove_connected, Id}). 

stop()->
    gen_server:call(?MODULE, stop).

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
init({NDeb, NFin, NClients}) ->
	?PRINTDEBUG2("starting ...",?DEB),
    {Msec, Sec, Nsec} = ts_utils:init_seed(),
    random:seed(Msec,Sec,Nsec),

    List =  build_idlist(NDeb, NFin, NClients+1),
    {Offline, [H|T]} = select_client(List, NClients+1, []),
	?PRINTDEBUG2("ok",?DEB),
    State = #state{list_user = T, 
		   list_offline = Offline,
		   list_connected = [],
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
    case length(State#state.list_user) of
	0 ->
	    {reply, [], State};
	_Other ->
		%% pourquoi pas [Element | List2] = State#state.list_user,
		Element = lists:nth(1, State#state.list_user),
	    State2 = State#state{list_user = lists:delete(Element, State#state.list_user),
				 list_connected = lists:append(State#state.list_connected, [Element])},
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
    [H|T] = build_idlist(NDeb, NFin, NClients+1) ,
    State2 = #state{list_user = T, 
		   list_connected = [],
		   first_client= {H, not_connected}},
    {reply, ok, State2};


handle_call({remove_connected, Id}, From, State) ->
    State2 = State#state{list_connected = lists:delete(Id, State#state.list_connected),
			list_user = lists:append(State#state.list_user, [Id])},
    {reply, ok, State2};


handle_call( {get_one_connected, Id}, From, State) ->
    ?PRINTDEBUG("~w ~w~n", [State#state.list_user, State#state.list_connected],?DEB),
    List = State#state.list_user ++ State#state.list_connected,
    case length(List) of
		0 ->
			{reply, [], State};
		Length ->
			Indice = random:uniform(Length) ,
			case lists:nth(Indice, List) of
				Id ->
					Indice2 = (Indice rem Length) + 1,
					Element2 = lists:nth(Indice2, List),
					{reply, Element2, State}; 
				Element ->
					{reply, Element, State}
			end
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
%%% Build list of potential users id.
%%%----------------------------------------------------------------------
select_client(List, 0, L)->
    {List, L};
select_client(List, N, L)->
    Indice = random:uniform( length(List) ) ,
    Element = lists:nth(Indice, List),
    List2 = lists:delete(Element, List),
    select_client(List2, N-1, L ++ [Element]).



%% Build list from NDeb to Nfin  

buildidlist(NFin, NFin, L)->
    L ++ [NFin];
buildidlist(N, NFin, L)->
    buildidlist(N+1, NFin, L ++ [N]). 

build_idlist(NDeb, NFin, NClients) when NDeb =< NFin ->
    List = buildidlist(NDeb, NFin, []);

build_idlist(NDeb, NFin, NClients)  ->
	?PRINTDEBUG("Bad interval ! ~p is not < ~p~n",[NDeb, NFin],?ERR),
	badinterval.

