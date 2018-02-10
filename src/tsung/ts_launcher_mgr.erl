%%%
%%%  Copyright 2009 © Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 09 déc. 2009 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

-module(ts_launcher_mgr).
-vc('$Id: ts_launcher_mgr.erl,v 0.0 2009/12/09 11:54:33 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_config.hrl").
-include("ts_profile.hrl").

-behaviour(gen_server).

%% API
-export([start/0, alive/1, die/1, check_registered/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {launchers=0, synced, check_timeout}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

die(Type)->
    gen_server:cast(?MODULE, {die, Type}).

alive(Type)->
    gen_server:cast(?MODULE, {alive, Type}).

check_registered()->
    gen_server:call(?MODULE, {check_registered}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ?LOG("starting",?INFO),
    {ok, #state{check_timeout=?check_noclient_timeout}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({check_registered}, _From,State=#state{synced=undefined}) ->
%% Check if global names are synced; Annoying "feature" of R10B7 and up
    case global:registered_names() of
        ["cport"++_Tail] ->
            ?LOG("Only cport server registered  ! syncing ...~n", ?WARN),
            global:sync();
        [] ->
            ?LOG("No registered processes ! syncing ...~n", ?WARN),
            global:sync();
        _ ->
            ok
    end,
    ts_mon:launcher_is_alive(),
    {reply, ok, State#state{synced=yes}};

handle_call({check_registered}, _From,State=#state{synced=yes}) ->
    ?LOG("syncing already done, skip~n", ?INFO),
    {reply, ok, State#state{synced=yes}};

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({alive, Type}, State=#state{launchers=N}) ->
    ?LOGF("~p launcher is starting on node ~p ~n",[Type,node()],?DEB),
    {noreply, State#state{launchers=N+1}};

handle_cast({die, _Type}, State=#state{launchers=1}) ->
    ?LOGF("All launchers are done on node ~p, wait for active clients to finish~n",[node()],?INFO),
    ts_config_server:endlaunching(node()),
    check_clients(State#state{launchers=0});

handle_cast({die, Type}, State=#state{launchers=N}) ->
    ?LOGF("~p launcher is stopping on node ~p ~n",[Type, node()],?DEB),
    {noreply, State#state{launchers=N-1}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({timeout, _Ref, check_noclient}, State) ->
    check_clients(State);
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    case ts_utils:is_controller() of
        false ->
            slave:stop(node()); %% commit suicide.
        true ->
            ok
    end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

check_clients(State=#state{check_timeout=CheckTimeout}) ->
    case ts_client_sup:active_clients() of
        0 -> % no users left, and no more launchers, stop
            ?LOGF("No more active users ~p ~p~n",[node(), os:getpid()], ?NOTICE),
            timer:sleep(?CACHE_DUMP_STATS_INTERVAL+10), %% let ts_mon_cache send it's last stats
            ts_mon:stop(), %% we must warn ts_mon that our clients have finished
            case ts_sup:has_cport(node()) of
                true ->  %%do not finish this beam
                    ?LOGF("Beam will not be terminated because it has a cport server ~p ~p~n",[node(), os:getpid()], ?NOTICE),
                    {noreply, State};
                false ->
                    {stop, normal, State}
            end;
        ActiveClients when ActiveClients > 1000 ->
            %% the call to active_clients can be cpu hungry if lot's of clients are running
            %% use a long timer in this case.
            ?LOGF("Still ~p active client(s)~n", [ActiveClients],?NOTICE),
            erlang:start_timer(CheckTimeout, self(), check_noclient ),
            {noreply, State};
        ActiveClients ->
            ?LOGF("Still ~p active client(s)~n", [ActiveClients],?DEB),
            erlang:start_timer(?fast_check_noclient_timeout, self(), check_noclient ),
            {noreply, State}
    end.
