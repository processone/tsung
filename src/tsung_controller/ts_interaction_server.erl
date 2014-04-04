%%%-------------------------------------------------------------------
%%% @author Nicolas Niclausse <nicolas@niclux.org>
%%% @copyright (C) 2012, Nicolas Niclausse <nicolas@niclux.org>
%%% @doc
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
%%%
%%% @end
%%% Created: 20 août 2009 by Nicolas Niclausse  <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_interaction_server).

-behaviour(gen_server).

-include("ts_macros.hrl").

%% API
-export([start/0, send/1, rcv/1, notify/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {to, notify}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @spec send({StatsName::atom(), Date::term()}) -> ok
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
send({StatsName, Date}) when is_atom(StatsName) ->
    gen_server:cast({global, ?SERVER}, {send, StatsName, Date}).

%%--------------------------------------------------------------------
%% @spec rcv({StatsName, Date}) -> ok
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
rcv({StatsName, Date}) when is_atom(StatsName) ->
    gen_server:cast({global, ?SERVER}, {'receive', StatsName, Date}).


%%--------------------------------------------------------------------
%% @spec delete({StatsName}) -> ok
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
delete({StatsName}) when is_atom(StatsName) ->
    gen_server:cast({global, ?SERVER}, {delete, StatsName}).

%%--------------------------------------------------------------------
%% @spec notify({Action::atom(), StatsName::atom(), Pid::pid()}) -> ok
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
notify({Action, StatsName, Pid}) when is_atom(Action), is_atom(StatsName), is_pid(Pid) ->
    gen_server:cast({global, ?SERVER}, {notify,Action,StatsName,Pid}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{to=ets:new(to, []), notify=ets:new(notify, [bag])}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send, StatsName, Date}, State=#state{to=To, notify=Notify}) ->
    case ets:lookup(To,StatsName) of
        [] ->
            ?LOGF("interaction: setting send timestamp for ~p~n",[StatsName],?DEB);
        _Val ->
            ?LOGF("interaction: resetting send timestamp for ~p~n",[StatsName],?WARN)
    end,
    ets:insert(To,{StatsName, Date}),
    handle_notification(Notify,{send, StatsName}),
    {noreply, State};

handle_cast({'receive',StatsName, EndDate}, State=#state{to=To, notify=Notify}) ->
    ?LOGF("~p ~n",[StatsName],?DEB),
    case ets:lookup(To,StatsName) of
        [] ->
            handle_notification(Notify,{'receive',StatsName}),
            {noreply, State};
        [{_Key, StartDate}] ->
            ?LOGF("to/from ended, logging ~p ~n",[StatsName],?DEB),
            handle_notification(Notify,{'receive',StatsName}),
            ts_mon:add({sample,StatsName, ts_utils:elapsed(StartDate,EndDate)}),
            {noreply, State}
    end;

handle_cast({delete, StatsName},  State=#state{to=To}) ->
    ets:delete(To,StatsName),
    {noreply, State};

handle_cast({notify, Action, StatsName, Pid},  State=#state{notify=Notify}) ->
    %% TODO: check if event already exists ?
    ets:insert(Notify,{{StatsName, Action}, {Pid}}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_notification(Notify, {Action, StatsName}) ->
    case ets:lookup(Notify, {StatsName, Action}) of
        [] ->
            ok;
        List ->
            ?LOGF("gotlist ~p~n",List,?DEB),
            Fun = fun({{Stats, Action2},{Pid}}) ->
                          ?LOGF("sending msg to pid ~p~n",[Pid],?DEB),
                          Pid ! {notify, Action2, Stats}
                  end,
            lists:foreach(Fun,List),
            ets:delete(Notify,StatsName)
    end.
