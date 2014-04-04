%%%
%%%  Copyright 2009 © Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas.nniclausse@niclux.org>
%%%  Created: 17 mar 2009 by Nicolas Niclausse <nicolas.nniclausse@niclux.org>
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

-module(ts_cport).
-vc('$Id: ts_cport.erl,v 0.0 2009/03/17 10:26:56 nniclaus Exp $ ').
-author('nniclausse@niclux.org').

-behaviour(gen_server).

-include("ts_macros.hrl").

%% API
-export([start_link/1, get_port/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(EPMD_PORT,4369).

-record(state, {
         min_port = 1025,
         max_port = 65535
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [], []).


get_port(CPortServer,IP)->
    gen_server:call({global,CPortServer},{get,IP}).

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
    %% registering can be long (global:sync needed), do it after the
    %% init phase (the config_server will send us a message)
    {Min, Max} = {?config(cport_min),?config(cport_max)},
    ts_utils:init_seed(),
    %% set random port for the initial value.
    case catch Min+random:uniform(Max-Min) of
        Val when is_integer(Val) ->
            ?LOGF("Ok, starting with ~p value~n",[Val],?NOTICE),
            {ok, #state{min_port=Min, max_port=Max}};
        Err ->
            ?LOGF("ERR starting:  ~p~n",[Err],?ERR),
            {ok, #state{}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, ClientIP}, _From, State) ->
    %% use the process dictionnary to store the last port of each ip
    %% should we use ets instead ?
    Reply = case get(ClientIP) of
                ?EPMD_PORT ->
                    ?EPMD_PORT + 1;
                Val when Val > State#state.max_port ->
                    State#state.min_port;
                Val ->
                    Val
                end,
    put(ClientIP,Reply+1),
    ?LOGF("Give port number ~p to IP ~p~n",[Reply,ClientIP],?DEB),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

