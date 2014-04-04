%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 22 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

%%%-------------------------------------------------------------------
%%% File    : ts_proxy_listener.erl
%%% Author  :  <nicolas.niclaussse@IDEALX.com>
%%% Description :
%%% Created : 22 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------


-module(ts_proxy_listener).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_macros.hrl").
-include("ts_recorder.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API

-export([start/0]).

%% Self callbacks

-record(state, {
          plugin,
          acceptsock,  % The socket we are accept()ing at
          acceptloop_pid, % The PID of the companion process that blocks
                                                % in accept().
          accept_count = 0 % The number of accept()s done so far.
         }).

%%====================================================================
%% Server and API functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: starts a listener process.
%%--------------------------------------------------------------------

start()->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server. This is launched from the
%%       subprocess and should return a state record. The argument
%%       is a configuration function
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init(_Config) ->
    State=#state{plugin=?config(plugin)},
    activate(State).

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Purpose: The companion process does synchronous calls to
%%          us everytime accept() returns (either as a new socket or an error).
%%          We get to tell him whether it should continue or stop in the
%%          return value of the call. We also honor destroy requests from
%%          , shutting down the whole listener.
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    case State#state.acceptsock of
        undefined -> nothing;
        Socket -> ssl:close(Socket)
    end,
    NewState=State#state{acceptsock=undefined},
    {stop, normal, ok, NewState};

handle_call({accepted, _Tag, ClientSock}, _From, State) ->
    ?LOGF("New socket:~p~n", [ClientSock],?DEB),
    case ts_client_proxy_sup:start_child(ClientSock) of
        {ok, Pid} ->
            ?LOGF("New connection from~p~n", [inet:peername(ClientSock)],?INFO),
            ok = gen_tcp:controlling_process(ClientSock, Pid),
            ts_client_proxy:set_active(Pid);
        Error ->
            ?LOGF("Failed to launch new client ~p~n",[Error],?ERR),
            gen_tcp:close(ClientSock)
    end,
    NumCnx = State#state.accept_count,
    {reply, continue, State#state{accept_count=NumCnx+1}};

handle_call({accept_error, _Tag, Error}, _From, State) ->
    ?LOGF("accept() failed ~p~n",[Error],?ERR),
    case Error of
        {error, esslaccept} ->
            %% Someone may be testing the app by trying plain telnets.
            %% Let go.
            {reply, continue, State};
        _ ->
            {stop, Error, stop, State}
    end;

handle_call(_, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    case State#state.acceptsock of
        undefined -> nothing;
        Socket -> gen_tcp:close(Socket)
    end,
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Func: do_activate/1
%% Params: State
%% Return: NewState
%% Description: activates the listener instance described by State
%%   and returns the new state. If the instance is already active, do
%%   nothing.
%%--------------------------------------------------------------------
activate(State=#state{plugin=Plugin})->
    case State#state.acceptsock of
        undefined ->
            Portno=?config(proxy_listen_port),
            Opts = lists:append(Plugin:socket_opts(),
                                [{reuseaddr, true}, {active, false}]),
            case gen_tcp:listen(Portno, Opts) of
                {ok, ServerSock} ->
                    {ok, State#state
                      {acceptsock=ServerSock,
                       acceptloop_pid = spawn_link(ts_utils,
                                                   accept_loop,
                                                   [self(), unused, ServerSock])}};
                {error, Reason} ->
                    io:format("Error when trying to listen to socket: ~p~n",[Reason]),
                    {stop, Reason}
            end;
        _ -> %% Already active
            {ok, State}
    end.


%% Local Variables:
%% tab-width:4
%% End:
