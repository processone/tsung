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

-module(ts_client_rcv).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-behaviour(gen_server).

-include("../include/ts_profile.hrl").

%% External exports
-export([start/1, stop/1, wait_ack/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {socket,		% unused ?
				timeout,	% ?
				ack,        % 
				ppid,		% pid of send process
				clienttype, % module name (jabber, etc.)
				parsetype   % parse|noparse
			   }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% reconnection: new socket
start(Opts) ->
	?PRINTDEBUG("Starting with opts: ~p~n",[Opts],?DEB),
	gen_server:start_link(?MODULE, [Opts], []).

stop(Pid) ->
	?PRINTDEBUG2("Stoping ~n",?DEB),
	gen_server:cast(Pid, {stop}).

wait_ack(Pid, Ack) ->
	gen_server:cast(Pid, {wait_ack, Ack}).


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
init([{PType, CType, PPid, Socket, Timeout, Ack}]) ->
	?PRINTDEBUG2("init ...~n",?DEB),
	ts_mon:newclientrcv({self(), now()}),
	{ok, #state{socket = Socket, timeout= Timeout, ack = Ack,
				ppid= PPid, parsetype = PType, clienttype = CType}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
	?PRINTDEBUG("Unknown call ! ~p~n",[Request], ?ERR),
	Reply = ok,
	{reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%% parse the response
%%if not complete ? or several responses in a single packet ?)

%% ack value -> wait
handle_cast({wait_ack, Ack}, State) ->
	?PRINTDEBUG("receive wait_ack: ~p~n",[Ack], ?DEB),
	{noreply, State#state{ack=Ack}};

handle_cast({stop}, State) ->
	{stop, normal, State};

%% ack value -> wait
handle_cast(Message, State) ->
	?PRINTDEBUG("Unknown messages ! ~p~n",[Message], ?ERR),
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State) when State#state.parsetype == parse ->
	?PRINTDEBUG("receive data: ~p~n",[Data], ?DEB),
	ts_mon:rcvmes({self(), now(), Data}),
	Module = State#state.clienttype,
	Module:parse(Data, State#state.ack),
	case State#state.ack of
		local ->
			ts_client:next(State#state.ppid);
		global ->
			ts_timer:connected(State#state.ppid);
		_Other ->
			continue
	end,
	{noreply, State#state{ack=no_ack}};

%% no parsing
handle_info({tcp, Socket, Data}, State) ->
	?PRINTDEBUG("receive data: ~p~n",[Data], ?DEB),
	ts_mon:rcvmes({self(), now(), Data}),
	case State#state.ack of
		local ->
			?PRINTDEBUG("send next (ack) to father ~p ~n",[State#state.ppid] ,?DEB),
			ts_client:next(State#state.ppid);
		global ->
			ts_timer:connected(State#state.ppid);
		_Other ->
			continue
	end,
	{noreply, State#state{ack=no_ack}};

handle_info({tcp_closed, Socket}, State) ->
	?PRINTDEBUG2("TCP close: ~n", ?NOTICE),
	ts_client:close(State#state.ppid),
	{noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
	?PRINTDEBUG("TCP error: ~p~n",[Reason], ?WARN),
	ts_client:close(State#state.ppid),
	{noreply, State};

handle_info(Info, State) ->
	?PRINTDEBUG("Unknown info message ! ~p~n",[Info], ?ERR),
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
	?PRINTDEBUG("Stop, reason= ~p~n",[Reason],?DEB),
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

