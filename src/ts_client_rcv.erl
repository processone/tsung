%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2002 IDEALX
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
-export([start/1, stop/1, wait_ack/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {socket,		% unused ?
				timeout,	% ?
				ack,        % 
				ack_timestamp,
				datasize=0,
				ppid,		% pid of send process
				clienttype, % module name (jabber, etc.)
				parsetype,  % parse|noparse
				monitor     % type of monitoring (full, light, none)
			   }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% reconnection: new socket
start(Opts) ->
	gen_server:start_link(?MODULE, [Opts], []).

stop(Pid) ->
	gen_server:cast(Pid, {stop}).

wait_ack(Pid, Ack, When) ->
	gen_server:cast(Pid, {wait_ack, Ack, When}).


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
init([{PType, CType, PPid, Socket, Timeout, Ack, Monitor}]) ->
	ts_mon:newclientrcv({self(), now()}),
	{ok, #state{socket = Socket, timeout= Timeout, ack = Ack,
				ppid= PPid, parsetype = PType, clienttype = CType,
				monitor = Monitor }}.

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
handle_cast({wait_ack, Ack, When}, State) ->
	?PRINTDEBUG("receive wait_ack: ~p~n",[Ack], ?DEB),
	{noreply, State#state{ack=Ack, ack_timestamp= When}};

handle_cast({stop}, State) ->
	{stop, normal, State};

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
	case State#state.monitor of
		none ->
			skip;
		_ ->
			ts_mon:rcvmes({self(), now(), Data})
	end,
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
	Size = handle_data_msg(Data, State),
	{noreply, State#state{ack=no_ack, datasize = Size}};

% ssl case, same code; make a function to factorize code ?
handle_info({ssl, Socket, Data}, State) ->
	Size = handle_data_msg(Data, State),
	{noreply, State#state{ack=no_ack, datasize = Size}};

handle_info({tcp_closed, Socket}, State) ->
	?PRINTDEBUG2("TCP close: ~n", ?NOTICE),
	ts_client:close(State#state.ppid),
	{noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
	?PRINTDEBUG("TCP error: ~p~n",[Reason], ?WARN),
	ts_client:close(State#state.ppid),
	{noreply, State};

handle_info({ssl_closed, Socket}, State) ->
	?PRINTDEBUG2("SSL close: ~n", ?NOTICE),
	ts_client:close(State#state.ppid),
	{noreply, State};

handle_info({ssl_error, Socket, Reason}, State) ->
	?PRINTDEBUG("SSL error: ~p~n",[Reason], ?WARN),
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
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: handle_data_msg/2
%%----------------------------------------------------------------------
handle_data_msg(Data, State) ->
	case State#state.monitor of
		none ->
			skip;
		_ ->
			ts_mon:rcvmes({self(), now(), Data})
	end,
	DataSize = size(Data),
	case State#state.ack of
		no_ack ->
			DataSize+State#state.datasize; % still same message, increase size
		AckType -> % should be local or global
			Now = now(),
			Elapsed = ts_utils:elapsed(State#state.ack_timestamp, Now),
			ts_mon:addsample({self(), Now, latency, Elapsed}),
			ts_mon:addsample({self(), Now, size, State#state.datasize}),
			doack(AckType, State#state.ppid),
			DataSize % ack for a new message, init size
	end.

%%----------------------------------------------------------------------
%% Func: doack/2
%%----------------------------------------------------------------------
doack(local, Pid) ->
	ts_client:next(Pid);
doack(global, Pid) ->
	ts_timer:connected(Pid).

