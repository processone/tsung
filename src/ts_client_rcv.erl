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
	{ok, #state_rcv{socket = Socket, timeout= Timeout, ack = Ack,
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
	{noreply, State#state_rcv{ack=Ack, ack_done=false, ack_timestamp= When}};

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
handle_info({tcp, Socket, Data}, State) ->
	NewState = handle_data_msg(Data, State),
	{noreply, NewState};

%% ssl case
handle_info({ssl, Socket, Data}, State) ->
	NewState = handle_data_msg(Data, State),
	{noreply, NewState};

handle_info({tcp_closed, Socket}, State) ->
	?PRINTDEBUG2("TCP close: ~n", ?NOTICE),
	ts_client:close(State#state_rcv.ppid),
	{noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
	?PRINTDEBUG("TCP error: ~p~n",[Reason], ?WARN),
	ts_client:close(State#state_rcv.ppid),
	{noreply, State};

handle_info({ssl_closed, Socket}, State) ->
	?PRINTDEBUG2("SSL close: ~n", ?NOTICE),
	ts_client:close(State#state_rcv.ppid),
	{noreply, State};

handle_info({ssl_error, Socket, Reason}, State) ->
	?PRINTDEBUG("SSL error: ~p~n",[Reason], ?WARN),
	ts_client:close(State#state_rcv.ppid),
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
	case State#state_rcv.monitor of
		none ->
			skip;
		_ ->
			ts_mon:rcvmes({self(), now(), Data})
	end,
	DataSize = size(Data),
	case {State#state_rcv.ack, State#state_rcv.ack_done} of
		{no_ack, _} ->
			State;
		{parse, _} ->
			Module = State#state_rcv.clienttype,
			NewState = Module:parse(Data, State),
			if 
				NewState#state_rcv.ack_done == true ->
					?PRINTDEBUG("Response done:~p~n", [NewState#state_rcv.datasize], ?DEB),
					update_stats(NewState);
				true ->
					?PRINTDEBUG("Response: continue:~p~n",[NewState#state_rcv.datasize], ?DEB),
					nothing
			end,
			NewState;
		{AckType, true} ->
			% still same message, increase size
			OldSize = State#state_rcv.datasize,
			State#state_rcv{datasize = OldSize+DataSize};
		{AckType, false} ->
			update_stats(State),
			State#state_rcv{datasize= DataSize, ack_done = true} % ack for a new message, init size
	end.

update_stats(State) ->
	Now = now(),
	Elapsed = ts_utils:elapsed(State#state_rcv.ack_timestamp, Now),
	ts_mon:addsample({self(), Now, response_time, Elapsed}), % response time
	ts_mon:addsample({self(), Now, size, State#state_rcv.datasize}),
	doack(State#state_rcv.ack, State#state_rcv.ppid).

%%----------------------------------------------------------------------
%% Func: doack/2
%%----------------------------------------------------------------------
doack(parse, Pid) ->
	ts_client:next(Pid);
doack(local, Pid) ->
	ts_client:next(Pid);
doack(global, Pid) ->
	ts_timer:connected(Pid).

