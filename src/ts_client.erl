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
%%%  Created : 15 Feb 2001 by Nicolas Niclausse <nniclausse@IDEALX.com>

-module(ts_client).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').
-modified_by('jflecomte@IDEALX.com').

-behaviour(gen_server).

-include("../include/ts_profile.hrl").

%% External exports
-export([start/1, next/1, close/1]).

%% gen_server callbacks
-export([init/1, init/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {rcvpid, % pid of receiving process
				server, % name (or IP) of server
				port,   % server port
				socket, % 
				clienttype, %
				parsetype,   % type of client (parse or noparse)
				mestype,   % type of messages (dynamic or static)
				profile,% list of requests parameters
				count  % number of requests waiting to be sent
			   }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Opts) ->
	?PRINTDEBUG("Starting with opts: ~p~n",[Opts],?DEB),
	gen_server:start(?MODULE, Opts, []).

close(Pid) ->
	gen_server:cast(Pid, {closed, Pid}).

next(Pid) ->
	gen_server:cast(Pid, {next_msg}).

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
init([Profile, {CType, PType, static}]) ->
	?PRINTDEBUG2("Init ... static~n",?DEB),
	init([Profile, {CType, PType, static}], length(Profile));

init([Profile, {CType, PType, dynamic}]) ->
	?PRINTDEBUG2("Init ... dynamic~n",?DEB),
	random:seed(),
	init([Profile, {CType, PType, static}], ?messages_number + length(Profile) - 1);

init(Args) ->
	?PRINTDEBUG("Init ... with unknown args ~p~n",[Args],?DEB).
	

init([Profile, {CType, PType, MType}], Count) ->
	%%init seed
	?PRINTDEBUG("Init ... started with count = ~p  ~n",[Count],?DEB),
%    random:seed(ts_utils:init_seed()),
%	?PRINTDEBUG2("seed OK  ~n",?DEB),

    {Server, Port} = ts_profile:get_server(), % get server profile
    % open TCP connection
    case gen_tcp:connect(Server, Port,
						 [binary, 
						  {active, true},
						  {keepalive, true},
						  {sndbuf, ?snd_size},
						  {recbuf, ?rcv_size}]) of
		{ok, Socket} -> 
	    % start a new process for receiving messages from the server
			case ts_client_rcv:start({PType,
												CType, self(),
												Socket, 
												?tcp_timeout, 
												no_ack}) of 
				{ok, Pid} ->
					?PRINTDEBUG2("rcv server started ~n",?DEB),
					gen_tcp:controlling_process(Socket, Pid),
					ts_mon:newclient({self(), now()}),
					{ok, #state{rcvpid = Pid, socket = Socket, port = Port,
								server= Server, profile= Profile, 
								clienttype = CType, mestype = MType,
								count = Count, parsetype = PType}, 1};
				{error, Reason} ->
					?PRINTDEBUG("Can't start rcv process ~p~n",
								[Reason],?ERR),
					{stop, Reason}
			end;
		{error, Reason} ->
			?PRINTDEBUG("Error: ~p~n",[Reason],?ERR),
			{stop, connfailed}
    end.


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
	Reply = ok,
	{reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({next_msg}, State) ->
	?PRINTDEBUG2("next_msg",?DEB),
	{noreply, State, 1};


%% to be done : what about the current timer ?
handle_cast({add_messages, Messages}, State) ->
	OldProfile = State#state.profile,
	OldCount = State#state.count,
	{noreply, State#state{profile = Messages ++ OldProfile, 
						  count = OldCount + length(Messages)}, 1};

handle_cast({closed, Pid}, State) ->
	{stop, normal, State};

handle_cast({timeout, Pid}, State) ->
	ts_mon:endclient({self(), now()}),
	{stop, timeoutrcv, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(timeout, State) when State#state.count > 0 ->
	Len = length(State#state.profile),
	[Profile | Pending] = State#state.profile,
	case {Profile#message.type, State#state.count} of 
		{dynamic, Len } when Len == length(State#state.profile) ->
			%% no dynamic message remaining
			Thinktime = round(ts_stats:exponential(?messages_intensity)),
			PendingProfile = Pending;
		{static, _Len} ->
			Thinktime = Profile#message.thinktime,
			PendingProfile = Pending;
		{dynamic, _} -> %% keep the same profile until all dynamic messages done
			Thinktime = round(ts_stats:exponential(?messages_intensity)),
			PendingProfile = State#state.profile
	end,
	Count = State#state.count-1,
	Message = ts_profile:get_message(State#state.clienttype,
								  Profile#message.param),
	ts_client_rcv:wait_ack(State#state.rcvpid,Profile#message.ack),
    case gen_tcp:send(State#state.socket, Message) of
		ok -> 
			ts_mon:sendmes({self(), now(), Message}),
			{noreply, State#state{count = Count, profile = PendingProfile},
			 Thinktime}; 
		{error, Reason} -> 
			?PRINTDEBUG(
			   "Error: Unable to send data from process ~p, reason: ~p~n.", 
			   [self(), Reason],?ERR),
			{stop, Reason, State}
	end;

%% no more messages to send
handle_info(timeout, State)  ->
	ts_mon:endclient({self(), now()}),
	{stop, normal,State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
	?PRINTDEBUG("Stop, reason= ~p~n",[Reason],?INFO),
	gen_tcp:close(State#state.socket),
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

