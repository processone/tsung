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

%%%  Created :  8 Feb 2001 by Nicolas Niclausse <nniclausse@idealx.com>

%%----------------------------------------------------------------------
%% HEADER ts_mon
%% COPYRIGHT IDEALX (C) 2001
%% PURPOSE monitor and log events and stats
%% DESCRIPTION
%%   TODO ...
%%----------------------------------------------------------------------

-module(ts_mon).
-author('nniclausse@idealx.com').
-vc('$Id$ ').

-behaviour(gen_server).

-include("../include/ts_profile.hrl").

%% External exports
-export([start/0, stop/0, newclient/1, endclient/1, newclient/1, sendmes/1,
		rcvmes/1, error/1, newclientrcv/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {log,
				client=0,
				type}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% FUNCTION start/0
%% PURPOSE Start the monitoring process
%% RETURN VALUE ok | throw({error, Reason})
%%----------------------------------------------------------------------
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, {stop}).

newclient({Who, When}) ->
	gen_server:cast(?MODULE, {newclient, Who, When}).

newclientrcv({Who, When}) ->
	gen_server:cast(?MODULE, {newclientrcv, Who, When}).

endclient({Who, When}) ->
	gen_server:cast(?MODULE, {endclient, Who, When}).

sendmes({Who, When, What}) ->
	gen_server:cast(?MODULE, {sendmsd, Who, When, What}).

rcvmes({Who, When, What}) ->
	gen_server:cast(?MODULE, {rcvmes, Who, When, What}).

error({Who, When, What}) ->
	gen_server:cast(?MODULE, {error, Who, When, What}).


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
init([]) ->
    Filename = ?log_file ++ integer_to_list(?nclients_deb),
    case file:open(Filename,write) of 
		{ok, Stream} ->
			?PRINTDEBUG2("starting monitor~n",?DEB),
			{ok, #state{type=?monitoring, log=Stream}};
		{error, Reason} ->
			{stop,openerror}
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
handle_cast({sendmsd, Who, When, What}, State) when State#state.type == none -> 
	{noreply, State,?monitor_timeout};

handle_cast({sendmsd, Who, When, What}, State) when State#state.type == light -> 
	io:format(State#state.log,"Send:~w:~w:~-44s~n",[When,Who,
												 binary_to_list(What)]),
	{noreply, State,?monitor_timeout};

handle_cast({sendmsd, Who, When, What}, State) ->
	io:format(State#state.log,"Send:~w:~w:~s~n",[When,Who,
												 binary_to_list(What)]),
	{noreply, State,?monitor_timeout};

handle_cast({rcvmes, Who, When, What}, State) when State#state.type == none ->
	{noreply, State,?monitor_timeout};

handle_cast({rcvmes, Who, When, What}, State) when State#state.type == light ->
	io:format(State#state.log,"Recv:~w:~w:~-44s~n",[When,Who, 
													binary_to_list(What)]),
	{noreply, State,?monitor_timeout};

handle_cast({rcvmes, Who, When, What}, State) ->
	io:format(State#state.log,"Recv:~w:~w:~s~n",[When,Who, 
													binary_to_list(What)]),
	{noreply, State,?monitor_timeout};


handle_cast({newclient, Who, When}, State) ->
	Clients =  State#state.client+1,
	io:format(State#state.log,"NewClient:~w:~w~n",[When, Who]),
	io:format(State#state.log,"load:~w~n",[Clients]),
	{noreply, State#state{client = Clients},?monitor_timeout};

handle_cast({newclientrcv, Who, When}, State) ->
	io:format(State#state.log,"NewClientRcv:~w:~w~n",[When, Who]),
	{noreply, State, ?monitor_timeout};

handle_cast({endclient, Who, When}, State) ->
	Clients =  State#state.client-1,
	io:format(State#state.log,"EndClient:~w:~w~n",[When, Who]),
	io:format(State#state.log,"load:~w~n",[Clients]),
	{noreply, State#state{client = Clients},?monitor_timeout}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(timeout, State) ->
	?PRINTDEBUG2("timeout monitor~n",?DEB),
	case State#state.client of 
		0 -> 
			io:format(State#state.log,"EndMonitor:~w~n",[now()]),
			{stop, normal, State};
		_ -> 
			{noreply, State, ?monitor_timeout}
	end;

handle_info(Info, State) ->
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
	?PRINTDEBUG2("stoping monitor~n",?INFO),
	file:close(State#state.log),
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
