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

-module(ts_req_server).
-author('nniclausse@schultze.ird.idealx.com').

-behaviour(gen_server).

%% External exports
-export([start/0, get_random_req/0, get_all_req/0, stop/0, read/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {items, %%lists of messages read from a file
				open=0
			   }).

-include("../include/ts_profile.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
read(Filename) ->
	gen_server:call(?MODULE, {read, Filename}).

start() ->
	?PRINTDEBUG2("Starting~n",?DEB),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_random_req()->
	gen_server:call(?MODULE,get_random_req).

get_all_req()->
	gen_server:call(?MODULE,get_all_req).

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
init([]) ->
	{ok, #state{}}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(get_all_req, From, State) ->
	Reply = {ok, State#state.items},
	{reply, Reply, State};

handle_call(get_random_req, From, State) ->
	I = random:uniform(length(State#state.items)),
	Reply = {ok,  lists:nth(I, State#state.items)},
	{reply, Reply, State};

handle_call({read, Filename}, From, State) when State#state.open == 0->
	?PRINTDEBUG("Opening file ~p~n",[Filename],?INFO),
    {Status, File} = file:open(Filename, read),
    case Status of
        error ->
            {stop, file_not_found, State};
        _ ->
            
            List_items = read_item(File, []),
            % Close the config file
            file:close(File),
			{reply, ok, #state{items = List_items, open= 1}}
    end;
handle_call({read, Filename}, From, State) ->
	?PRINTDEBUG("~p already opened~n",[Filename],?INFO),
	{reply, ok, State};

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

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% Treate one line of the file
read_item(File, L)->
    % Read one line
    Line = io:get_line(File, ""),
    case Line of
        eof ->
	    %%io:format(" ~s~n", [L]),
            L;
		_->
			Tokens = string:tokens(Line, "\n"),
            case Tokens of
                [] ->
                    read_item(File, L);
				
                ["#" | _] ->
                    read_item(File, L);
				
				[Value] ->
					List = lists:append(L, [Value]),
					read_item(File, List)
			end
    end.


