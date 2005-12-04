%%%  Copyright (C) 2005 Nicolas Niclausse
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

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : ts_file_server.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : Read a line-based file
%%%
%%% Created :  6 Jul 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------

-module(ts_file_server).
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%% External exports
-export([start/0, get_random_line/0, get_next_line/0, get_all_lines/0, stop/0,
         read/1, read/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {items,    %% lists of lines read from a file
				open = 0,
				size,     %% total number of lines
                current=-1   %% current line in file
			   }).

-include("ts_profile.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
read(Filename) ->
	gen_server:call({global, ?MODULE}, {read, Filename}, ?config(file_server_timeout)).

read(Filename,Timeout) ->
	gen_server:call({global, ?MODULE}, {read, Filename}, Timeout).

start() ->
	?LOG("Starting~n",?DEB),
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_random_line()->
	gen_server:call({global, ?MODULE},get_random_line).

get_next_line()->
	gen_server:call({global, ?MODULE},get_next_line).

get_all_lines()->
	gen_server:call({global, ?MODULE},get_all_lines).

stop()->
    gen_server:call({global, ?MODULE}, stop).

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
handle_call(get_all_lines, _From, State) ->
	Reply = {ok, State#state.items},
	{reply, Reply, State};

handle_call(get_random_line, _From, State) ->
	I = random:uniform(State#state.size),
	Reply = {ok,  lists:nth(I, State#state.items)},
	{reply, Reply, State};

handle_call(get_next_line, _From, State) ->
	I = (State#state.current + 1)  rem State#state.size,
	Reply = {ok,  lists:nth(I+1, State#state.items)},
	{reply, Reply, State#state{current=I}};

handle_call({read, Filename}, _From, State) when State#state.open == 0 ->
	?LOGF("Opening file ~p~n",[Filename],?INFO),
    {Status, File} = file:open(Filename, read),
    case Status of
        error ->
            ?LOGF("Error while opening ~p file ~p~n",[File, Filename],?ERR),
			{reply, {error, File}, State};
        _ ->
            List_items = read_item(File, []),
            % Close the config file
            file:close(File),
			{reply, ok, State#state{items = List_items, open= 1, size=length(List_items)}}
    end;
handle_call({read, Filename}, _From, State) ->
	?LOGF("~p already opened~n",[Filename],?INFO),
	{reply, {error, already_open}, State};

handle_call(stop, _From, State)->
    {stop, normal, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Treate one line of the file
%% Lines starting by '#' are skipped
%%----------------------------------------------------------------------
read_item(File, L)->
    % Read one line
    Line = io:get_line(File, ""),
    case Line of
        eof ->
            L;
		_->
			Tokens = string:tokens(Line, "\n"),
            case Tokens of
                [] ->
                    read_item(File, L);
				
                ["#" | _] ->
                    read_item(File, L);
				
				[Value] ->
                    %%% FIXME: maybe we should use an ets table instead ?
					List = lists:append(L, [Value]),
					read_item(File, List)
			end
    end.
