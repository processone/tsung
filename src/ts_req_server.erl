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
-export([read_sesslog/1, get_next_session/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {items, %%lists of messages read from a file
				open=0,
				table,  %% ets  table
				current='$end_of_table' %% position in ets table
			   }).

-include("../include/ts_profile.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
read(Filename) ->
	gen_server:call({global, ?MODULE}, {read, Filename}, ?req_server_timeout).

read_sesslog(Filename) ->
	gen_server:call({global, ?MODULE}, {read_sesslog, Filename}, ?req_server_timeout).

start() ->
	?PRINTDEBUG2("Starting~n",?DEB),
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_next_session()->
	gen_server:call({global, ?MODULE},get_next_session).

get_random_req()->
	gen_server:call({global, ?MODULE},get_random_req).

get_all_req()->
	gen_server:call({global, ?MODULE},get_all_req).

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
	Table = ets:new(sessiontable, [ordered_set, private]),
	{ok, #state{table=Table}}.


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

%% get a new session, ie. a list of pages, each page can contain several requests
handle_call(get_next_session, From, State) ->
	Tab = State#state.table,
	case State#state.current of 
		'$end_of_table' ->
			Current = ets:first(Tab);
		_ ->
			Current = State#state.current
	end,
	Next = ets:next(Tab, Current),
	case ets:lookup(Tab, Current) of 
		[{Key, Session}] -> 
			{reply, {ok, Session}, State#state{current=Next}};
		Other ->
			{reply, {error, Other}, State#state{current=Next}}
	end;
			

handle_call(get_random_req, From, State) ->
	I = random:uniform(length(State#state.items)),
	Reply = {ok,  lists:nth(I, State#state.items)},
	{reply, Reply, State};

%% read Httperf's style of sessions
handle_call({read_sesslog, Filename}, From, State) when State#state.open == 0 ->
    {Status, File} = file:open(Filename, read),
    case Status of
        error ->
            {stop, file_not_found, State};
        _ ->
            read_sesslog(File, State#state.table),
            % Close the config file
            file:close(File),
			{reply, ok, State#state{open= 1}}
    end;	
	
handle_call({read_sesslog, Filename}, From, State) ->
	?PRINTDEBUG("~p already opened~n",[Filename],?INFO),
	{reply, ok, State};

handle_call({read, Filename}, From, State) when State#state.open == 0 ->
	?PRINTDEBUG("Opening file ~p~n",[Filename],?INFO),
    {Status, File} = file:open(Filename, read),
    case Status of
        error ->
            {stop, file_not_found, State};
        _ ->
            
            List_items = read_item(File, []),
            % Close the config file
            file:close(File),
			{reply, ok, State#state{items = List_items, open= 1}}
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
	ets:delete(State#state.table),
	ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: read_sesslog/2
%% 
%%----------------------------------------------------------------------
read_sesslog(File, Table)->
	read_sesslog(io:get_line(File, ""), [], [],  File, Table, 1).

%%----------------------------------------------------------------------
%% Func: read_sesslog/6
%% 
%%----------------------------------------------------------------------
read_sesslog(eof, [], [], File, Table, Id) -> %the end
	done;
read_sesslog(eof, Session, Page, File, Table, Id) -> % the end
	ets:insert(Table, {Id, lists:reverse([lists:reverse(Page) | Session  ])});
read_sesslog("#" ++ Tail, Session, Page, File, Table, Id) -> % skip comment
	read_sesslog(io:get_line(File, ""), Session, Page, File, Table, Id);
read_sesslog("\n", [], [], File, Table, Id)-> % newline again, skip
	read_sesslog(io:get_line(File, ""), [], [], File, Table, Id);
read_sesslog("\n", Session, Page, File, Table, Id)-> % end of a session
	ets:insert(Table, {Id, lists:reverse([lists:reverse(Page) | Session ]) }),
	read_sesslog(io:get_line(File, ""), [], [], File, Table, Id+1);
read_sesslog(" " ++ Tail, Session, [], File, Table, Id)-> % No current page, error
	session_formating_error;
read_sesslog("\t" ++ Tail, Session, [], File, Table, Id)-> % No current page, error
	session_formating_error;
read_sesslog(" " ++ Tail, Session, Page, File, Table, Id)-> % new item in a Page
	Object = ts_utils:chop(string:strip(Tail)),
	read_sesslog(io:get_line(File, ""), Session, [Object | Page], File, Table, Id);
read_sesslog("\t" ++ Tail, Session, Page, File, Table, Id)-> % new item in a Page
	Object = ts_utils:chop(string:strip(Tail)),
	read_sesslog(io:get_line(File, ""), Session, [Object | Page], File, Table, Id);
read_sesslog(Line, [], [], File, Table, Id)-> % new Session
	Object = ts_utils:chop(Line),
	read_sesslog(io:get_line(File, ""),  [ ], [Object], File, Table, Id);
read_sesslog(Line, Session, Page, File, Table, Id)-> % new Page ?
	Object = ts_utils:chop(Line),
	read_sesslog(io:get_line(File, ""),  [lists:reverse(Page) | Session ], [Object], File, Table, Id).

%%----------------------------------------------------------------------
%% Treate one line of the file
%%----------------------------------------------------------------------
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


