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
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

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
-export([start/0,
         get_random_line/0,
         get_random_line/1,
         get_next_line/0,
         get_next_line/1,
         get_all_lines/0,
         get_all_lines/1,
         stop/0,
         read/1, read/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(file, {bin,         %% binary representation of file
               size,        %% total number of bytes
               current=0   %% current byte offset
              }).

-record(state, {files}).

-define(DICT, dict).

-include("ts_config.hrl").
-include("xmerl.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: Binary
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------

read(Filenames) ->
    gen_server:call({global, ?MODULE}, {read, Filenames}, ?config(file_server_timeout)).

read(Filenames, Timeout) ->
    gen_server:call({global, ?MODULE}, {read, Filenames}, Timeout).

start() ->
    ?LOG("Starting~n",?DEB),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).



get_random_line({Pid,_DynData}) when is_pid(Pid)->
    %% called within a substitution (eg. file is 'default')
    case get_random_line(default) of
        {ok, Val} -> Val;
        Error     ->  Error
    end;
get_random_line(FileID)->
    gen_server:call({global, ?MODULE}, {get_random_line, FileID}).
get_random_line() ->
    get_random_line(default).

get_next_line({Pid,_DynData}) when is_pid(Pid)->
    %% called within a substitution (eg. file is 'default')
    case get_next_line(default) of
        {ok, Val} -> Val;
        Error     ->  Error
    end;
get_next_line(FileID)->
    gen_server:call({global, ?MODULE}, {get_next_line, FileID}).
get_next_line() ->
    get_next_line(default).

get_all_lines({Pid,_DynData}) when is_pid(Pid)->
    %% called within a substitution (eg. file is 'default')
    case get_all_lines(default) of
        {ok, Val} -> Val;
        Error     ->  Error
    end;
get_all_lines(FileID)->
    gen_server:call({global, ?MODULE}, {get_all_lines, FileID}).
get_all_lines() ->
    get_all_lines(default).

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
    {ok, #state{files=?DICT:new()}}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({get_all_lines, FileID}, _From, State) ->
    FileDesc = ?DICT:fetch(FileID, State#state.files),
    List_items = binary:split(FileDesc#file.bin, <<"\n">> , [global,trim]),
    Reply = {ok, List_items},
    {reply, Reply, State};

handle_call({get_random_line, FileID}, _From, State) ->
    File = ?DICT:fetch(FileID, State#state.files),
    Reply = {ok, get_random_line(File#file.bin, File#file.size)},
    {reply, Reply, State};

handle_call({get_next_line, FileID}, _From, State) ->
    FD = ?DICT:fetch(FileID, State#state.files),
    #file{
        bin=Bin,
        size=Size,
        current=Current
    } = FD,
    case read_line_from_pos(Bin, Size, Current) of
        undefined ->
            {reply, {ok, <<>>}, State};
        Line ->
            FD1 = FD#file{current=Current + byte_size(Line)},
            {
                reply,
                {ok, Line},
                State#state{files=?DICT:store(FileID, FD1, State#state.files)}
            }
    end;

handle_call({read, Filenames}, _From, State) ->
    lists:foldl(fun open_file/2, {reply, ok, State}, Filenames);

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
%% Open a file and return a new state
%%----------------------------------------------------------------------
open_file({ID, Path}, {reply, Result, State}) ->
    case ?DICT:find(ID, State#state.files) of
        {ok, _} ->
            ?LOGF("File with id ~p already opened (path is ~p)~n",[ID, Path], ?WARN),
            {reply, {error, already_open}, State};
        error ->
            ?LOGF("Opening file ~p~n",[Path], ?INFO),

            case file:read_file(Path) of
                {ok, Bin} ->
                    FileDesc = #file{bin=Bin, size=byte_size(Bin)},
                    {reply, Result, State#state{files = ?DICT:store(ID, FileDesc, State#state.files)}};
                {error,Reason} ->
                    ?LOGF("Error while opening file ~p :~p~n",[ Path, Reason], ?ERR),
                    {reply, {error, Path}, State}
            end
    end.

get_random_line(Bin, Size) ->
    I = random:uniform(Size),
    case read_line_from_pos(Bin, Size, I) of
        undefined -> get_random_line(Bin, Size);
        Line -> Line
    end.

read_line_from_pos(Bin, _, 0) ->
    case binary:match(Bin, <<"\n">>) of
        nomatch -> undefined;
        {End, _} ->
            binary:part(Bin, 0, End)
    end;
read_line_from_pos(Bin, Size, Pos) ->
    case binary:match(Bin, <<"\n">>, [{scope, {Pos, Size - Pos}}]) of
        nomatch -> undefined;
        {Start, _} ->
            % Wooo off-by-one errors
            Start1 = Start + 1,
            case binary:match(Bin, <<"\n">>, [{scope, {Start1, Size - Start1}}]) of
                nomatch -> undefined;
                {End, _} ->
                    binary:part(Bin, Start1, End - Start1)
            end
    end.
