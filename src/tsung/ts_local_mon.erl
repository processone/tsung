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
%%% File    : ts_local_mon.erl
%%% Author  : Nicolas Niclausse <nniclausse@niclux.org>
%%% Description : local logger for protocol_local option
%%%
%%% Created :  5 May 2014 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------


-module(ts_local_mon).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start/0, dump/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


-record(state, {
          dump_iodev       % ioDev for local dump file
         }).

-define(DUMP_STATS_INTERVAL, 500). % in milliseconds

-include("ts_macros.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    ?LOG("Starting~n",?INFO),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: add/1
%% Description: Add stats data. Will be accumulated sent periodically
%%              to ts_mon
%%--------------------------------------------------------------------
dump({_Type, Who, What})  ->
    gen_server:cast(?MODULE, {dump, Who, ?NOW, What}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    %% erlang:start_timer(?DUMP_STATS_INTERVAL, self(), dump_stats ),
    Id  = integer_to_list(ts_utils:get_node_id()),
    LogFileEnc = ts_config_server:decode_filename(?config(log_file)),
    FileName = filename:join(LogFileEnc, "tsung-"++Id ++ ".dump"),
    LogDir = filename:dirname(FileName),
    ok = ts_utils:make_dir_raw(LogDir),
    case file:open(FileName,[write,raw, delayed_write]) of
        {ok, IODev} ->
            {ok, #state{dump_iodev=IODev}};
        {error, Reason} ->
            ?LOGF("Can't open dump file ~p on node ~p: ~p",[FileName, node(), Reason],?ERR),
            {ok,#state{}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_, State=#state{dump_iodev=undefined}) ->
    {noreply, State};

handle_cast({dump, Who, When, What}, State=#state{dump_iodev=IODev}) ->
    Data = io_lib:format("~w;~w;~s~n",[ts_utils:time2sec_hires(When),Who,What]),
    file:write(IODev,Data),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

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
terminate(_, #state{dump_iodev=undefined}) ->
    ok;
terminate(_Reason, #state{dump_iodev=IODev}) ->
    file:close(IODev).

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
