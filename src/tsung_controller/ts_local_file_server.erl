%%%  Copyright (C) 2017 Sebastian Cohnen

%%%-------------------------------------------------------------------
%%% File    : ts_local_file_server.erl
%%% Author  : Sebastian Cohnen <sebastian.cohnen@gmail.com>
%%% Description : Read a local, line-based file
%%%-------------------------------------------------------------------

-module(ts_local_file_server).
-author('sebastian.cohnen@gmail.com').

-behaviour(gen_server).

-export([
    start_local/1,
    distribute_files/2,
    start/1,
    get_random_line/1,
    stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("ts_config.hrl").

-record(file, {
    items,  %% tuple of lines read from file
    size    %% total number of lines
}).

% Timeout in milliseconds for distributing each file to every subset
% of Nodes (see DISTRIBUTION_MAX_CONCURRENCY).
-define(RPC_TIMEOUT, 120000). % 2 minutes

% Number of nodes that will be send files to concurrently.
-define(DISTRIBUTION_MAX_CONCURRENCY, 10).


%%%---------------------------------------------------------------------
%%% Public API
%%%---------------------------------------------------------------------
start_local(FilePaths) when is_list(FilePaths) ->
    lists:foreach(fun({FileId, FilePath}) ->
        {ok, Lines} = read_file(FilePath),
        ok, _Pid = start({FileId, Lines})
    end, FilePaths).

distribute_files(Nodes, FilePaths) when is_list(FilePaths) ->
    lists:foreach(fun({FileId, FilePath}) ->
        {ok, Lines} = read_file(FilePath),
        ok = distribute_file_in_batches(Nodes, {FileId, Lines})
    end, FilePaths).

distribute_file_in_batches(Nodes, File) when length(Nodes) > ?DISTRIBUTION_MAX_CONCURRENCY ->
    {CurrentNodes, RestNodes} = lists:split(?DISTRIBUTION_MAX_CONCURRENCY, Nodes),
    ok = send_file_to_nodes(CurrentNodes, File),
    ok = distribute_file_in_batches(RestNodes, File);

distribute_file_in_batches(Nodes, File) ->
    ok = send_file_to_nodes(Nodes, File).

send_file_to_nodes(Nodes, {FileId, Lines}) ->
    {Responses, BadNodes} = rpc:multicall(Nodes, ?MODULE, start, [{FileId, Lines}], ?RPC_TIMEOUT),
    ?LOGF("Local File Server: RPC Result ~p ~n", [Responses], ?DEB),
    case BadNodes of
        [] ->
            % check that all resported back okay
            true = lists:all(fun({ok, _}) -> true end, Responses),
            ok;
        _ ->
            ?LOGF("Can't distribute data for ~p for local file servers to all nodes ~p~n", [FileId, BadNodes], ?ERR),
            {error, rpc_error}
    end.

start({FileId, Lines}) ->
    gen_server:start({local, get_server_name(FileId)}, ?MODULE, Lines, []).

stop(FileId) ->
    gen_server:call(get_server_name(FileId), stop).

get_random_line(FileId)->
    gen_server:call(get_server_name(FileId), {get_random_line}).



%%%---------------------------------------------------------------------
%%% Callback functions from gen_server
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(Lines) ->
    {ok, Lines}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({get_random_line}, _From, State) ->
    I = random:uniform(State#file.size),
    Reply = {ok, element(I, State#file.items)},
    {reply, Reply, State};

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


%%%---------------------------------------------------------------------
%%% Private
%%%---------------------------------------------------------------------

get_server_name(FileId) ->
    list_to_atom(?MODULE_STRING ++ "_" ++ atom_to_list(FileId)).

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global, trim]),
            {ok, #file{items = list_to_tuple(Lines), size = length(Lines)}};
        {error, Reason} ->
            ?LOGF("Local File Server: Error while opening file ~p :~p~n", [Path, Reason], ?ERR),
            {error, Reason}
    end.
