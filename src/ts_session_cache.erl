%%%-------------------------------------------------------------------
%%% File    : ts_session_cache.erl
%%% Author  : Nicolas Niclausse <nniclausse@schultze.ird.idealx.com>
%%% Description : cache sessions request from ts_req_server
%%%
%%% Created :  2 Dec 2003 by Nicolas Niclausse <nniclausse@schultze.ird.idealx.com>
%%%-------------------------------------------------------------------
-module(ts_session_cache).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start/0, get_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
          table, % ets table
          hit  =0.0, % number of hits
          total=0.0  % total number of requests
         }).

-include("ts_profile.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    ?LOG("Starting~n",?DEB),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_req(Id, Count)->
	gen_server:call(?MODULE,{get_req, Id, Count}).

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
	Table = ets:new(sessiontable, [set, private]),
    {ok, #state{table=Table}}.

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
%% get Nth request from given session Id
handle_call({get_req, Id, N}, From, State) ->
	Tab = State#state.table,
    Total = State#state.total+1,
    ?LOGF("look for ~p th request in session ~p for ~p~n",[N,Id,From],?DEB),
	case ets:lookup(Tab, {Id, N}) of 
		[{Key, Session}] -> 
            Hit = State#state.hit+1,
            ?LOGF("ok, found in cache for ~p~n",[From],?DEB),
%            ?LOGF("hitrate is ~.3f~n",[100.0*Hit/Total],?DEB),
			{reply, Session, State#state{hit= Hit, total = Total}};
		[] -> %% no match, ask the config_server
            ?LOGF("not found in cache (~p th request in session ~p for ~p)~n",[N,Id,From],?DEB),
            Reply = ts_config_server:get_req(Id, N),
            %% cache the response FIXME: handle bad response ?
            ets:insert(Tab, {{Id, N}, Reply}), 
			{reply, Reply, State#state{total = Total}};
		Other -> %% 
            ?LOGF("error ! (~p)~n",[Other],?DEB),
			{reply, {error, Other}, State}
	end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ?LOGF("Die ! (~p)~n",[Reason],?ERR),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
