%%%
%%%  Copyright 2011 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@inria.fr>
%%%  Created: 04 mai 2011 by Nicolas Niclausse <nicolas.niclausse@inria.fr>
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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.
%%%
%%% @doc
%%%
%%% @end

-module(ts_job_notify).
-vc('$Id: ts_notify.erl,v 0.0 2011/05/04 11:18:48 nniclaus Exp $ ').
-author('nicolas.niclausse@inria.fr').


-behaviour(gen_server).

-include("ts_macros.hrl").
-include("ts_job.hrl").

%% API
-export([start_link/0]).

-export([listen/1, monitor/1, demonitor/1, wait_jobs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port,           % listen port
                acceptsock,     % The socket we are accept()ing at
                acceptloop_pid, % The PID of the companion process that blocks
                jobs}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

listen(Port) ->
    gen_server:cast({global, ?MODULE}, {listen, Port}).

monitor({JobID, OwnerPid, StartTime, QueuedTime, Dump}) ->
    gen_server:cast({global, ?MODULE}, {monitor, {JobID, OwnerPid, StartTime, QueuedTime,Dump}}).

demonitor({JobID}) ->
    gen_server:cast({global, ?MODULE}, {monitor, {JobID}}).

wait_jobs(Pid) ->
    gen_server:cast({global, ?MODULE}, {wait_jobs, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?LOG("Starting~n",?INFO),
    case global:whereis_name(ts_config_server) of
        undefined ->
            {ok, #state{jobs=ets:new(jobs,[{keypos, #job_session.jobid}])}};
        _Pid ->
            ?LOG("Config server is alive !~n",?INFO),
            case ts_config_server:get_jobs_state() of
                {Jobs,Port} ->
                    ?LOG("Got backup of node state~n",?DEB),
                    {noreply,NewState} = handle_cast({listen,Port}, #state{jobs=Jobs,port=Port}),
                    {ok, NewState};
                Else ->
                    ?LOGF("Got this from config server:~p~n",[Else],?DEB),
                    {ok, #state{jobs=ets:new(jobs,[{keypos, #job_session.jobid}])}}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({accepted, _Tag, Sock}, _From, State) ->
    ?LOGF("New socket:~p~n", [Sock],?DEB),
    {reply, continue, State#state{}};

handle_call({accept_error, _Tag, Error}, _From, State) ->
    ?LOGF("accept() failed ~p~n",[Error],?ERR),
    {stop, Error, stop, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({monitor, {JobID, OwnerPid, SubmitTS, QueuedTS,Dump}}, State=#state{jobs=Jobs}) ->
    ?LOGF("monitoring job ~p from pid ~p~n",[JobID,OwnerPid],?DEB),
    ets:insert(Jobs,#job_session{jobid=JobID,owner=OwnerPid, submission_time=SubmitTS, queue_time=QueuedTS,dump=Dump}),
    SubmitTime=ts_utils:elapsed(SubmitTS,QueuedTS),
    ts_mon:add([{sum,job_queued,1},{sample,tr_job_submit,SubmitTime}]),
    {noreply, State};
handle_cast({demonitor, {JobID}}, State=#state{jobs=Jobs}) ->
    ets:delete(Jobs,JobID),
    {noreply, State};
handle_cast({wait_jobs, Pid}, State=#state{jobs=Jobs}) ->
    %% look for all jobs started by this pid
    ?LOGF("look for job of ~p~n",[Pid],?DEB),
    check_jobs(Jobs,Pid),
    {noreply, State};

handle_cast({listen, undefined}, State) ->
    ?LOG("No listen port defined, can't open listening socket (don't worry: it's normal if you don't use job notifications) ~n",?INFO),
    {noreply, State};
handle_cast({listen,Port}, State) ->
    Opts = [{reuseaddr, true}, {active, once}],
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSock} ->
            ?LOGF("Listening on port ~p done, start accepting loop~n",[Port],?INFO),
            {noreply, State#state
                   {acceptsock=ListenSock,
                    port=Port,
                    acceptloop_pid = spawn_link(ts_utils,
                                                accept_loop,
                                                [self(), unused, ListenSock])}};
        {error, Reason} ->
            ?LOGF("Error when trying to listen to socket: ~p~n",[Reason],?ERR),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State=#state{jobs=Jobs}) ->
%% OAR:
%% args are job_id,job_name,TAG,comment
%% TAG can be:
%%   - RUNNING : when the job is launched
%%   - END : when the job is finished normally
%%   - ERROR : when the job is finished abnormally
%%   - INFO : used when oardel is called on the job
%%   - SUSPENDED : when the job is suspended
%%   - RESUMING : when the job is resumed
    ?LOGF("received ~p from socket ~p",[Data,Socket],?DEB),
    case string:tokens(Data," ") of
        [Id, _Name, "RUNNING"|_] ->
            ?LOGF("look for job ~p in table",[Id],?DEB),
            case ets:lookup(Jobs,Id) of
                [] ->
                    ?LOGF("Job owner of ~p is unknown",[Id],?NOTICE);
                [Job] ->
                    Now=?NOW,
                    Queued=ts_utils:elapsed(Job#job_session.queue_time,Now),
                    ts_mon:add([{sample,tr_job_wait,Queued},{sum,job_running,1}, {sum,job_queued,-1}]),
                    ets:update_element(Jobs,Id,{#job_session.start_time,Now})
            end;
        [Id, Name, "END"|_] ->
            case ets:lookup(Jobs,Id) of
                [] ->
                    ?LOGF("Job owner of ~p is unknown",[Id],?NOTICE);
                [Job=#job_session{start_time=undefined}] ->
                    ?LOGF("ERROR: Start time of job ~p is unknown",[Id],?ERR),
                    ts_mon:add([{sum,job_running,-1}, {sum,ok_job ,1}]),
                    ets:delete_object(Jobs,Job),
                    check_jobs(Jobs,Job#job_session.owner);
                [Job]->
                    Now=?NOW,
                    Duration=ts_utils:elapsed(Job#job_session.start_time,Now),
                    ts_mon:add([{sample,tr_job_duration,Duration},{sum,job_running,-1}, {sum,ok_job ,1}]),
                    ts_job:dump(Job#job_session.dump,{none,Job#job_session{end_time=Now,status="ok"},Name,undefined,undefined}),
                    ets:delete_object(Jobs,Job),
                    check_jobs(Jobs,Job#job_session.owner)
            end;
        [Id, Name, "ERROR"|_] ->
            case ets:lookup(Jobs,Id) of
                [] ->
                    ?LOGF("Job owner of ~p is unknown",[Id],?NOTICE);
                [Job=#job_session{start_time=undefined}] ->
                    ?LOGF("ERROR: start time of job ~p is unknown",[Id],?ERR),
                    ts_mon:add([{sum,job_running,-1}, {sum,error_job,1}]),
                    ets:delete_object(Jobs,Job),
                    check_jobs(Jobs,Job#job_session.owner);
                [Job]->
                    Now=?NOW,
                    Duration=ts_utils:elapsed(Job#job_session.start_time,Now),
                    ts_mon:add([{sample,tr_job_duration,Duration},{sum,job_running,-1}, {sum,error_job,1}]),
                    ts_job:dump(Job#job_session.dump,{none,Job#job_session{end_time=Now,status="error"},Name,undefined,undefined}),
                    ets:delete_object(Jobs,Job),
                    check_jobs(Jobs,Job#job_session.owner)
                end;
        [_Id, _Name, "INFO"|_] ->
            ok;
        [_Id, _Name, "SUSPENDED"|_] ->
            ok;
        [_Id, _Name, "RESUMING"|_] ->
            ok
    end,
    inet:setopts(Socket,[{active,once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, State};
handle_info({'ETS-TRANSFER',_Tab,_FromPid,_GiftData}, State=#state{}) ->
    ?LOG("Got ownership on job state table", ?NOTICE),
    {noreply, State};
handle_info(Info, State) ->
    ?LOGF("Unexpected message received: ~p", [Info], ?WARN),
   {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG("Terminating for normal reason", ?WARN),
    ok;
terminate(Reason, State) when is_integer(State#state.port)->
    ?LOGF("Terminating for reason ~p", [Reason], ?WARN),
    Pid=global:whereis_name(ts_config_server),
    ?LOGF("Config server pid is  ~p", [Pid], ?DEB),
    ets:give_away(State#state.jobs,Pid,State#state.port),
    ok;
terminate(Reason, State) ->
    ?LOGF("Terminating for reason ~p ~p", [Reason,State], ?WARN),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_jobs(Jobs,Pid)->
    case ets:match_object(Jobs, #job_session{owner=Pid, _='_'}) of
        [] ->
            ?LOGF("no jobs for pid ~p~n",[Pid],?DEB),
            Pid ! {erlang, ok, nojobs};
        PidJobs->
            ?LOGF("still ~p jobs for pid ~p~n",[length(PidJobs),Pid],?INFO)
    end.

