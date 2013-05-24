%%%
%%%  Copyright 2011 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@inria.fr>
%%%  Created: 4 mai 2011 by Nicolas Niclausse <nicolas.niclausse@inria.fr>
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

-module(ts_job).
-author('nicolas.niclausse@inria.fr').

-behaviour(ts_plugin).

-include("ts_macros.hrl").
-include("ts_profile.hrl").
-include("ts_job.hrl").
-include_lib("kernel/include/file.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         dump/2,
         parse/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).


%%====================================================================
%% Data Types
%%====================================================================

%% @type dyndata() = #dyndata{proto=ProtoData::term(),dynvars=list()}.
%% Dynamic data structure
%% @end

%% @type server() = {Host::tuple(),Port::integer(),Protocol::atom()}.
%% Host/Port/Protocol tuple
%% @end

%% @type param() = {dyndata(), server()}.
%% Dynamic data structure
%% @end

%% @type hostdata() = {Host::tuple(),Port::integer()}.
%% Host/Port pair
%% @end

%% @type client_data() = binary() | closed.
%% Data passed to a protocol implementation is either a binary or the
%% atom closed indicating that the server closed the tcp connection.
%% @end

%%====================================================================
%% API
%%====================================================================

parse_config(El,Config) ->
     ts_config_job:parse_config(El, Config).

%% @spec session_defaults() -> {ok, Persistent} | {ok, Persistent, Bidi}
%% Persistent = bool()
%% Bidi = bool()
%% @doc Default parameters for sessions of this protocol. Persistent
%% is true if connections are preserved after the underlying tcp
%% connection closes. Bidi should be true for bidirectional protocols
%% where the protocol module needs to reply to data sent from the
%% server. @end
session_defaults() ->
    {ok, true}. % not relevant for erlang type (?).

%% @spec new_session() -> State::term()
%% @doc Initialises the state for a new protocol session.
%% @end
new_session() ->
    #job_session{}.

%% @spec decode_buffer(Buffer::binary(),Session::record(job)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#job_session{}) ->
    Buffer.

%% @spec add_dynparams(Subst, dyndata(), param(), hostdata()) -> {dyndata(), server()} | dyndata()
%% Subst = term()
%% @doc Updates the dynamic request data structure created by
%% {@link ts_protocol:init_dynparams/0. init_dynparams/0}.
%% @end
add_dynparams(false, {_DynVars,Session}, Param, HostData) ->
    add_dynparams(Session, Param, HostData);
add_dynparams(true,  {DynVars,Session}, Param, HostData) ->
    NewParam = subst(Param, DynVars),
    add_dynparams(Session,NewParam, HostData).

add_dynparams(#job_session{}, Param, _HostData) ->
    Param.

%%----------------------------------------------------------------------
%% @spec subst(record(job), term()) -> record(job)
%% @doc Replace on the fly dynamic element of the request.
%% @end
%%----------------------------------------------------------------------
subst(Job=#job{duration=D,req=Req,walltime=WT,resources=Res,options=Opts,jobid=Id}, DynVars) ->
    Job#job{duration=ts_search:subst(D,DynVars),
            req=ts_search:subst(Req,DynVars),
            resources=ts_search:subst(Res,DynVars),
            walltime=ts_search:subst(WT,DynVars),
            options=ts_search:subst(Opts,DynVars),
            jobid=ts_search:subst(Id,DynVars)}.


dump(protocol,{none,#job_session{jobid=JobId,owner=Owner,submission_time=Sub,queue_time=Q,
                                 start_time=Start,end_time=E,status=Status},Name,_,_,_,Transactions})->
    {R,_}=lists:mapfoldl(fun(A,Acc) ->
                                 {integer_to_list(round(ts_utils:elapsed(Acc,A))),A}
                         end,Sub,[Q,Start,E]),
    Date=integer_to_list(round(ts_utils:time2sec_hires(Sub))),
    Tr=ts_utils:log_transaction(Transactions),
    Data=ts_utils:join(";",[JobId,Name,Tr,Date]++R++[Status]),
    ts_mon:dump({protocol, Owner, Data });
dump(_P,_Args) ->
    ok.

%% @spec parse(Data::client_data(), State) -> {NewState, Opts, Close}
%% State = #state_rcv{}
%% Opts = proplist()
%% Close = bool()
%% @doc
%% Opts is a list of inet:setopts socket options. Don't change the
%% active/passive mode here as tsung will set {active,once} before
%% your options.
%% Setting Close to true will cause tsung to close the connection to
%% the server.
%% @end
parse({os, cmd, _Args, Res},State=#state_rcv{session=S,dump=Dump}) when is_list(Res)->
    ?LOGF("os:cmd result: ~p",[Res],?DEB),
  %% oarsub output:
  %% [ADMISSION RULE] Modify resource description with type constraints
  %% Generate a job key...
  %% OAR_JOB_ID=468822
    Lines = string:tokens(Res,"\n"),
    case lists:last(Lines) of
        "OAR_JOB_ID="++ID ->
            ?LOGF("OK,job id is ~p",[ID],?INFO),
            ts_job_notify:monitor({ID,self(),S#job_session.submission_time, ?NOW,Dump}),
            {State#state_rcv{ack_done=true,datasize=length(Res)}, [], false};
        _ ->
            {State#state_rcv{ack_done=true,datasize=length(Res)}, [], false}
    end;
parse(nojobs,State) ->
    ?LOGF(" no jobs in queue for ~p, stop waiting",[self()],?DEB),
    {State#state_rcv{ack_done=true}, [], false};
parse({Mod, Fun, Args, Res},State) ->
    ?LOGF(" result: ~p",[{Mod, Fun, Args, Res}],?DEB),
    {State#state_rcv{ack_done=false}, [], false}.

%% @spec parse_bidi(Data, State) -> {nodata, NewState} | {Data, NewState}
%% Data = client_data()
%% NewState = term()
%% State = term()
%% @doc Parse a block of data from the server. No reply will be sent
%% if the return value is nodata, otherwise the Data binary will be
%% sent back to the server immediately.
%% @end
parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

%% @spec get_message(record(job),record(state_rcv)) -> {Message::term(),record(state_rcv)}
%% @doc Creates a new message to send to the connected server.
%% @end
get_message(#job{type=oar,req=wait_jobs},#state_rcv{session=Session}) ->
    ts_job_notify:wait_jobs(self()),
    {{erlang, now,[], 0},Session}; % we could use any function call, the result is not used
get_message(Job=#job{duration=D},State) when is_integer(D)->
    get_message(Job#job{duration=integer_to_list(D)},State);
get_message(Job=#job{notify_port=P},State) when is_integer(P)->
    get_message(Job#job{notify_port=integer_to_list(P)},State);
get_message(#job{type=oar,user=U,req=submit, name=N,script=S, resources=R, queue=Q, walltime=W,notify_port=P, notify_script=NS,duration=D,options=Opts},#state_rcv{session=Session}) ->
    Submit = case U of
                 undefined -> "oarsub ";
                 User      -> "sudo -u "++User++" oarsub "
             end,
    Queue = case Q of
                "" -> "";
                _  -> "-q "++ Q
            end,
    Cmd=Submit++Queue++" -l "++R++ ",walltime="++W
        ++" -n " ++N ++" "
        ++ Opts ++ " "
        ++" --notify \"exec:" ++NS++" "++P++"\" "
        ++"\""++S++" "++D++"\"",
    ?LOGF("Will run ~p",[Cmd],?INFO),
    Message = {os, cmd, [Cmd], length(Cmd) },
    {Message, Session#job_session{submission_time=?NOW}}.
