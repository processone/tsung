%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2002 IDEALX
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

-module(ts_client_rcv).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-behaviour(gen_server).

-include("ts_profile.hrl").

%% External exports
-export([start/1, stop/1, wait_ack/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% reconnection: new socket
start(Opts) ->
	gen_server:start_link(?MODULE, [Opts], []).

stop(Pid) ->
	gen_server:cast(Pid, {stop}).

wait_ack({Pid, Ack, When, EndPage, Socket, Protocol, Host}) ->
	gen_server:cast(Pid, {wait_ack, Ack, When, EndPage, Socket, Protocol, Host}).


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
init([{CType, PPid, Socket, Protocol, ServerName, Timeout, Ack, Monitor}]) ->
	{ok, #state_rcv{socket = Socket, timeout= Timeout, ack = Ack,
					ppid= PPid, clienttype = CType, protocol= Protocol,
					session = CType:new_session(), host=ServerName,
					monitor = Monitor }}.

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
	?LOGF("Unknown call ! ~p~n",[Request], ?ERR),
	Reply = ok,
	{reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%% ack value -> wait
handle_cast({wait_ack, Ack, When, EndPage, Socket, Protocol, Host}, State) ->
	?DebugF("receive wait_ack: ~p , endpage=~p~n",[Ack, EndPage]),
	case State#state_rcv.page_timestamp of 
		0 -> %first request of a page
			NewPageTimestamp = When;
		_ -> %page already started
			NewPageTimestamp = State#state_rcv.page_timestamp
	end,
    case Ack of 
        no_ack -> % don't wait for incoming data, ack now
            NewState=State#state_rcv{ ack_done = true,
                                      ack = Ack,
                                      endpage=EndPage,
                                      ack_timestamp = When,
                                      page_timestamp = NewPageTimestamp},
            PageTimeStamp = update_stats(NewState, false),
            {noreply, NewState#state_rcv{ page_timestamp = PageTimeStamp,
                                          endpage = false}};
        _ ->
            {noreply, State#state_rcv{ack=Ack,
                                      socket=Socket,
                                      ack_done=false,
                                      endpage=EndPage,
                                      ack_timestamp= When,
                                      protocol= Protocol,
                                      host= Host,
                                      page_timestamp = NewPageTimestamp}}
    end;
        
handle_cast({stop}, State) ->
	{stop, normal, State};

handle_cast(Message, State) -> 
	?LOGF("Unknown message !: ~p~n",[Message], ?ERR),
	{noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State) ->
	{NewState, Opts} = handle_data_msg(Data, State),
	ts_utils:inet_setopts(State#state_rcv.protocol, NewState#state_rcv.socket,
						  [{active, once} | Opts], State#state_rcv.ppid),
	{noreply, NewState, State#state_rcv.timeout};

%% ssl case
handle_info({ssl, Socket, Data}, State) ->
	{NewState, Opts}  = handle_data_msg(Data, State),
	ts_utils:inet_setopts(State#state_rcv.protocol, NewState#state_rcv.socket,
						  [{active, once} | Opts], State#state_rcv.ppid),
	{noreply, NewState, State#state_rcv.timeout};

handle_info({tcp_closed, Socket}, State) ->
	?LOG("TCP close: ~n", ?INFO),
    ts_utils:close_socket(State#state_rcv.protocol, Socket),% is it necessary for tcp ?
	ts_client:close(State#state_rcv.ppid),
	{noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
	?LOGF("TCP error: ~p~n",[Reason], ?WARN),
    CountName="tcp_err_"++atom_to_list(Reason),
	ts_client:close({CountName, State#state_rcv.ppid}),
	{noreply, State};

handle_info({ssl_closed, Socket}, State) ->
	?LOG("SSL close: ~n", ?INFO),
    ts_utils:close_socket(State#state_rcv.protocol, Socket),% mandatory (see ssl man page)
	ts_client:close(State#state_rcv.ppid),
	{noreply, State};

handle_info({ssl_error, Socket, Reason}, State) ->
	?LOGF("SSL error: ~p~n",[Reason], ?WARN),
    CountName="ssl_err_"++atom_to_list(Reason),
	ts_client:close({CountName, State#state_rcv.ppid}),
	{noreply, State};

handle_info(timeout, State) ->
	?LOG("timeout ~n", ?WARN),
    %% FIXME: we should close not on all cases ? if we wait for a
    %% global sync for example.
	ts_client:close({timeout, State#state_rcv.ppid}),
    {noreply, State};

handle_info(Data, State) ->%% test if client implement parse ?
	?LOGF("Unknown data received ~p~n",[Data], ?WARN),
	{NewState, Opts} = handle_data_msg(Data, State),
	Socket = NewState#state_rcv.socket,
	ts_utils:inet_setopts(State#state_rcv.protocol, Socket,
						  [{active, once} | Opts], State#state_rcv.ppid),
	{noreply, NewState, State#state_rcv.timeout}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
	ok.
%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(OldVsn, StateData, Extra) ->
    {ok, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: handle_data_msg/2
%% Args: Data (binary), State ('state_rcv' record)
%% Returns: {NewState ('state_rcv' record), Socket options (list)}
%% Purpose: handle data received from a socket
%%----------------------------------------------------------------------
handle_data_msg(Data, State=#state_rcv{ack=no_ack}) ->
	ts_mon:rcvmes({State#state_rcv.monitor, self(), Data}),
    {State, []};

handle_data_msg(Data, State=#state_rcv{ack=parse, clienttype=Type}) ->
	ts_mon:rcvmes({State#state_rcv.monitor, self(), Data}),
	
    {NewState, Opts, Close} = Type:parse(Data, State),
    case NewState#state_rcv.ack_done of
        true ->
            ?DebugF("Response done:~p~n", [NewState#state_rcv.datasize]),
            PageTimeStamp = update_stats(NewState, Close),
            case Close of
                true ->
                    ?Debug("Close connection required by protocol~n"),
                    ts_utils:close_socket(State#state_rcv.protocol,State#state_rcv.socket),
                    {NewState#state_rcv{endpage = false, % reinit in case of
                                        page_timestamp = PageTimeStamp,
                                        socket  = undefined}, Opts};
                false -> 
                    {NewState#state_rcv{endpage = false, % reinit in case of
                                        page_timestamp = PageTimeStamp}, Opts}
            end;
        _ ->
            ?DebugF("Response: continue:~p~n",[NewState#state_rcv.datasize]),
            {NewState, Opts}
    end;

handle_data_msg(Data, State=#state_rcv{ack_done=true}) ->
    %% still same message, increase size
	ts_mon:rcvmes({State#state_rcv.monitor, self(), Data}),
	DataSize = size(Data),
    OldSize = State#state_rcv.datasize,
    {State#state_rcv{datasize = OldSize+DataSize}, []};

handle_data_msg(Data, State=#state_rcv{ack_done=false}) ->
	ts_mon:rcvmes({State#state_rcv.monitor, self(), Data}),
	DataSize = size(Data),
    PageTimeStamp = update_stats(State, false),
    {State#state_rcv{datasize=DataSize,%ack for a new message, init size
                     ack_done = true, endpage=false,
                     page_timestamp= PageTimeStamp},[]}.

%%----------------------------------------------------------------------
%% Func: update_stats/1
%% Args: State
%% Returns: State (state_rcv record)
%% Purpose: update the statistics
%%----------------------------------------------------------------------
update_stats(State, Close) ->
	Now = now(),
	Elapsed = ts_utils:elapsed(State#state_rcv.ack_timestamp, Now),
	Stats= [{ sample, response_time, Elapsed},
			{ sum, size, State#state_rcv.datasize}],
	case State#state_rcv.endpage of
		true -> % end of a page, compute page reponse time 
			PageElapsed = ts_utils:elapsed(State#state_rcv.page_timestamp, Now),
			ts_mon:add(lists:append([Stats,[{sample, page_resptime, PageElapsed}]])),
			doack(State#state_rcv.ack, State#state_rcv.ppid, 
                  State#state_rcv.dyndata, Close),
			0;
		_ ->
			ts_mon:add(Stats),
			doack(State#state_rcv.ack, State#state_rcv.ppid,
                  State#state_rcv.dyndata, Close),
			State#state_rcv.page_timestamp
	end.

%%----------------------------------------------------------------------
%% Func: doack/3 
%% Args: parse|local|no_ack|global, Pid, DynData
%% Purpose: warn the sending process that the request is over
%%----------------------------------------------------------------------
doack(parse, Pid, DynData, Close) ->
	ts_client:next({Pid, DynData, Close});
doack(local, Pid, DynData, Close) ->
	ts_client:next({Pid, DynData, Close});
doack(no_ack, Pid, DynData, Close) ->
	ts_client:next({Pid, DynData, Close});
doack(global, Pid, DynData,Close) ->
	ts_timer:connected(Pid).

