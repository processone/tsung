%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%	 Author : Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%%  Created: 04 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
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
%%%-------------------------------------------------------------------
%%% File    : ts_config_server.erl
%%% Author  : Nicolas Niclausse <nniclausse@idealx.com>
%%% Description : 
%%%
%%% Created :  4 Dec 2003 by Nicolas Niclausse <nniclausse@idealx.com>
%%%-------------------------------------------------------------------
-module(ts_config_server).

-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("ts_profile.hrl").
-include("ts_config.hrl").

-include_lib("xmerl/inc/xmerl.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1, read_config/1, get_req/2, get_next_session/1,
         get_client_config/1, newbeam/1, newbeam/2, get_server_config/0,
		 get_monitor_hosts/0, encode_filename/1, decode_filename/1]).

%%debug
-export([choose_client_ip/2, choose_session/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {config,
                logdir,
                start_date,       % 
                last_beam_id = 0, % last tsunami beam id (used to set nodenames)
                lastips,          % store next ip to choose for each client host
                total_weight      % total weight of client machines
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(LogDir) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

%%--------------------------------------------------------------------
%% Function: newbeam/1
%% Description: start a new beam 
%%--------------------------------------------------------------------
newbeam(Host)->
	gen_server:cast({global, ?MODULE},{newbeam, Host, [] }).
%%--------------------------------------------------------------------
%% Function: newbeam/2
%% Description: start a new beam with given config.
%%--------------------------------------------------------------------
newbeam(Host, {Arrivals, MaxUsers})->
	gen_server:cast({global, ?MODULE},{newbeam, Host, {Arrivals, MaxUsers} }).

%%--------------------------------------------------------------------
%% Function: get_req/2
%% Description: get Nth request from given session Id
%% Returns: #message | {error, Reason}
%%--------------------------------------------------------------------
get_req(Id, Count)->
	gen_server:call({global, ?MODULE},{get_req, Id, Count}).

%%--------------------------------------------------------------------
%% Function: read_config/1
%% Description: Read Config file
%% Returns: ok | {error, Reason}
%%--------------------------------------------------------------------
read_config(ConfigFile)->
	gen_server:call({global,?MODULE},{read_config, ConfigFile},?config_timeout).

%%--------------------------------------------------------------------
%% Function: get_client_config/1
%% Description: get client machine setup (for the launcher)
%% Returns: {ok, {ArrivalList, StartDate, MaxUsers}} | {error, notfound}
%%--------------------------------------------------------------------
get_client_config(Host)->
	gen_server:call({global,?MODULE},{get_client_config, Host}).

%%--------------------------------------------------------------------
%% Function: get_server_config/0
%% Returns: {Hostname, Port (integer), Protocol type (ssl|gen_tcp|gen_udp)}
%%--------------------------------------------------------------------
get_server_config()->
	gen_server:call({global,?MODULE},{get_server_config}).

%%--------------------------------------------------------------------
%% Function: get_monitor_hosts/0
%% Returns: [Hosts]
%%--------------------------------------------------------------------
get_monitor_hosts()->
        gen_server:call({global,?MODULE},{get_monitor_hosts}).

%%--------------------------------------------------------------------
%% Function: get_next_session/0
%% Description: choose randomly a session
%% Returns: {ok, Session ID, Session Size (integer), IP (tuple)}
%%--------------------------------------------------------------------
get_next_session(Host)->
	gen_server:call({global, ?MODULE},{get_next_session, Host}).

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
init([LogDir]) ->
	ts_utils:init_seed(),
    ?LOGF("Config server started, logdir is ~p~n ",[LogDir],?NOTICE),
    {ok, #state{logdir=LogDir}}.

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
handle_call({read_config, ConfigFile}, From, State) ->
    case ts_config:read(ConfigFile) of
        {ok, Config=#config{session_tab=Tab,curid=LastReqId,sessions=[LastSess| SList]}} -> 
            check_popularity(Config#config.sessions),
            application:set_env(tsunami_controller, clients, Config#config.clients),
            application:set_env(tsunami_controller, monitoring, Config#config.monitoring),
            application:set_env(tsunami_controller, debug_level, Config#config.loglevel),
            SumWeights = fun(X, Sum) -> X#client.weight + Sum end,
            Sum = lists:foldl(SumWeights, 0, Config#config.clients),
            %% we only know now the size of last session from the file: add it
            %% in the table
            ets:insert(Tab, {{LastSess#session.id, size}, LastReqId}),
            {reply, ok, State#state{config=Config, total_weight = Sum}};
        {error, Reason} -> 
            ?LOGF("Error while parsing XML config file: ~p~n",[Reason],?EMERG),
            {reply, {error, Reason}, State}
    end;

%% get Nth request from given session Id
handle_call({get_req, Id, N}, From, State) ->
    Config = State#state.config,
	Tab    = Config#config.session_tab,
    ?DebugF("look for ~p th request in session ~p for ~p~n",[N,Id,From]),
	case ets:lookup(Tab, {Id, N}) of 
		[{Key, Session}] -> 
            ?DebugF("ok, found ~p for ~p~n",[Session,From]),
			{reply, Session, State};
		Other ->
			{reply, {error, Other}, State}
	end;

%% get a new session id and an ip for the given node
handle_call({get_next_session, HostName}, From, State) ->
    Config = State#state.config,
	Tab    = Config#config.session_tab,

    {value, Client} = lists:keysearch(HostName, #client.host, Config#config.clients),
    
    {ok,IP,NewPos} = choose_client_ip(Client,State#state.lastips),
    
    ?DebugF("get new session for ~p~n",[From]),
	NewState=State#state{lastips=NewPos},
    case choose_session(Config#config.sessions) of
        {ok, Session=#session{id=Id}} ->
            ?LOGF("Session ~p choosen~n",[Id],?INFO),
            case ets:lookup(Tab, {Id, size}) of 
                [{Key, Size}] -> 
                    {reply, {ok, {Session, Size, IP}}, NewState};
                Other ->
                    {reply, {error, Other}, NewState}
            end;
		Other ->
			{reply, {error, Other}, NewState}
    end;

%%
handle_call({get_server_config}, From, State) ->
    Config = State#state.config,
    Server = Config#config.server,
    {reply,{Server#server.host, Server#server.port, Server#server.type}, State};

%% 
handle_call({get_client_config, Host}, From, State) ->
    Config = State#state.config,
    %% set start date if not done yet
    StartDate = case State#state.start_date of 
                    undefined ->
                        ts_utils:add_time(now(), ?config(warm_time));
                    Date -> Date
                end,
    case get_client_cfg(Config#config.arrivalphases,
                        Config#config.clients, State#state.total_weight,
                        Host) of
        {ok,List,Max} ->
            {reply, {ok, {List, StartDate,Max}},
             State#state{start_date = StartDate}};
        _ ->
            {reply, {error, notfound}, State}
    end;

%%
handle_call({get_monitor_hosts}, From, State) ->
    Config = State#state.config,
    {reply, Config#config.monitor_hosts, State};

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
%% start a new beam with slave module 
handle_cast({newbeam, Host, Arrivals}, State=#state{last_beam_id = NodeId}) ->
    Config = State#state.config,
    Name = "tsunami" ++ integer_to_list(NodeId),
    {ok, [[BootController]]}    = init:get_argument(boot),
    ?DebugF("BootController ~p~n", [BootController]), 
    {ok, Boot, _} = regexp:gsub(BootController,"tsunami_controller","tsunami"),
    ?DebugF("Boot ~p~n", [Boot]), 
	Sys_Args= ts_utils:erl_system_args(),
    LogDir = encode_filename(State#state.logdir),
    Args = lists:append([ Sys_Args," -boot ", Boot,
        " -tsunami debug_level ", integer_to_list(?config(debug_level)),
        " -tsunami monitoring ", atom_to_list(?config(monitoring)),
        " -tsunami ssl_ciphers ", Config#config.ssl_ciphers,
        " -tsunami log_file ", LogDir,
        " -tsunami controller ", atom_to_list(node())
        ]),
    ?LOGF("starting newbeam on host ~p with Args ~p~n", [Host, Args], ?DEB), 
    case slave:start_link(Host, Name, Args) of
        {ok, Node} ->
            ?LOGF("started newbeam on node ~p ~n", [Node], ?NOTICE), 
            Res = net_adm:ping(Node),
            ?LOGF("ping ~p ~p~n", [Node,Res], ?NOTICE),
            ts_launcher:launch({Node, Arrivals}),
            {noreply, State#state{last_beam_id = NodeId +1}};
        {error, Reason} ->
            ?LOGF("Can't start newbeam on host ~p ! Aborting!~n",[Host],?EMERG),
            ts_mon:abort(),
            {stop, normal}
    end;

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

%%----------------------------------------------------------------------
%% Func: choose_client_ip/1
%% Args: #client
%% Purpose: choose an IP for a client
%% Returns: {ok, {IP, NewList}} IP=IPv4 address {A1,A2,A3,A4}
%%----------------------------------------------------------------------
choose_client_ip(Client=#client{ip = [IP]},RR) -> %% only one IP
    {ok, IP, RR};
choose_client_ip(Client,undefined) ->
	choose_client_ip(Client, dict:new());
choose_client_ip(Client=#client{ip = IPList, host=Host},RRval) ->
    case dict:find(Host, RRval) of 
		{ok, NextVal} -> 
			NewRR = dict:update_counter(Host,1,RRval),
			I = (NextVal rem length(IPList))+1, %% round robin
			{ok, lists:nth(I, IPList), NewRR};
		error ->
			Dict = dict:store(Host,1,RRval),
			[IP|_] = IPList,
			{ok, IP, Dict}
	end.

%%----------------------------------------------------------------------
%% Func: choose_session/1
%% Args: List of #session
%% Purpose: choose an session randomly
%% Returns: #session
%%----------------------------------------------------------------------
choose_session([Session]) -> %% only one Session
    {ok, Session};
choose_session(Sessions) ->
    choose_session(Sessions, random:uniform(100),0).

choose_session([S=#session{popularity=P} | SList],Rand,Cur) when Rand =< P+Cur->
    {ok, S};
choose_session([S=#session{popularity=P} | SList], Rand, Cur) ->
    choose_session(SList, Rand, Cur+P).


%%----------------------------------------------------------------------
%% Func: get_client_cfg/3
%% Args: list of #arrivalphase, list of #client, String
%% Purpose: set parameters for given host client
%% Returns: {ok, {Intensity = float, Users=integer, StartDate = tuple}} 
%%          | {error, Reason}
%%----------------------------------------------------------------------
get_client_cfg(Arrival, Clients, TotalWeight, Host) ->
    SortedPhases=lists:keysort(#arrivalphase.phase, Arrival),
    get_client_cfg(SortedPhases, Clients, TotalWeight,Host, []).

get_client_cfg([], Clients, TotalWeight, Host, Cur) ->
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    Max = Client#client.maxusers,
    {ok, lists:reverse(Cur), Max};
get_client_cfg([Arrival=#arrivalphase{duration = Duration, 
                                      intensity= PhaseIntensity, 
                                      maxnumber= MaxNumber } | AList],
               Clients, TotalWeight, Host, Cur) ->
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    Weight = Client#client.weight,
    ClientIntensity = PhaseIntensity * Weight / TotalWeight,
    NUsers = case MaxNumber of 
                 infinity -> %% only use the duration to set the number of users
                     Duration * 1000 * ClientIntensity;
                 Val ->
                     lists:min([MaxNumber, Duration*1000*ClientIntensity])
             end,
	%% TODO: store the max number of clients
    ?LOGF("New arrival phase ~p for client ~p: will start ~p users~n",
		  [Arrival#arrivalphase.phase, Host, NUsers],?NOTICE),
    get_client_cfg(AList, Clients, TotalWeight, Host,
                   [{ClientIntensity, round(NUsers)} | Cur]).

%%----------------------------------------------------------------------
%% Func: get_client_cfg/1
%% Purpose: Check if the sum of session's popularity is 100
%%----------------------------------------------------------------------
check_popularity(Sessions) ->
    Sum = lists:foldl(fun(X, Sum) -> X#session.popularity+Sum end, 0, Sessions),
    Epsilon = 0.01, %% popularity may be a float number. 10-2 precision
    Delta = abs(Sum - 100),
    case Delta < Epsilon of
        true -> ok;
        false -> 
            ?LOGF("*** Total sum of popularity is not 100 (~p) !",[Sum],?ERR),
			throw({error,bad_popularity_sum})
    end.

%%----------------------------------------------------------------------
%% Func: encode_filename/1
%% Purpose: kludge: the command line erl doesn't like special characters 
%%   in strings when setting up environnement variables for application,
%%   so we encode these characters ! 
%%----------------------------------------------------------------------
encode_filename(String)->
    {ok, String2,_} = regexp:gsub("ts_encoded" ++ String,"\/","_47"),
    {ok, String3,_} = regexp:gsub(String2,"\-","_45"),
    {ok, String4,_} = regexp:gsub(String3,"\:","_58"),
    String4.

%%----------------------------------------------------------------------
%% Func: decode_filename/1
%%----------------------------------------------------------------------
decode_filename("ts_encoded" ++ String)->
    {ok, String2,_} = regexp:gsub(String,"_47","\/"), 
    {ok, String3,_} = regexp:gsub(String2,"_45","\-"),
    {ok, String4,_} = regexp:gsub(String3,"_58","\:"),
    String4.


    
