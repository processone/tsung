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
-export([start_link/0, read_config/1, get_req/2, get_next_session/0,
         get_client_config/1, newbeam/1, newbeam/2, get_server_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {config,
                start_date,       % 
                last_beam_id = 0, % last tsunami beam id (used to set nodenames)
                total_weight      % total weight of client machines
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

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
%% Function: read_config/2
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
%% Function: get_next_session/0
%% Description: choose randomly a session
%% Returns: {ok, Session ID, Session Size (integer), IP (tuple)}
%%--------------------------------------------------------------------
get_next_session()->
	gen_server:call({global, ?MODULE},{get_next_session, node()}).

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
    {Msec, Sec, Nsec} = ts_utils:init_seed(),
    random:seed(Msec,Sec,Nsec),
    {ok, #state{}}.

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
            {reply, ok, #state{config=Config, total_weight = Sum}};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

%% get Nth request from given session Id
handle_call({get_req, Id, N}, From, State) ->
    Config = State#state.config,
	Tab    = Config#config.session_tab,
    ?LOGF("look for ~p th request in session ~p for ~p~n",[N,Id,From],?DEB),
	case ets:lookup(Tab, {Id, N}) of 
		[{Key, Session}] -> 
            ?LOGF("ok, found ~p for ~p~n",[Session,From],?DEB),
			{reply, Session, State};
		Other ->
			{reply, {error, Other}, State}
	end;


%% get a new session id and an ip for the given node
handle_call({get_next_session, Node}, From, State) ->
    Config = State#state.config,
	Tab    = Config#config.session_tab,

    {ok, HostName}  = ts_utils:node_to_hostname(Node),
    {value, Client} = lists:keysearch(HostName, #client.host, Config#config.clients),
    
    {ok, IP} = choose_client_ip(Client),
    
    ?LOGF("get new session for ~p~n",[From],?DEB),
    case choose_session(Config#config.sessions) of
        {ok, Session=#session{id=Id}} ->
            ?LOGF("Session ~p choosen~n",[Id],?INFO),
            case ets:lookup(Tab, {Id, size}) of 
                [{Key, Size}] -> 
                    {reply, {ok, {Session, Size, IP}}, State};
                Other ->
                    {reply, {error, Other}, State}
            end;
		Other ->
			{reply, {error, Other}, State}
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
    {ok, [[PathName, PathVal]]} = init:get_argument(boot_var),
    {ok, [[BootController]]}    = init:get_argument(boot),
    Shared = case init:get_argument(shared) of 
                 error     -> "";
                 {ok,[[]]} -> " -shared"
             end,
    {ok, Boot, _} = regexp:sub(BootController,"tsunami_controller","tsunami"),
    Args = lists:append(["-rsh ssh -setcookie ",atom_to_list(erlang:get_cookie()),
        " -boot ", Boot,
        " -boot_var ", PathName, " ",PathVal,
        " -tsunami debug_level ", integer_to_list(?config(debug_level)),
        " -tsunami monitoring ", atom_to_list(?config(monitoring)), Shared, 
        " +Mea r10b" ]),
    {ok, Node} = slave:start_link(Host, Name, Args),
    ?LOGF("started newbeam on node ~p~n", [Node], ?NOTICE),
    Res = net_adm:ping(Node),
    ts_launcher:launch({Node, Arrivals}),
    {noreply, State#state{last_beam_id = NodeId +1}};

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
%% Returns: IPv4 address {A1,A2,A3,A4}
%%----------------------------------------------------------------------
choose_client_ip(Client=#client{ip = [IP]}) -> %% only one IP
    {ok, IP};
choose_client_ip(Client=#client{ip = IPList}) ->
    %% FIXME: optimize (use round robin for ex. ?)
    N = random:uniform(length(IPList)), %% random IP
    {ok, lists:nth(N,IPList)}.

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
get_client_cfg([Arrival=#arrivalphase{duration=Duration, 
                                      intensity=PhaseIntensity, 
                                      maxnumber= MaxNumber } | AList],
               Clients, TotalWeight, Host, Cur) ->
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    Weight = Client#client.weight,
    ClientIntensity = PhaseIntensity * Weight / TotalWeight,
    NUsers = case MaxNumber of 
                 infinity -> %% only use the duration to set the number of users
                     Duration * 1000 * ClientIntensity;
                 Val ->
                     lists:min([MaxNumber, Duration * 1000 * ClientIntensity])
             end,
    ?LOGF("New arrival phase: will start ~p users~n",[NUsers],?NOTICE),
    get_client_cfg(AList, Clients, TotalWeight, Host,
                   [{ClientIntensity, round(NUsers)} | Cur]).

%%----------------------------------------------------------------------
%% Func: get_client_cfg/1
%% Purpose: Check if the sum of session's popularity is 100
%%----------------------------------------------------------------------
check_popularity(Sessions) ->
    Sum = lists:foldl(fun(X, Sum) -> X#session.popularity+Sum end, 0, Sessions),
    Epsilon = 0.01, %% popularity may be a float number. 10-3 precision
    Delta = abs(Sum - 100),
    case Delta < Epsilon of
        true -> ok;
        false -> 
            ?LOGF("*** Total sum of popularity is not 100 (~p) !",[Sum],?ERR)
    end.
    
