%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 04 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

%%%-------------------------------------------------------------------
%%% File    : ts_config_server.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created :  4 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------

-module(ts_config_server).

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("ts_profile.hrl").
-include("ts_config.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1, read_config/1, read_config/2, get_req/2, get_next_session/1,
         get_client_config/1, newbeams/1, newbeam/2,
         get_monitor_hosts/0, encode_filename/1, decode_filename/1,
         endlaunching/1, status/0, start_file_server/1, get_user_agents/0,
         get_client_config/2, get_user_param/1, get_user_port/1, get_jobs_state/0 ]).

%%debug
-export([choose_client_ip/1, choose_session/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {config,
                logdir,
                curcfg = 0,       % number of configured launchers
                client_static_users = 0, % number of clients that already have their static users
                static_users = 0, % static users not yet given to a client
                ports,            % dict, used if we need to choose the client port
                users=1,          % userid (incremental counter)
                start_date,       %
                hostname,         % controller hostname
                last_beam_id = 0, % last tsung beam id (used to set nodenames)
                ending_beams = 0, % number of beams with no new users to start
                lastips,          % store next ip to choose for each client host
                total_weight      % total weight of client machines
               }).

-define(RPC_TIMEOUT, 30000).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(LogDir) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

status() ->
    gen_server:call({global, ?MODULE}, {status}).

%%--------------------------------------------------------------------
%% Function: newbeam/1
%% Description: start a new beam
%%--------------------------------------------------------------------
newbeams(HostList)->
    gen_server:cast({global, ?MODULE},{newbeams, HostList }).

%%--------------------------------------------------------------------
%% Function: newbeam/2
%% Description: start a new beam with given config. Use by launcher
%%  when maxclient is reached. In this case, the arrival rate is known
%%--------------------------------------------------------------------
newbeam(Host, Args)->
    gen_server:cast({global, ?MODULE},{newbeam, Host, Args }).

%%--------------------------------------------------------------------
%% Function: get_req/2
%% Description: get Nth request from given session Id
%% Returns: #message | {error, Reason}
%%--------------------------------------------------------------------
get_req(Id, Count)->
    gen_server:call({global, ?MODULE},{get_req, Id, Count}).

%%--------------------------------------------------------------------
%% Function: get_user_agents/0
%% Description:
%% Returns: List
%%--------------------------------------------------------------------
get_user_agents()->
    gen_server:call({global, ?MODULE},{get_user_agents}).

%%--------------------------------------------------------------------
%% Function: read_config/1
%% Description: Read Config file
%% Returns: ok | {error, Reason}
%%--------------------------------------------------------------------
read_config(ConfigFile)->
    read_config(ConfigFile,?config_timeout).
read_config(ConfigFile,Timeout)->
    gen_server:call({global,?MODULE},{read_config, ConfigFile},Timeout).

%%--------------------------------------------------------------------
%% Function: get_client_config/1
%% Description: get client machine setup (for the launcher)
%% Returns: {ok, {ArrivalList, StartDate, MaxUsers}} | {error, notfound}
%%--------------------------------------------------------------------
get_client_config(Host)->
    gen_server:call({global,?MODULE},{get_client_config, Host}, ?config_timeout).

get_client_config(Type, Host)->
    gen_server:call({global,?MODULE},{get_client_config, Type, Host}, ?config_timeout).

%%--------------------------------------------------------------------
%% @spec get_monitor_hosts() -> List
%%  List = [Hosts::string()]
%% @doc get list of hosts to monitor  @end
%%--------------------------------------------------------------------
get_monitor_hosts()->
        gen_server:call({global,?MODULE},{get_monitor_hosts}).

%%--------------------------------------------------------------------
%% @spec get_next_session({Host::string(), PhaseId::integer()}) ->
%%  {ok, SessionId::integer(), SessionSize::integer(), IP::tuple(), UserId::integer() }
%% @doc Choose randomly a session
%% @end
%%--------------------------------------------------------------------
get_next_session({Host, PhaseId})->
    gen_server:call({global, ?MODULE},{get_next_session, Host, PhaseId}).

get_user_param(Host)->
    gen_server:call({global, ?MODULE},{get_user_param, Host}).

get_user_port(Ip) ->
    gen_server:call({global, ?MODULE},{get_user_port, Ip}).

endlaunching(Node) ->
    gen_server:cast({global, ?MODULE},{end_launching, Node}).


get_jobs_state() ->
    gen_server:call({global, ?MODULE},{get_jobs_state}).


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
    process_flag(trap_exit,true),
    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
    ?LOGF("Config server started, logdir is ~p~n ",[LogDir],?NOTICE),
    {ok, #state{logdir=LogDir, hostname=list_to_atom(MyHostName)}}.

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
handle_call({read_config, ConfigFile}, _From, State=#state{logdir=LogDir}) ->
    case catch ts_config:read(ConfigFile, LogDir) of
        {ok, Config=#config{curid=LastReqId,sessions=[LastSess| Sessions]}} ->
            ts_utils:init_seed(Config#config.seed),
            ts_user_server:init_seed(Config#config.seed),
            application:set_env(tsung_controller, clients, Config#config.clients),
            application:set_env(tsung_controller, dump, Config#config.dump),
            application:set_env(tsung_controller, stats_backend, Config#config.stats_backend),
            application:set_env(tsung_controller, debug_level, Config#config.loglevel),
            SumWeights = fun(X, Sum) -> X#client.weight + Sum end,
            Sum = lists:foldl(SumWeights, 0, Config#config.clients),
            %% we only know now the size of last session from the file: add it
            %% in the table
            print_info(),
            NewLast=LastSess#session{size = LastReqId, type=Config#config.main_sess_type},
            %% start the file server (if defined) using a separate process (it can be long)
            spawn(?MODULE, start_file_server, [Config]),
            ConfigTmp = loop_load(sort_static(Config#config{sessions=[NewLast]++Sessions})),
            %% Compute per phase popularities
            NewConfig = compute_popularities(ConfigTmp),
            ts_job_notify:listen(NewConfig#config.job_notify_port),
            case check_config(NewConfig) of
                ok ->
                    {reply, ok, State#state{config=NewConfig, static_users=NewConfig#config.static_users,total_weight = Sum}};
                {error, Reason} ->
                    ?LOGF("Error while checking config: ~p~n",[Reason],?EMERG),
                    {reply, {error, Reason}, State}
            end;
        {error, {{case_clause, {error, enoent}},
                  [{xmerl_scan, fetch_DTD, 2,_}|_]}} ->
            ?LOG("Error while parsing XML: DTD not found !~n",?EMERG),
            {reply, {error, dtd_not_found}, State};
        {error, Reason} ->
            ?LOGF("Error while parsing XML config file: ~p~n",[Reason],?EMERG),
            {reply, {error, Reason}, State};
        {'EXIT', Reason} ->
            ?LOGF("Error while parsing XML config file: ~p~n",[Reason],?EMERG),
            {reply, {error, Reason}, State}
    end;

%% get Nth request from given session Id
handle_call({get_req, Id, N}, _From, State) ->
    Config = State#state.config,
    Tab    = Config#config.session_tab,
    ?DebugF("look for ~p th request in session ~p for ~p~n",[N,Id,_From]),
    case ets:lookup(Tab, {Id, N}) of
        [{_, Session}] ->
            ?DebugF("ok, found ~p for ~p~n",[Session,_From]),
            {reply, Session, State};
        Other ->
            {reply, {error, Other}, State}
    end;

handle_call({get_user_agents}, _From, State) ->
    Config = State#state.config,
    case ets:lookup(Config#config.session_tab, {http_user_agent, value}) of
        [] ->
            {reply, empty, State};
        [{_Key, UserAgents}] ->
            {reply, UserAgents, State}
    end;

%% get  user parameters (static user: the session id is already known)
handle_call({get_user_param, HostName}, _From, State=#state{users=UserId}) ->
    Config = State#state.config,
    {value, Client} = lists:keysearch(HostName, #client.host, Config#config.clients),
    {IPParam, Server} = get_user_param(Client,Config),
    ts_mon:newclient({static,?NOW}),
    {reply, {ok, { IPParam, Server, UserId,Config#config.dump,Config#config.seed}}, State#state{users=UserId+1}};

%% get  user port. This is needed by bosh, as there are more than one socket per bosh connection.
handle_call({get_user_port, IP}, _From, State=#state{ports=Ports}) ->
    Config = State#state.config,
    {NewPorts,CPort}   = choose_port(IP, Ports,Config#config.ports_range),
    {reply, {ok, CPort}, State#state{ports = NewPorts}};

%% get a new session id and user parameters for the given node
handle_call({get_next_session, HostName, PhaseId}, _From, State=#state{users=Users}) ->
    Config = State#state.config,
    {value, Client} = lists:keysearch(HostName, #client.host, Config#config.clients),
    ?DebugF("get new session for ~p~n",[_From]),
    case choose_session(Config#config.sessions, Config#config.total_popularity, PhaseId) of
        {ok, Session=#session{id=Id}} ->
            ?LOGF("Session ~p chosen~n",[Id],?INFO),
            ts_mon:newclient({Id,?NOW}),
            {IPParam, Server} = get_user_param(Client,Config),
            {reply, {ok, Session#session{client_ip= IPParam, server=Server,userid=Users,
                                         dump=Config#config.dump, seed=Config#config.seed}},
             State#state{users=Users+1} };
        Other ->
            {reply, {error, Other}, State}
    end;

handle_call({get_client_config, static, Host}, _From, State=#state{config=Config}) ->
%% static users (eg. each user started once at fixed time)
%% we must spread this list of fixed users to each beam
%% If we have N users and M client beams
    Clients=Config#config.clients,
    StaticUsers=State#state.static_users,
    Done=State#state.client_static_users, % number of clients that already have their static users
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    StartDate = set_start_date(State#state.start_date),
    case Done +1 == length(Clients) of
        true -> % last client, give him all pending users
            {reply,{ok,StaticUsers,StartDate},State#state{start_date=StartDate,static_users=[]}};
        false ->
            Weight = Client#client.weight,
            Number=ts_utils:ceiling(length(StaticUsers)*Weight/State#state.total_weight),
            {NewUsers,Tail}=lists:split(Number,StaticUsers),
            {reply,{ok,NewUsers,StartDate},State#state{client_static_users=Done+1,start_date=StartDate,static_users=Tail}}
    end;
%% get randomly generated users
handle_call({get_client_config, Host}, _From, State=#state{curcfg=OldCfg,total_weight=Total_Weight}) ->
    ?DebugF("get_client_config from ~p~n",[Host]),
    Config = State#state.config,
    Clients=Config#config.clients,
    %% set start date if not done yet
    StartDate = set_start_date(State#state.start_date),
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    IsLast = OldCfg + 1 >= length(Clients),% test if this is the last launcher to ask for it's config
    Get = fun(Phase,Args)-> {get_client_cfg(Phase,Args),Args} end,
    {Res, _Acc} = lists:mapfoldl(Get, {Total_Weight,Client,IsLast},Config#config.arrivalphases),
    {NewPhases,ClientParams} = lists:unzip(Res),
    Reply = {ok,{ClientParams,StartDate,Client#client.maxusers}},
    NewConfig=Config#config{arrivalphases=NewPhases},
    {reply,Reply,State#state{config=NewConfig,start_date=StartDate, curcfg = OldCfg +1}};

%%
handle_call({get_monitor_hosts}, _From, State) ->
    Config = State#state.config,
    {reply, Config#config.monitor_hosts, State};

% get status: send the number of actives nodes, number of phases
handle_call({status}, _From, State) ->
    Config = State#state.config,
    Reply = {ok, length(Config#config.clients), State#state.ending_beams, length(Config#config.arrivalphases) },
    {reply, Reply, State};

handle_call({get_jobs_state}, _From, State) when State#state.config == undefined ->
    {reply, not_configured, State};
handle_call({get_jobs_state}, {Pid,_Tag}, State) ->
    Config = State#state.config,
    Reply = case Config#config.job_notify_port of
                {Ets,Port} ->
                    ets:give_away(Ets,Pid,Port),
                    {Ets,Port};
                Else -> Else
            end,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    ?LOGF("Unknown call ~p !~n",[Request],?ERR),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% start the launcher on the current beam
handle_cast({newbeams, HostList}, State=#state{logdir   = LogDir,
                                               hostname = LocalHost,
                                               config   = Config}) ->
    LocalVM  = Config#config.use_controller_vm,
    GetLocal = fun(Host)-> is_vm_local(Host,LocalHost,LocalVM) end,
    {LocalBeams, RemoteBeams} = lists:partition(GetLocal,HostList),
    case local_launcher(LocalBeams, LogDir, Config) of
        {error, _Reason} ->
            ts_mon:abort(),
            {stop, normal,State};
        Id0 ->
            Seed = Config#config.seed,
            Args = set_remote_args(LogDir, Config#config.ports_range),
            {BeamsIds, LastId} = lists:mapfoldl(fun(A,Acc) -> {{A, Acc}, Acc+1} end, Id0, RemoteBeams),
            Fun = fun({Host,Id}) -> remote_launcher(Host, Id, Args) end,
            %% start beams in parallel, at most 10 in parallel
            %% (because sshd MaxSessions = 10, in case we have to
            %% start more than 10 beams on a single host)
            RemoteNodes = ts_utils:pmap(Fun, BeamsIds,9),
            check_remotes_ok(RemoteNodes),
            ?LOG("All remote beams started, syncing ~n",?NOTICE),
            global:sync(),
            ?LOG("Syncing done, start remote tsung application ~n", ?DEB),
            {Resl, BadNodes} = rpc:multicall(RemoteNodes,tsung,start,[],?RPC_TIMEOUT),
            ?LOGF("RPC result: ~p ~p ~n",[Resl,BadNodes],?DEB),
            case BadNodes of
                [] ->
                    StartLaunchers = fun(Node) ->
                                             ts_launcher_static:launch({Node,[]}),
                                             ts_launcher:launch({Node, [], Seed})
                                     end,
                    case Config#config.ports_range of
                        undefined ->
                            ?LOG("Undefined ports_range config ~n",?NOTICE),
                            ok;
                        _ ->
                            ?LOG("Start client port server on remote nodes ~n",?NOTICE),
                            %% first, get a single erlang node per host, and start the cport gen_server on this node
                            UNodes = get_one_node_per_host(RemoteNodes),
                            SetParams = fun(Node) ->
                                                {ok, MyHostName} =ts_utils:node_to_hostname(Node),
                                                {Node, "cport-" ++ MyHostName}
                                        end,
                            CPorts = lists:map(SetParams, UNodes),
                            ?LOGF("Will run start_cport with arg:~p ~n",[CPorts],?DEB),
                            lists:foreach(fun ts_sup:start_cport/1 ,CPorts)
                    end,
                    lists:foreach(StartLaunchers, RemoteNodes),
                    set_max_duration(Config#config.duration),

                    {noreply, State#state{last_beam_id = LastId}};
                Bad ->
                    ?LOGF("Can't start tsung application on all remote clients, abort ~p~n",[Bad],?ERR),
                    ts_mon:abort(),
                    {stop,normal,State}
            end
    end;


%% use_controller_vm and max number of concurrent users reached , big trouble !
handle_cast({newbeam, Host, _}, State=#state{ hostname=LocalHost,config=Config})
  when Config#config.use_controller_vm and ( ( LocalHost == Host ) or ( Host == 'localhost' )) ->
    Msg ="Maximum number of concurrent users in a single VM reached and 'use_controller_vm' is true, can't start new beam !!! Check 'maxusers' value in <client> configuration.~n",
    ?LOG(Msg, ?EMERG),
    erlang:display(Msg),
    {noreply, State};

%% start a launcher on a new beam with slave module
handle_cast({newbeam, Host, Arrivals}, State=#state{last_beam_id = NodeId, config=Config, logdir = LogDir}) ->
    Args = set_remote_args(LogDir,Config#config.ports_range),
    Seed = Config#config.seed,
    Node = remote_launcher(Host, NodeId, Args),
    case rpc:call(Node,tsung,start,[],?RPC_TIMEOUT) of
        {badrpc, Reason} ->
            ?LOGF("Fail to start tsung on beam ~p, reason: ~p",[Node,Reason], ?ERR),
            slave:stop(Node),
            {noreply, State};
        _ ->
            ts_launcher_static:stop(Node), % no need for static launcher in this case (already have one)
            ts_launcher:launch({Node, Arrivals, Seed}),
            {noreply, State#state{last_beam_id = NodeId+1}}
    end;

handle_cast({end_launching, _Node}, State=#state{ending_beams=Beams}) ->
    {noreply, State#state{ending_beams = Beams+1}};

handle_cast(Msg, State) ->
    ?LOGF("Unknown cast ~p ! ~n",[Msg],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout, _Ref, end_tsung}, State) ->
    ts_mon:abort(),
    ?LOG("Tsung test max duration reached, exits ! ~n",?EMERG),
    {stop, normal, State};
handle_info({'EXIT', _Pid, {slave_failure,timeout}}, State) ->
    ts_mon:abort(),
    ?LOG("Abort ! ~n",?EMERG),
    {stop, normal, State};
handle_info({'EXIT', Pid, normal}, State) ->
    ?LOGF("spawned process termination (~p) ~n",[Pid],?INFO),
    {noreply, State};
handle_info({'ETS-TRANSFER',Tab,_FromPid,GiftData}, State=#state{config=Config}) ->
    {noreply, State#state{config=Config#config{job_notify_port={Tab,GiftData}}}};
handle_info(Info, State) ->
    ?LOGF("Unknown info ~p ! ~n",[Info],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec is_vm_local(Host::atom(),Localhost::atom(),UseController::boolean()) -> boolean()
is_vm_local(Host,Host,true)     -> true;
is_vm_local('localhost',_,true) -> true;
is_vm_local(_,_,_)              -> false.

set_start_date(undefined)->
     ts_utils:add_time(?NOW, ?config(warm_time));
set_start_date(Date) -> Date.

get_user_param(Client,Config)->
    {ok,IP} = choose_client_ip(Client),
    {ok, Server} = choose_server(Config#config.servers, Config#config.total_server_weights),
    CPort = choose_port(IP, Config#config.ports_range),
    { {IP, CPort}, Server}.

%%----------------------------------------------------------------------
%% Func: choose_client_ip/1
%% Args: #client, Dict
%% Purpose: choose an IP for a client
%% Returns: {ok, IP, NewDict} IP=IP address
%%----------------------------------------------------------------------
choose_client_ip(#client{ip = IPList, host=Host}) ->
    choose_rr(IPList, Host, {0,0,0,0}).

%%----------------------------------------------------------------------
%% Func: choose_server/1
%% Args: List
%% Purpose: choose a server for a new client
%% Returns: {ok, #server}
%%----------------------------------------------------------------------
choose_server([Server], _TotalWeight) ->
    {ok, Server};
choose_server(Servers, Total) ->
    choose_server(Servers, random:uniform() * Total, 0).

choose_server([S=#server{weight=P} | _],Rand,Cur) when Rand =< P+Cur->
    {ok, S};
choose_server([#server{weight=P} | SList], Rand, Cur) ->
    choose_server(SList, Rand, Cur+P).

%%----------------------------------------------------------------------
%% Func: choose_rr/3
%% Args: List, Key, Default
%% Purpose: choose an value in list in a round robin way. Use last
%%          value stored in the process dictionnary
%           Return Default if list is empty
%% Returns: {ok, Val}
%%----------------------------------------------------------------------
choose_rr([],_, Def) -> % no val, return default
    {ok, Def};
choose_rr([Val],_,_) -> % only one value
    {ok, Val};
choose_rr(List, Key, _) ->
    I = case get({rr,Key}) of
          undefined -> 1 ; % first use of this key, init index to 1
          Val when is_integer(Val) ->
            (Val rem length(List))+1 % round robin
    end,
    put({rr, Key},I),
    {ok, lists:nth(I, List)}.

%%----------------------------------------------------------------------
%% Func: choose_session/2
%% Args: List of #session
%% Purpose: choose an session randomly
%% Returns: #session
%%----------------------------------------------------------------------
choose_session([Session], _Total, _PhaseId) -> %% only one Session
    {ok, Session};
choose_session(Sessions,Total,PhaseId) when is_number(Total)->
    choose_session(Sessions, random:uniform() * Total, 0, PhaseId);
choose_session(Sessions,Total,PhaseId) when is_list(Total) ->
    choose_session(Sessions, random:uniform() * lists:nth(PhaseId, Total), 0, PhaseId).

choose_session([S=#session{popularity=P} | _],Rand,Cur,_PhaseId) when is_number(P) andalso Rand =< P+Cur->
    {ok, S};
choose_session([#session{popularity=P} | SList], Rand, Cur, PhaseId) when is_number(P)->
    choose_session(SList, Rand, Cur+P, PhaseId);
choose_session([S=#session{popularity=PopList} | SList],Rand,Cur,PhaseId) ->
    P = lists:nth(PhaseId,PopList),
    if
        Rand =< P+Cur ->
            {ok, S};
        true ->
            choose_session(SList, Rand, Cur+P, PhaseId)
    end.


%%----------------------------------------------------------------------
%% @spec get_client_cfg(ArrivalPhase::record(arrivalphase),
%%                      Acc::{Total_weight::integer(),Client::record(client),IsLast::binary()}) ->
%%                      {{UpdatedPhase::record(arrivalphase),{Intensity::number(),NUsers::integer(),Duration::integer()}}, Acc}
%% @doc set parameters for given host client and phase.
%% @end
get_client_cfg(Arrival=#arrivalphase{duration = Duration,
                                     intensity= PhaseIntensity,
                                     curnumber= CurNumber,
                                     maxnumber= MaxNumber },
               {TotalWeight,Client,IsLast} ) ->
    Weight = Client#client.weight,
    ClientIntensity = PhaseIntensity * Weight / TotalWeight,
    NUsers = round(case MaxNumber of
                       infinity -> %% only use the duration to set the number of users
                           Duration * ClientIntensity;
                       _ ->
                           TmpMax = case {IsLast,CurNumber == MaxNumber} of
                                        {true,_} ->
                                            MaxNumber-CurNumber;
                                        {false,true} ->
                                            0;
                                        {false,false} ->
                                            lists:max([1,trunc(MaxNumber * Weight / TotalWeight)])
                                    end,
                           lists:min([TmpMax, Duration*ClientIntensity])
                   end),
    ?LOGF("New arrival phase ~p for client ~p (last ? ~p): will start ~p users~n",
          [Arrival#arrivalphase.phase,Client#client.host, IsLast,NUsers],?NOTICE),
    {Arrival#arrivalphase{curnumber=CurNumber+NUsers}, {ClientIntensity, NUsers, Duration}}.

%%----------------------------------------------------------------------
%% Func: encode_filename/1
%% Purpose: kludge: the command line erl doesn't like special characters
%%   in strings when setting up environnement variables for application,
%%   so we encode these characters !
%%----------------------------------------------------------------------
encode_filename(String) when is_list(String)->
    Transform=[{"\\.","_46"},{"\/","_47"},{"\-","_45"}, {"\:","_58"}, {",","_44"}],
    lists:foldl(fun replace_str/2, "ts_encoded" ++ String, Transform);
encode_filename(Term) -> Term.


%%----------------------------------------------------------------------
%% Func: decode_filename/1
%%----------------------------------------------------------------------
decode_filename("ts_encoded" ++ String)->
    Transform=[{"_46","."},{"_47","\/"},{"_45","\-"}, {"_58","\:"}, {"_44",","}],
    lists:foldl(fun replace_str/2, String, Transform).

replace_str({A,B},X) ->
    re:replace(X,A,B,[{return,list},global]).

%%----------------------------------------------------------------------
%% Func: print_info/0 Print system info
%%----------------------------------------------------------------------
print_info() ->
    VSN = case lists:keysearch(tsung_controller,1,application:loaded_applications()) of
             {value, {_,_ ,V}} -> V;
              _ -> "unknown"
          end,
    ?LOGF("SYSINFO:Tsung version: ~s~n",[VSN],?WARN),
    ?LOGF("SYSINFO:Erlang version: ~s~n",[erlang:system_info(system_version)],?WARN),
    ?LOGF("SYSINFO:System architecture ~s~n",[erlang:system_info(system_architecture)],?WARN),
    ?LOGF("SYSINFO:Current path: ~s~n",[code:which(tsung)],?WARN).

%%----------------------------------------------------------------------
%% Func: start_file_server/1
%%----------------------------------------------------------------------
start_file_server(#config{file_server=[]}) ->
    ?LOG("No File server defined, skip~n",?DEB);
start_file_server(Config=#config{file_server=Filenames}) ->
    ?LOG("Starting File server~n",?INFO),
    FileSrv  = {ts_file_server, {ts_file_server, start, []}, transient, 2000,
                worker, [ts_msg_server]},
    supervisor:start_child(ts_controller_sup, FileSrv),
    ts_file_server:read(Filenames),
    ?LOG("Starting user servers if needed~n",?INFO),
    setup_user_servers(Config#config.vhost_file,Config#config.user_server_maxuid).


%%----------------------------------------------------------------------
%% Func: setup_user_servers/2
%%----------------------------------------------------------------------
setup_user_servers(_,none) ->
    ?LOG("Don't start any user server, as user_server_maxuid not defined~n",?DEB),
    ok;
setup_user_servers(none,Val) when is_integer(Val) ->
    ts_user_server:reset(Val);
setup_user_servers(FileId,Val) when is_atom(FileId), is_integer(Val) ->
    ?LOGF("Starting user servers with params ~p ~p~n",[FileId,Val],?DEB),
    {ok,Domains} = ts_file_server:get_all_lines(FileId),
    ?LOGF("Domains:~p~n",[Domains],?DEB),
    lists:foreach(fun(Domain) ->
                    {ok,_} = ts_user_server_sup:start_user_server(list_to_atom("us_" ++binary_to_list(Domain)))
                  end, Domains),
    ts_user_server:reset_all(Val).




%%----------------------------------------------------------------------
%% Func: check_config/1
%% Returns: ok | {error, ErrorList}
%%----------------------------------------------------------------------
check_config(Config=#config{use_weights=UseWeights})->
    case lists:dropwhile(fun(Pop) -> check_popularity(UseWeights,Pop) == ok end, Config#config.total_popularity) of
        [] ->
            ts_config_http:check_user_agent_sum(Config#config.session_tab);
        [BadPop|_] ->
            {error, {bad_sum, BadPop, ?SESSION_POP_ERROR_MSG}}
    end.

check_popularity(false, Val) when abs(100-Val) < 0.05 -> ok;
check_popularity(false,_Val) -> {error, bad_sum };
check_popularity(true, _Val) -> ok.

load_app(Name) when is_atom(Name) ->
    FName = atom_to_list(Name) ++ ".app",
    case code:where_is_file(FName) of
    non_existing ->
        {error, {file:format_error(error_enoent), FName}};
    FullName ->
        case file:consult(FullName) of
        {ok, [Application]} ->
            {ok, Application};
        {error, Reason} ->
            {error, {file:format_error(Reason), FName}}
        end
    end.

%%----------------------------------------------------------------------
%% Func: loop_load/1
%% Args: #config
%% Returns: #config
%% Purpose: duplicate phases 'load_loop' times.
%%----------------------------------------------------------------------
loop_load(Config=#config{load_loop=Loop,arrivalphases=Arrival}) when is_integer(Loop) ->
    Sorted=lists:keysort(#arrivalphase.phase, Arrival),
    {SortedWithId,_} = lists:mapfoldl(fun(Phase, Id) -> {Phase#arrivalphase{id=Id}, Id+1} end, 1, Sorted),
    loop_load(Config#config{arrivalphases=SortedWithId}, ts_utils:keymax(#arrivalphase.phase, Arrival), SortedWithId ).

%% We have a list of n phases: duplicate the list and increase by the
%% max to get a new unique id for all phases. Here we don't care about
%% the order, so we start with the last iteration (Loop* Max)
loop_load(Config=#config{load_loop=0},_,Current) ->
    Sorted=lists:keysort(#arrivalphase.phase, Current),
    ?LOGF("sorted phases: ~p ~n", [Sorted], ?DEB),
    Config#config{arrivalphases=Sorted};
loop_load(Config=#config{load_loop=Loop, arrivalphases=Arrival},Max,Current) ->
    Fun= fun(Phase) -> Phase+Max*Loop end,
    NewArrival = lists:keymap(Fun,#arrivalphase.phase,Arrival),
    loop_load(Config#config{load_loop=Loop-1},Max,lists:append(Current, NewArrival)).

%% @doc sort static users by start time
sort_static(Config=#config{static_users=S})->
    ?LOGF("sort static users: ~p ~n", [S], ?DEB),
    ES = expand_static(S,Config#config.sessions),
    SortedL= lists:keysort(1,ES),
    Config#config{static_users=static_name_to_session(Config#config.sessions,SortedL)}.

%% expand static users (if it contains wildcards)
expand_static(StaticUsers, Sessions) ->
    Names = lists:map(fun(#session{name=A}) -> A end ,Sessions),
    expand_static(StaticUsers, Names, []).

expand_static([], _Names, Static) -> Static;
expand_static([{Delay, Name} | Static],SessionsNames, Acc) ->
    Names  = ts_utils:wildcard(Name, SessionsNames),
    NewStatic = lists:map(fun(N) -> {Delay, N} end, Names),
    expand_static(Static, SessionsNames, Acc ++ NewStatic).


%%
%% @doc start a remote beam
%%
start_slave(Host, Name, Args) when is_atom(Host), is_atom(Name)->
    case slave:start(Host, Name, Args) of
        {ok, Node} ->
            ?LOGF("Remote beam started on node ~p ~n", [Node], ?NOTICE),
            Res = net_adm:ping(Node),
            ?LOGF("ping ~p ~p~n", [Node,Res], ?INFO),
            Node;
        {error, Reason} ->
            ?LOGF("Can't start newbeam on host ~p (reason: ~p) ! Aborting!~n",[Host, Reason],?EMERG),
            {error, {slave_failure, Reason}}
    end.
choose_port(_,_, undefined) ->
    {[],0};
choose_port(Client,undefined, Range) ->
    choose_port(Client,dict:new(), Range);
choose_port(ClientIp,Ports, {Min, Max}) ->
    case dict:find(ClientIp,Ports) of
        {ok, Val} when Val =< Max ->
            NewPorts=dict:update_counter(ClientIp,1,Ports),
            {NewPorts,Val};
        _ -> % Max Reached or new entry
            NewPorts=dict:store(ClientIp,Min+1,Ports),
            {NewPorts,Min}
    end.

choose_port(_,undefined) -> 0;
choose_port(_, _Range)   -> -1.

%% @spec static_name_to_session(Sessions::list(), Static::list() ) -> StaticUsers::list()
%% @doc convert session name to session id in static users list @end
static_name_to_session(Sessions, Static) ->
    ?LOGF("Static users with session id ~p~n",[Static],?DEB),
    Search = fun({Delay,Name})->
                     {value, Session} = lists:keysearch(Name, #session.name, Sessions),
                     {Delay, Session}
             end,
    Res=lists:map(Search, Static),
    ?LOGF("Static users with session id ~p~n",[Res],?DEB),
    Res.

%% @spec set_nodename(NodeId::integer()) -> string()
%% @doc set slave node name: check if controller node name has an id,
%%      and put it in the slave name
set_nodename(NodeId) when is_integer(NodeId)->
    CId = case atom_to_list(node()) of
              "tsung_controller@"++_ ->
                  "";
              "tsung_controller"++Tail ->
                  [Id|_] = string:tokens(Tail,"@"),
                  Id++"_"
          end,
    list_to_atom("tsung"++ CId++ integer_to_list(NodeId)).

%% @spec set_max_duration(integer()) -> ok
%% @doc start a timer for the maximum duration of the load test. The
%% maximum duration is 49 days
set_max_duration(0) -> ok; % nothing to do
set_max_duration(Duration) when Duration =< 4294967 ->
    ?LOGF("Set max duration of test: ~p s ~n",[Duration],?NOTICE),
    erlang:start_timer((Duration+?config(warm_time))*1000, self(), end_tsung ).




local_launcher([],_,_) ->
    0;
local_launcher([Host],LogDir,Config) ->
    ?LOGF("Start a launcher on the controller beam ~p~n", [Host], ?NOTICE),
    LogDirEnc = encode_filename(LogDir),
    %% set the application spec (read the app file and update some env. var.)
    {ok, {_,_,AppSpec}} = load_app(tsung),
    {value, {env, OldEnv}} = lists:keysearch(env, 1, AppSpec),
    NewEnv = [ {debug_level,?config(debug_level)}, {log_file,LogDirEnc}],
    RepKeyFun = fun(Tuple, List) ->  lists:keyreplace(element(1, Tuple), 1, List, Tuple) end,
    Env = lists:foldl(RepKeyFun, OldEnv, NewEnv),
    NewAppSpec = lists:keyreplace(env, 1, AppSpec, {env, Env}),

    ok = application:load({application, tsung, NewAppSpec}),
    case application:start(tsung) of
        ok ->
            ?LOG("Application started, activate launcher, ~n", ?INFO),
            application:set_env(tsung, debug_level, Config#config.loglevel),
            case Config#config.ports_range of
                {Min, Max} ->
                    application:set_env(tsung, cport_min, Min),
                    application:set_env(tsung, cport_max, Max);
                undefined ->
                    ""
            end,
            ts_launcher_static:launch({node(), Host, []}),
            ts_launcher:launch({node(), Host, [], Config#config.seed}),
            1 ;
        {error, Reason} ->
            ?LOGF("Can't start launcher application (reason: ~p) ! Aborting!~n",[Reason],?EMERG),
            {error, Reason}
    end.

remote_launcher(Host, NodeId, Args) when is_list(Host)->
    remote_launcher(list_to_atom(Host), NodeId, Args);
remote_launcher(Host, NodeId, Args) when is_list(NodeId)->
    remote_launcher(Host, list_to_integer(NodeId), Args);
remote_launcher(Host, NodeId, Args)->
    Name = set_nodename(NodeId),
    ?LOGF("starting newbeam ~p on host ~p with Args ~p~n", [Name, Host, Args], ?INFO),
    start_slave(Host, Name, Args).

check_remotes_ok(Remotes) ->
    lists:foreach(fun({error, Reason}) ->
                          ts_mon:abort(),
                          exit(Reason);
                     (_) ->
                          ok
                  end, Remotes).

set_remote_args(LogDir,PortsRange)->
    {ok, PAList}    = init:get_argument(pa),
    PA = lists:flatmap(fun(A) -> [" -pa "] ++A end,PAList),
    ?DebugF("PA list ~p ~n", [PA]),
    Sys_Args= ts_utils:erl_system_args(),
    LogDirEnc = encode_filename(LogDir),
    Ports = case PortsRange of
                {Min, Max} ->
                    " -tsung cport_min " ++ integer_to_list(Min) ++ " -tsung cport_max " ++ integer_to_list(Max);
                undefined ->
                    ""
            end,
    lists:flatten([ Sys_Args,
                    PA,
                    " +K true ",
                    " -tsung debug_level ", integer_to_list(?config(debug_level)),
                    " -tsung log_file ", LogDirEnc, Ports
                  ]).


%% @spec get_one_node_per_host(RemoteNodes::list()) -> Nodes::list()
%% @doc From a list if erlang nodenames, return a list with only a
%%      single node per host
%% @end

get_one_node_per_host([]) ->
    %%no remote nodes, we are using a controller vm
    [node()];
get_one_node_per_host(RemoteNodes) ->
    get_one_node_per_host(RemoteNodes,dict:new()) .

get_one_node_per_host([], Dict) ->
    {_,Nodes} = lists:unzip(dict:to_list(Dict)),
    Nodes;
get_one_node_per_host([Node | Nodes], Dict) ->
    Host = ts_utils:node_to_hostname(Node),
    case dict:is_key(Host, Dict) of
        true ->
            get_one_node_per_host(Nodes,Dict);
        false ->
            NewDict = dict:store(Host, Node, Dict),
            get_one_node_per_host(Nodes,NewDict)
    end.


%% compute popularities of sessions for all phases
compute_popularities(Config=#config{arrivalphases=Phases, sessions=Sessions}) ->

    %% popularities can contains wildcards, need to expand them
    Names = lists:map(fun(#session{name=A}) -> A end ,Sessions),
    Expand = fun( Phase = #arrivalphase{popularities= Pops} ) ->
                     NewPops = lists:foldl(fun({Name,Popularity},Acc) ->
                                                   Expanded = ts_utils:wildcard(Name, Names),
                                                   Acc ++ lists:map(fun(X) -> {X, Popularity} end, Expanded)
                                           end, [], Pops),
                     Phase#arrivalphase{popularities=NewPops}
             end,
    NewPhases = lists:map(Expand,Phases),
    ?LOGF("Compute popularities per phases ~p",[NewPhases],?DEB),

    F = fun(Session=#session{popularity=Pop, name=Name}) ->
           NewPop = set_pop(Name, Pop, NewPhases),
           Session#session{popularity=NewPop}
        end,
    NewSessions = lists:map(F, Sessions),
    ?LOGF("Old sessions:~p",[Sessions],?DEB),
    ?LOGF("New sessions:~p",[NewSessions],?DEB),
    Config#config{sessions=NewSessions, arrivalphases = NewPhases, total_popularity=update_total_pop(Config#config.use_weights, NewPhases, NewSessions)}.

update_total_pop(UseWeight,Phases, Sessions) ->
    update_total_pop(UseWeight, length(Phases), Sessions, []).

update_total_pop(_UseWeight,0, _, Total) ->
    ?LOGF("New Total popularities:~w",[Total],?DEB),
    Total;
update_total_pop(UseWeight,N, Sessions, Total)   ->
    Sum = fun(#session{popularity=P},Acc) when is_number(P) ->
                  Acc+P ;
             (#session{popularity=L},Acc) ->
                  Acc+lists:nth(N,L)
          end,
    PhaseTotal = lists:foldl(Sum, 0, Sessions),
    update_total_pop(UseWeight, N-1, Sessions, [PhaseTotal |Total]).

%% set popularity of session 'Name' per phase (needed when <session_setup> is used)
set_pop(_Name,Popularity,[]) ->
    Popularity;
set_pop(Name,Popularity,Phases) ->
    set_pop(Name,Popularity,Phases,[]).

set_pop(_Name,_Popularity,[], Acc) ->
    %% optimization: if all values are equal, return a single value and not a list
    Min=lists:min(Acc),
    case lists:max(Acc) of
        Min -> Min; % min = max
        _   -> lists:reverse(Acc)
    end;
set_pop(Name,Popularity,[#arrivalphase{popularities=Pop}|Tail], Acc) ->
    New = case lists:keysearch(Name,1,Pop) of
              false ->
                  Popularity;
              {value, {_, Val}} ->
                  Val
          end,
    set_pop(Name,Popularity,Tail, [New|Acc]).

