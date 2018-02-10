%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
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

-module(ts_user_server).
-author('jflecomte@IDEALX.com').

-vc('$Id$ ').

-include("ts_macros.hrl").

%%-compile(export_all).
-export([reset/1,
         init_seed/1,
         get_unique_id/1,
         get_really_unique_id/1,
         get_id/0,
         get_idle/0,
         get_offline/0,
         get_online/1,
         add_to_online/1,
         remove_from_online/1,
         remove_connected/1,
         add_to_connected/1,
         set_offline_fileid/1,
         set_random_fileid/1,
         set_fileid_delimiter/1,
         get_first/0]).

%% for multiple user_server process, one per virtual host
-export([reset/2,
         get_id/1,
         get_idle/1,
         get_offline/1,
         get_online/2,
         add_to_online/2,
         remove_from_online/2,
         remove_connected/2,
         get_first/1,
         reset_all/1]).


-behaviour(gen_server).

%% External exports
-export([start/0,start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          offline,        %ets table
          last_offline,
          connected,      %ets table
          last_connected,
          online,         %ets table
          last_online,
          first_client,   % id (integer)
          random_server_id,  % file_server id for random users
          offline_server_id, % file_server id for initial offline users
          delimiter = << ";" >>,   % delimiter for file_server id username
          userid_max      % max number of ids (starts at 1)
         }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    ?LOGF("Starting default user_server ~n",[],?INFO),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

start(Name) ->
    ?LOGF("Starting user_server with name ~p ~n",[Name],?INFO),
    gen_server:start_link({global, Name}, ?MODULE, [], []).

reset(default,NFin) ->
    reset(NFin);
reset(UserServer,NFin) ->
    gen_server:call(UserServer,{reset,NFin}).

reset(NFin)->
    gen_server:call({global, ?MODULE}, {reset, NFin}).

reset_all(NFin) ->
     lists:foreach(fun(Pid) -> reset(Pid,NFin) end,
                   ts_user_server_sup:all_children()).

get_id(default) ->
    get_id();
get_id(UserServer) ->
    gen_server:call(UserServer, get_id).

get_id()->
    gen_server:call({global, ?MODULE }, get_id).

%% return a unique id. deprecated since tsung_userid dyn var exists
get_unique_id({_, DynVar})->
    case ts_dynvars:lookup(tsung_userid,DynVar) of
        {ok, Val} -> ts_utils:term_to_list(Val);
        false ->
            ?LOG("tsung_userid not found ! Can't create unique id~n", ?ERR),
            "0"
    end.

%% return a really unique id, one that is unique across runs.
get_really_unique_id({Pid, DynVars}) ->
    Sec = ts_utils:now_sec(),
    ?DebugF("Sec=~p",[Sec]),
    [[integer_to_list(Sec),"-",get_unique_id({Pid, DynVars})]].

%% get an idle id (offline), and add it to the connected table
get_idle(default) ->
    get_idle();
get_idle(UserServer) ->
    gen_server:call(UserServer,get_idle).

get_idle()->
    gen_server:call({global, ?MODULE}, get_idle).

%% FIXME: handle vhost
add_to_connected(Id)->
    gen_server:cast({global, ?MODULE}, {add_to_connected, Id}).

get_online(default,Id) ->
    get_online(Id);
get_online(UserServer,Id) when is_list(Id)->
    get_online(UserServer,list_to_integer(Id));
get_online(UserServer,Id) ->
    gen_server:call(UserServer, {get_online, Id}).

get_online(Id) when is_list(Id) ->
    get_online(list_to_integer(Id));
get_online(Id) ->
    gen_server:call({global, ?MODULE}, {get_online, Id}).

%% get an offline id, don't change the connected table.
get_offline(default) ->
    get_offline();
get_offline(UserServer) ->
    gen_server:call(UserServer, get_offline).

get_offline()->
    gen_server:call({global, ?MODULE}, get_offline).


get_first(default)->
    get_first();
get_first(UserServer)->
    gen_server:call(UserServer, get_first).

get_first()->
    gen_server:call({global, ?MODULE}, get_first).

remove_connected(default,ID) ->
    remove_connected(ID);
remove_connected(UserServer,Id) when  is_list(Id) ->
    remove_connected(UserServer,list_to_integer(Id));
remove_connected(UserServer,Id) ->
    gen_server:cast(UserServer, {remove_connected, Id}).

remove_connected(Id) when  is_list(Id) ->
    remove_connected(list_to_integer(Id));
remove_connected(Id) ->
    gen_server:cast({global, ?MODULE}, {remove_connected, Id}).

add_to_online(default,Id) ->
    add_to_online(Id);
add_to_online(UserServer,Id) when  is_list(Id) ->
    add_to_online(UserServer,list_to_integer(Id));
add_to_online(UserServer,Id) ->
    gen_server:cast(UserServer, {add_to_online, Id}).

add_to_online(Id) when  is_list(Id) ->
    add_to_online(list_to_integer(Id));
add_to_online(Id) ->
    gen_server:cast({global, ?MODULE}, {add_to_online, Id}).

remove_from_online(default,Id) ->
    remove_from_online(Id);
remove_from_online(UserServer,Id) when  is_list(Id) ->
    remove_from_online(UserServer,list_to_integer(Id));
remove_from_online(UserServer,Id) ->
    gen_server:cast(UserServer, {remove_from_online, Id}).

remove_from_online(Id) when  is_list(Id) ->
    remove_from_online(list_to_integer(Id));
remove_from_online(Id)  ->
    gen_server:cast({global, ?MODULE}, {remove_from_online, Id}).

%% @spec set_random_fileid(Id::atom()) -> ok
%% @doc Set file_server id for random users
%%      This is useful and usernames and password are set from a CSV file.
%% @end
set_random_fileid(Id) ->
    gen_server:cast({global, ?MODULE}, {set_random_fileid, Id}).

%% @spec set_offline_fileid(Id::atom()) -> ok
%% @doc Set file_server id for initial offline users
%%      This is useful and usernames and password are set from a CSV file.
%% @end
set_offline_fileid(Id) ->
    gen_server:cast({global, ?MODULE}, {set_offline_fileid, Id}).

%% @spec set_fileid_delimiter(D::string()) -> ok
%% @doc Set file_server delimiter for random users @end
set_fileid_delimiter(D) ->
    gen_server:cast({global, ?MODULE}, {set_fileid_delimiter, D}).

stop()->
    lists:foreach(fun(Pid) ->
                    gen_server:call(Pid, stop)
                 end,ts_user_server_sup:all_children()).




init_seed(A) ->
    gen_server:cast({global, ?MODULE}, {init_seed, A}).

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
init(_Args) ->
    ?LOG("ok, started unconfigured~n", ?INFO),
    {ok, #state{}}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%%Get one id in the full list of potential users
handle_call(get_id, _From, State=#state{random_server_id = Id, delimiter=D}) when Id /= undefined->
    case ts_file_server:get_random_line(Id) of
        {ok, Line} ->
            [Val|_] = ts_utils:split(Line,D),
            {reply, {binary_to_list(Val), unused}, State}; %% FIXME: use binaries in jabber_common everywhere and remove the binary_to_list call here.
        _Else ->
            {reply, {error, userid_max_zero }, State}
    end;
handle_call(get_id, _From, State=#state{userid_max = 0}) ->
    % no user defined in the pool, probably we are using usernames from external file (CSV)
    {reply, {error, userid_max_zero }, State};
handle_call(get_id, _From, State) ->
    Key = random:uniform( State#state.userid_max ),
    {reply, Key, State};

%%Get one id in the users whos have to be connected
handle_call(get_idle, _From, State=#state{offline=Offline,connected=Connected}) ->
    case ets_iterator_next(Offline, State#state.last_offline ) of
        {error, empty_ets} ->
            ?LOG("No more free users !~n", ?WARN),
            {reply, {error, no_free_userid}, State};
        {ok, Key} ->
            NextOffline = ets_iterator_del(Offline, Key, State#state.last_offline),
            ets:insert(Connected, {Key,1}),
            case State#state.first_client of
                undefined ->
                    {reply, Key, State#state{first_client=Key,last_connected=Key,
                                             last_offline=NextOffline}};
                _Id ->
                    {reply, Key, State#state{last_connected=Key,
                                             last_offline=NextOffline}}
            end
    end;

%%Get one offline id
handle_call(get_offline, _From, State=#state{offline=Offline,last_offline=Prev}) ->
    case ets_iterator_next(Offline, Prev) of
        {error, _Reason} ->
            {reply, {error, no_offline}, State};
        {ok, {Next,Pwd}} ->
            ?DebugF("Choose (next is user defined) offline user ~p~n",[Next]),
            {reply, {ok, {Next,Pwd}}, State#state{last_offline={Next,Pwd}}};
        {ok, Next} ->
            ?DebugF("Choose offline user ~p~n",[Prev]),
            {reply, {ok, Next}, State#state{last_offline=Next}}
    end;

handle_call(get_first, _From, State) ->
    {reply, State#state.first_client, State};

handle_call({reset, NFin}, _From, State) ->
    Offline = ets:new(offline,[ordered_set, private]),
    Online  = ets:new(online, [set, private]),
    Connected  = ets:new(connected, [set, private]),

    ?LOGF("Reset offline and online lists (maxid=~p)~n",[NFin],?NOTICE),
    fill_offline(NFin, Offline, {State#state.offline_server_id, State#state.delimiter}),
    State2 = State#state{offline        = Offline,
                         first_client   = undefined,
                         last_offline   = undefined,
                         connected      = Connected,
                         last_connected = undefined,
                         last_online    = undefined,
                         online =Online, userid_max=NFin},
    {reply, ok, State2};

%%% Get a online id different from 'Id'
handle_call( {get_online, Id}, _From, State=#state{ online     = Online,
                                                    last_online = Prev}) ->
    ?DebugF("get_online from ~p~n",[Id]),
    case ets_iterator_next(Online, Prev, Id) of
        {error, _Reason} ->
            ?DebugF("No online users (~p,~p), ets table was ~p ~n",[Id, Prev,ets:info(Online)]),
            {reply, {error, no_online}, State};
        {ok, {User,Pwd}} ->
            ?DebugF("Choose user defined online user ~p for ~p ~n",[User, Id]),
            {reply, {ok, {User,Pwd}}, State#state{last_online={User,Pwd}}};
        {ok, Next} ->
            ?DebugF("Choose online user ~p for ~p ~n",[Next, Id]),
            {reply, {ok, Next}, State#state{last_online=Next}}
    end;

handle_call(stop, _From, State)->
    {stop, normal, ok, State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%%Get one id in the full list of potential users
handle_cast({init_seed, Val}, State) ->
    ts_utils:init_seed(Val),
    {noreply, State};

handle_cast({remove_connected, Id}, State=#state{online=Online,offline=Offline,connected=Connected}) ->
    %% session config may not include presence:final, so we need to check/delete from Online to be safe
    {noreply, LastOnline} = ets_delete_online(Online,Id,State),
    ets:delete(Connected,Id),
    ets:insert(Offline, {Id,2}),
    case {State#state.last_offline,ets:first(Offline)}  of
        {undefined, Id} ->
            %% if we don't set last_offline, the next get_idle will
            %% respond with Id again. If possible we prefer to use
            %% another offline user
            {noreply, State#state{last_online=LastOnline, last_offline=Id}};
        _Else ->
            {noreply, State#state{last_online=LastOnline}}
    end;

%% user_defined user case
handle_cast({add_to_connected, Id}, State=#state{connected=Connected, offline=Offline,first_client=First}) ->
    ets:insert(Connected, {Id,1}),
    NextOffline = ets_iterator_del(Offline, Id, State#state.last_offline),
    case First of
        undefined ->
            {noreply, State#state{last_connected=Id, first_client=Id, last_offline=NextOffline}};
        _ ->
            {noreply, State#state{last_connected=Id,last_offline=NextOffline}}
    end;

handle_cast({add_to_online, Id}, State=#state{online=Online, connected=Connected}) ->
    ?DebugF("add_to_online ~p~n",[Id]),
    case ets:member(Connected,Id) of
        true ->
            ets:delete(Connected,Id),
            ets:insert(Online, {Id,1}),
            {noreply, State#state{last_online=Id}};
        false ->
            ?LOGF("add_to_online: warn, id ~p is not connected,do not add to online~n",[Id],?NOTICE),
            {noreply, State}
    end;

handle_cast({remove_from_online, Id}, State=#state{online=Online,connected=Connected}) ->
    ?DebugF("remove_from_online ~p~n",[Id]),
    {noreply, LastOnline} = ets_delete_online(Online,Id,State),
    ets:insert(Connected, {Id,1}),
    {noreply, State#state{last_online=LastOnline}};

handle_cast({set_random_fileid, Id}, State) ->
    ?LOGF("Set file_server id for random users to ~p~n",[Id],?INFO),
    {noreply, State#state{random_server_id=Id}};

handle_cast({set_offline_fileid, Id}, State) ->
    ?LOGF("Set file_server id for offline users to ~p~n",[Id],?INFO),
    {noreply, State#state{offline_server_id=Id}};

handle_cast({set_fileid_delimiter, D}, State) ->
    ?LOGF("Set file_server delimiter ~p~n",[D],?DEB),
    {noreply, State#state{delimiter=D}}.


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

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
fill_offline(0, _, {undefined,_})->
    ?LOG("no offline user defined",?DEB),
    ok;
fill_offline(0, Offline, {FileId, Delimiter})->
    ?LOGF("fill offline from file id ~p",[FileId],?DEB),
    case ts_file_server:get_all_lines(FileId) of
        {ok, Data} ->
            ?DebugF("offline user from csv ~p",[Data]),
            Fun = fun(Line) ->
                          [User,Pwd| _]= ts_utils:split(Line,Delimiter),
                          ets:insert(Offline,{{binary_to_list(User),binary_to_list(Pwd)},1})
                  end,
            lists:foreach(Fun, Data),
            ok;
        Error ->
            ?LOGF("error no offline user from csv~p",[Error],?DEB),
            ok
    end;
fill_offline(N, Tab, Opts) when is_integer(N) ->
    ets:insert(Tab,{N, 0}),
    fill_offline(N-1, Tab, Opts).

%%%----------------------------------------------------------------------
%%% Func: ets_iterator_del/3
%%% Args: Ets, Key, Iterator
%%% Purpose: delete entry Key from Ets, update iterator if needed
%%% Returns: Key:: integer | {string(),string()}|undefined
%%%----------------------------------------------------------------------

%% iterator equal key:it will no longer be valid
ets_iterator_del(Ets, Key, Key) ->
    Last = ets:prev(Ets,Key),
    ets:delete(Ets,Key),
    case Last of
        '$end_of_table' ->
            case ets:first(Ets) of
                '$end_of_table' ->
                    undefined;
                Key ->
                    undefined;
                NewIter ->
                    NewIter
            end;
        NewIter ->
            NewIter
    end;
ets_iterator_del(Ets, Key, Iterator) ->
    ets:delete(Ets,Key),
    Iterator.

%%%----------------------------------------------------------------------
%%% Func: ets_iterator_next/2
%%% Args: Ets, Iterator
%%% Purpose: get next key; no requirements on value
%%% Returns: {ok, NextKey} or {error, empty_ets}
%%%----------------------------------------------------------------------
ets_iterator_next(Ets, Iterator) ->
    ets_iterator_next(Ets, Iterator, undefined).

%%%----------------------------------------------------------------------
%%% Func: ets_iterator_next/3
%%% Args: Ets, Iterator, Key
%%% Purpose: get next key, should be different from 'Key', if possible
%%%----------------------------------------------------------------------
ets_iterator_next(Ets, undefined, Key) ->
    case ets:first(Ets) of
        '$end_of_table' ->
            {error, empty_ets};
        Key ->
            case ets:next(Ets,Key) of
                '$end_of_table' ->
                    %% Key is the only entry of offline table
                    {ok, Key};
                Iter ->
                    {ok, Iter}
            end;
        NewIter ->
            {ok, NewIter}
    end;
ets_iterator_next(Ets, Iterator, Key) ->
    case ets:next(Ets,Iterator) of
        '$end_of_table' ->
            %% start again from the beginnig
            ets_iterator_next(Ets, undefined, Key);
        Key -> % not this one, try again
            ets_iterator_next(Ets, Key, Key);
        Next ->
            {ok, Next}
    end.

%%%----------------------------------------------------------------------
%%% Func: ets_delete_online/3
%%% Purpose: verify user is in Online table, delete, and update last_online if necessary
%%%----------------------------------------------------------------------
ets_delete_online(Online,Id,State) ->
    case ets:lookup(Online,Id) of
        [] ->
            {noreply, State#state.last_online};
        [_|_] ->
            LastOnline = ets_iterator_del(Online,Id,State#state.last_online),
            %% reset the last_online entries if it's equal to Id
            case State#state.last_online of
                Id ->
                    ?LOGF("Reset last id (~p) because its offline ~n",[Id],?INFO),
                    {noreply, LastOnline};
                _ ->
                    {noreply, State#state.last_online}
            end
    end.
