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
%%%  Created : 9 Aug 2010 by Nicolas Niclausse <nicolas@niclux.org>

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

%%%-------------------------------------------------------------------
%%% @author Nicolas Niclausse <nicolas@niclux.org>
%%% @copyright (C) 2010, Nicolas Niclausse
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2010 by Nicolas Niclausse <>
%%%-------------------------------------------------------------------
-module(ts_ip_scan).

-behaviour(gen_server).

-include("ts_macros.hrl").

%% API
-export([start_link/0, get_ip/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ips}).

%%%===================================================================
%%% API
%%%===================================================================
get_ip(Interface) ->
    gen_server:call(?MODULE, {get_ip, Interface}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ?LOG("Starting ~n",?INFO),
    {ok, #state{}}.

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
handle_call({get_ip, Interface}, _From, State=#state{ips=undefined}) ->
    [Val|Rest] = get_intf_aliases(Interface),
    {reply, Val, State#state{ips=Rest ++ [Val]}};
handle_call({get_ip, _}, _From, State=#state{ips=[Val|Rest]}) ->
    {reply, Val, State#state{ips=Rest ++ [Val]}}.

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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

%%
get_intf_aliases(Interface) ->
    case file:read_file_info("/sbin/ip") of
        {ok,_} ->
            Res=os:cmd("LC_ALL=C /sbin/ip -o -f inet addr show dev "++Interface),
            get_ip_aliases(string:tokens(Res,"\n"), []);
        {error,Reason} ->
            ?LOGF("ip command not found (~p), using ifconfig instead~n",[Reason],?NOTICE),
            Res=os:cmd("LC_ALL=C /sbin/ifconfig "),
            get_intf_aliases(string:tokens(Res,"\n"), Interface,[],[])
    end.


get_ip_aliases([], Res) ->
    Res;
get_ip_aliases([Line|Tail], Res) ->
    [_,_,_,Net|_] =string:tokens(Line," "),
    [TmpIP|_] =string:tokens(Net,"/"),
    ?LOGF("found IP: ~p~n",[TmpIP],?DEB),
    {ok, IP } = inet:getaddr(TmpIP,inet),
    get_ip_aliases(Tail,  [IP|Res]).


get_intf_aliases([], _, _, Res) ->
    Res;
get_intf_aliases(["          inet addr:"++Line|Tail], Interface, Interface, Res) ->
    [TmpIP|_] =string:tokens(Line," "),
    ?LOGF("found IP: ~p~n",[TmpIP],?DEB),
    {ok, IP } = inet:getaddr(TmpIP,inet),
    get_intf_aliases(Tail, Interface, Interface, lists:append([IP],Res));
get_intf_aliases(["          "++_Line|Tail], Interface, Current, Res) ->
    get_intf_aliases(Tail, Interface, Current, Res);
get_intf_aliases([" "|Tail], Interface, Old, Res) ->
    get_intf_aliases(Tail, Interface, Old, Res);
get_intf_aliases([Line|Tail], Interface, Old, Res) ->
    ?LOGF("scan line : ~p~n",[Line],?DEB),
    %% ?DebugF("scan line : ~p~n",[Line]),
    case string:str(Line,Interface) of
        1 ->
            [Current|_] =string:tokens(Line," "),
            ?LOGF("found interface (old is ~p): ~p~n",[Old,Current],?DEB),
            case string:str(Current, Old++":") of
                1 -> % subinterface, don't change current
                    get_intf_aliases(Tail, Interface, Old, Res);
                _ ->
                    get_intf_aliases(Tail, Interface, Current, Res)
            end;
        _ ->
            get_intf_aliases(Tail, Interface, "", Res)
    end.
