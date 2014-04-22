%%%
%%%  Copyright 2014 (c) Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas@niclux.org>
%%%  Created: 15 avril 2014 by Nicolas Niclausse <nicolas@niclux.org>
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

-module(ts_ssl_session_cache).
-vc('$Id: ts_ssl_session_cache.erl,v 0.0 2014/04/15 07:28:58 nniclaus Exp $ ').
-author('nicolas@niclux.org').

-behaviour(ssl_session_cache_api).

-include("ts_macros.hrl").

-export([init/1, terminate/1, lookup/2, update/3, delete/2, foldl/3,
         select_session/2]).

%%--------------------------------------------------------------------
%% Description: Return table reference. Called by ssl_manager process.
%%--------------------------------------------------------------------
init(_) ->
    ets:new(cache_name(), [protected]).

terminate(Cache) ->
    ets:delete(Cache).

lookup(Cache, Key) ->
    ?DebugF("Lookup key ~p from session cache",[Key]),
    case ets:lookup(Cache, Key) of
        [{Key, Session}] ->
            Session;
        [] ->
            undefined
    end.

update(Cache, Key, Session) ->
    case application:get_env(tsung,ssl_session_cache) of
        {ok, 0} ->
            ?Debug("SSL session cache is disabled, skip");
        _ ->
            ?DebugF("SSL update entry ~p ~p",[Key,Session]),
            ets:insert(Cache, {Key, Session})
    end.
delete(Cache, Key) ->
    ?DebugF("Delete key from session cache ~p",[Key]),
    ets:delete(Cache, Key).

foldl(Fun, Acc0, Cache) ->
    ets:foldl(Fun, Acc0, Cache).

select_session(Cache, PartialKey) ->
    ?DebugF("SSL cache select ~p",[PartialKey]),
    Res= ets:select(Cache,
                    [{{{PartialKey,'$1'}, '$2'},[],['$$']}]),
    Res.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
cache_name() ->
    ts_ssl_session_cache.
