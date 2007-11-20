%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2003 IDEALX
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
%%%  the two.

-module(ts_controller_sup).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(LogDir) ->
    ?LOG("starting supervisor ...~n",?INFO),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LogDir]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
init([LogDir]) ->
    ?LOG("starting",?INFO),
    Config = {ts_config_server, {ts_config_server, start_link,
                                 [LogDir]}, transient, 2000,
              worker, [ts_config_server]},
    Stats_Mon = {ts_mon, {ts_mon, start, [LogDir]}, transient, 2000,
                 worker, [ts_mon]},
    Os_Mon = {ts_os_mon, {ts_os_mon, start, []}, transient, 2000,
               worker, [ts_os_mon]},
    Timer = {ts_timer, {ts_timer, start, [?config(nclients)]}, transient, 2000,
               worker, [ts_timer]},
    Msg  = {ts_msg_server, {ts_msg_server, start, []}, transient, 2000,
               worker, [ts_msg_server]},
    User = {ts_user_server, {ts_user_server, start,
                          [[?config(nclients_deb), ?config(nclients_fin),
                            ?config(nclients)]]},
            transient, 2000, worker, [ts_user_server]},
    {ok,{{one_for_one,?retries,10},
         [Config, Stats_Mon, Timer, Msg, User, Os_Mon]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

