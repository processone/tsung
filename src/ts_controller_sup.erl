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

-module(ts_controller_sup).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("ts_profile.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
	?LOG("starting supervisor ...~n",?DEB),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([]) ->
	?LOG("starting",?DEB),
    Config = {ts_config_server, {ts_config_server, start_link, 
                                 []}, transient, 2000,
              worker, [ts_config_server]},
    Stats_Mon = {ts_mon, {ts_mon, start, []}, transient, 2000, 
                 worker, [ts_mon]},
    Os_Mon = {ts_os_mon, {ts_os_mon, start, []}, transient, 2000, 
			   worker, [ts_os_mon]},
    Timer = {ts_timer, {ts_timer, start, [?config(nclients)]}, transient, 2000, 
			   worker, [ts_timer]},
    Request = {ts_req_server, {ts_req_server, start, []}, transient, 2000, 
			   worker, [ts_req_server]},
    Msg = {ts_msg_server, {ts_msg_server, start, []}, transient, 2000, 
			   worker, [ts_msg_server]},
    User = {ts_user_server, {ts_user_server, start, 
						  [[?config(nclients_deb), ?config(nclients_fin),
							?config(nclients)]]}, 
			transient, 2000, worker, [ts_user_server]},
    {ok,{{one_for_one,?retries,10},
		 [Config, Stats_Mon, Timer, Request, Msg, User, Os_Mon]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

