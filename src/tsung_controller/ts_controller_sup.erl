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
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_controller_sup).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_macros.hrl").

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
    Mon = {ts_mon, {ts_mon, start, [LogDir]}, transient, 2000,
                 worker, [ts_mon]},
    Stats_Mon = {ts_stats_mon, {ts_stats_mon, start, []}, transient, 2000,
                 worker, [ts_stats_mon]},
    Request_Mon = {request, {ts_stats_mon, start, [request]}, transient, 2000,
                 worker, [ts_stats_mon]},
    Page_Mon = {page, {ts_stats_mon, start, [page]}, transient, 2000,
                 worker, [ts_stats_mon]},
    Connect_Mon = {connect, {ts_stats_mon, start, [connect]}, transient, 2000,
                 worker, [ts_stats_mon]},
    Transaction_Mon = {transaction, {ts_stats_mon, start, [transaction]},
                       transient, 2000, worker, [ts_stats_mon]},
    Match_Log = {ts_match_logger, {ts_match_logger, start, [LogDir]}, transient, 2000,
                 worker, [ts_match_logger]},
    ErlangSup   = {ts_erlang_mon_sup, {ts_os_mon_sup, start_link, [erlang]},
                    permanent, 2000, supervisor, [ts_os_mon_sup]},
    MuninSup   = {ts_munin_mon_sup, {ts_os_mon_sup, start_link, [munin]},
                    permanent, 2000, supervisor, [ts_os_mon_sup]},
    SNMPSup   = {ts_snmp_mon_sup, {ts_os_mon_sup, start_link, [snmp]},
                    permanent, 2000, supervisor, [ts_os_mon_sup]},
    Timer = {ts_timer, {ts_timer, start, [?config(nclients)]}, transient, 2000,
               worker, [ts_timer]},
    Msg  = {ts_msg_server, {ts_msg_server, start, []}, transient, 2000,
               worker, [ts_msg_server]},
    UserSup = {ts_user_server_sup,{ts_user_server_sup,start_link,[]},transient,2000,
                 supervisor,[ts_user_server_sup]},
    Notify = {ts_job_notify, {ts_job_notify, start_link, []}, transient, 2000,
               worker, [ts_job_notify]},
    Interaction = {ts_interaction_server, {ts_interaction_server, start, []}, transient, 2000,
                   worker, [ts_interaction_server]},
    start_inets(LogDir),
    {ok,{{one_for_one,?retries,10},
         [Config, Mon, Stats_Mon, Request_Mon, Page_Mon, Connect_Mon, Transaction_Mon,
          Match_Log, Timer, Msg, Notify, Interaction, UserSup, ErlangSup, MuninSup,SNMPSup]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

start_inets(LogDir) ->
    case application:get_env(tsung_controller,web_gui) of
        {ok, true} ->
            inets:start(),
            Path = filename:join(filename:dirname(code:which(tsung_controller)),"../../../../../share/tsung/templates/style"),
            {ok,Styles} = file:list_dir(Path),
            DestDir = filename:join(LogDir,"style"),
            file:make_dir(DestDir),
            lists:foreach(fun(CSS) ->
                                  DestName = filename:join(DestDir,CSS),
                                  file:copy(filename:join(Path,CSS),DestName)
                          end,Styles),

            Redirect= << "<meta http-equiv=\"refresh\" content=\"0; url=/es/ts_web:status\">\n" >>,
            file:write_file(filename:join(LogDir,"index.html"), Redirect),
            Inets = inets:start(httpd, [{port, 8091},
                                        {modules,[mod_esi,
                                                  mod_dir,
                                                  mod_alias,
                                                  mod_get,
                                                  mod_head,
                                                  mod_log,
                                                  mod_disk_log]},
                                        {erl_script_alias, {"/es", [ts_web, ts_api]}},
                                        {error_log, "inets_error.log"},
                                        %% {transfer_log, "inets_access.log"},
                                        {directory_index, ["index.html"]},
                                        {mime_types,[ {"html","text/html"},
                                                      {"css","text/css"},
                                                      {"png","image/png"},
                                                      {"xml","text/xml"},
                                                      {"json","application/json"},
                                                      {"js","application/x-javascript"}]},
                                        {server_name,"tsung_controller"}, {server_root,LogDir},
                                        {document_root,LogDir}]),
            ?LOGF("Starting inets on port 8091: ~p",[Inets],?INFO);
        _ ->
            ?LOG("Web gui disabled, skip inets",?INFO)
    end.
