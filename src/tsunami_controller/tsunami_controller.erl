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

-module(tsunami_controller).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-export([start/2, start_phase/3, stop/1, stop_all/1]).
-behaviour(application).

-include("ts_profile.hrl").

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(Type, _StartArgs) ->
	error_logger:tty(false),
    LogFile = ts_utils:setsubdir(?config(log_file)),
	error_logger:logfile({open, LogFile ++ "-" ++ atom_to_list(node())}),
    case ts_controller_sup:start_link() of
		{ok, Pid} -> 
			{ok, Pid};
		Error ->
			?LOGF("Can't start ! ~p ~n",[Error], ?ERR),
			Error
    end.

start_phase(load_config, StartType, PhaseArgs) ->
    Config = ?config(config_file),
    backup_config(Config, ?config(log_file)), 
    ts_config_server:read_config(Config);
start_phase(start_os_monitoring, StartType, PhaseArgs) ->
    ts_os_mon:activate();
start_phase(start_clients, StartType, PhaseArgs) ->
    ts_mon:start_clients({?config(clients),?config(monitoring)}).
    

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(State) ->
    stop.

%%----------------------------------------------------------------------
%% Func: stop_all/0
%% Returns: any 
%%----------------------------------------------------------------------
stop_all([Host]) ->
    List= net_adm:world_list([Host]),
    global:sync(),
    Pid = global:whereis_name('ts_mon'),
    Controller_Node = node(Pid),
    slave:stop(Controller_Node).

%%----------------------------------------------------------------------
%% Func: backup_config/2
%% Purpose: copy a backup copy of the config file in the log directory
%%   This is useful to have an history of all parameters of a test.
%%----------------------------------------------------------------------
backup_config(Config, Logfile) ->
    RealLogFile = ts_utils:setsubdir(Logfile),
    Path = filename:dirname(RealLogFile),
    Backup = filename:basename(Config),
    {ok, BytesCopied} = file:copy(Config, filename:join(Path,Backup)).
