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

-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-record(message, {thinktime, ack, param, type=static}).

-define(restart_sleep, 2000).
-define(retries, 4).

-define(restart_try, 3).
-define(debug_level, ts_utils:get_val(debug_level)).

-define(messages_intensity, 1/(ts_utils:get_val(messages_interarrival)*1000)).
-define(clients_intensity, 1/(ts_utils:get_val(interarrival)*1000)).

-define(messages_ack, ts_utils:get_val(messages_ack)).
-define(messages_size, ts_utils:get_val(messages_size)).
-define(messages_number, ts_utils:get_val(messages_number)).
-define(messages_last_time, ts_utils:get_val(messages_last_time)).

-define(nclients, ts_utils:get_val(nclients)).
-define(nclients_deb, ts_utils:get_val(nclients_deb)).
-define(nclients_fin, ts_utils:get_val(nclients_fin)).
-define(client_type, ts_utils:get_val(client_type)).
-define(parse_type, ts_utils:get_val(parse_type)).
-define(mes_type, ts_utils:get_val(mes_type)).

-define(snd_size, ts_utils:get_val(snd_size)).
-define(rcv_size, ts_utils:get_val(rcv_size)).
-define(tcp_timeout, ts_utils:get_val(tcp_timeout)).

-define(log_file, ts_utils:get_val(log_file)).
-define(monitoring, ts_utils:get_val(monitoring)).
-define(monitor_timeout, ts_utils:get_val(monitor_timeout)).
-define(clients_timeout, ts_utils:get_val(clients_timeout)).

-define(server_adr, ts_utils:get_val(server_adr)).
-define(server_port, ts_utils:get_val(server_port)).


%% errors messages
-define(DEBUG, TRUE).

-ifdef(DEBUG).
    -define(PRINTDEBUG(Msg, Args, Level),
            ts_utils:debug(?MODULE, Msg, Args, Level)).
    -define(PRINTDEBUG2(Msg, Level),
            ts_utils:debug(?MODULE, Msg, Level)).
-else.
    -define(PRINTDEBUG(Msg, Args, Level), ok).
    -define(PRINTDEBUG2(Msg, Level), ok).
-endif.

-define(EMERG, 0). % The system is unusable. 
-define(ALERT, 1). % Action should be taken immediately to address the problem.
-define(CRIT, 2).  % A critical condition has occurred. 
-define(ERR, 3).   % An error has occurred. 
-define(WARN, 4).  % A significant event that may require attention has occurred. 
-define(NOTICE, 5).% An event that does not affect system operation has occurred. 
-define(INFO, 6).  % An normal operation has occurred. 
-define(DEB, 7).   % Debugging info
