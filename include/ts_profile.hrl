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

-record(ts_request,
        {thinktime, 
         ack,
         param,
         endpage=false,
         host,       % override global server hostname
         port,       % override global server port
         scheme      % override global server type (ssl or gen_tcp) 
        }).

% state of ts_client_rcv gen_server
-record(state_rcv, 
		{socket,	  %  
		 timeout,	  % ?
		 host,	      % hostname (or IP) of remote server
		 protocol,	  % gen_udp, gen_tcp or ssl
		 ack,         % type of ack: no_ack, local, global or parse
		 ack_done=false, % 'true' if the ack was sent, else 'false' (unused if ack=no_ack)
		 ack_timestamp,  % date when the 'request' was sent 
		 page_timestamp=0,  % date when the first 'request' of a page was sent 
		 endpage=false,  % if true, a page is ending 
		 session,    % record of session status; depends on 'clienttype'
		 datasize=0,
		 dyndata=[],
		 ppid,		 % pid of send process
		 clienttype, % module name (jabber, etc.)
		 monitor     % type of monitoring (full, light, none)
		}).

-define(restart_sleep, 2000).
-define(infinity_timeout, 15000).
-define(short_timeout, 1).
-define(config_timeout, 60000).
-define(check_noclient_timeout, 60000).
-define(retries, 4).

-define(CRLF, "\r\n").
-define(CR,13).
-define(LF,10).

%% retry sending message after this timeout (in microsec.)
-define(config(Var), ts_utils:get_val(Var)).

-define(messages_intensity, 1/(ts_utils:get_val(messages_interarrival)*1000)).
-define(clients_intensity, 1/(ts_utils:get_val(interarrival)*1000)).


%% errors messages

-define(LOGF(Msg, Args, Level),
		ts_utils:debug(?MODULE, Msg, Args, Level)).
-define(LOG(Msg, Level),
		ts_utils:debug(?MODULE, Msg, Level)).

%% Debug messages can be completely disabled if DEBUG is not defined
-ifdef(DEBUG).
    -define(DebugF(Msg, Args),
            ts_utils:debug(?MODULE, Msg, Args, ?DEB)).
    -define(Debug(Msg),
            ts_utils:debug(?MODULE, Msg, ?DEB)).
-else.
    -define(DebugF(Msg, Args), ok).
    -define(Debug(Msg), ok).
-endif.

-define(EMERG, 0). % The system is unusable. 
-define(ALERT, 1). % Action should be taken immediately to address the problem.
-define(CRIT, 2).  % A critical condition has occurred. 
-define(ERR, 3).   % An error has occurred. 
-define(WARN, 4).  % A significant event that may require attention has occurred. 
-define(NOTICE, 5).% An event that does not affect system operation has occurred. 
-define(INFO, 6).  % An normal operation has occurred. 
-define(DEB, 7).   % Debugging info
