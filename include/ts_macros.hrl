%%%
%%%  Copyright 2012 © nicolas niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas@niclux.org>
%%%  Created: 22 août 2012 by Nicolas Niclausse <nicolas@niclux.org>
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

-vc('$Id: ts_macros.hrl,v 0.0 2012/08/22 09:07:50 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-ifndef(new_time_api).
-define(NOW, erlang:now()).
-define(TIMESTAMP, erlang:now()).
-else.
-define(NOW, erlang:monotonic_time()).
-define(TIMESTAMP, erlang:timestamp()).
-endif.

-define(CRLF, "\r\n").
-define(CR,13).
-define(LF,10).

%% retry sending message after this timeout (in microsec.)
-define(config(Var), ts_utils:get_val(Var)).

%% errors messages

-define(LOGF(Msg, Args, Level),
        ts_utils:debug(?MODULE, Msg, Args, Level)).
-define(LOG(Msg, Level),
        ts_utils:debug(?MODULE, Msg, Level)).

%% Debug messages can be completely disabled if DEBUG is not defined
-ifdef(DEBUG).
    -define(TRACE, [{debug, [trace]}]).
    -define(DebugF(Msg, Args),
            ts_utils:debug(?MODULE, Msg, Args, ?DEB)).
    -define(Debug(Msg),
            ts_utils:debug(?MODULE, Msg, ?DEB)).
-else.
    -define(TRACE, []).
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

-define(TIMEOUT_PARALLEL_SPAWN, 60000).
-define(MAX_PHASE_EXCEED_PERCENT, 20).
-define(MAX_PHASE_EXCEED_NUSERS, 10).


-define(restart_sleep, 2000).
-define(infinity_timeout, 15000).
-define(config_timeout, 60000).
-define(check_noclient_timeout, 60000).
-define(fast_check_noclient_timeout, 5000).
-define(retries, 4).


