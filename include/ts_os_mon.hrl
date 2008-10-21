%%%  Copyright (C) 2008 Nicolas Niclausse
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


%% two types of monotoring: snmp or using an erlang agent
-record(os_mon, {timer,
                pids,      % dict of remote pids (pid is the key and type is the value
                interval,  % get data every 'interval' msec, default
                           % value is ?INTERVAL
                mon_server,% monitoring server to which every data is
                           % sent to (can be a pid or a registered
                           % process )
                dnscache=[]
               }).

-define(INTERVAL, 10000).
