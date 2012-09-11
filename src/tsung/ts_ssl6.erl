%%%
%%%  Copyright 2012 © Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nicolas.nniclausse@niclux.org>
%%%  Created: 7 sep 2012 by Nicolas Niclausse <nicolas.nniclausse@niclux.org>
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

-module(ts_ssl6).

-export([ connect/3,connect/2, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").


protocol_options(Opts) ->
    [inet6]++ts_ssl:protocol_options(Opts).

%% -> {ok, Socket}
connect(Host, Port, Opts) ->
    ssl:connect(Host, Port, Opts).

connect(Socket, Opts)->
    ssl:connect(Socket,  Opts).

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->
    ssl:send(Socket, Data).

close(none)   -> ok;
close(Socket) ->
    ssl:close(Socket).

% set_opts/2 -> socket()
set_opts(Socket, Opts) ->
    ssl:setopts(Socket, Opts),
    Socket.

normalize_incomming_data(Socket, Data) ->
    ts_ssl:normalize_incomming_data(Socket,Data).


