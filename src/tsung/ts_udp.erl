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
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_udp).

-export([ connect/4, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").

protocol_options(#proto_opts{udp_rcv_size=Rcv, udp_snd_size=Snd}) ->
    [binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd}
    ].

%% -> {ok, Socket}
connect(_Host, _Port, Opts, _Timeout) ->
    gen_udp:open(0, Opts).

%% send/3 -> ok | {error, Reason}
send(Socket, Data, [{host, Host}, {port, Port}])  ->
    gen_udp:send(Socket, Host,Port, Data).

close(none)   -> ok;
close(Socket) ->
    gen_udp:close(Socket).

% set_opts/2 -> socket()
set_opts(none,  _Opts) -> none;
set_opts(Socket, Opts) ->
    inet:setopts(Socket, Opts),
    Socket.


normalize_incomming_data(Socket, {udp, Socket,_IP,_InPortNo, Data}) ->
    ?DebugF("UDP packet received: size=~p ~n",[size(Data)]),
    {gen_ts_transport, Socket, Data};
normalize_incomming_data(_Socket, X) ->
    X. %%Other, non gen_udp packet.


