%%%
%%%  Copyright 2010 © ProcessOne
%%%
%%%  Author : Eric Cestari <ecestari@mac.com>
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

-module(ts_tcp).

-export([ connect/4, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").

protocol_options(#proto_opts{tcp_rcv_size = Rcv, tcp_snd_size = Snd,
                             tcp_reuseaddr = Reuseaddr}) ->
    [binary,
     {active, once},
     {reuseaddr, Reuseaddr},
     {recbuf, Rcv},
     {sndbuf, Snd},
     {keepalive, true} %% FIXME: should be an option
    ].
%% -> {ok, Socket}

connect(Host, Port, Opts, ConnectTimeout) ->
    gen_tcp:connect(Host, Port, opts_to_tcp_opts(Opts), ConnectTimeout).

opts_to_tcp_opts(Opts) -> Opts.

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->
    gen_tcp:send(Socket, Data).

close(none)   -> ok;
close(Socket) ->
    gen_tcp:close(Socket).

% set_opts/2 -> socket()
set_opts(none,  _Opts) -> none;
set_opts(Socket, Opts) ->
    inet:setopts(Socket, Opts),
    Socket.


normalize_incomming_data(Socket, {tcp, Socket, Data}) ->
    {gen_ts_transport, Socket, Data};
normalize_incomming_data(Socket, {tcp_closed, Socket}) ->
    {gen_ts_transport, Socket, closed};
normalize_incomming_data(Socket, {tcp_error, Socket, Error}) ->
    {gen_ts_transport, Socket, error, Error};
normalize_incomming_data(_Socket, X) ->
    X. %%Other, non gen_tcp packet.


