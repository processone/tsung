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

-module(ts_bosh_ssl).

-export([ connect/4, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

%% This is exactly like ts_bosh, but using ssl instead of plain connections.
%% It is easier (fewer tsung modifications required) to have two separate modules,
%% and delegate from here to the original.
connect(Host, Port, Opts, Timeout) ->
    ts_bosh:connect(Host, Port, Opts, Timeout, ssl).

send(Pid, Data, _Opts) ->
    ts_bosh:send(Pid, Data, _Opts).

close(Pid) ->
    ts_bosh:close(Pid).

set_opts(Pid, _Opts) ->
    ts_bosh:set_opts(Pid, _Opts).

protocol_options(_P) ->
    ts_bosh:protocol_options(_P).

normalize_incomming_data(_Socket, X) ->
    X. %% nothing to do here, ts_bosh uses a special process to handle http requests,
       %% the incoming data is already delivered to ts_client as {gen_ts_transport, ..} instead of gen_tcp | ssl
