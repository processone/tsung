%%%
%%%  Copyright 2009 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 20 août 2009 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
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

-module(ts_erlang).
-vc('$Id: ts_erlang.erl,v 0.0 2009/08/20 16:31:58 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(gen_ts_transport).

-define(TIMEOUT,36000000). % 1 hour

-include("ts_profile.hrl").

-export([ connect/4, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2,
          client/4]).

client(MasterPid,Server,Port,Opts)->
    receive
        {Module, Fun, Args, _Size} ->
            Res=apply(Module,Fun,Args),
            MasterPid ! {erlang,self(),{Module,Fun,Args,Res}},
            client(MasterPid,Server,Port,Opts)
    after ?TIMEOUT ->
            MasterPid ! timeout
    end.


protocol_options(_Opts) ->
   [].

%% -> {ok, Socket}
connect(Host, Port, Opts, _Timeout) ->
    Pid=spawn_link(ts_erlang,client,[self(),Host,Port,Opts]),
    {ok, Pid}.

%% send/3 -> ok | {error, Reason}
send(Pid, Data, _Opts)  ->
    Pid ! Data,
    ok.

close(_Socket) -> ok.

set_opts(Socket, _Opts) ->
    Socket.



normalize_incomming_data(_Socket, Data={timeout,_,_}) ->
    Data;
normalize_incomming_data(Socket, Data) ->
    {gen_ts_transport, Socket, Data}.
