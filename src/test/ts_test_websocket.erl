%%%	 Author: Zhihui Jiao <jzhihui521@gmail.com>
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

-module(ts_test_websocket).
-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_websocket.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->ok.

handshake_test() ->
    {_, Accept} = websocket:get_handshake("127.0.0.1", "/chat", [], 13, ""),
    Response1 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual(ok, websocket:check_handshake(list_to_binary(Response1), Accept)),

    Response2 = ["HTTP/1.1 201 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual({error, {mismatch, "Status", "101", "201"}},
                 websocket:check_handshake(list_to_binary(Response2), Accept)),

    Response3 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Accept: anything\r\n\r\n"],
    ?assertEqual({error, {mismatch, "Sec-WebSocket-Accept", Accept, "anything"}},
                 websocket:check_handshake(list_to_binary(Response3), Accept)),

    Response4 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Upgrade: anything\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual({error, {mismatch, "Upgrade", "websocket", "anything"}},
                 websocket:check_handshake(list_to_binary(Response4), Accept)),

    Response5 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: anything\r\n",
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual({error, {mismatch, "Connection", "Upgrade", "anything"}},
                 websocket:check_handshake(list_to_binary(Response5), Accept)),

    Response6 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual({error, {miss_headers, "Upgrade"}},
                 websocket:check_handshake(list_to_binary(Response6), Accept)),

    Response7 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n"
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual({error, {miss_headers, "Connection"}},
                 websocket:check_handshake(list_to_binary(Response7), Accept)),

    Response8 = ["HTTP/1.1 101 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n\r\n"],
    ?assertEqual({error, {miss_headers, "Sec-WebSocket-Accept"}},
                 websocket:check_handshake(list_to_binary(Response8), Accept)),

    Response9 = ["HTTP/1.0 101 Switching Protocols\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n\r\n"],
    ?assertEqual({error, {mismatch, "Version", "HTTP/1.1", "http/1.0"}},
                 websocket:check_handshake(list_to_binary(Response9), Accept)).

decode_test() ->
    Data1 = <<16#81,16#05,16#48,16#65,16#6c,16#6c,16#6f>>,
    {Opcode, Payload, Left} = websocket:decode(Data1),
    ?assertEqual(?OP_TEXT, Opcode),
    ?assertEqual(<<"Hello">>, Payload),
    ?assertEqual(<<>>, Left),

    Data2 = <<16#81,16#05,16#48,16#65,16#6c,16#6c>>,
    Result = websocket:decode(Data2),
    ?assertEqual(more, Result).

parse_partial_test() ->
    Data  = <<16#81,16#05,16#48,16#65,16#6c,16#6c >>,
    Data2  = <<16#6f >>,
    State  = #state_rcv{session=#websocket_session{status=connected}},
    {State2,_,_} = ts_websocket:parse(Data, State),
    ?assertEqual(State#state_rcv{ack_done=false,acc= Data, datasize=size(Data)}, State2),
    {State3,_,_} = ts_websocket:parse(Data2, State2),
    ?assertEqual(State#state_rcv{ack_done=true, acc= <<  >>, datasize=size(Data)+size(Data2)}, State3).

myset_env()->
    myset_env(0).
myset_env(N)->
    application:set_env(stdlib, debug_level, N).
