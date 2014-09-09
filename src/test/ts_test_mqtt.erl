%%%  This code was developped by Zhihui Jiao(jzhihui521@gmail.com).
%%%
%%%  Copyright (C) 2013 Zhihui Jiao
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

-module(ts_test_mqtt).
-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-compile(export_all).

-include("ts_profile.hrl").
-include("mqtt.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

encode_connect_test() ->
    ClientId = "tsung-test-id",
    PublishOptions = mqtt_frame:set_publish_options([{qos, 0},
                                                     {retain, false}]),
    Will = #will{topic = "will_topic", message = "will_message",
                 publish_options = PublishOptions},

    Options = mqtt_frame:set_connect_options([{client_id, ClientId},
                                              {clean_start, true},
                                              {keepalive, 10},
                                              Will]),
    Message = #mqtt{type = ?CONNECT, arg = Options},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<16,53,0,6,77,81,73,115,100,112,3,6,0,10,0,13,116,115,117,110,
                   103,45,116,101,115,116,45,105,100,0,10,119,105,108,108,95,
                   116,111,112,105,99,0,12,119,105,108,108,95,109,101,115,115,
                   97,103,101>>, EncodedData).

decode_connect_test() ->
    Data = <<16,53,0,6,77,81,73,115,100,112,3,6,0,10,0,13,116,115,117,110,
             103,45,116,101,115,116,45,105,100,0,10,119,105,108,108,95,
             116,111,112,105,99,0,12,119,105,108,108,95,109,101,115,115,
             97,103,101>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?CONNECT, Type).

encode_disconnect_test() ->
    Message = #mqtt{type = ?DISCONNECT},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<224,0>>, EncodedData).

decode_disconnect_test() ->
    Data = <<224,0>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?DISCONNECT, Type).

encode_publish_test() ->
    Message = #mqtt{id = 1, type = ?PUBLISH, qos = 0, retain = 0,
                    arg = {"test_topic", "test_message"}},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<48,24,0,10,116,101,115,116,95,116,111,112,105,99,116,101,115,116,95,109,101,115,115,97,103,101>>, EncodedData).

decode_publish_test() ->
    Data = <<48,24,0,10,116,101,115,116,95,116,111,112,105,99,116,101,115,116,95,109,101,115,115,97,103,101>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?PUBLISH, Type).

encode_subscribe_test() ->
    Arg = [#sub{topic = "test_topic", qos = 0}],
    Message = #mqtt{id = 1, type = ?SUBSCRIBE, arg = Arg},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<128,15,0,1,0,10,116,101,115,116,95,116,111,112,105,99,0>>, EncodedData).

decode_subscribe_test() ->
    Data = <<128,15,0,1,0,10,116,101,115,116,95,116,111,112,105,99,0>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?SUBSCRIBE, Type).

encode_unsubscribe_test() ->
    Arg = [#sub{topic = "test_topic"}],
    Message = #mqtt{id = 1, type = ?UNSUBSCRIBE, arg = Arg},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<160,14,0,1,0,10,116,101,115,116,95,116,111,112,105,99>>, EncodedData).

decode_unsubscribe_test() ->
    Data = <<160,14,0,1,0,10,116,101,115,116,95,116,111,112,105,99>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?UNSUBSCRIBE, Type).

encode_puback_test() ->
    Message = #mqtt{type = ?PUBACK, arg = 1},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<64,2,0,1>>, EncodedData).

decode_puback_test() ->
    Data = <<64,2,0,1>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?PUBACK, Type).

encode_ping_test() ->
    Message = #mqtt{type = ?PINGREQ},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<192,0>>, EncodedData).

decode_ping_test() ->
    Data = <<192,0>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?PINGREQ, Type).

encode_pong_test() ->
    Message = #mqtt{type = ?PINGRESP},
    EncodedData = mqtt_frame:encode(Message),
    ?assertEqual(<<208,0>>, EncodedData).

decode_pong_test() ->
    Data = <<208,0>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<>>, Left),
    ?assertEqual(?PINGRESP, Type).

more_fixedheader_only_test() ->
    Data = <<208>>,
    Result = mqtt_frame:decode(Data),
    ?assertEqual(more, Result).

more_test() ->
    Data = <<64,2,0>>,
    Result = mqtt_frame:decode(Data),
    ?assertEqual(more, Result).

left_test() ->
    Data = <<64,2,0,1,2,3>>,
    {#mqtt{type = Type}, Left} = mqtt_frame:decode(Data),
    ?assertEqual(<<2,3>>, Left),
    ?assertEqual(?PUBACK, Type).

myset_env()->
    myset_env(0).
myset_env(N)->
    application:set_env(stdlib, debug_level, N).
