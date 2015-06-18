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

-module(ts_mqtt).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_mqtt.hrl").
-include("mqtt.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         parse/2,
         dump/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).
-export([ping_loop/3]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (persistent & bidirectional)
%% Returns: {ok, true|false, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true, true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(jabber)) ->
%%      NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer, #mqtt_session{}) ->
    Buffer.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #mqtt_session{}.

dump(A, B) ->
    ts_plugin:dump(A,B).
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#mqtt_request{type = connect, clean_start = CleanStart,
                          keepalive = KeepAlive, will_topic = WillTopic,
                          will_qos = WillQos, will_msg = WillMsg,
                          will_retain = WillRetain, username = UserName, password = Password},
            #state_rcv{session = MqttSession}) ->
    ClientId = ["tsung-", ts_utils:randombinstr(10)],
    PublishOptions = mqtt_frame:set_publish_options([{qos, WillQos},
                                                     {retain, WillRetain}]),
    Will = #will{topic = WillTopic, message = WillMsg,
                 publish_options = PublishOptions},

    Options = mqtt_frame:set_connect_options([{client_id, ClientId},
                                              {clean_start, CleanStart},
                                              {keepalive, KeepAlive},
                                              {username, UserName},
                                              {password, Password},
                                              Will]),
    Message = #mqtt{type = ?CONNECT, arg = Options},
    {mqtt_frame:encode(Message),
     MqttSession#mqtt_session{wait = ?CONNACK, keepalive = KeepAlive}};
get_message(#mqtt_request{type = disconnect},
            #state_rcv{session = MqttSession}) ->
    PingPid = MqttSession#mqtt_session.ping_pid,
    PingPid ! stop,
    Message = #mqtt{type = ?DISCONNECT},
    ts_mon:add({count, mqtt_disconnected}),
    {mqtt_frame:encode(Message),
     MqttSession#mqtt_session{wait = none, status = disconnect}};
get_message(#mqtt_request{type = publish, topic = Topic, qos = Qos,
                          retained = Retained, payload = Payload},
            #state_rcv{session = MqttSession = #mqtt_session{curr_id = Id}}) ->
    NewMqttSession = case Qos of
        0 -> MqttSession;
        _ -> MqttSession#mqtt_session{curr_id = Id + 1}
    end,

    MsgId = NewMqttSession#mqtt_session.curr_id,
    Message = #mqtt{id = MsgId, type = ?PUBLISH, qos = Qos, retain = Retained,
                    arg = {Topic, Payload}},
    Wait = case Qos of
        1 -> ?PUBACK;
        _ -> none
    end,
    ts_mon:add({count, mqtt_published}),
    {mqtt_frame:encode(Message), NewMqttSession#mqtt_session{wait = Wait}};
get_message(#mqtt_request{type = subscribe, topic = Topic, qos = Qos},
            #state_rcv{session = MqttSession = #mqtt_session{curr_id = Id}}) ->
    NewMqttSession = MqttSession#mqtt_session{curr_id = Id + 1},
    Arg = [#sub{topic = Topic, qos = Qos}],
    MsgId = NewMqttSession#mqtt_session.curr_id,
    Message = #mqtt{id = MsgId, type = ?SUBSCRIBE, arg = Arg},
    {mqtt_frame:encode(Message), NewMqttSession#mqtt_session{wait = ?SUBACK}};
get_message(#mqtt_request{type = unsubscribe, topic = Topic},
            #state_rcv{session = MqttSession = #mqtt_session{curr_id = Id}}) ->
    NewMqttSession = MqttSession#mqtt_session{curr_id = Id + 1},
    Arg = [#sub{topic = Topic}],
    MsgId = NewMqttSession#mqtt_session.curr_id,
    Message = #mqtt{id = MsgId, type = ?UNSUBSCRIBE, arg = Arg},
    {mqtt_frame:encode(Message),NewMqttSession#mqtt_session{wait = ?UNSUBACK}}.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:	Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize=0}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize= 0}) ->
    parse(Data, State#state_rcv{datasize= size(Data)});

%% normal mqtt message
parse(Data, State=#state_rcv{acc = [], session = MqttSession, socket = Socket}) ->
    Wait = MqttSession#mqtt_session.wait,
    AckBuf = MqttSession#mqtt_session.ack_buf,
    case mqtt_frame:decode(Data) of
        {_MqttMsg = #mqtt{type = Wait}, Left} ->
            ?DebugF("receive mqtt_msg: ~p ~p~n",
                    [mqtt_frame:command_for_type(Wait), _MqttMsg]),
            NewLeft = case Wait of
                ?SUBACK -> <<>>;
                _ -> Left
            end,
            case Wait of
                ?CONNACK -> ts_mon:add({count, mqtt_connected});
                ?PUBACK -> ts_mon:add({count, mqtt_server_pubacked});
                ?SUBACK ->
                    case {AckBuf, Left} of
                        {<<>>, <<>>} -> ok;
                        _ -> self() ! {gen_ts_transport, Socket, Left}
                    end;
                _ -> ok
            end,
            NewMqttSession = case Wait of
                ?CONNACK ->
                    Proto = State#state_rcv.protocol,
                    KeepAlive = MqttSession#mqtt_session.keepalive,
                    PingPid = create_ping_proc(Proto, Socket, KeepAlive),
                    MqttSession#mqtt_session{ping_pid = PingPid};
                _ -> MqttSession
            end,
            {State#state_rcv{ack_done = true, acc = NewLeft,
                             session = NewMqttSession}, [], false};
        {_MqttMsg = #mqtt{id = MessageId, type = Type, qos = Qos}, Left} ->
            ?DebugF("receive mqtt_msg, expecting: ~p, actual: ~p ~p~n",
                    [mqtt_frame:command_for_type(Wait),
                     mqtt_frame:command_for_type(Type), _MqttMsg]),
            NewMqttSession = case {Wait, Type, Qos} of
                {?SUBACK, ?PUBLISH, 1} ->
                    Message = #mqtt{type = ?PUBACK, arg = MessageId},
                    EncodedData = mqtt_frame:encode(Message),
                    ts_mon:add({count, mqtt_server_published}),
                    NewAckBuf =  <<AckBuf/binary, EncodedData/binary>>,
                    MqttSession#mqtt_session{ack_buf = NewAckBuf};
                _ -> MqttSession
            end,
            {State#state_rcv{ack_done = false, acc = Left,
                             session = NewMqttSession}, [], false};
        more ->
            ?DebugF("incomplete mqtt frame: ~p~n", [Data]),
            {State#state_rcv{acc = Data}, [], false}
    end;
%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc = Acc, datasize = DataSize}) ->
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary, Data/binary >>,
          State#state_rcv{acc = [], datasize = NewSize}).

parse_bidi(<<>>, State=#state_rcv{acc = [], session = MqttSession}) ->
    AckBuf = MqttSession#mqtt_session.ack_buf,
    Ack = case AckBuf of
        <<>> -> nodata;
        _ -> AckBuf
    end,
    NewMqttSession = MqttSession#mqtt_session{ack_buf = <<>>},
    ?DebugF("ack buf: ~p~n", [AckBuf]),
    {Ack, State#state_rcv{session = NewMqttSession}, think};
parse_bidi(Data, State=#state_rcv{acc = [], session = MqttSession}) ->
    AckBuf = MqttSession#mqtt_session.ack_buf,
    case mqtt_frame:decode(Data) of
        {_MqttMsg = #mqtt{type = ?PUBLISH, qos = Qos, id = MessageId}, Left} ->
            ?DebugF("receive bidi mqtt_msg: ~p ~p~n",
                    [mqtt_frame:command_for_type(?PUBLISH), _MqttMsg]),

            ts_mon:add({count, mqtt_server_published}),
            ts_mon:add({count, mqtt_pubacked}),
            Ack = case Qos of
                1 ->
                    Message = #mqtt{type = ?PUBACK, arg = MessageId},
                    mqtt_frame:encode(Message);
                _ -> <<>>
            end,
            NewAckBuf = <<AckBuf/binary, Ack/binary>>,
            NewMqttSession = MqttSession#mqtt_session{ack_buf = NewAckBuf},
            parse_bidi(Left, State#state_rcv{session = NewMqttSession});
        {_MqttMsg = #mqtt{type = _Type}, Left} ->
            ?DebugF("receive bidi mqtt_msg: ~p ~p~n",
                    [mqtt_frame:command_for_type(_Type), _MqttMsg]),
            parse_bidi(Left, State);
        more ->
            {nodata, State#state_rcv{acc = Data},think}
    end;
parse_bidi(Data, State=#state_rcv{acc = Acc, datasize = DataSize}) ->
    NewSize = DataSize + size(Data),
    ?DebugF("parse mqtt bidi data: ~p ~p~n", [Data, Acc]),
    parse_bidi(<<Acc/binary, Data/binary>>,
               State#state_rcv{acc = [], datasize = NewSize}).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
	ts_config_mqtt:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #websocket_request
%%----------------------------------------------------------------------
add_dynparams(true, {DynVars, _S},
              Param = #mqtt_request{type = connect, clean_start = CleanStart,
				    keepalive = KeepAlive, will_topic = WillTopic,
				    will_qos = WillQos, will_msg = WillMsg,
				    will_retain = WillRetain, username = UserName,
				    password = Password},
              _HostData) ->
    NewUserName = ts_search:subst(UserName, DynVars),
    NewPassword = ts_search:subst(Password, DynVars),
    Param#mqtt_request{ type = connect,
			clean_start = CleanStart,
			keepalive = KeepAlive, will_topic = WillTopic,
			will_qos = WillQos, will_msg = WillMsg,
			will_retain = WillRetain,
			username = NewUserName,
			password = NewPassword };
add_dynparams(true, {DynVars, _S},
              Param = #mqtt_request{type = publish, topic = Topic,
                                    payload = Payload},
              _HostData) ->
    NewTopic = ts_search:subst(Topic, DynVars),
    NewPayload = ts_search:subst(Payload, DynVars),
    Param#mqtt_request{topic = NewTopic, payload = NewPayload};
add_dynparams(true, {DynVars, _S},
              Param = #mqtt_request{type = subscribe, topic = Topic},
              _HostData) ->
    NewTopic = ts_search:subst(Topic, DynVars),
    Param#mqtt_request{topic = NewTopic};
add_dynparams(true, {DynVars, _S},
              Param = #mqtt_request{type = unsubscribe, topic = Topic},
              _HostData) ->
    NewTopic = ts_search:subst(Topic, DynVars),
    Param#mqtt_request{topic = NewTopic};
add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#mqtt_request{}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_ping_proc(Proto, Socket, KeepAlive) ->
    PingPid = proc_lib:spawn_link(?MODULE, ping_loop, [Proto, Socket, KeepAlive]),
    erlang:send_after(KeepAlive * 1000, PingPid, ping),
    PingPid.

ping_loop(Proto, Socket, KeepAlive) ->
    receive
        ping ->
            try
                Message = #mqtt{type = ?PINGREQ},
                PingFrame = mqtt_frame:encode(Message),
                Proto:send(Socket, PingFrame, [])
            catch
                Error ->
                    ?LOGF("Error sending mqtt pingreq: ~p~n",[Error], ?ERR)
            end,
            erlang:send_after(KeepAlive * 1000, self(), ping),
            ping_loop(Proto, Socket, KeepAlive);
        stop -> ok
    end.
