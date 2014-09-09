%% The MIT License (MIT)
%%
%% Copyright (c) <2013> <hellomatty@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% Modified from: https://code.google.com/p/my-mqtt4erl/source/browse/src/mqtt_core.erl.
-module(mqtt_frame).
-author('hellomatty@gmail.com').

%%
%% An erlang client for MQTT (http://www.mqtt.org/)
%%

-include_lib("mqtt.hrl").

-export([encode/1, decode/1]).
-export([set_connect_options/1, set_publish_options/1, command_for_type/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
encode(#mqtt{} = Message) ->
  {VariableHeader, Payload} = encode_message(Message),
  FixedHeader = encode_fixed_header(Message),
  EncodedLength = encode_length(size(VariableHeader) + size(Payload)),
  <<FixedHeader/binary, EncodedLength/binary, VariableHeader/binary, Payload/binary>>.

decode(<<FixedHeader:8/big, Rest/binary>>) ->
  case decode_length(Rest) of
    more ->
      more;
    {RemainingLength, Rest1} ->
      Size = size(Rest1),
      if
        Size >= RemainingLength ->
          <<Body:RemainingLength/binary-unit:8, Left/binary>> = Rest1,
          {decode_message(decode_fixed_header(<<FixedHeader>>), Body), Left};
        true -> more
      end
  end;
decode(_Data) ->
  more.

set_connect_options(Options) ->
    set_connect_options(Options, #connect_options{}).

set_publish_options(Options) ->
  set_publish_options(Options, #publish_options{}).

command_for_type(Type) ->
  case Type of
    ?CONNECT -> connect;
    ?CONNACK -> connack;
    ?PUBLISH -> publish;
    ?PUBACK  -> puback;
    ?PUBREC -> pubrec;
    ?PUBREL -> pubrel;
    ?PUBCOMP -> pubcomp;
    ?SUBSCRIBE -> subscribe;
    ?SUBACK -> suback;
    ?UNSUBSCRIBE -> unsubscribe;
    ?UNSUBACK -> unsuback;
    ?PINGREQ -> pingreq;
    ?PINGRESP -> pingresp;
    ?DISCONNECT -> disconnect;
    _ -> unknown
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_connect_options([], Options) ->
    Options;
set_connect_options([{keepalive, KeepAlive}|T], Options) ->
    set_connect_options(T, Options#connect_options{keepalive = KeepAlive});
set_connect_options([{retry, Retry}|T], Options) ->
    set_connect_options(T, Options#connect_options{retry = Retry});
set_connect_options([{client_id, ClientId}|T], Options) ->
    set_connect_options(T, Options#connect_options{client_id = ClientId});
set_connect_options([{clean_start, Flag}|T], Options) ->
    set_connect_options(T, Options#connect_options{clean_start = Flag});
set_connect_options([{connect_timeout, Timeout}|T], Options) ->
    set_connect_options(T, Options#connect_options{connect_timeout = Timeout});
set_connect_options([{username, UserName}|T], Options) ->
    set_connect_options(T, Options#connect_options{username = UserName});
set_connect_options([{password, Password}|T], Options) ->
    set_connect_options(T, Options#connect_options{password = Password});
set_connect_options([#will{} = Will|T], Options) ->
    set_connect_options(T, Options#connect_options{will = Will});
set_connect_options([UnknownOption|_T], _Options) ->
    exit({connect, unknown_option, UnknownOption}).

set_publish_options([], Options) ->
  Options;
set_publish_options([{qos, QoS}|T], Options) when QoS >= 0, QoS =< 2 ->
  set_publish_options(T, Options#publish_options{qos = QoS});
set_publish_options([{retain, true}|T], Options) ->
  set_publish_options(T, Options#publish_options{retain = 1});
set_publish_options([{retain, false}|T], Options) ->
  set_publish_options(T, Options#publish_options{retain = 0});
set_publish_options([UnknownOption|_T], _Options) ->
  exit({unknown, publish_option, UnknownOption}).

construct_will(WT, WM, WillQoS, WillRetain) ->
    #will{
        topic = WT,
        message = WM,
        publish_options = #publish_options{qos = WillQoS, retain = WillRetain}
      }.

decode_message(#mqtt{type = ?CONNECT} = Message, Rest) ->
  <<ProtocolNameLength:16/big, _/binary>> = Rest,
  {VariableHeader, Payload} = split_binary(Rest, 2 + ProtocolNameLength + 4),
  <<_:16, ProtocolName:ProtocolNameLength/binary, ProtocolVersion:8/big, UsernameFlag:1, PasswordFlag:1, WillRetain:1, WillQoS:2/big, WillFlag:1, CleanStart:1, _:1, KeepAlive:16/big>> = VariableHeader,
  {ClientId, Will, Username, Password} = case {WillFlag, UsernameFlag, PasswordFlag} of
    {1, 0, 0} ->
      [C, WT, WM] = decode_strings(Payload),
      W = construct_will(WT, WM, WillQoS, WillRetain),
      {C, W, undefined, undefined};
    {1, 1, 0} ->
      [C, WT, WM, U] = decode_strings(Payload),
      W = construct_will(WT, WM, WillQoS, WillRetain),
      {C, W, U, undefined};
    {1, 1, 1} ->
      [C, WT, WM, U, P] = decode_strings(Payload),
      W = construct_will(WT, WM, WillQoS, WillRetain),
      {C, W, U, P};
    {0, 1, 0} ->
      [C, U] = decode_strings(Payload),
      {C, undefined, U, undefined};
    {0, 1, 1} ->
      [C, U, P] = decode_strings(Payload),
      {C, undefined, U, P};
    {0, 0, 0} ->
      [C] = decode_strings(Payload),
      {C, undefined, undefined, undefined}
  end,
  Message#mqtt{
    arg = #connect_options{
      client_id = ClientId,
      protocol_name = binary_to_list(ProtocolName),
      protocol_version = ProtocolVersion,
      clean_start = CleanStart =:= 1,
      will = Will,
      username = Username,
      password = Password,
      keepalive = KeepAlive
    }
  };
decode_message(#mqtt{type = ?CONNACK} = Message, Rest) ->
  <<_:8, ResponseCode:8/big>> = Rest,
  Message#mqtt{arg = ResponseCode};
decode_message(#mqtt{type = ?PINGRESP} = Message, _Rest) ->
  Message;
decode_message(#mqtt{type = ?PINGREQ} = Message, _Rest) ->
  Message;
decode_message(#mqtt{type = ?PUBLISH, qos = 0} = Message, Rest) ->
  {<<TopicLength:16/big>>, _} = split_binary(Rest, 2),
  {<<_:16, Topic/binary>>, Payload} = split_binary(Rest, 2 + TopicLength),
  Message#mqtt{
    arg = {binary_to_list(Topic), binary_to_list(Payload)}
  };
decode_message(#mqtt{type = ?PUBLISH} = Message, Rest) ->
  {<<TopicLength:16/big>>, _} = split_binary(Rest, 2),
  {<<_:16, Topic:TopicLength/binary, MessageId:16/big>>, Payload} = split_binary(Rest, 4 + TopicLength),
   Message#mqtt{
    id = MessageId,
    arg = {binary_to_list(Topic), binary_to_list(Payload)}
  };
decode_message(#mqtt{type = Type} = Message, Rest)
    when
      Type =:= ?PUBACK;
      Type =:= ?PUBREC;
      Type =:= ?PUBREL;
      Type =:= ?PUBCOMP ->
  <<MessageId:16/big>> = Rest,
  Message#mqtt{
    arg = MessageId
  };
decode_message(#mqtt{type = ?SUBSCRIBE} = Message, Rest) ->
  {<<MessageId:16/big>>, Payload} = split_binary(Rest, 2),
  Message#mqtt{
    id = MessageId,
    arg = decode_subs(Payload, [])
  };
decode_message(#mqtt{type = ?SUBACK} = Message, Rest) ->
  {<<MessageId:16/big>>, Payload} = split_binary(Rest, 2),
  GrantedQoS  = lists:map(fun(Item) ->
      <<_:6, QoS:2/big>> = <<Item>>,
      QoS
    end,
    binary_to_list(Payload)
  ),
  Message#mqtt{
    arg = {MessageId, GrantedQoS}
  };
decode_message(#mqtt{type = ?UNSUBSCRIBE} = Message, Rest) ->
  {<<MessageId:16/big>>, Payload} = split_binary(Rest, 2),
  Message#mqtt{
    id = MessageId,
    arg = {MessageId, lists:map(fun(T) -> #sub{topic = T} end, decode_strings(Payload))}
  };
decode_message(#mqtt{type = ?UNSUBACK} = Message, Rest) ->
  <<MessageId:16/big>> = Rest,
  Message#mqtt{
    arg = MessageId
  };
decode_message(#mqtt{type = ?DISCONNECT} = Message, _Rest) ->
  Message;
decode_message(Message, Rest) ->
  exit({decode_message, unexpected_message, {Message, Rest}}).

decode_subs(<<>>, Subs) ->
  lists:reverse(Subs);
decode_subs(Bytes, Subs) ->
  <<TopicLength:16/big, _/binary>> = Bytes,
  <<_:16, Topic:TopicLength/binary, ?UNUSED:6, QoS:2/big, Rest/binary>> = Bytes,
  decode_subs(Rest, [#sub{topic = binary_to_list(Topic), qos = QoS}|Subs]).

encode_message(#mqtt{type = ?CONNACK, arg = ReturnCode}) ->
  {<<?UNUSED:8, ReturnCode:8/big>>,<<>>};
encode_message(#mqtt{type = ?CONNECT, arg = Options}) ->
  CleanStart = case Options#connect_options.clean_start of
    true ->
      1;
    false ->
      0
  end,
  {UserNameFlag, UserNameValue} = case Options#connect_options.username of
    undefined ->
      {0, undefined};
    UserName ->
      {1, UserName}
  end,
  {PasswordFlag, PasswordValue} = case Options#connect_options.password of
    undefined ->
      {0, undefined};
    Password ->
      {1, Password}
  end,
  {WillFlag, WillQoS, WillRetain, PayloadList} =
        case Options#connect_options.will of
            #will{ topic = undefined } ->
                {0, 0, 0, [encode_string(Options#connect_options.client_id)]};
            #will{ topic = "" } ->
                {0, 0, 0, [encode_string(Options#connect_options.client_id)]};
            undefined ->
                {0, 0, 0, [encode_string(Options#connect_options.client_id)]};
            #will{ topic = WillTopic, message = WillMessage, publish_options = WillOptions } ->
                {1,
                 WillOptions#publish_options.qos,
                 WillOptions#publish_options.retain,
                 [encode_string(Options#connect_options.client_id),
                  encode_string(WillTopic),
                  encode_string(WillMessage)]
                }
        end,
  Payload1 = case UserNameValue of
    undefined -> list_to_binary(PayloadList);
    _ ->
      case PasswordValue of
        undefined -> list_to_binary(lists:append(PayloadList, [encode_string(UserNameValue)]));
        _ -> list_to_binary(lists:append(PayloadList, [encode_string(UserNameValue), encode_string(PasswordValue)]))
      end
    end,
  {
    list_to_binary([
      encode_string(Options#connect_options.protocol_name),
      <<(Options#connect_options.protocol_version)/big>>,
      <<UserNameFlag:1, PasswordFlag:1, WillRetain:1, WillQoS:2/big, WillFlag:1, CleanStart:1, ?UNUSED:1, (Options#connect_options.keepalive):16/big>>
    ]),
    Payload1
  };
encode_message(#mqtt{type = ?PUBLISH, arg = {Topic, Payload}} = Message) ->
  if
    Message#mqtt.qos =:= 0 ->
        {
          encode_string(Topic),
          list_to_binary(Payload)
        };
    Message#mqtt.qos > 0 ->
        {
          list_to_binary([encode_string(Topic), <<(Message#mqtt.id):16/big>>]),
          list_to_binary(Payload)
        }
  end;
encode_message(#mqtt{type = ?PUBACK, arg = MessageId}) ->
  {
    <<MessageId:16/big>>,
    <<>>
  };
encode_message(#mqtt{type = ?SUBSCRIBE, arg = Subs} = Message) ->
  {
    <<(Message#mqtt.id):16/big>>,
    list_to_binary( lists:flatten( lists:map(fun({sub, Topic, RequestedQoS}) -> [encode_string(Topic), <<?UNUSED:6, RequestedQoS:2/big>>] end, Subs)))
  };
encode_message(#mqtt{type = ?SUBACK, arg = {MessageId, Subs}}) ->
  {
    <<MessageId:16/big>>,
    list_to_binary(lists:map(fun(S) -> <<?UNUSED:6, (S#sub.qos):2/big>> end, Subs))
  };
encode_message(#mqtt{type = ?UNSUBSCRIBE, arg = Subs} = Message) ->
  {
    <<(Message#mqtt.id):16/big>>,
    list_to_binary(lists:map(fun({sub, T, _Q}) -> encode_string(T) end, Subs))
  };
encode_message(#mqtt{type = ?UNSUBACK, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt{type = ?PINGREQ}) ->
  {<<>>, <<>>};
encode_message(#mqtt{type = ?PINGRESP}) ->
  {<<>>, <<>>};
encode_message(#mqtt{type = ?PUBREC, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt{type = ?PUBREL, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt{type = ?PUBCOMP, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt{type = ?DISCONNECT}) ->
  {<<>>, <<>>};
encode_message(#mqtt{} = Message) ->
  exit({encode_message, unknown_type, Message}).

decode_length(<<>>) -> more;
decode_length(Data) ->
  decode_length(Data, 1, 0).
decode_length(<<>>, Multiplier, Value) ->
  {0, <<>>};
decode_length(<<0:1, Length:7, Rest/binary>>, Multiplier, Value) ->
  {Value + Multiplier * Length, Rest};
decode_length(<<1:1, Length:7, Rest/binary>>, Multiplier, Value) ->
  decode_length(Rest, Multiplier * 128, Value + Multiplier * Length).

encode_length(Length) ->
  encode_length(Length, <<>>).

encode_length(Length, Buff) when Length div 128 > 0 ->
  Digit = Length rem 128,
  Current = <<1:1, Digit:7/big>>,
  encode_length(Length div 128, <<Buff/binary, Current/binary>>);
encode_length(Length, Buff) ->
  Digit = Length rem 128,
  Current = <<0:1, Digit:7/big>>,
  <<Buff/binary, Current/binary>>.

encode_fixed_header(Message) when is_record(Message, mqtt) ->
  <<(Message#mqtt.type):4/big, (Message#mqtt.dup):1, (Message#mqtt.qos):2/big, (Message#mqtt.retain):1>>.

decode_fixed_header(Byte) ->
  <<Type:4/big, Dup:1, QoS:2/big, Retain:1>> = Byte,
  #mqtt{type = Type, dup = Dup, qos = QoS, retain = Retain}.

encode_string(String) ->
  Bytes = list_to_binary(String),
  Length = size(Bytes),
  <<Length:16/big, Bytes/binary>>.

decode_strings(Bytes) when is_binary(Bytes) ->
  decode_strings(Bytes, []).
decode_strings(<<>>, Strings) ->
  lists:reverse(Strings);
decode_strings(<<Length:16/big, _/binary>> = Bytes, Strings) ->
  <<_:16, Binary:Length/binary, Rest/binary>> = Bytes,
  decode_strings(Rest, [binary_to_list(Binary)|Strings]).
