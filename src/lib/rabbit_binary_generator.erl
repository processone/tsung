%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2012 VMware, Inc.  All rights reserved.
%%

-module(rabbit_binary_generator).
-include("rabbit_framing.hrl").
-include("rabbit.hrl").

-export([build_simple_method_frame/3,
         build_simple_content_frames/4,
         build_heartbeat_frame/0]).
-export([generate_table/1]).
-export([check_empty_frame_size/0]).
-export([ensure_content_encoded/2, clear_encoded_content/1]).
-export([map_exception/3]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(frame() :: [binary()]).

-spec(build_simple_method_frame/3 ::
        (rabbit_channel:channel_number(), rabbit_framing:amqp_method_record(),
         rabbit_types:protocol())
        -> frame()).
-spec(build_simple_content_frames/4 ::
        (rabbit_channel:channel_number(), rabbit_types:content(),
         non_neg_integer(), rabbit_types:protocol())
        -> [frame()]).
-spec(build_heartbeat_frame/0 :: () -> frame()).
-spec(generate_table/1 :: (rabbit_framing:amqp_table()) -> binary()).
-spec(check_empty_frame_size/0 :: () -> 'ok').
-spec(ensure_content_encoded/2 ::
        (rabbit_types:content(), rabbit_types:protocol()) ->
                                       rabbit_types:encoded_content()).
-spec(clear_encoded_content/1 ::
        (rabbit_types:content()) -> rabbit_types:unencoded_content()).
-spec(map_exception/3 :: (rabbit_channel:channel_number(),
                          rabbit_types:amqp_error() | any(),
                          rabbit_types:protocol()) ->
                              {rabbit_channel:channel_number(),
                               rabbit_framing:amqp_method_record()}).

-endif.

%%----------------------------------------------------------------------------

build_simple_method_frame(ChannelInt, MethodRecord, Protocol) ->
    MethodFields = Protocol:encode_method_fields(MethodRecord),
    MethodName = rabbit_misc:method_record_type(MethodRecord),
    {ClassId, MethodId} = Protocol:method_id(MethodName),
    create_frame(1, ChannelInt, [<<ClassId:16, MethodId:16>>, MethodFields]).

build_simple_content_frames(ChannelInt, Content, FrameMax, Protocol) ->
    #content{class_id = ClassId,
             properties_bin = ContentPropertiesBin,
             payload_fragments_rev = PayloadFragmentsRev} =
        ensure_content_encoded(Content, Protocol),
    {BodySize, ContentFrames} =
        build_content_frames(PayloadFragmentsRev, FrameMax, ChannelInt),
    HeaderFrame = create_frame(2, ChannelInt,
                               [<<ClassId:16, 0:16, BodySize:64>>,
                                ContentPropertiesBin]),
    [HeaderFrame | ContentFrames].

build_content_frames(FragsRev, FrameMax, ChannelInt) ->
    BodyPayloadMax = if FrameMax == 0 -> iolist_size(FragsRev);
                        true          -> FrameMax - ?EMPTY_FRAME_SIZE
                     end,
    build_content_frames(0, [], BodyPayloadMax, [],
                         lists:reverse(FragsRev), BodyPayloadMax, ChannelInt).

build_content_frames(SizeAcc, FramesAcc, _FragSizeRem, [],
                     [], _BodyPayloadMax, _ChannelInt) ->
    {SizeAcc, lists:reverse(FramesAcc)};
build_content_frames(SizeAcc, FramesAcc, FragSizeRem, FragAcc,
                     Frags, BodyPayloadMax, ChannelInt)
  when FragSizeRem == 0 orelse Frags == [] ->
    Frame = create_frame(3, ChannelInt, lists:reverse(FragAcc)),
    FrameSize = BodyPayloadMax - FragSizeRem,
    build_content_frames(SizeAcc + FrameSize, [Frame | FramesAcc],
                         BodyPayloadMax, [], Frags, BodyPayloadMax, ChannelInt);
build_content_frames(SizeAcc, FramesAcc, FragSizeRem, FragAcc,
                     [Frag | Frags], BodyPayloadMax, ChannelInt) ->
    Size = size(Frag),
    {NewFragSizeRem, NewFragAcc, NewFrags} =
        if Size == 0           -> {FragSizeRem, FragAcc, Frags};
           Size =< FragSizeRem -> {FragSizeRem - Size, [Frag | FragAcc], Frags};
           true                -> <<Head:FragSizeRem/binary, Tail/binary>> =
                                      Frag,
                                  {0, [Head | FragAcc], [Tail | Frags]}
        end,
    build_content_frames(SizeAcc, FramesAcc, NewFragSizeRem, NewFragAcc,
                         NewFrags, BodyPayloadMax, ChannelInt).

build_heartbeat_frame() ->
    create_frame(?FRAME_HEARTBEAT, 0, <<>>).

create_frame(TypeInt, ChannelInt, Payload) ->
    [<<TypeInt:8, ChannelInt:16, (iolist_size(Payload)):32>>, Payload,
     ?FRAME_END].

%% table_field_to_binary supports the AMQP 0-8/0-9 standard types, S,
%% I, D, T and F, as well as the QPid extensions b, d, f, l, s, t, x,
%% and V.
table_field_to_binary({FName, T, V}) ->
    [short_string_to_binary(FName) | field_value_to_binary(T, V)].

field_value_to_binary(longstr,   V) -> ["S", long_string_to_binary(V)];
field_value_to_binary(signedint, V) -> ["I", <<V:32/signed>>];
field_value_to_binary(decimal,   V) -> {Before, After} = V,
                                       ["D", Before, <<After:32>>];
field_value_to_binary(timestamp, V) -> ["T", <<V:64>>];
field_value_to_binary(table,     V) -> ["F", table_to_binary(V)];
field_value_to_binary(array,     V) -> ["A", array_to_binary(V)];
field_value_to_binary(byte,      V) -> ["b", <<V:8/unsigned>>];
field_value_to_binary(double,    V) -> ["d", <<V:64/float>>];
field_value_to_binary(float,     V) -> ["f", <<V:32/float>>];
field_value_to_binary(long,      V) -> ["l", <<V:64/signed>>];
field_value_to_binary(short,     V) -> ["s", <<V:16/signed>>];
field_value_to_binary(bool,      V) -> ["t", if V -> 1; true -> 0 end];
field_value_to_binary(binary,    V) -> ["x", long_string_to_binary(V)];
field_value_to_binary(void,     _V) -> ["V"].

table_to_binary(Table) when is_list(Table) ->
    BinTable = generate_table(Table),
    [<<(size(BinTable)):32>>, BinTable].

array_to_binary(Array) when is_list(Array) ->
    BinArray = generate_array(Array),
    [<<(size(BinArray)):32>>, BinArray].

generate_table(Table) when is_list(Table) ->
    list_to_binary(lists:map(fun table_field_to_binary/1, Table)).

generate_array(Array) when is_list(Array) ->
    list_to_binary(lists:map(fun ({T, V}) -> field_value_to_binary(T, V) end,
                             Array)).

short_string_to_binary(String) when is_binary(String) ->
    Len = size(String),
    if Len < 256 -> [<<Len:8>>, String];
       true      -> exit(content_properties_shortstr_overflow)
    end;
short_string_to_binary(String) ->
    Len = length(String),
    if Len < 256 -> [<<Len:8>>, String];
       true      -> exit(content_properties_shortstr_overflow)
    end.

long_string_to_binary(String) when is_binary(String) ->
    [<<(size(String)):32>>, String];
long_string_to_binary(String) ->
    [<<(length(String)):32>>, String].

check_empty_frame_size() ->
    %% Intended to ensure that EMPTY_FRAME_SIZE is defined correctly.
    case iolist_size(create_frame(?FRAME_BODY, 0, <<>>)) of
        ?EMPTY_FRAME_SIZE -> ok;
        ComputedSize      -> exit({incorrect_empty_frame_size,
                                   ComputedSize, ?EMPTY_FRAME_SIZE})
    end.

ensure_content_encoded(Content = #content{properties_bin = PropBin,
                                          protocol = Protocol}, Protocol)
  when PropBin =/= none ->
    Content;
ensure_content_encoded(Content = #content{properties = none,
                                          properties_bin = PropBin,
                                          protocol = Protocol}, Protocol1)
  when PropBin =/= none ->
    Props = Protocol:decode_properties(Content#content.class_id, PropBin),
    Content#content{properties = Props,
                    properties_bin = Protocol1:encode_properties(Props),
                    protocol = Protocol1};
ensure_content_encoded(Content = #content{properties = Props}, Protocol)
  when Props =/= none ->
    Content#content{properties_bin = Protocol:encode_properties(Props),
                    protocol = Protocol}.

clear_encoded_content(Content = #content{properties_bin = none,
                                         protocol = none}) ->
    Content;
clear_encoded_content(Content = #content{properties = none}) ->
    %% Only clear when we can rebuild the properties_bin later in
    %% accordance to the content record definition comment - maximum
    %% one of properties and properties_bin can be 'none'
    Content;
clear_encoded_content(Content = #content{}) ->
    Content#content{properties_bin = none, protocol = none}.

%% NB: this function is also used by the Erlang client
map_exception(Channel, Reason, Protocol) ->
    {SuggestedClose, ReplyCode, ReplyText, FailedMethod} =
        lookup_amqp_exception(Reason, Protocol),
    {ClassId, MethodId} = case FailedMethod of
                              {_, _} -> FailedMethod;
                              none   -> {0, 0};
                              _      -> Protocol:method_id(FailedMethod)
                          end,
    case SuggestedClose orelse (Channel == 0) of
        true  -> {0, #'connection.close'{reply_code = ReplyCode,
                                         reply_text = ReplyText,
                                         class_id   = ClassId,
                                         method_id  = MethodId}};
        false -> {Channel, #'channel.close'{reply_code = ReplyCode,
                                            reply_text = ReplyText,
                                            class_id   = ClassId,
                                            method_id  = MethodId}}
    end.

lookup_amqp_exception(#amqp_error{name        = Name,
                                  explanation = Expl,
                                  method      = Method},
                      Protocol) ->
    {ShouldClose, Code, Text} = Protocol:lookup_amqp_exception(Name),
    ExplBin = amqp_exception_explanation(Text, Expl),
    {ShouldClose, Code, ExplBin, Method};
lookup_amqp_exception(Other, Protocol) ->
    rabbit_log:warning("Non-AMQP exit reason '~p'~n", [Other]),
    {ShouldClose, Code, Text} = Protocol:lookup_amqp_exception(internal_error),
    {ShouldClose, Code, Text, none}.

amqp_exception_explanation(Text, Expl) ->
    ExplBin = list_to_binary(Expl),
    CompleteTextBin = <<Text/binary, " - ", ExplBin/binary>>,
    if size(CompleteTextBin) > 255 -> <<CompleteTextBin:252/binary, "...">>;
       true                        -> CompleteTextBin
    end.
