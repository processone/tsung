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

-module(ts_amqp).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_amqp.hrl").
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         parse/2,
         dump/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).


%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, ack_type = parse|no_ack|local, persistent = true|false} 
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(jabber)) -> 
%%      NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#amqp_session{}) ->
    Buffer. % nothing to do for amqp

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #amqp_session{map_num_pa = gb_trees:empty(), ack_buf = <<>>}.

dump(A,B) ->
    ts_plugin:dump(A,B).
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Request = #amqp_request{channel = ChannelStr}, State) ->
    ?DebugF("get message on channel: ~p ~p~n", [ChannelStr, Request]),
    ChannelNum = list_to_integer(ChannelStr),
    get_message1(Request#amqp_request{channel = ChannelNum}, State).

get_message1(#amqp_request{type = connect}, #state_rcv{session = AMQPSession}) ->
    Waiting = {0, 'connection.start'},
    {?PROTOCOL_HEADER, AMQPSession#amqp_session{status = handshake,
                                                waiting = Waiting,
                                                protocol = ?PROTOCOL}};

get_message1(#amqp_request{type = 'connection.start_ok', username = UserName,
                          password = Password},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    ?DebugF("start with: user=~p, password=~p~n", [UserName, Password]),
    
    Resp = plain(none, list_to_binary(UserName), list_to_binary(Password)),
    StartOk = #'connection.start_ok'{client_properties = client_properties([]),
                                     mechanism = <<"PLAIN">>, response = Resp},
    Frame = assemble_frame(0, StartOk, Protocol),
    Waiting = {0, 'connection.tune'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}};

get_message1(#amqp_request{type = 'connection.tune_ok', heartbeat = HeartBeat},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Tune = #'connection.tune_ok'{frame_max = 131072, heartbeat = HeartBeat},
    Frame = assemble_frame(0, Tune, Protocol),
    {Frame, AMQPSession#amqp_session{waiting = none}};

get_message1(#amqp_request{type = 'connection.open', vhost = VHost},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Open = #'connection.open'{virtual_host = list_to_binary(VHost)},
    Frame = assemble_frame(0, Open, Protocol),
    Waiting = {0, 'connection.open_ok'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}};

get_message1(#amqp_request{type = 'channel.open', channel = Channel},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,
    MapNPA = AMQPSession#amqp_session.map_num_pa,

    ChannelOpen = #'channel.open'{},
    case new_number(Channel, AMQPSession) of
        {ok, Number} ->
            MapNPA1 = gb_trees:enter(Number, unused, MapNPA),
            put({chstate, Number}, #ch{unconfirmed_set = gb_sets:new(),
                                       next_pub_seqno = 0}),
            Frame = assemble_frame(Number, ChannelOpen, Protocol),
            Waiting = {Number, 'channel.open_ok'},
            {Frame, AMQPSession#amqp_session{waiting = Waiting,
                                             map_num_pa = MapNPA1}};
        {error, _} ->
            {<<>>, AMQPSession#amqp_session{waiting = none}}
    end;

get_message1(#amqp_request{type = 'channel.close', channel = Channel},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    ChannelClose = #'channel.close'{reply_text = <<"Goodbye">>,
                                    reply_code = 200,
                                    class_id   = 0,
                                    method_id  = 0},
    Frame = assemble_frame(Channel, ChannelClose, Protocol),
    Waiting = {Channel, 'channel.close_ok'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}};

get_message1(#amqp_request{type = 'confirm.select', channel = Channel},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Confirm = #'confirm.select'{},
    Frame = assemble_frame(Channel, Confirm, Protocol),
    Waiting = {Channel, 'confirm.select_ok'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}};

get_message1(#amqp_request{type = 'basic.qos', prefetch_size = PrefetchSize,
                          channel = Channel,
                          prefetch_count = PrefetchCount},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Qos = #'basic.qos'{prefetch_size = PrefetchSize,
                       prefetch_count = PrefetchCount},
    Frame = assemble_frame(Channel, Qos, Protocol),
    Waiting = {Channel, 'basic.qos_ok'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}};

get_message1(#amqp_request{type = 'basic.publish', channel = Channel,
                          exchange = Exchange, routing_key = RoutingKey,
                          payload_size = Size, payload = Payload,
                          persistent = Persistent},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,
    MsgPayload = case Payload of
                     "" -> list_to_binary(ts_utils:urandomstr_noflat(Size));
                     _ -> list_to_binary(Payload)
                 end,
    Publish = #'basic.publish'{exchange = list_to_binary(Exchange),
                               routing_key = list_to_binary(RoutingKey)},
    Msg = case Persistent of 
        true ->
            Props = #'P_basic'{delivery_mode = 2}, %% persistent message
            build_content(Props, MsgPayload);
        false ->
            Props = #'P_basic'{},
            build_content(Props, MsgPayload)
    end,
    Frame = assemble_frames(Channel, Publish, Msg, ?FRAME_MIN_SIZE, Protocol),
    ChState = get({chstate, Channel}),
    NewChState = case ChState#ch.next_pub_seqno of
        0 ->
            ChState;
        SeqNo ->
            USet = ChState#ch.unconfirmed_set,
            ChState#ch{unconfirmed_set = gb_sets:add(SeqNo, USet),
                                     next_pub_seqno = SeqNo + 1}
    end,
    put({chstate, Channel}, NewChState),
    ts_mon:add({count, amqp_published}),
    {Frame, AMQPSession};

get_message1(#amqp_request{type = 'basic.consume', channel = Channel,
                           queue = Queue, ack = Ack},
             #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    NoAck = case Ack of
        true -> false;
        _ -> true
    end,

    ConsumerTag = list_to_binary(["tsung-", ts_utils:randombinstr(10)]),
    Sub = #'basic.consume'{queue = list_to_binary(Queue),
                           consumer_tag = ConsumerTag, no_ack = NoAck},
    ChState = get({chstate, Channel}),
    put({chstate, Channel}, ChState#ch{ack = Ack}),
    Frame = assemble_frame(Channel, Sub, Protocol),
    Waiting = {Channel, 'basic.consume_ok'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}};

get_message1(#amqp_request{type = 'connection.close'},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Close = #'connection.close'{reply_text = <<"Goodbye">>,
                                reply_code = 200,
                                class_id   = 0,
                                method_id  = 0},
    Frame = assemble_frame(0, Close, Protocol),
    Waiting = {0, 'connection.close_ok'},
    {Frame, AMQPSession#amqp_session{waiting = Waiting}}.
%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:	Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize = 0}) ->
    parse(Data, State#state_rcv{datasize = size(Data)});

%% handshake stage, parse response, and validate
parse(Data, State=#state_rcv{acc = []}) ->
    do_parse(Data, State);

%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc = Acc, datasize = DataSize}) ->
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary, Data/binary >>,
          State#state_rcv{acc = [], datasize = NewSize}).

parse_bidi(<<>>, State=#state_rcv{acc = [], session = AMQPSession}) ->
    AckBuf = AMQPSession#amqp_session.ack_buf,
    NewAMQPSession = AMQPSession#amqp_session{ack_buf = <<>>},
    ?DebugF("ack buf: ~p~n", [AckBuf]),
    {confirm_ack_buf(AckBuf), State#state_rcv{session = NewAMQPSession},think};
parse_bidi(Data, State=#state_rcv{acc = [], session = AMQPSession}) ->
    ?DebugF("parse bidi data: ~p ~p~n", [size(Data), Data]),
    Protocol = AMQPSession#amqp_session.protocol,
    AckBuf = AMQPSession#amqp_session.ack_buf,
    case decode_frame(Protocol, Data) of
        {error, _Reason} ->
            ?DebugF("decode error: ~p~n", [_Reason]),
            {nodata, State, think};
        {ok, heartbeat, Left} ->
            ?DebugF("receive bidi: ~p~n", [heartbeat]),
            HB = list_to_binary(rabbit_binary_generator:build_heartbeat_frame()),
            NewAckBuf = <<AckBuf/binary, HB/binary>>, 
            NewAMQPSession = AMQPSession#amqp_session{ack_buf = NewAckBuf},
            parse_bidi(Left, State#state_rcv{session = NewAMQPSession});
        {ok, _, none, Left} ->
            parse_bidi(Left, State);
        {ok, Channel, Method, Left} ->
            ?DebugF("receive bidi: ~p ~p~n", [Channel, Method]),
            NewAMQPSession = should_ack(Channel, AckBuf, Method, AMQPSession),
            parse_bidi(Left, State#state_rcv{session = NewAMQPSession});
        {incomplete, Left} ->
            ?DebugF("incomplete frame: ~p~n", [Left]),
            {confirm_ack_buf(AckBuf), State#state_rcv{acc = Left},think}
    end;
parse_bidi(Data, State=#state_rcv{acc = Acc, datasize = DataSize,
                                  session = AMQPSession}) ->
    NewSize = DataSize + size(Data),
    ?DebugF("parse bidi data: ~p ~p~n", [NewSize, Data, Acc]),
    parse_bidi(<<Acc/binary, Data/binary>>,
               State#state_rcv{acc = [], datasize = NewSize, session =
                               AMQPSession#amqp_session{ack_buf = <<>>}}).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
	ts_config_amqp:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #amqp_request
%%----------------------------------------------------------------------
add_dynparams(false, {_DynVars, _Session}, Param, _HostData) ->
    Param;
add_dynparams(true, {DynVars, _Session},
              Req = #amqp_request{channel = Channel, payload = Payload,
                                  exchange = Exchange, routing_key = RoutingKey,
                                  queue = Queue}, _HostData) ->
    SubstChannel = ts_search:subst(Channel, DynVars),
    SubstPayload = ts_search:subst(Payload, DynVars),
    SubstExchange = ts_search:subst(Exchange, DynVars),
    SubstRoutingKey = ts_search:subst(RoutingKey, DynVars),
    SubstQueue = ts_search:subst(Queue, DynVars),
    Req#amqp_request{channel = SubstChannel, payload = SubstPayload,
                     exchange = SubstExchange, routing_key = SubstRoutingKey,
                     queue = SubstQueue}.

%%----------------------------------------------------------------------
plain(none, Username, Password) ->
    <<0, Username/binary, 0, Password/binary>>.

do_parse(Data, State = #state_rcv{session = AMQPSession}) ->
    ?DebugF("start do_parse: ~p ~n", [Data]),
    Protocol = AMQPSession#amqp_session.protocol,
    Waiting = AMQPSession#amqp_session.waiting,
    case decode_and_check(Data, Waiting, State, Protocol) of
        {ok, _Method, Result} ->
            Result;
        {fail, Result} ->
            Result
    end.

get_post_fun(_Channel, 'connection.open_ok') ->
    fun({NewState, Options, Close}) ->
            AMQPSession = NewState#state_rcv.session,
            NewAMQPSession = AMQPSession#amqp_session{status = connected},
            NewState1 = NewState#state_rcv{session = NewAMQPSession},
            ts_mon:add({count, amqp_connected}),
            {NewState1, Options, Close}
    end;

get_post_fun(_Channel, 'channel.open_ok') ->
    fun({NewState, Options, Close}) ->
            ts_mon:add({count, amqp_channel_opened}),
            {NewState, Options, Close}
    end;

get_post_fun(_Channel, 'channel.close_ok') ->
    fun({NewState, Options, Close}) ->
            ts_mon:add({count, amqp_channel_closed}),
            {NewState, Options, Close}
    end;

get_post_fun(Channel, 'confirm.select_ok') ->
    fun({NewState, Options, Close}) ->
            ChState = get({chstate, Channel}),
            NewChState = ChState#ch{next_pub_seqno = 1},
            put({chstate, Channel}, NewChState),
            NewState1 = NewState#state_rcv{acc = []},
            {NewState1, Options, Close}
    end;

get_post_fun(_Channel, 'basic.consume_ok') ->
    fun({NewState, Options, Close}) ->
            AMQPSession = NewState#state_rcv.session,
            Socket = NewState#state_rcv.socket,
            ts_mon:add({count, amqp_consumer}),
            LeftData = NewState#state_rcv.acc,
            NewAMQPSession = AMQPSession#amqp_session{waiting = none},
            NewState1 = NewState#state_rcv{acc = [], session = NewAMQPSession},
            case LeftData of
                <<>> -> ok;
                %% trick, trigger the parse_bidi call
                _ -> self() ! {gen_ts_transport, Socket, LeftData}
            end,
            {NewState1, Options, Close}
    end;

get_post_fun(_Channel, 'connection.close_ok') ->
    fun({NewState, Options, _Close}) ->
            ts_mon:add({count, amqp_closed}),
            {NewState, Options, true}
    end;

get_post_fun(_Channel, _) ->
    fun({NewState, Options, Close}) ->
            AMQPSession = NewState#state_rcv.session,
            NewAMQPSession = AMQPSession#amqp_session{waiting = none},
            NewState1 = NewState#state_rcv{session = NewAMQPSession},
            {NewState1, Options, Close}
    end.

new_number(0, #amqp_session{channel_max = ChannelMax,
                               map_num_pa = MapNPA}) ->
    case gb_trees:is_empty(MapNPA) of
        true  -> {ok, 1};
        false -> {Smallest, _} = gb_trees:smallest(MapNPA),
                 if Smallest > 1 ->
                        {ok, Smallest - 1};
                    true ->
                        {Largest, _} = gb_trees:largest(MapNPA),
                        if Largest < ChannelMax -> {ok, Largest + 1};
                           true                 -> find_free(MapNPA)
                        end
                 end
    end;
new_number(Proposed, Session = #amqp_session{channel_max = ChannelMax,
                                             map_num_pa  = MapNPA}) ->
    IsValid = Proposed > 0 andalso Proposed =< ChannelMax andalso
        not gb_trees:is_defined(Proposed, MapNPA),
    case IsValid of true  -> {ok, Proposed};
                    false -> new_number(none, Session)
    end.

find_free(MapNPA) ->
    find_free(gb_trees:iterator(MapNPA), 1).

find_free(It, Candidate) ->
    case gb_trees:next(It) of
        {Number, _, It1} -> if Number > Candidate ->
                                   {ok, Number - 1};
                               Number =:= Candidate ->
                                   find_free(It1, Candidate + 1)
                            end;
        none             -> {error, out_of_channel_numbers}
    end.

confirm_ack_buf(AckBuf) ->
    case AckBuf of
        <<>> -> nodata;
        _ -> AckBuf
    end.

should_ack(Channel, AckBuf, #'basic.deliver'{delivery_tag = DeliveryTag},
           AMQPSession = #amqp_session{protocol = Protocol}) ->
    ChState = get({chstate, Channel}),
    case ChState#ch.ack of
        true ->
            ?DebugF("delivered: ~p ~n", [ack]),
            Ack = #'basic.ack'{delivery_tag = DeliveryTag},
            Frame = assemble_frame(Channel, Ack, Protocol),
            ts_mon:add({count, amqp_delivered}),
            NewAckBuf = case AckBuf of
                nodata -> Frame;
                _ -> <<AckBuf/binary, Frame/binary>>
            end,
            AMQPSession#amqp_session{ack_buf = NewAckBuf};
        false ->
            ?DebugF("delivered: ~p ~n", [noack]),
            ts_mon:add({count, amqp_delivered}),
            AMQPSession#amqp_session{ack_buf = AckBuf}
    end;
should_ack(Channel, AckBuf, Method = #'basic.ack'{}, AMQPSession) ->
    ?DebugF("publish confirm: ~p ~n", [ack]),
    update_confirm_set(Channel, Method),
    AMQPSession#amqp_session{ack_buf = AckBuf};
should_ack(Channel, AckBuf, Method = #'basic.nack'{}, AMQPSession) ->
    ?DebugF("publish confirm: ~p ~n", [nack]),
    update_confirm_set(Channel, Method),
    AMQPSession#amqp_session{ack_buf = AckBuf};
should_ack(_Channel, AckBuf, _Method, AMQPSession) ->
    ?DebugF("delivered: ~p ~n", [other]),
    AMQPSession#amqp_session{ack_buf = AckBuf}.

update_confirm_set(Channel, #'basic.ack'{delivery_tag = SeqNo, multiple = Multiple}) ->
    ChState = get({chstate, Channel}),
    USet = ChState#ch.unconfirmed_set,
    USet1 = update_unconfirmed(ack, SeqNo, Multiple, USet),
    put({chstate, Channel}, ChState#ch{unconfirmed_set = USet1});
update_confirm_set(Channel, #'basic.nack'{delivery_tag = SeqNo, multiple = Multiple}) ->
    ChState = get({chstate, Channel}),
    USet = ChState#ch.unconfirmed_set,
    USet1 = update_unconfirmed(nack, SeqNo, Multiple, USet),
    put({chstate, Channel}, ChState#ch{unconfirmed_set = USet1}).

update_unconfirmed(AckType, SeqNo, false, USet) ->
    add_ack_stat(AckType),
    gb_sets:del_element(SeqNo, USet);
update_unconfirmed(AckType, SeqNo, true, USet) ->
    case gb_sets:is_empty(USet) of
        true  -> USet;
        false -> {S, USet1} = gb_sets:take_smallest(USet),
                 case S > SeqNo of
                     true  -> USet;
                     false ->
                        add_ack_stat(AckType),
                        update_unconfirmed(AckType, SeqNo, true, USet1)
                 end
    end.

add_ack_stat(ack) ->
    ts_mon:add({count, amqp_confirmed});
add_ack_stat(nack) ->
    ts_mon:add({count, amqp_unconfirmed}).

client_properties(UserProperties) ->
    Default = [{<<"product">>,   longstr, <<"Tsung">>},
               {<<"version">>,   longstr, list_to_binary("0.0.1")},
               {<<"platform">>,  longstr, <<"Erlang">>},
               {<<"capabilities">>, table, ?CLIENT_CAPABILITIES}],
    lists:foldl(fun({K, _, _} = Tuple, Acc) ->
                    lists:keystore(K, 1, Acc, Tuple)
                end, Default, UserProperties).

assemble_frame(Channel, MethodRecord, Protocol) ->
    list_to_binary(rabbit_binary_generator:build_simple_method_frame(
            Channel, MethodRecord, Protocol)).

assemble_frames(Channel, MethodRecord, Content, FrameMax, Protocol) ->
    MethodName = rabbit_misc:method_record_type(MethodRecord),
    true = Protocol:method_has_content(MethodName), % assertion
    MethodFrame = rabbit_binary_generator:build_simple_method_frame(
                    Channel, MethodRecord, Protocol),
    ContentFrames = rabbit_binary_generator:build_simple_content_frames(
                      Channel, Content, FrameMax, Protocol),
    list_to_binary([MethodFrame | ContentFrames]).

build_content(Properties, BodyBin) when is_binary(BodyBin) ->
    build_content(Properties, [BodyBin]);

build_content(Properties, PFR) ->
    %% basic.publish hasn't changed so we can just hard-code amqp_0_9_1
    {ClassId, _MethodId} =
        rabbit_framing_amqp_0_9_1:method_id('basic.publish'),
    #content{class_id = ClassId,
             properties = Properties,
             properties_bin = none,
             protocol = none,
             payload_fragments_rev = PFR}.

decode_and_check(Data, Waiting, State, Protocol) ->
    case decode_frame(Protocol, Data) of
        {error, _Reason} ->
            ?DebugF("decode error: ~p~n", [_Reason]),
            ts_mon:add({count, amqp_error}),
            {fail, {State#state_rcv{ack_done = true}, [], true}};
        {ok, heartbeat, Left} ->
            {ok, heartbeat, {State#state_rcv{ack_done = false, acc = Left},
                             [], true}};
        {ok, Channel, Method, Left} ->
            check(Channel, Waiting, Method, State, Left);
        {incomplete, Left} ->
            ?DebugF("incomplete frame: ~p~n", [Left]),
            {fail, {State#state_rcv{ack_done = false, acc = Left}, [], false}}
    end.

check(Channel, {Channel, Expecting}, Method, State, Left) ->
    ?DebugF("receive from server: ~p~n", [Method]),
    case {Expecting, element(1, Method)} of
        {E, M} when E =:= M ->
            PostFun = get_post_fun(Channel, Expecting),
            {ok, Method,
             PostFun({State#state_rcv{ack_done = true, acc = Left}, [], false})};
        _ ->
            ts_mon:add({count, amqp_unexpected}),
            ?DebugF("unexpected_method: ~p, expecting ~p~n",
                    [Method, Expecting]),
            {fail, {State#state_rcv{ack_done = true}, [], true}}
    end;
check(Channel, Waiting = {WaitingCh, Expecting}, Method = #'basic.deliver'{},
      State = #state_rcv{session = AMQPSession}, Left) ->
    ?LOGF("waiting on ~p, expecting ~p, but receive deliver on ~p ~p~n",
          [WaitingCh, Expecting, Channel, Method], ?NOTICE),
    AckBuf = AMQPSession#amqp_session.ack_buf,
    NewAMQPSession = should_ack(Channel, AckBuf, Method, AMQPSession),
    Protocol = AMQPSession#amqp_session.protocol,
    decode_and_check(Left, Waiting,
                     State#state_rcv{session = NewAMQPSession}, Protocol);
check(Channel, Waiting = {WaitingCh, Expecting}, Method,
      State = #state_rcv{session = AMQPSession}, Left) ->
    ?LOGF("waiting on ~p, but received on ~p, expecting: ~p, actual: ~p~n",
          [WaitingCh, Channel, Expecting, Method], ?NOTICE),
    Protocol = AMQPSession#amqp_session.protocol,
    decode_and_check(Left, Waiting, State, Protocol).

decode_frame(Protocol, <<Type:8, Channel:16, Length:32, Body/binary>>)
        when size(Body) > Length ->
    <<PayLoad:Length/binary, ?FRAME_END, Left/binary>> = Body,
    case rabbit_command_assembler:analyze_frame(Type, PayLoad, Protocol) of 
        heartbeat -> {ok, heartbeat, Left};
        AnalyzedFrame -> process_frame(AnalyzedFrame, Channel, Protocol, Left)
    end;
decode_frame(_Protocol, Data) ->
    {incomplete, Data}.

process_frame(Frame, Channel, Protocol, Left) ->
    AState = case get({channel, Channel}) of
        undefined -> {ok, InitAState} = rabbit_command_assembler:init(Protocol),
            InitAState;
        AState1-> AState1
    end,
    case process_channel_frame(Frame, AState, Left) of
        {ok, Method, NewAState, Left} ->
            put({channel, Channel}, NewAState),
            {ok, Channel, Method, Left};
        Other -> Other
    end.

process_channel_frame(Frame, AState, Left) ->
    case rabbit_command_assembler:process(Frame, AState) of
        {ok, NewAState} ->
            {ok, none, NewAState, Left};
        {ok, Method, NewAState} ->
            {ok, Method, NewAState, Left};
        {ok, Method, _Content, NewAState} ->
            {ok, Method, NewAState, Left};
        {error, Reason} -> {error, Reason}
    end.
