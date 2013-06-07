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
%%%  the two.
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
    #amqp_session{unconfirmed_set = gb_sets:new(),
                  next_pub_seqno = 0, ack_buf = <<>>}.

dump(A,B) ->
    ts_plugin:dump(A,B).
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#amqp_request{type = connect}, #state_rcv{session = AMQPSession}) ->
    NewAMQPSession = AMQPSession#amqp_session{status = wait_start,
                                              protocol = ?PROTOCOL},
    {?PROTOCOL_HEADER, NewAMQPSession};

get_message(#amqp_request{type = 'connection.start_ok', username = UserName,
                          password = Password},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    ?DebugF("start with: user=~p, password=~p~n", [UserName, Password]),
    
    Resp = plain(none, list_to_binary(UserName), list_to_binary(Password)),
    StartOk = #'connection.start_ok'{client_properties = client_properties([]),
                                     mechanism = <<"PLAIN">>, response = Resp},
    Frame = assemble_frame(0, StartOk, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = wait_tune},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'connection.tune_ok', heartbeat = HeartBeat},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Tune = #'connection.tune_ok'{frame_max = 131072, heartbeat = HeartBeat},
    Frame = assemble_frame(0, Tune, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = opening},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'connection.open', vhost = VHost},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Open = #'connection.open'{virtual_host = list_to_binary(VHost)},
    Frame = assemble_frame(0, Open, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = wait_open},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'channel.open'},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    ChannelOpen = #'channel.open'{},
    Frame = assemble_frame(1, ChannelOpen, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = wait_channel},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'confirm.select'},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Confirm = #'confirm.select'{},
    Frame = assemble_frame(1, Confirm, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = wait_select},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'basic.qos', prefetch_size = PrefetchSize,
                          prefetch_count = PrefetchCount},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Qos = #'basic.qos'{prefetch_size = PrefetchSize,
                       prefetch_count = PrefetchCount},
    Frame = assemble_frame(1, Qos, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = wait_qos},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'basic.publish', exchange = Exchange,
                          routing_key = RoutingKey, size = Size,
                          persistent = Persistent},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,
    Payload = list_to_binary(ts_utils:urandomstr_noflat(Size)),
    Publish = #'basic.publish'{exchange = list_to_binary(Exchange),
                               routing_key = list_to_binary(RoutingKey)},
    Msg = case Persistent of 
        true ->
            Props = #'P_basic'{delivery_mode = 2}, %% persistent message
            build_content(Props, Payload);
        false ->
            Props = #'P_basic'{},
            build_content(Props, Payload)
    end,
    Frame = assemble_frames(1, Publish, Msg, ?FRAME_MIN_SIZE, Protocol),
    NewAMQPSession = case AMQPSession#amqp_session.next_pub_seqno of
        0 ->
            AMQPSession;
        SeqNo ->
            USet = AMQPSession#amqp_session.unconfirmed_set,
            AMQPSession#amqp_session{unconfirmed_set = gb_sets:add(SeqNo, USet),
                                     next_pub_seqno = SeqNo + 1}
    end,
    ts_mon:add({count, amqp_published}),
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'basic.consume', queue = Queue, ack = Ack},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    NoAck = case Ack of
        true -> false;
        _ -> true
    end,

    ConsumerTag = list_to_binary(["tsung-", ts_utils:urandomstr_noflat(10)]),
    Sub = #'basic.consume'{queue = list_to_binary(Queue),
                           consumer_tag = ConsumerTag ,no_ack = NoAck},
    Frame = assemble_frame(1, Sub, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{ack = Ack, status = wait_consume},
    {Frame, NewAMQPSession};

get_message(#amqp_request{type = 'connection.close'},
            #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,

    Close = #'connection.close'{reply_text = <<"Goodbye">>,
                                reply_code = 200,
                                class_id   = 0,
                                method_id  = 0},
    Frame = assemble_frame(0, Close, Protocol),
    NewAMQPSession = AMQPSession#amqp_session{status = wait_close},
    {Frame, NewAMQPSession}.
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

%% handshake stage, parse response, and validate
parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_start ->
    Expecting = 'connection.start',
    do_parse(Data, Expecting, State);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_tune ->
    Expecting = 'connection.tune',
    do_parse(Data, Expecting, State);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_open ->
    Expecting = 'connection.open_ok',
    do_parse(Data, Expecting, State);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_channel ->
    Expecting = 'channel.open_ok',
    PostFun = fun({NewState, Options, Close}) ->
            NewAMQPSession = AMQPSession#amqp_session{status = connected},
            NewState1 = NewState#state_rcv{session = NewAMQPSession},
            ts_mon:add({count, amqp_connected}),
            {NewState1, Options, Close}
    end,
    do_parse(Data, Expecting, State, PostFun);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_select ->
    Expecting = 'confirm.select_ok',
    PostFun = fun({NewState, Options, Close}) ->
            NewAMQPSession = AMQPSession#amqp_session{status = select_ok,
                                                      next_pub_seqno = 1},
            NewState1 = NewState#state_rcv{acc = [], session = NewAMQPSession},
            {NewState1, Options, Close}
    end,
    do_parse(Data, Expecting, State, PostFun);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_qos ->
    Expecting = 'basic.qos_ok',
    do_parse(Data, Expecting, State);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_confirm ->
    Expecting = 'basic.ack',
    PostFun = fun(Result) ->
            ts_mon:add({count, amqp_confirmed}),
            Result
    end,
    do_parse(Data, Expecting, State, PostFun);

parse(Data, State=#state_rcv{socket = Socket, acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_consume ->
    Expecting = 'basic.consume_ok',
    PostFun = fun({NewState, Options, Close}) ->
            ts_mon:add({count, amqp_consumer}),
            LeftData = NewState#state_rcv.acc,
            NewAMQPSession = AMQPSession#amqp_session{status = consume_ok},
            NewState1 = NewState#state_rcv{acc = [], session = NewAMQPSession},
            %% trick, trigger the parse_bidi call
            self() ! {gen_ts_transport, Socket, LeftData},
            {NewState1, Options, Close}
    end,
    do_parse(Data, Expecting, State, PostFun);

parse(Data, State=#state_rcv{acc = [], session = AMQPSession})
        when AMQPSession#amqp_session.status == wait_close ->
    Expecting = 'connection.close_ok',
    PostFun = fun({NewState, Options, _Close}) ->
            ts_mon:add({count, amqp_closed}),
            {NewState, Options, true}
    end,
    do_parse(Data, Expecting, State, PostFun);

%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc = Acc, datasize = DataSize}) ->
    NewSize= DataSize + size(Data),
    ?DebugF("parse data: ~p ~p~n", [Data, Acc]),
    parse(<< Acc/binary, Data/binary >>,
          State#state_rcv{acc = [], datasize = NewSize}).

do_parse(Data, Expecting, State = #state_rcv{session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,
    case decode_and_check(Data, Expecting, State, Protocol) of
        {ok, _Method, Result} ->
            Result;
        {fail, Result} ->
            Result
    end.
do_parse(Data, Expecting, State = #state_rcv{session = AMQPSession}, PostFun) ->
    Protocol = AMQPSession#amqp_session.protocol,
    case decode_and_check(Data, Expecting, State, Protocol) of
        {ok, _Method, Result} ->
            PostFun(Result);
        {fail, Result} ->
            Result
    end.

plain(none, Username, Password) ->
    <<0, Username/binary, 0, Password/binary>>.

parse_bidi(<<>>, State=#state_rcv{acc = [], session = AMQPSession}) ->
    AckBuf = AMQPSession#amqp_session.ack_buf,
    NewAMQPSession = AMQPSession#amqp_session{ack_buf = <<>>},
    ?DebugF("ack buf: ~p~n", [AckBuf]),
    {confirm_ack_buf(AckBuf), State#state_rcv{session = NewAMQPSession}};
parse_bidi(Data, State=#state_rcv{acc = [], session = AMQPSession}) ->
    Protocol = AMQPSession#amqp_session.protocol,
    AckBuf = AMQPSession#amqp_session.ack_buf,
    case decode_frame(Protocol, Data) of
        {error, Reason} ->
            ?DebugF("decode error: ~p~n", [Reason]),
            {nodata, State};
        {ok, heartbeat, Left} ->
            HeartBeat = rabbit_binary_generator:build_heartbeat_frame(),
            NewAckBuf = <<AckBuf/binary, HeartBeat/binary>>, 
            NewAMQPSession = AMQPSession#amqp_session{ack_buf = NewAckBuf},
            parse_bidi(Left, State#state_rcv{session = NewAMQPSession});
        {ok, none, Left} ->
            parse_bidi(Left, State);
        {ok, Method, Left} ->
            ?DebugF("receive bidi: ~p~n", [Method]),
            NewAMQPSession = should_ack(AckBuf, Method, AMQPSession),
            parse_bidi(Left, State#state_rcv{session = NewAMQPSession});
        {ok, Method, Content, Left} ->
            ?DebugF("receive bidi: ~p ~p~n", [Method, Content]),
            NewAMQPSession = should_ack(AckBuf, Method, AMQPSession),
            parse_bidi(Left, State#state_rcv{session = NewAMQPSession});
        {incomplete, Left} ->
            ?DebugF("incomplete frame: ~p~n", [Left]),
            {confirm_ack_buf(AckBuf), State#state_rcv{acc = Left}}
    end;
parse_bidi(Data, State=#state_rcv{acc = Acc, datasize = DataSize,
                                  session = AMQPSession}) ->
    NewSize= DataSize + size(Data),
    ?DebugF("parse bidi data: ~p ~p~n", [Data, Acc]),
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
add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param.

%%----------------------------------------------------------------------
confirm_ack_buf(AckBuf) ->
    case AckBuf of
        <<>> -> nodata;
        _ -> AckBuf
    end.

should_ack(AckBuf, #'basic.deliver'{delivery_tag = DeliveryTag},
           AMQPSession = #amqp_session{ack = true, protocol = Protocol}) ->
    ?DebugF("delivered: ~p ~n", [ack]),
    Ack = #'basic.ack'{delivery_tag = DeliveryTag},
    Frame = assemble_frame(1, Ack, Protocol),
    ts_mon:add({count, amqp_delivered}),
    NewAckBuf = case AckBuf of
        nodata -> Frame;
        _ -> <<AckBuf/binary, Frame/binary>>
    end,
    AMQPSession#amqp_session{ack_buf = NewAckBuf};
should_ack(AckBuf, #'basic.deliver'{}, AMQPSession) ->
    ?DebugF("delivered: ~p ~n", [noack]),
    ts_mon:add({count, amqp_delivered}),
    AMQPSession#amqp_session{ack_buf = AckBuf};
should_ack(AckBuf, Method = #'basic.ack'{}, AMQPSession) ->
    ?DebugF("publish confirm: ~p ~n", [ack]),
    NewAMQPSession = update_confirm_set(Method, AMQPSession),
    NewAMQPSession#amqp_session{ack_buf = AckBuf};
should_ack(AckBuf, Method = #'basic.nack'{}, AMQPSession) ->
    ?DebugF("publish confirm: ~p ~n", [nack]),
    NewAMQPSession = update_confirm_set(Method, AMQPSession),
    NewAMQPSession#amqp_session{ack_buf = AckBuf};
should_ack(AckBuf, _Method, AMQPSession) ->
    ?DebugF("delivered: ~p ~n", [other]),
    AMQPSession#amqp_session{ack_buf = AckBuf}.

update_confirm_set(#'basic.ack'{delivery_tag = SeqNo,
                                multiple     = Multiple},
                   AMQPSession = #amqp_session{unconfirmed_set = USet}) ->
    AMQPSession#amqp_session{unconfirmed_set =
                             update_unconfirmed(ack, SeqNo, Multiple, USet)};
update_confirm_set(#'basic.nack'{delivery_tag = SeqNo,
                                 multiple     = Multiple},
                   AMQPSession = #amqp_session{unconfirmed_set = USet}) ->
    AMQPSession#amqp_session{unconfirmed_set =
                update_unconfirmed(nack, SeqNo, Multiple, USet)}.

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

decode_and_check(Data, Expecting, State, Protocol) ->
    case decode_frame(Protocol, Data) of
        {error, Reason} ->
            ?DebugF("decode error: ~p~n", [Reason]),
            ts_mon:add({count, amqp_error}),
            {fail, {State#state_rcv{ack_done = true}, [], true}};
        {ok, heartbeat, Left} ->
            {ok, heartbeat, {State#state_rcv{ack_done = false, acc = Left},
                             [], true}};
        {ok, Method, Left} ->
            check(Expecting, Method, State, Left);
        {ok, Method, _Content, Left} ->
            check(Expecting, Method, State, Left);
        {incomplete, Left} ->
            ?DebugF("incomplete frame: ~p~n", [Left]),
            {fail, {State#state_rcv{ack_done = false, acc = Left}, [], false}}
    end.

check(Expecting, Method, State, Left) ->
    ?DebugF("receive from server: ~p~n", [Method]),
    case {Expecting, element(1, Method)} of
        {E, M} when E =:= M ->
            {ok, Method,
             {State#state_rcv{ack_done = true, acc = Left}, [], false}};
        _ ->
            ts_mon:add({count, amqp_unexpedted}),
            ?DebugF("unexpected_method: ~p, expecting ~p~n",
                    [Method, Expecting]),
            {fail, {State#state_rcv{ack_done = true}, [], true}}
    end.

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
    process_channel_frame(Frame, Channel, AState, Left).

process_channel_frame(Frame, Channel, AState, Left) ->
    case rabbit_command_assembler:process(Frame, AState) of
        {ok, NewAState} ->
            put({channel, Channel}, NewAState),
            {ok, none, Left};
        {ok, Method, NewAState} ->
            put({channel, Channel}, NewAState),
            {ok, Method, Left};
        {ok, Method, Content, NewAState} ->
            put({channel, Channel}, NewAState),
            {ok, Method, Content, Left};
        {error, Reason} -> {error, Reason}
    end.
