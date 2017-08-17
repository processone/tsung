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

-module(ts_websocket).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_websocket.hrl").

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
decode_buffer(Buffer,#websocket_session{}) ->
    case websocket:decode(Buffer) of
        more -> <<>>;
        {_Opcode, Payload, _Rest} -> Payload
    end.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #websocket_session{}.

dump(A,B) ->
    ts_plugin:dump(A,B).
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request ,
%% Args:	record
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#websocket_request{type = connect, path = Path,
                               subprotos = SubProtocol, version = Version,
                               origin = Origin},
            State=#state_rcv{session = WebsocketSession}) ->
    {Request, Accept} = websocket:get_handshake(State#state_rcv.host, Path,
                                                SubProtocol, Version, Origin),
    {Request, WebsocketSession#websocket_session{status = waiting_handshake,
                                                 accept = Accept}};
get_message(#websocket_request{type = message, data = Data, frame = Frame},
            #state_rcv{session = WebsocketSession})
  when WebsocketSession#websocket_session.status == connected ->
    ResultData = case Frame of
        "text" -> websocket:encode_text(list_to_binary(Data));
        _ -> websocket:encode_binary(list_to_binary(Data))
    end,
    {ResultData, WebsocketSession};
get_message(#websocket_request{type = close},
            #state_rcv{session = WebsocketSession})
  when WebsocketSession#websocket_session.status == connected ->
    {websocket:encode_close(<<"close">>), WebsocketSession}.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:	Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, acc = [], datasize=0}, [], true};
%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize= 0}) ->
    parse(Data, State#state_rcv{datasize= size(Data)});

%% handshake stage, parse response, and validate
parse(Data, State=#state_rcv{acc = [],
                             session = WebsocketSession})
  when WebsocketSession#websocket_session.status == waiting_handshake ->
    Acc = list_to_binary(State#state_rcv.acc),
    Header = <<Acc/binary, Data/binary>>,
    Accept = WebsocketSession#websocket_session.accept,

    case websocket:check_handshake(Header, Accept) of
        ok ->
            ?Debug("handshake success:~n"),
            ts_mon_cache:add({count, websocket_succ}),
            {State#state_rcv{ack_done = true,
                             session = WebsocketSession#websocket_session{
                                         status = connected}}, [], false};
        {error, _Reason} ->
            ?DebugF("handshake fail: ~p~n", [_Reason]),
            ts_mon_cache:add({count, websocket_fail}),
            {State#state_rcv{ack_done = true}, [], true}
    end;

%% normal websocket message
parse(Data, State=#state_rcv{acc = [], session = WebsocketSession})
  when WebsocketSession#websocket_session.status == connected ->
    case websocket:decode(Data) of
        {?OP_CLOSE, _Reason, _} ->
            ?DebugF("receive close from server: ~p~n", [_Reason]),
            {State#state_rcv{ack_done = true}, [], true};
        {_Opcode, _Payload, Left} ->
            ?DebugF("receive from server: ~p ~p~n", [_Opcode, _Payload]),
            {State#state_rcv{ack_done = true, acc = Left}, [], false};
        more ->
            ?DebugF("receive incomplete frame from server: ~p~n", [Data]),
            {State#state_rcv{ack_done = false, acc = Data}, [], false}
    end;
%% more data, add this to accumulator and parse, update datasize
parse(Data, State=#state_rcv{acc = Acc, datasize = DataSize}) ->
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary, Data/binary >>,
          State#state_rcv{acc = [], datasize = NewSize}).

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data, State).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
	ts_config_websocket:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: we dont actually do anything
%% Returns: #websocket_request
%%----------------------------------------------------------------------
add_dynparams(true, {DynVars, _S},
              Param = #websocket_request{type = message, data = Data},
              _HostData) ->
    NewData = ts_search:subst(Data, DynVars),
    Param#websocket_request{data = NewData};
add_dynparams(true, {DynVars, _S},
              Param = #websocket_request{type = connect, path = Path},
              _HostData) ->
    NewPath = ts_search:subst(Path, DynVars),
    Param#websocket_request{path = NewPath};
add_dynparams(_Bool, _DynData, Param, _HostData) ->
    Param#websocket_request{}.
