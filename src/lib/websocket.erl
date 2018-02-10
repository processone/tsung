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

-module(websocket).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-export([get_handshake/5, check_handshake/2, encode_binary/1, encode_text/1,
         encode_close/1, encode/2, decode/1]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_websocket.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
get_handshake(Host, Path, SubProtocol, Version, Origin) ->
    {Key, Accept} = gen_accept_key(),
    Req = list_to_binary(["GET ", Path, " HTTP/1.1\r\n",
                          "Host: ", Host ,"\r\n",
                          "Upgrade: websocket\r\n",
                          "Connection: Upgrade\r\n",
                          "Origin: ",
                          case Origin of "" -> Host;
                                         _  -> Origin
                          end, "\r\n",
                          "Sec-WebSocket-Key: ", Key, "\r\n",
                          "Sec-WebSocket-Version: ", Version, "\r\n"]),
    SubProHeader = case SubProtocol of
        [] -> [];
        _ -> "Sec-WebSocket-Protocol: " ++ SubProtocol ++ "\r\n"
    end,
    Handshake = list_to_binary([Req, SubProHeader, "\r\n" ]),
    {Handshake, Accept}.

check_handshake(Response, Accept) ->
    ?DebugF("check handshake, response is : ~p~n",[Response]),
    [HeaderPart, _] = binary:split(Response, <<"\r\n\r\n">>),
    [StatusLine | Headers] = binary:split(HeaderPart, <<"\r\n">>, [global, trim]),

    Map = dict:new(),
    {Prefix, _} = split_binary(StatusLine, 12),
    [Version, Code] = binary:split(Prefix, <<" ">>),
    Map1 = dict:store("version", string:to_lower(binary_to_list(Version)), Map),
    Map2 = dict:store("status", binary_to_list(Code), Map1),

    MapFun = fun(HeaderLine, Acc) ->
            [Header, Value] = binary:split(HeaderLine, <<": ">>),
            HeaderStr = string:to_lower(binary_to_list(Header)),
            ValueStr = case HeaderStr of
                "sec-websocket-accept" -> binary_to_list(Value);
                _ -> string:to_lower(binary_to_list(Value))
            end,
            dict:store(HeaderStr, ValueStr, Acc)
    end,

    HeaderMap = lists:foldl(MapFun, Map2, Headers),
    RequiredHeaders = [{"Version", "HTTP/1.1"},
                       {"Status", "101"},
                       {"Upgrade", "websocket"},
                       {"Connection", "Upgrade"},
                       {"Sec-WebSocket-Accept", Accept}],
    lists:foldl(fun(_, Acc = {error, _}) -> Acc;
            ({Key, Value}, ok) ->
                TargetKey = string:to_lower(Key),
                TargetValue = case TargetKey of
                    "sec-websocket-accept" -> Value;
                    _ ->  string:to_lower(Value)
                end,
                case dict:is_key(TargetKey, HeaderMap) of
                    true -> case dict:find(TargetKey, HeaderMap) of
                            {ok, TargetValue} -> ok;
                            {ok, Other} -> {error, {mismatch, Key, Value, Other}}
                        end;
                    _ -> {error, {miss_headers, Key}}
                end end, ok, RequiredHeaders).

encode_binary(Data) ->
    encode(Data, ?OP_BIN).

encode_text(Data) ->
    encode(Data, ?OP_TEXT).

encode_close(Reason) ->
    %% According RFC6455, we shoud add a status code for close frame,
    %% check here: http://tools.ietf.org/html/rfc6455#section-7.4,
    %% we add a normal closure status code 1000 here.
    StatusCode = <<3, 232>>,
    Data = <<StatusCode/binary, Reason/binary>>,
    encode(Data, ?OP_CLOSE).

encode(Data, Opcode) ->
    Key = crypto:strong_rand_bytes(4),
    PayloadLen = erlang:size(Data),
    MaskedData = mask(Data, Key),
    Length = if
        PayloadLen < 126    -> <<PayloadLen:7>>;
        PayloadLen < 65536  -> <<126:7, PayloadLen:16>>;
        true                -> <<127:7, PayloadLen:64>>
    end,
    <<1:1, 0:3, Opcode:4, 1:1, Length/bitstring, Key/binary, MaskedData/bitstring>>.

decode(Data) ->
    parse_frame(Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_accept_key() ->
    random:seed(erlang:now()),
    Key = crypto:strong_rand_bytes(16),
    KeyStr = base64:encode_to_string(Key),
    Accept = binary:list_to_bin(KeyStr ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"),
    AcceptStr = base64:encode_to_string(crypto:hash(sha, Accept)),
    {KeyStr, AcceptStr}.

%%%===================================================================
%% NOTE: The code of the following two functions are borrowed from
%% https://github.com/wulczer/tsung_ws/blob/master/src/tsung/ts_websocket.erl.

% mask(binary, binary) -> binary
%
% Mask the given payload using a 4 byte masking key.
mask(Payload, MaskingKey) ->
    % create a mask with the same length as the payload by repeating
    % the masking key
    Div = size(Payload) div size(MaskingKey),
    Rem = size(Payload) rem size(MaskingKey),
    LongPart = binary:copy(MaskingKey, Div),
    Rest = binary:part(MaskingKey, {0, Rem}),
    Mask = << LongPart/bitstring, Rest/bitstring >>,
    % xor the payload and the mask
    crypto:exor(Payload, Mask).

% parse_payload(integer, binary) -> {integer, binary, binary} | more
%
% Try to parse out a frame payload from binary data. Gets passed an
% opcode and returns a tuple of opcode, payload and remaining data. If
% not enough data is available, return a more atom.
parse_payload(Opcode, << 0:1, % MASK
                         Length:7,
                         Payload:Length/binary,
                         Rest/bitstring >>)
  when Length < 126 ->
    {Opcode, Payload, Rest};
parse_payload(Opcode, << 0:1, % MASK
                         126:7,
                         Length:16,
                         Payload:Length/binary,
                         Rest/bitstring >>)
  when Length < 65536 ->
    {Opcode, Payload, Rest};
parse_payload(Opcode, << 0:1, % MASK
                         127:7,
                         0:1,
                         Length:63,
                         Payload:Length/binary,
                         Rest/bitstring >>) ->
    {Opcode, Payload, Rest};
parse_payload(_Opcode, _Data) ->
    more.


% parse_frame(binary) -> {integer, binary, binary} | more
%
% Try to parse out a WebSocket frame from binary data. Returns a tuple
% of opcode, payload and remaining data or a more atom if not enough
% data is available.
parse_frame(<< 1:1, % FIN
               0:3, % RSV
               Opcode:4, % OPCODE
               MaskLengthAndPayload/bitstring >>) ->
    parse_payload(Opcode, MaskLengthAndPayload);
parse_frame(_Data) ->
    more.
