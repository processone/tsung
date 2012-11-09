%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
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
%%%  the two.

-module(websocket).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-export([handshake_request/4, check_handshake/2, encode_binary/1, encode_text/1,
         encode_close/1, encode/2, decode/1]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_websocket.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
handshake_request(Host, Path, SubProtocol, Version) ->
    {Key, Accept} = gen_accept_key(),
    Req = list_to_binary(["GET ", Path, " HTTP/1.1\r\n",
                          "Host: ", Host ,"\r\n",
                          "Connection: Upgrade\r\n",
                          "Origin: http://", Host, "\r\n",
                          "Sec-WebSocket-Version: ", Version, "\r\n"]),
    Value = case SubProtocol of
                [] ->
                    [];
                _ ->
                    "Sec-WebSocket-Protocol: " ++ SubProtocol ++ "\r\n"
            end,
    Handshake = list_to_binary([
                                Req, Value,
                                "Sec-WebSocket-Key: ", Key, "\r\n",
                                "Pragma: no-cache:\r\n",
                                "Cache-control: no-cache:\r\n",
                                "Upgrade: WebSocket\r\n\r\n"
                               ]),
    {Handshake, Accept}.

check_handshake(Response, Accept) ->
    ?DebugF("Check handshake, response is : ~p~n",[Response]),
    RespString = binary_to_list(Response),
    case parse_headers(#ws_http{}, RespString) of
        {ok, Result=#ws_http{status = 101}} ->
            RequiredHeaders = [
                               {'Upgrade', "websocket"},
                               {'Connection', "upgrade"},
                               {'Sec-WebSocket-Accept', ignore}
                              ],
            case check_headers(Result#ws_http.headers,
                                          RequiredHeaders) of
                true ->
                    RecvAcc = Result#ws_http.accept,
                    case RecvAcc of
                        Accept ->
                            ok;
                        _ ->
                            {error, mismatch_accept}
                    end;
                Miss ->
                    ?LOGF("Websocket handshake: Missing header ~p~n",[Miss],?DEB),
                    {error, miss_headers}
            end;
        _ ->
            {error, error_status}
    end.

encode_binary(Data) ->
    encode(Data, ?OP_BIN).

encode_text(Data) ->
    encode(Data, ?OP_TEXT).

encode_close(Reason) ->
    encode(Reason, ?OP_CLOSE).

encode(Data, Opcode) ->
    Key = crypto:rand_bytes(4),
    Len = erlang:size(Data),
    if
        Len < 126 ->
            list_to_binary([<<1:1, 0:3, Opcode:4, 1:1, Len:7>>, Key,
                            mask(Key, Data)]);
        Len < 65536 ->
            list_to_binary([<<1:1, 0:3, Opcode:4, 1:1, 126:7, Len:16>>, Key,
                            mask(Key, Data)]);
        true ->
            list_to_binary([<<1:1, 0:3, Opcode:4, 1:1, 127:7, Len:64>>, Key,
                            mask(Key, Data)])
    end.

decode(Data) ->
    handle_data(Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_accept_key() ->
    random:seed(erlang:now()),
    Key = crypto:rand_bytes(16),
    KeyString = base64:encode_to_string(Key),
    A = binary:list_to_bin(KeyString ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"),
    Accept = base64:encode_to_string(crypto:sha(A)),
    {KeyString, Accept}.

encode_key(Key) ->
    A = binary:encode_unsigned(Key),
    case size(A) of
        4 -> A;
        _Other ->
            Bits = (4 - _Other) * 8,
            <<0:Bits, A/binary>>
    end.

mask(Key, Data) ->
    K = binary:copy(Key, 512 div 32),
    <<LongKey:512>> = K,
    <<ShortKey:32>> = Key,
    mask(ShortKey, LongKey, Data, <<>>).

mask(Key, LongKey, Data, Accu) ->
    case Data of
        <<A:512, Rest/binary>> ->
            C = A bxor LongKey,
            mask(Key, LongKey, Rest, <<Accu/binary, C:512>>);
        <<A:32, Rest/binary>> ->
            C = A bxor Key,
            mask(Key, LongKey, Rest, <<Accu/binary, C:32>>);
        <<A:24>> ->
            <<B:24, _:8>> = encode_key(Key),
            C = A bxor B,
            <<Accu/binary, C:24>>;
        <<A:16>> ->
            <<B:16, _:16>> = encode_key(Key),
            C = A bxor B,
            <<Accu/binary, C:16>>;
        <<A:8>> ->
            <<B:8, _:24>> = encode_key(Key),
            C = A bxor B,
            <<Accu/binary, C:8>>;
        <<>> ->
            Accu
    end.

%%Last frame of a segment
handle_frame(1, ?OP_CONT, _Len,_Data) ->
    {close, seg_not_support};
handle_frame(_,_,Len,Data) when Len > size(Data) ->
    {incomplete, Data};
%%Frame w/o segment
handle_frame(1, Opcode, Len, Data) ->
    <<Data1:Len/binary, Rest/binary>> = Data,
    Result = case Opcode of
                 ?OP_BIN ->
                     Data1;
                 ?OP_TEXT ->
                     Data1;
                 ?OP_CLOSE ->
                     {close, close};
                 _Any ->
                     {close, error}
             end,

    case Rest of
        <<>> ->
            {Result, none};
        Left ->
            {Result, Left}
    end;
%%Cont. frame of a segment
handle_frame(0, ?OP_CONT, _Len, _Data) ->
    {close, seg_not_support};
%%first frame of a segment
handle_frame(0, _Opcode, _Len, _Data) ->
    {close, seg_not_support}.

handle_data(<<Fin:1,0:3, Opcode:4,0:1, PayloadLen:7, PayloadData/binary>>)
  when PayloadLen < 126 andalso PayloadLen =< size(PayloadData) ->
    handle_frame(Fin, Opcode, PayloadLen, PayloadData);
handle_data(<<Fin:1,0:3, Opcode:4,0:1,126:7, PayloadLen:16,
              PayloadData/binary>>)
  when PayloadLen =< size(PayloadData) ->
    handle_frame(Fin, Opcode, PayloadLen, PayloadData);
handle_data(<<Fin:1, 0:3, Opcode:4, 0:1, 127:7, 0:1, PayloadLen:63,
              PayloadData/binary>>)
  when PayloadLen =< size(PayloadData) ->
    handle_frame(Fin, Opcode, PayloadLen, PayloadData);

%% Error, the MSB of extended payload length must be 0
handle_data(<<_Fin:1, 0:3, _Opcode:4,_:1, 127:7, 1:1, _PayloadLen:63,
              _PayloadData/binary>>) ->
    {close, error};
handle_data(<<_Fin:1, 0:3, _Opcode:4, 1:1, _PayloadLen:7,
              _PayloadData/binary>>) ->
    %% Error, Server to client message can't be masked
    {close, masked}.


%%--------------------------------------------------------------------
%% @spec parse_headers(record(#ws_http), Header::string()) -> {ok, record(ws_http)}
%% @doc Parse HTTP headers line by line
%% @end
%%--------------------------------------------------------------------
parse_headers(Http, Tail) ->
    case ts_http_common:get_line(Tail) of
        {line, Line, Tail2} ->
            parse_headers(parse_line(Line, Http), Tail2);
        {lastline, Line, _} ->
            {ok, parse_line(Line, Http)}
    end.

check_headers(Headers, RequiredHeaders) ->
    F = fun({Tag, Val}) ->
                %% see if the required Tag is in the Headers
                case proplists:get_value(Tag, Headers) of
                    %% header not found, keep in list
                    undefined -> true;
                    HVal ->
                        case Val of
                            %% ignore value -> ok, remove from list
                            ignore -> false;
                            %% expected val -> ok, remove from list
                            HVal -> false;
                            %% val is different, keep in list
                            H ->
                                case string:to_lower(HVal) of
                                    Val -> false;
                                    _ ->
                                        ?LOGF("wrong val ~p ~p~n",[HVal,Val],?DEB),
                                        true
                                end
                        end
                end
        end,

    case lists:filter(F, RequiredHeaders) of
        [] -> true;
        MissingHeaders -> MissingHeaders
    end.

%%--------------------------------------------------------------------
%% @spec parse_status(Status::string(), record(#ws_http)) -> record(ws_http)
%% @doc Parse HTTP status
%% @end
%%--------------------------------------------------------------------
parse_status([A,B,C|_], Http) ->
    Status=list_to_integer([A,B,C]),
    ts_mon:add({ count, Status }),
    Http#ws_http{status = Status}.

%%--------------------------------------------------------------------
%% @spec parse_line(Header::string(), record(#ws_http)) ->record(ws_http)
%% @doc Parse a HTTP header and look for Websocket headers
%% @end
%%--------------------------------------------------------------------
parse_line("http/1.1 " ++ TailLine, Http)->
    parse_status(TailLine, Http);
parse_line("http/1.0 " ++ TailLine, Http)->
    parse_status(TailLine, Http#ws_http{close=true});

parse_line("upgrade: " ++ TailLine, Http) ->
    Headers = [{'Upgrade', TailLine} | Http#ws_http.headers],
    Http#ws_http{headers=Headers};
parse_line("connection: " ++ TailLine, Http) ->
    Headers = [{'Connection', TailLine} | Http#ws_http.headers],
    Http#ws_http{headers=Headers};
parse_line("sec-websocket-accept: " ++ TailLine, Http) ->
    Headers = [{'Sec-WebSocket-Accept', TailLine} |
               Http#ws_http.headers],
    Http#ws_http{headers=Headers, accept=TailLine};
parse_line(_Line, Http) ->
    Http.
