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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-module(ts_http).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_http.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/2,
         session_defaults/0,
         dump/2,
         parse/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, "parse"|"no_ack"|"local", "true"|"false"}
%%----------------------------------------------------------------------
session_defaults() ->
    %% we parse the server response, and continue if the tcp
    %% connection is closed
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #http{}.

%% @spec decode_buffer(Buffer::binary(),Session::record(http)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#http{chunk_toread = -1, compressed={_,false}}) ->
    Buffer;
decode_buffer(Buffer,#http{chunk_toread = -1, compressed={_,Val}}) ->
    {Headers, CompressedBody} = split_body(Buffer),
    Body = decompress(CompressedBody, Val),
    << Headers/binary,  "\r\n\r\n", Body/binary >>;
decode_buffer(Buffer,#http{compressed={_,Comp}})->
    {Headers, Body} = decode_chunk(Buffer),
    ?DebugF("body is ~p~n",[Body]),
    RealBody = decompress(Body, Comp),
    ?DebugF("decoded buffer: ~p",[RealBody]),
    <<Headers/binary, "\r\n\r\n", RealBody/binary >>.

%% @spec dump(protocol, {Request::ts_request(),Session::term(), Id::integer(),
%%             Host::string(),DataSize::integer()}) -> ok
%% @doc log request and response summary
%% @end
dump(protocol,{#ts_request{param=HttpReq},HttpResp,UserId,Server,Size})->
    Status = case element(2,HttpResp#http.status) of
                 none -> "error_no_http_status"; % something is really wrong here ... http 0.9 response ?
                 Int when is_integer(Int) ->
                     integer_to_list(Int)
             end,
    Match = case erase(last_match) of
                undefined ->
                    "";
                {count, Val} ->
                    atom_to_list(Val)
            end,
    Error = case erase(protocol_error) of
                undefined ->
                    "";
                Err ->
                    atom_to_list(Err)
            end,
    Data=ts_utils:join(";",[integer_to_list(UserId),
                            atom_to_list(HttpReq#http_request.method), Server,
                            get(last_url), Status,integer_to_list(Size),Match, Error]),
    ts_mon:dump({protocol, self(), Data });
dump(_,_) ->
    ok.


%%----------------------------------------------------------------------
%% Function: get_message/21
%% Purpose: Build a message/request ,
%% Args:    #http_request
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req=#http_request{url=URL},#state_rcv{session=S}) ->
    put(last_url,URL),
    {get_message2(Req),S}.
get_message2(Req=#http_request{method=get}) ->
    ts_http_common:http_no_body(?GET, Req);

get_message2(Req=#http_request{method=head}) ->
    ts_http_common:http_no_body(?HEAD, Req);

get_message2(Req=#http_request{method=delete}) ->
    ts_http_common:http_no_body(?DELETE, Req);

get_message2(Req=#http_request{method=post}) ->
    ts_http_common:http_body(?POST, Req);

get_message2(Req=#http_request{method=options}) ->
    ts_http_common:http_no_body(?OPTIONS, Req);

get_message2(Req=#http_request{method=put}) ->
    ts_http_common:http_body(?PUT, Req).

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: Parse the given data and return a new state
%% Args:    Data (binary)
%%            State (record)
%% Returns: {NewState, Options for socket (list), Close}
%%----------------------------------------------------------------------
parse(Data, State) ->
    ts_http_common:parse(Data, State).

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data, State).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_http:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%          this is used for ex. for Cookies in HTTP
%% Args: Subst (true|false), DynData = #dyndata, Param = #http_request,
%%                                               HostData  = {Hostname, Port}
%% Returns: #http_request or { #http_request, {Host, Port, Scheme}}
%%----------------------------------------------------------------------
add_dynparams(false, DynData, Param, HostData) ->
    add_dynparams(DynData#dyndata.proto, Param, HostData);
add_dynparams(true, DynData, OldReq=#http_request{url=OldUrl}, HostData={PrevHost, PrevPort, PrevProto}) ->
    Req = subst(OldReq, DynData#dyndata.dynvars),
    case Req#http_request.url of
        OldUrl ->
            add_dynparams(DynData#dyndata.proto,Req, HostData);
        "http" ++ Rest -> % URL has changed and is absolute
            URL=ts_config_http:parse_URL(Req#http_request.url),
            ?DebugF("URL dynamic subst: ~p~n",[URL]),
            NewPort = ts_config_http:set_port(URL),
            NewReq  = add_dynparams(DynData#dyndata.proto,
                                    Req#http_request{host_header=undefined},
                                    {URL#url.host, NewPort, PrevProto, URL#url.scheme}), % add scheme
            case OldUrl of
                "http"++_ -> % old url absolute: useproxy must be true
                    NewReq#http_request{url="http"++Rest};
                _ ->
                    NewUrl=ts_config_http:set_query(URL),
                    {NewReq#http_request{url=NewUrl}, {URL#url.host, NewPort,ts_config_http:set_scheme({URL#url.scheme,PrevProto})}}
                end;
        _ -> % Same host:port
            add_dynparams(DynData#dyndata.proto, Req, HostData)
    end.

%% Function: add_dynparams/3
add_dynparams(DynData,Param=#http_request{host_header=undefined}, HostData )->
    Header = case HostData of
                 {Host,80, _,http}->
                     Host;
                 {Host,443,_,https}->
                     Host;
                 {Host,Port,_,_} ->
                     Host++":"++ integer_to_list(Port);
                 {Host,Port,_Proto} ->
                     Host++":"++ integer_to_list(Port)
             end,
    ?DebugF("set host header dynamically: ~s~n",[Header]),
    add_dynparams(DynData, Param#http_request{host_header=Header},HostData);
%% no cookies
add_dynparams(#http_dyndata{cookies=[],user_agent=UA},Param, _) ->
    Param#http_request{user_agent=UA};
%% cookies
add_dynparams(#http_dyndata{cookies=DynCookie,user_agent=UA}, Req, _) ->
    %% FIXME: should we use the Port value in the Cookie ?
    Cookie=DynCookie++Req#http_request.cookie,
    Req#http_request{cookie=Cookie,user_agent=UA}.

init_dynparams() ->
    %% FIXME: optimization: suppress this call if we don't need
    %% customised users agents
    UserAgent = ts_session_cache:get_user_agent(),
    #dyndata{proto=#http_dyndata{user_agent=UserAgent}}.


%%----------------------------------------------------------------------
%% @spec subst(Req::#http_request{}, DynData::#dynvars{} ) -> #http_request{}
%% @doc Replace on the fly dynamic element of the HTTP request For
%%          the moment, we only do dynamic substitution in URL, body,
%%          userid, passwd, because we see no need for the other HTTP
%%          request parameters.
%% @end
%%----------------------------------------------------------------------
subst(Req=#http_request{url=URL, body=Body, headers = Headers, userid=UserId, passwd=Passwd}, DynData) ->
    Req#http_request{url = escape_url(ts_search:subst(URL, DynData)),
             body   = ts_search:subst(Body, DynData),
             headers = lists:foldl(fun ({Name, Value}, Result) ->
                                           [{Name, ts_search:subst(Value, DynData)} | Result]
                                   end, [], Headers),
             userid = ts_search:subst(UserId, DynData),
             passwd = ts_search:subst(Passwd, DynData)}.

%% URL substitution, we must escape some characters
%% currently, we only handle space conversion to %20
escape_url(URL)->
    re:replace(URL," ","%20",[{return,list},global]).


decompress(Buffer,gzip)->
    zlib:gunzip(Buffer);
decompress(Buffer,uncompress)->
    zlib:uncompress(Buffer);
decompress(Buffer,deflate)->
    zlib:unzip(Buffer);
decompress(Buffer,false)->
    Buffer;
decompress(Buffer,Else)->
    ?LOGF("Unknown compression method, skip decompression ~p",[Else],?WARN),
    Buffer.

decode_chunk(Data)->
    decode_chunk_header(Data,<<>>).

decode_chunk_header(<<CRLF:4/binary, Data/binary >>,Headers) when CRLF == << "\r\n\r\n">> ->
    decode_chunk_size(Data,Headers,<< >>, << >>);
decode_chunk_header(<<CRLF:1/binary, Data/binary >>, Head) ->
    decode_chunk_header(Data, <<Head/binary, CRLF/binary>> ).

decode_chunk_size(<< >>, Headers, Body, _Digits) ->
    {Headers, Body};
decode_chunk_size(<<Head:2/binary >>, Headers, Body, <<>>) when Head ==  << "\r\n" >> ->
    %last CRLF, remove
    {Headers, Body};
decode_chunk_size(<<Head:2/binary, Data/binary >>, Headers, Body, <<>>) when Head ==  << "\r\n" >> ->
    % CRLF but no digits, end of chunk
    ?Debug("decode chunk: crlf, no digit"),
    decode_chunk_size(Data, Headers, Body, <<>>);
decode_chunk_size(<<Head:2/binary, Data/binary >>, Headers, Body,Digits) when Head ==  << "\r\n" >> ->
    case httpd_util:hexlist_to_integer(binary_to_list(Digits)) of
        0 ->
            decode_chunk_size(Data, Headers, Body ,<<>>);
        Size ->
            ?DebugF("decode chunk size ~p~n",[Size]),
            << Chunk:Size/binary, Tail/binary >> = Data,
            decode_chunk_size(Tail, Headers, << Body/binary, Chunk/binary>> ,<<>>)
    end;
decode_chunk_size(<<Digit:1/binary, Data/binary >>, Headers, Body, PrevDigit) ->
    ?DebugF("chunk one digit ~p~n",[Digit]),
    decode_chunk_size(Data, Headers, Body, <<PrevDigit/binary, Digit/binary>>).

split_body(Data) ->
    case re:run(Data,"(.*)\r\n\r\n(.*)$",[{capture,all_but_first,binary},ungreedy,dotall]) of
        nomatch        -> Data;
        {match, [Header,Body]} -> {Header,<< Body/binary,"\n" >>};
        _              -> Data
    end.

