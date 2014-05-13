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

-include("ts_macros.hrl").
-include("ts_profile.hrl").
-include("ts_http.hrl").

-export([add_dynparams/4,
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
    UserAgent = ts_session_cache:get_user_agent(),
    #http{user_agent=UserAgent}.

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
dump(protocol, Args)->
    Data = dump2str(Args),
    ts_mon_cache:dump({protocol, self(), Data });
dump(protocol_local, Args)->
    Data = dump2str(Args),
    ?DebugF("local protocol: ~p",[Data]),
    ts_local_mon:dump({protocol, self(), Data });
dump(_,_) ->
    ok.

dump2str({#ts_request{param=HttpReq},HttpResp,UserId,Server,Size,Duration,Transactions})->
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
    Tr=ts_utils:log_transaction(Transactions),
    ts_utils:join(";",[integer_to_list(UserId),
                            atom_to_list(HttpReq#http_request.method), Server,
                            get(last_url), Status,integer_to_list(Size),
                            Duration,Tr,Match,Error,
                            HttpReq#http_request.tag]
                      ).


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
    ts_http_common:http_body(?DELETE, Req);

get_message2(Req=#http_request{method=post}) ->
    ts_http_common:http_body(?POST, Req);

get_message2(Req=#http_request{method=options}) ->
    ts_http_common:http_no_body(?OPTIONS, Req);

get_message2(Req=#http_request{method=put}) ->
    ts_http_common:http_body(?PUT, Req);

get_message2(Req=#http_request{method=patch}) ->
    ts_http_common:http_body(?PATCH, Req).

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
%% Args: Subst (true|false), {DynVars = #dynvars, #http_session}, Param = #http_request,
%%                                               HostData  = {Hostname, Port}
%% Returns: #http_request or { #http_request, {Host, Port, Scheme}}
%%----------------------------------------------------------------------
add_dynparams(false, {_DynVars, Session}, Param, HostData) ->
    add_dynparams(Session, Param, HostData);
add_dynparams(SubstParam,  {DynVars, Session}, OldReq=#http_request{url=OldUrl}, HostData={_PrevHost, _PrevPort, PrevProto}) ->
    Req = subst(SubstParam, OldReq, DynVars),
    case Req#http_request.url of
        OldUrl ->
            add_dynparams(Session,Req, HostData);
        "http" ++ Rest -> % URL has changed and is absolute
            URL=ts_config_http:parse_URL(Req#http_request.url),
            ?LOGF("URL dynamic subst: ~p~n",[URL],?INFO),
            NewPort = ts_config_http:set_port(URL),
            NewReq  = add_dynparams(Session,
                                    Req#http_request{host_header=undefined},
                                    {URL#url.host, NewPort, PrevProto, URL#url.scheme}), % add scheme
            case OldReq#http_request.use_proxy of
                true ->
                    NewReq#http_request{url="http"++Rest};
                _ ->
                    NewUrl=ts_config_http:set_query(URL),
                    {NewReq#http_request{url=NewUrl}, {URL#url.host, NewPort,ts_config_http:set_scheme({URL#url.scheme,PrevProto})}}
                end;
        _ -> % Same host:port
            add_dynparams(Session, Req, HostData)
    end.

%% Function: add_dynparams/3
add_dynparams(Session,Param=#http_request{host_header=undefined}, HostData )->
    Header = case HostData of
                 {Host,80, _,http}->
                     Host;
                 {Host,443,_,https}->
                     Host;
                 {Host,80,   ts_tcp}->
                     Host;
                 {Host,443,  ts_ssl}->
                     Host;
                 {Host,80,   ts_tcp6}->
                     ts_config_http:encode_ipv6_address(Host);
                 {Host,443,  ts_ssl6}->
                     ts_config_http:encode_ipv6_address(Host);
                 {Host,Port,_,_} ->
                     ts_config_http:encode_ipv6_address(Host)++":"++ integer_to_list(Port);
                 {Host,Port,_Proto} ->
                     ts_config_http:encode_ipv6_address(Host)++":"++ integer_to_list(Port)
             end,
    ?DebugF("set host header dynamically: ~s~n",[Header]),
    add_dynparams(Session, Param#http_request{host_header=Header},HostData);
%% no cookies
add_dynparams(#http{session_cookies=[],user_agent=UA},Param, _) ->
    Param#http_request{user_agent=UA};
%% cookies
add_dynparams(#http{session_cookies=DynCookie,user_agent=UA}, Req, _) ->
    %% FIXME: should we use the Port value in the Cookie ?
    Cookie=DynCookie++Req#http_request.cookie,
    Req#http_request{cookie=Cookie,user_agent=UA}.



%%----------------------------------------------------------------------
%% @spec subst(SubstParam::true|all_except_body, Req::#http_request{},
%%             DynData::#dynvars{} ) -> #http_request{}
%% @doc Replace on the fly dynamic element of the HTTP request For
%%          the moment, we only do dynamic substitution in URL, body,
%%          userid, passwd, because we see no need for the other HTTP
%%          request parameters.
%% @end
%%----------------------------------------------------------------------
subst(SubstParam, Req=#http_request{url=URL, body=Body, headers = Headers, oauth_url=OUrl,
                        oauth_access_token=AToken, oauth_access_secret=ASecret,digest_qop = QOP,
                        digest_cnonce=CNonce, digest_nc=Nc,digest_nonce=Nonce, digest_opaque=Opaque,
                        realm=Realm, userid=UserId, passwd=Passwd, cookie = Cookies,
                        content_type=ContentType}, DynVars) ->
    Req#http_request{url =  escape_url(ts_search:subst(URL, DynVars)),
             body   = case SubstParam of
                        true ->
                            ts_search:subst(Body, DynVars);
                        all_except_body ->
                            Body
                      end,
             headers = lists:foldl(fun ({Name, Value}, Result) ->
                                           [{Name, ts_search:subst(Value, DynVars)} | Result]
                                   end, [], Headers),
             oauth_access_token = ts_search:subst(AToken, DynVars),
             digest_nonce  = ts_search:subst(Nonce, DynVars),
             digest_cnonce = ts_search:subst(CNonce, DynVars),
             digest_nc     = ts_search:subst(Nc, DynVars),
             digest_opaque = ts_search:subst(Opaque, DynVars),
             digest_qop    = ts_search:subst(QOP, DynVars),
             realm         = ts_search:subst(Realm, DynVars),
             content_type  = ts_search:subst(ContentType, DynVars),
             oauth_access_secret = ts_search:subst(ASecret, DynVars),
             oauth_url = ts_search:subst(OUrl, DynVars),
             cookie = lists:foldl(
                        fun (#cookie{ value = Value } = C, Result) ->
                            [C#cookie{ value = ts_search:subst(Value, DynVars) }
                             | Result]
                        end, [], Cookies),
             userid = ts_search:subst(UserId, DynVars),
             passwd = ts_search:subst(Passwd, DynVars)}.

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

