%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2004 IDEALX
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

%%% common functions used by http clients to:
%%%  - set HTTP requests
%%%  - parse HTTP response from server

-module(ts_http_common).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_http.hrl").

-include("ts_config.hrl").

-export([
         http_get/1,
         http_post/1,
         http_body/2,
         http_no_body/2,
         parse/2,
         parse_req/1,
         parse_req/2,
         get_line/1
        ]).

%%----------------------------------------------------------------------
%% Func: http_get/1
%%----------------------------------------------------------------------
http_get(Args) ->
    http_no_body(?GET, Args).

%%----------------------------------------------------------------------
%% Func: http_get/1
%% Args: #http_request
%%----------------------------------------------------------------------
%% normal request
http_no_body(Method,#http_request{url=URL, version=Version, cookie=Cookie,
                              headers=Headers, user_agent=UA,
                              get_ims_date=undefined, soap_action=SOAPAction,
                              host_header=Host}=Req)->
    ?DebugF("~p ~p~n",[Method,URL]),
    R = list_to_binary([Method, " ", URL," ", "HTTP/", Version, ?CRLF,
                    set_header("Host",Host,Headers, ""),
                    set_header("User-Agent",UA,Headers, ?USER_AGENT),
                    set_header("Content-Type", undefined, Headers, undefined),
                    set_header("Content-Length", undefined, Headers, undefined),
                    authenticate(Req),
                    oauth_sign(Method,Req),
                    soap_action(SOAPAction),
                    set_cookie_header({Cookie, Host, URL}),
                    headers(Headers),
                    ?CRLF]),
    ?DebugF("Headers~n-------------~n~s~n",[R]),
    R;
%% if modified since request
http_no_body(Method,#http_request{url=URL, version=Version, cookie=Cookie,
                             headers=Headers, user_agent=UA,
                             get_ims_date=Date, soap_action=SOAPAction,
                             host_header=Host}=Req) ->
    ?DebugF("~p ~p~n",[Method, URL]),
    list_to_binary([Method, " ", URL," ", "HTTP/", Version, ?CRLF,
                    ["If-Modified-Since: ", Date, ?CRLF],
                    set_header("Host",Host,Headers, ""),
                    set_header("User-Agent",UA,Headers, ?USER_AGENT),
                    set_header("Content-Type", undefined, Headers, undefined),
                    set_header("Content-Length", undefined, Headers, undefined),
                    soap_action(SOAPAction),
                    authenticate(Req),
                    oauth_sign(Method,Req),
                    set_cookie_header({Cookie, Host, URL}),
                    headers(Headers),
                    ?CRLF]).

%%----------------------------------------------------------------------
%% Func: http_post/1
%%----------------------------------------------------------------------
http_post(Args) ->
    http_body(?POST, Args).

%%----------------------------------------------------------------------
%% Func: http_body/2
%% Args: #http_request
%%----------------------------------------------------------------------
http_body(Method,#http_request{url=URL, version=Version,
                               cookie=Cookie, headers=Headers,
                               user_agent=UA, soap_action=SOAPAction,
                               content_type=ContentType,
                               body=Content, host_header=Host}=Req) ->
    ContentLength=integer_to_list(size(Content)),
    ?DebugF("Content Length of POST: ~p~n.", [ContentLength]),

    H = [Method, " ", URL," ", "HTTP/", Version, ?CRLF,
               set_header("Host",Host,Headers, ""),
               set_header("User-Agent",UA,Headers, ?USER_AGENT),
               set_header("Content-Type", ContentType, Headers, undefined),
               set_header("Content-Length", ContentLength, Headers, undefined),
               authenticate(Req),
               soap_action(SOAPAction),
               oauth_sign(Method, Req),
               set_cookie_header({Cookie, Host, URL}),
               headers(Headers),
               ?CRLF
              ],
    ?LOGF("Headers~n-------------~n~s~n",[H],?DEB),
    list_to_binary([H, Content ]).

%%----------------------------------------------------------------------
%% some HTTP headers functions
%%----------------------------------------------------------------------
authenticate(#http_request{userid=undefined})-> [];
authenticate(#http_request{passwd=undefined})-> [];
authenticate(#http_request{passwd=Passwd, auth_type="basic",userid=UserId})->
    AuthStr = ts_utils:encode_base64(lists:append([UserId,":",Passwd])),
    ["Authorization: Basic ",AuthStr,?CRLF];

authenticate(#http_request{method=Method, passwd=Passwd,userid=UserId,
                           auth_type="digest", realm=Realm,
                           digest_cnonce=CNonce, digest_nc=NC, digest_qop=QOP,
                           digest_nonce=Nonce, digest_opaque=Opaque,
                           url=URL
                            }) ->
    HA1 = md5_hex(string:join([UserId, Realm, Passwd], ":")),
    HA2 = md5_hex(string:join([string:to_upper(atom_to_list(Method)), URL], ":")),
    Response = digest_response({HA1, Nonce,NC, CNonce,QOP,HA2}),
    digest_header(UserId,Realm,Nonce,URL,QOP,NC,CNonce,Response,Opaque).

digest_header(User,Realm,Nonce,URI, QOP,NC,CNonce, Response,Opaque) ->
    Acc= ["Authorization: Digest "
          "username=\"",User,"\", ",
          "realm=\"", Realm, "\", ",
          "nonce=\"", Nonce, "\", ",
          "uri=\"", URI, "\", ",
          "response=\"", Response, "\""],
    digest_header_opt(Acc, QOP, NC, CNonce, Opaque).

%% qop and opaque are undefined
digest_header_opt(Acc, undefined, _NC, _CNonce, undefined) ->
    [Acc, ?CRLF];

digest_header_opt(Acc, QOP, NC, CNonce, Opaque) when is_list(Opaque)->
     NewAcc=[Acc,", opaque=\"",Opaque,"\""],
    digest_header_opt(NewAcc,QOP,NC,CNonce,undefined);

digest_header_opt(Acc, QOP, NC, CNonce,undefined) ->
    NewAcc=[Acc,", qop=\"",QOP,"\"",
                ", nc=", NC,
                ", cnonce=\"", CNonce, "\""
           ],
    digest_header_opt(NewAcc,undefined,"","",undefined).

digest_response({HA1,Nonce, _NC, _CNonce, undefined, HA2})-> %qop undefined
    md5_hex(string:join([HA1, Nonce, HA2], ":"));
digest_response({HA1,Nonce, NC, CNonce, QOP, HA2})->
    md5_hex(string:join([HA1,Nonce,NC,CNonce,QOP,HA2], ":")).

md5_hex(String)->
    lists:flatten([io_lib:format("~2.16.0b",[N])||N<-binary_to_list(erlang:md5(String))]).


oauth_sign(_, #http_request{oauth_consumer = undefined})->[];
oauth_sign(Method, #http_request{url=URL,
                         oauth_consumer=Consumer,
                         oauth_access_token=AccessToken,
                         oauth_access_secret=AccessSecret,
                         oauth_url=ServerURL})->
    %%UrlParams = oauth_uri:params_from_string(URL),
    [_He|Ta] = string:tokens(URL,"?"),
    UrlParams = oauth_uri:params_from_string(lists:flatten(Ta)),
    Params = oauth:signed_params(Method, ServerURL, UrlParams, Consumer, AccessToken, AccessSecret),
    ["Authorization: OAuth ", oauth_uri:params_to_header_string(Params),?CRLF].

%%----------------------------------------------------------------------
%% @spec set_header(Name::string, Val::string | undefined, Headers::List,
%%                  Default::string) -> list()
%% @doc If header Name is defined in Headers, print this one, otherwise,
%%      print the given Value (or the default one if undefined)
%% @end
%%----------------------------------------------------------------------
set_header(Name, Value, Headers, Default) when length(Headers) > 0 ->
    case lists:keysearch(string:to_lower(Name), 1, normalize_headers(Headers)) of
        {value, {_,Val}} -> [Name, ": ", Val, ?CRLF];
        false      -> set_header(Name,Value,[], Default)
    end;
set_header(_Name, undefined, [], undefined) ->
    [];
set_header(Name, undefined, [], Default) ->
    [Name++": ", Default, ?CRLF];
set_header(Name, Value, [], _) ->
    [Name++": ", Value, ?CRLF].

soap_action(undefined) -> [];
soap_action(SOAPAction) -> ["SOAPAction: \"", SOAPAction, "\"", ?CRLF].

% user defined headers
headers([]) -> [];
headers(Headers) ->
    HeadersToIgnore = ["host", "user-agent", "content-type", "content-length"],

    lists:foldl(fun({Name, Value}, Result) ->
        case lists:member(string:to_lower(Name), HeadersToIgnore) of
            true ->
                Result;
            _ ->
                [Name, ": ", Value, ?CRLF | Result]
        end
    end, [], lists:reverse(Headers)).

normalize_headers([]) -> [];
normalize_headers(Headers) ->
    lists:map(fun({Name, Value}) -> {string:to_lower(Name), Value} end, Headers).


%%----------------------------------------------------------------------
%% Function: set_cookie_header/1
%% Args: Cookies (list), Hostname (string), URL
%% Purpose: set Cookie: Header
%%----------------------------------------------------------------------
set_cookie_header({[], _, _})   -> [];
set_cookie_header({Cookies, Host, URL})->
    MatchDomain = fun (A) -> matchdomain_url(A,Host,URL) end,
    CurCookies = lists:filter(MatchDomain, Cookies),
    set_cookie_header(CurCookies, Host, []).

set_cookie_header([], _Host, [])    -> [];
set_cookie_header([], _Host, Acc)   -> [lists:reverse(Acc), ?CRLF];
set_cookie_header([Cookie|Cookies], Host, []) ->
    set_cookie_header(Cookies, Host, [["Cookie: ", cookie_rec2str(Cookie)]]);
set_cookie_header([Cookie|Cookies], Host, Acc) ->
    set_cookie_header(Cookies, Host, [["; ", cookie_rec2str(Cookie)]|Acc]).

cookie_rec2str(#cookie{key=Key, value=Val}) ->
    lists:append([Key,"=",Val]).

%%----------------------------------------------------------------------
%% Function: matchdomain_url/3
%% Purpose:  return a cookie only if domain match
%% Returns:  true|false
%%----------------------------------------------------------------------
matchdomain_url(Cookie, _Host, "http"++URL) -> % absolute URL, a proxy is used.
    %% FIXME: the domain stored is the domain of the proxy, we can't
    %% check the domain currently :( We assume it's OK
    %% FIXME: really check if it's a sub path; currently we only check
    %% that the path is somewhere in the URL which is obviously not
    %% the right thing to do.
    string:str(URL,Cookie#cookie.path) > 0;
matchdomain_url(Cookie, Host, URL) ->
    SubDomain = string:str([$.|Host],Cookie#cookie.domain),
    SubPath   = string:str(URL,Cookie#cookie.path), % FIXME:should use regexp:match
    case {SubDomain,SubPath} of
        {0,_} -> false;
        {_,1} -> true;
        {_,_} -> false
    end.


%%----------------------------------------------------------------------
%% Func: parse/2
%% Args: Data, State
%% Returns: {NewState, Options for socket (list), Close}
%% Purpose: parse the response from the server and keep information
%%  about the response if State#state_rcv.session
%%----------------------------------------------------------------------
parse(closed, State=#state_rcv{session=Http}) ->
    {State#state_rcv{session=reset_session(Http), ack_done = true}, [], true};

parse(Data, State=#state_rcv{session=HTTP}) when element(1,HTTP#http.status)  == none;
                                                 HTTP#http.partial == true ->

    List = binary_to_list(Data),
    TotalSize = size(Data),
    Header = State#state_rcv.acc ++ List,

    case parse_headers(HTTP, Header, State#state_rcv.host) of
        %% Partial header:
        {more, HTTPRec, Tail} ->
            ?LOGF("Partial Header: [HTTP=~p : Tail=~p]~n",[HTTPRec, Tail],?DEB),
            {State#state_rcv{ack_done=false,session=HTTPRec,acc=Tail},[],false};
        %% Complete header, chunked encoding
        {ok, Http=#http{content_length=0, chunk_toread=0}, Tail} ->
            NewCookies = concat_cookies(Http#http.cookie, Http#http.session_cookies),
            case parse_chunked(Tail, State#state_rcv{session=Http, acc=[]}) of
                {NewState=#state_rcv{ack_done=false, session=NewHttp}, Opts} ->
                    {NewState#state_rcv{session=NewHttp#http{session_cookies=NewCookies}}, Opts, false};
                {NewState=#state_rcv{session=NewHttp}, Opts} ->
                    {NewState#state_rcv{acc=[],session=NewHttp#http{session_cookies=NewCookies}}, Opts, Http#http.close}
            end;
        {ok, Http=#http{content_length=0, close=true}, _} ->
            %% no content length, close=true: the server will close the connection
            NewCookies = concat_cookies(Http#http.cookie, Http#http.session_cookies),
            {State#state_rcv{ack_done = false,
                             datasize = TotalSize,
                             session=Http#http{session_cookies=NewCookies}}, [], true};
        {ok, Http=#http{status={100,_}}, _} -> % Status 100 Continue, ignore.
            %% FIXME: not tested
            {State#state_rcv{ack_done=false,session=reset_session(Http)},[],false};
        {ok, Http, Tail} ->
            NewCookies = concat_cookies(Http#http.cookie, Http#http.session_cookies),
            check_resp_size(Http#http{session_cookies=NewCookies}, length(Tail), State#state_rcv{acc=[]}, TotalSize, State#state_rcv.dump)
    end;

%% continued chunked transfer
parse(Data, State=#state_rcv{session=Http}) when Http#http.chunk_toread >=0 ->
    ?DebugF("Parse chunk data = [~s]~n", [Data]),
    case read_chunk_data(Data,State,Http#http.chunk_toread,Http#http.body_size) of
        {NewState=#state_rcv{ack_done=false}, NewOpts}->
            {NewState, NewOpts, false};
        {NewState, NewOpts}->
            {NewState#state_rcv{acc=[]}, NewOpts, Http#http.close}
    end;

%% continued normal transfer
parse(Data,  State=#state_rcv{session=Http, datasize=PreviousSize}) ->
    DataSize = size(Data),
    ?DebugF("HTTP Body size=~p ~n",[DataSize]),
    CLength = Http#http.content_length,
    case Http#http.body_size + DataSize of
        CLength -> % end of response
            {State#state_rcv{session=reset_session(Http), acc=[], ack_done = true, datasize = DataSize+PreviousSize},
             [], Http#http.close};
        Size ->
            {State#state_rcv{session = Http#http{body_size = Size}, ack_done = false,
                             datasize = DataSize+PreviousSize}, [], false}
    end.

%%----------------------------------------------------------------------
%% Func: check_resp_size/5
%% Purpose: Check response size
%% Returns: {NewState= record(state_rcv), SockOpts, Close}
%%----------------------------------------------------------------------
check_resp_size(Http=#http{content_length=CLength, close=Close},
                CLength, State, DataSize, _Dump) ->
    %% end of response
    {State#state_rcv{session= reset_session(Http), ack_done = true, datasize = DataSize }, [], Close};
check_resp_size(Http=#http{content_length=CLength, close=Close},
                BodySize, State, DataSize, Dump) when BodySize > CLength ->
    ?LOGF("Error: HTTP Body (~p)> Content-Length (~p) !~n",
          [BodySize, CLength], ?ERR),
    log_error(Dump, error_http_bad_content_length),
    {State#state_rcv{session= reset_session(Http), ack_done = true,
                     datasize = DataSize }, [], Close};
check_resp_size(Http=#http{}, BodySize,  State, DataSize,_Dump) ->
    %% need to read more data
    {State#state_rcv{session  = Http#http{body_size = BodySize},
                     ack_done = false,
                     datasize = DataSize },[],false}.

%%----------------------------------------------------------------------
%% Func: parse_chunked/2
%% Purpose: parse 'Transfer-Encoding: chunked' for HTTP/1.1
%% Returns: {NewState= record(state_rcv), SockOpts, Close}
%%----------------------------------------------------------------------
parse_chunked(Body, State)->
    ?DebugF("Parse chunk data = [~s]~n", [Body]),
    read_chunk(list_to_binary(Body), State, 0, 0).

%%----------------------------------------------------------------------
%% Func: read_chunk/4
%% Purpose: the real stuff for parsing chunks is here
%% Returns: {NewState= record(state_rcv), SockOpts, Close}
%%----------------------------------------------------------------------
read_chunk(<<>>, State, Int, Acc) ->
    ?LOGF("No data in chunk [Int=~p, Acc=~p] ~n", [Int,Acc],?INFO),
    AccInt = list_to_binary(httpd_util:integer_to_hexlist(Int)),
    { State#state_rcv{acc = AccInt, ack_done = false }, [] }; % read more data
%% this code has been inspired by inets/http_lib.erl
%% Extensions not implemented
read_chunk(<<Char:1/binary, Data/binary>>, State=#state_rcv{session=Http}, Int, Acc) ->
    case Char of
    <<C>> when $0=<C,C=<$9 ->
        read_chunk(Data, State, 16*Int+(C-$0), Acc+1);
    <<C>> when $a=<C,C=<$f ->
        read_chunk(Data, State, 16*Int+10+(C-$a), Acc+1);
    <<C>> when $A=<C,C=<$F ->
        read_chunk(Data, State, 16*Int+10+(C-$A), Acc+1);
    <<?CR>> when Int>0 ->
        read_chunk_data(Data, State, Int+3, Acc+1);
    <<?CR>> when Int==0, size(Data) == 3 -> %% should be the end of transfer
            ?DebugF("Finish tranfer chunk ~p~n", [binary_to_list(Data)]),
            {State#state_rcv{session= reset_session(Http), ack_done = true,
                             datasize = Acc %% FIXME: is it the correct size?
                            }, []};
    <<?CR>> when Int==0, size(Data) < 3 ->  % lack ?CRLF, continue
            { State#state_rcv{acc =  <<48, ?CR , Data/binary>>, ack_done=false }, [] };
    <<C>> when C==$ -> % Some servers (e.g., Apache 1.3.6) throw in
               % additional whitespace...
        read_chunk(Data, State, Int, Acc+1);
    _Other ->
            ?LOGF("Unexpected error while parsing chunk ~p~n", [_Other] ,?WARN),
            log_error(State#state_rcv.dump, error_http_unexpected_chunkdata),
            {State#state_rcv{session= reset_session(Http), ack_done = true}, []}
    end.


%%----------------------------------------------------------------------
%% Func: read_chunk_data/4
%% Purpose: read 'Int' bytes of data
%% Returns: {NewState= record(state_rcv), SockOpts}
%%----------------------------------------------------------------------
read_chunk_data(Data, State=#state_rcv{acc=[]}, Int, Acc) when size(Data) > Int->
    ?DebugF("Read ~p bytes of chunk with size = ~p~n", [Int, size(Data)]),
    <<_NewData:Int/binary, Rest/binary >> = Data,
    read_chunk(Rest, State,  0, Int + Acc);
read_chunk_data(Data, State=#state_rcv{acc=[],session=Http}, Int, Acc) -> % not enough data in buffer
    BodySize = size(Data),
    ?DebugF("Partial chunk received (~p/~p)~n", [BodySize,Int]),
    NewHttp = Http#http{chunk_toread   = Int-BodySize,
                        body_size      = BodySize + Acc},
    {State#state_rcv{session  = NewHttp,
                     ack_done = false, % continue to read data
                     datasize = BodySize + Acc},[]};
read_chunk_data(Data, State=#state_rcv{acc=Acc}, _Int, AccSize) ->
    ?DebugF("Accumulated data = [~p]~n", [Acc]),
    NewData = <<Acc/binary, Data/binary>>,
    read_chunk(NewData, State#state_rcv{acc=[]}, 0, AccSize).

%%----------------------------------------------------------------------
%% Func: add_new_cookie/3
%% Purpose: Separate cookie values from attributes
%%----------------------------------------------------------------------
add_new_cookie(Cookie, Host, OldCookies) ->
    Fields = splitcookie(Cookie),
    %% FIXME: bad domain if we use a Proxy (the domain will be equal
    %% to the proxy domain instead of the server's domain
    New = parse_set_cookie(Fields, #cookie{domain=[$.|Host],path="/"}),
    concat_cookies([New],OldCookies).

%%----------------------------------------------------------------------
%% Function: splitcookie/3
%% Purpose:  split according to string ";".
%%  Not very elegant but 5x faster than the regexp:split version
%%----------------------------------------------------------------------
splitcookie(Cookie) -> splitcookie(Cookie, [], []).
splitcookie([], Cur, Acc) -> [lists:reverse(Cur)|Acc];
splitcookie(";"++Rest,Cur,Acc) ->
    splitcookie(string:strip(Rest, both),[],[lists:reverse(Cur)|Acc]);
splitcookie([Char|Rest],Cur,Acc)->splitcookie(Rest, [Char|Cur], Acc).

%%----------------------------------------------------------------------
%% Func: concat_cookie/2
%% Purpose: add new cookies to a list of old ones. If the keys already
%%          exists, replace with the new ones
%%----------------------------------------------------------------------
concat_cookies([],  Cookies) -> Cookies;
concat_cookies(Cookie, []) -> Cookie;
concat_cookies([New=#cookie{}|Rest], OldCookies)->
    case lists:keysearch(New#cookie.key, #cookie.key, OldCookies) of
        {value, #cookie{domain=Dom}} when Dom == New#cookie.domain -> %same domain
                ?DebugF("Reset key ~p with new value ~p~n",[New#cookie.key,
                                                            New#cookie.value]),
                NewList = lists:keyreplace(New#cookie.key, #cookie.key, OldCookies, New),
                concat_cookies(Rest, NewList);
        {value, _Val} -> % same key, but different domains
                concat_cookies(Rest, [New | OldCookies]);
        false ->
                concat_cookies(Rest, [New | OldCookies])
    end.


%%----------------------------------------------------------------------
%% Func: parse_set_cookie/2
%%       cf. RFC 2965
%%----------------------------------------------------------------------
parse_set_cookie([], Cookie) -> Cookie;
parse_set_cookie([Field| Rest], Cookie=#cookie{}) ->
    {Key,Val} = get_cookie_key(Field,[]),
    ?DebugF("Parse cookie key ~p with value ~p~n",[Key, Val]),
    parse_set_cookie(Rest, set_cookie_key(Key, Val, Cookie)).

%%----------------------------------------------------------------------
set_cookie_key([L|"ersion"],Val,Cookie) when L == $V; L==$v ->
    Cookie#cookie{version=Val};
set_cookie_key([L|"omain"],Val,Cookie) when L == $D; L==$d ->
    Cookie#cookie{domain=Val};
set_cookie_key([L|"ath"],Val,Cookie) when L == $P; L==$p ->
    Cookie#cookie{path=Val};
set_cookie_key([L|"ax-Age"],Val,Cookie) when L == $M; L==$m ->
    Cookie#cookie{max_age=Val}; % NOT IMPLEMENTED
set_cookie_key([L|"xpires"],Val,Cookie) when L == $E; L==$e ->
    Cookie#cookie{expires=Val}; % NOT IMPLEMENTED
set_cookie_key([L|"ort"],Val,Cookie) when L == $P; L==$p ->
    Cookie#cookie{port=Val};
set_cookie_key([L|"iscard"],_Val,Cookie) when L == $D; L==$d ->
    Cookie#cookie{discard=true}; % NOT IMPLEMENTED
set_cookie_key([L|"ecure"],_Val,Cookie) when L == $S; L==$s ->
    Cookie#cookie{secure=true}; % NOT IMPLEMENTED
set_cookie_key([L|"ommenturl"],_Val,Cookie) when L == $C; L==$c ->
    Cookie; % don't care about comment
set_cookie_key([L|"omment"],_Val,Cookie) when L == $C; L==$c ->
    Cookie; % don't care about comment
set_cookie_key(Key,Val,Cookie) ->
    Cookie#cookie{key=Key,value=Val}.

%%----------------------------------------------------------------------
get_cookie_key([],Acc)         -> {lists:reverse(Acc), []};
get_cookie_key([$=|Rest],Acc)  -> {lists:reverse(Acc), Rest};
get_cookie_key([Char|Rest],Acc)-> get_cookie_key(Rest, [Char|Acc]).



%%--------------------------------------------------------------------
%% Func: parse_headers/3
%% Purpose: Parse HTTP headers line by line
%% Returns: {ok, #http, Body}
%%--------------------------------------------------------------------
parse_headers(H, Tail, Host) ->
    case get_line(Tail) of
    {line, Line, Tail2} ->
        parse_headers(parse_line(Line, H, Host), Tail2, Host);
    {lastline, Line, Tail2} ->
        {ok, parse_line(Line, H#http{partial=false}, Host), Tail2};
    {more} -> %% Partial header
        {more, H#http{partial=true}, Tail}
    end.

%%--------------------------------------------------------------------
%% Func: parse_req/1
%% Purpose: Parse HTTP request
%% Returns: {ok, #http_request, Body} | {more, Http , Tail}
%%--------------------------------------------------------------------
parse_req(Data) ->
    parse_req([], Data).
parse_req([], Data) ->
    FunV = fun("http/"++V)->V;("HTTP/"++V)->V end,
    case get_line(Data) of
        {more} -> %% Partial header
            {more, [], Data};
        {line, Line, Tail} ->
            [Method, RequestURI, Version] = string:tokens(Line," "),
            parse_req(#http_request{method=http_method(Method),
                                    url=RequestURI,
                                    version=FunV(Version)},Tail);
        {lastline, Line, Tail} ->
            [Method, RequestURI, Version] = string:tokens(Line," "),
            {ok, #http_request{method=http_method(Method),
                               url=RequestURI,
                               version=FunV(Version)},Tail}
    end;
parse_req(Http=#http_request{headers=H}, Data) ->
    case get_line(Data) of
        {line, Line, Tail} ->
            NewH= [ts_utils:split2(Line,$:,strip) | H],
            parse_req(Http#http_request{headers=NewH}, Tail);
        {lastline, Line, Tail} ->
            NewH= [ts_utils:split2(Line,$:,strip) | H],
            {ok, Http#http_request{headers=NewH}, Tail};
        {more} -> %% Partial header
            {more, Http#http_request{id=partial}, Data}
    end.

%%--------------------------------------------------------------------
http_method("get")-> 'GET';
http_method("post")-> 'POST';
http_method("head")-> 'HEAD';
http_method("put")-> 'PUT';
http_method("delete")-> 'DELETE';
http_method("connect")-> 'CONNECT';
http_method("propfind")-> 'PROPFIND';
http_method("proppatch")-> 'PROPPATCH';
http_method("copy")-> 'COPY';
http_method("move")-> 'MOVE';
http_method("lock")-> 'LOCK';
http_method("unlock")-> 'UNLOCK';
http_method("mkcol")-> 'MKCOL';
http_method("mkactivity")-> 'MKACTIVITY';
http_method("report")-> 'REPORT';
http_method("options")-> 'OPTIONS';
http_method("checkout")-> 'CHECKOUT';
http_method("merge")-> 'MERGE';
http_method("patch")-> 'PATCH';
http_method(Method) ->
    ?LOGF("Unknown  HTTP method: ~p~n", [Method] ,?WARN),
    not_implemented.

%%--------------------------------------------------------------------
%% Func: parse_status/2
%% Purpose: Parse HTTP status
%% Returns: #http
%%--------------------------------------------------------------------
parse_status([A,B,C|_], Http=#http{status={Prev,_}}) ->
    Status=list_to_integer([A,B,C]),
    ?DebugF("HTTP Status ~p~n",[Status]),
    ts_mon:add({ count, Status }),
    Http#http{status={Status,Prev}}.

%%--------------------------------------------------------------------
%% Func: parse_line/3
%% Purpose: Parse a HTTP header
%% Returns: #http
%%--------------------------------------------------------------------
parse_line("http/1.1 " ++ TailLine, Http, _Host )->
    parse_status(TailLine, Http);
parse_line("http/1.0 " ++ TailLine, Http, _Host)->
    parse_status(TailLine, Http#http{close=true});

parse_line("content-length: "++Tail, Http, _Host) when hd(Tail) /= $\s ->
    %% tuning: handle common case (single LWS) to avoid a call to string:strip
    CL = list_to_integer(Tail),
    ?DebugF("HTTP Content-Length ~p~n",[CL]),
    Http#http{content_length=CL};
parse_line("content-length:  "++Tail, Http, _Host)-> % multiple white spaces
    CL = list_to_integer(string:strip(Tail)),
    ?DebugF("HTTP Content-Length ~p~n",[CL]),
    Http#http{content_length=CL};
parse_line("connection: close"++_Tail, Http, _Host)->
    ?Debug("Connection Closed in Header ~n"),
    Http#http{close=true};
parse_line("content-encoding: "++Tail, Http=#http{compressed={Prev,_}}, _Host)->
    ?DebugF("content encoding:~p ~n",[Tail]),
    Http#http{compressed={list_to_atom(Tail),Prev}};
parse_line("transfer-encoding:"++Tail, Http, _Host)->
    ?DebugF("~p transfer encoding~n",[Tail]),
    case string:strip(Tail) of
        [C|"hunked"++_] when C == $C; C == $c ->
            Http#http{chunk_toread=0};
        _ ->
            ?LOGF("Unknown transfer encoding ~p~n",[Tail],?NOTICE),
            Http
    end;
parse_line("set-cookie: "++Tail, Http=#http{cookie=PrevCookies}, Host)->
    Cookie = add_new_cookie(Tail, Host, PrevCookies),
    ?DebugF("HTTP New cookie val ~p~n",[Cookie]),
    Http#http{cookie=Cookie};
parse_line("proxy-connection: keep-alive"++_Tail, Http, _Host)->
    Http#http{close=false};
parse_line("connection: Keep-Alive"++_Tail, Http, _Host)->
    Http#http{close=false};
parse_line(_Line, Http, _Host) ->
    ?DebugF("Skip header ~p (Http record is ~p)~n", [_Line, Http]),
    Http.

%% code taken from yaws
is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).
% ret: {line, Line, Trail} | {lastline, Line, Trail}
get_line(L) ->
    get_line(L, true, []).
get_line("\r\n\r\n" ++ Tail, _Cap, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n", _, _) ->
    {more};
get_line("\r\n" ++ Tail, Cap, Cur) ->
    case is_nb_space(hd(Tail)) of
        true ->  %% multiline ... continue
            get_line(Tail, Cap,[$\n, $\r | Cur]);
        false ->
            {line, lists:reverse(Cur), Tail}
    end;
get_line([$:|T], true, Cur) -> % ':' separator
    get_line(T, false, [$:|Cur]);%the rest of the header isn't set to lower char
get_line([H|T], false, Cur) ->
    get_line(T, false, [H|Cur]);
get_line([Char|T], true, Cur) when Char >= $A, Char =< $Z ->
    get_line(T, true, [Char + 32|Cur]);
get_line([H|T], true, Cur) ->
    get_line(T, true, [H|Cur]);
get_line([], _, _) -> %% Headers are fragmented ... We need more data
    {more}.

%% we need to keep the compressed value of the current request
reset_session(#http{user_agent=UA,session_cookies=Cookies,
                    compressed={Compressed,_}, status= {Status,_}, chunk_toread=Val}) when Val > -1 ->
    #http{session_cookies=Cookies,user_agent=UA,compressed={false,Compressed}, chunk_toread=-2, status={none,Status}} ;
reset_session(#http{user_agent=UA,session_cookies=Cookies,
                    compressed={Compressed,_}, status= {Status,_}})  ->
    #http{session_cookies=Cookies,user_agent=UA,compressed={false,Compressed}, status={none,Status}}.

log_error(protocol,Error) ->
    put(protocol_error,Error),
    log_error2(protocol,Error);
log_error(Type,Error) ->
    log_error2(Type,Error).
log_error2(_,Error)->
    ts_mon:add({count, Error}).
