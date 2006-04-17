%%%
%%%  Copyright (C) Nicolas Niclausse 2005
%%%
%%%	 Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 09 Nov 2005 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
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

-module(ts_proxy_http).
-vc('$Id: ts_proxy_http.erl,v 0.0 2005/11/09 22:20:59 nniclausse Exp $ ').
-author('Nicolas.Niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_recorder.hrl").


-export([parse/4, record_request/2, socket_opts/0]).
-export([decode_basic_auth/1, gettype/0]).

-export([rewrite_serverdata/1]).
-export([rewrite_ssl/1]).

%%--------------------------------------------------------------------
%% Func: socket_opts/0
%%--------------------------------------------------------------------
socket_opts() -> [{packet, 0}].

%%--------------------------------------------------------------------
%% Func: gettype/0
%%--------------------------------------------------------------------
gettype() -> "ts_http".

%%--------------------------------------------------------------------
%% Func: rewrite_serverdata/1
%%--------------------------------------------------------------------
rewrite_serverdata(Data)->
    ts_utils:from_https(Data).

%%--------------------------------------------------------------------
%% Func: rewrite_serverdata/1
%%--------------------------------------------------------------------
rewrite_ssl(Data)->
    ts_utils:to_https(Data).

%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse HTTP request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State=#proxy{parse_status=Status},_,ServerSocket,String) when Status==new ->
    NewString = lists:append(State#proxy.buffer,String),
	case ts_http_common:parse_req(NewString) of 
		{more, _Http, _Head} -> 
            ?LOGF("Headers incomplete (~p), buffering ~n",[NewString],?DEB),
			{ok, State#proxy{parse_status=new, buffer=NewString}}; %FIXME: not optimal
        {ok, Http=#http_request{url=RequestURI, version=HTTPVersion}, Body} -> 
            ?LOGF("URL ~p ~n",[RequestURI],?DEB),
            ?LOGF("Method ~p ~n",[Http#http_request.method],?DEB),
            ?LOGF("Headers ~p ~n",[Http#http_request.headers],?DEB),
            case httpd_util:key1search(Http#http_request.headers,"content-length") of
                undefined -> % no body, everything received
                    ts_proxy_recorder:dorecord({Http }),
                    {NewSocket,RelURL} = check_serversocket(ServerSocket,RequestURI,State#proxy.clientsock),
                    ?LOGF("Remove server info from url:~p ~p in ~p~n",
                          [RequestURI,RelURL,NewString], ?INFO),
                    {ok, RealString} = relative_url(NewString,RequestURI,RelURL),
                    ts_client_proxy:send(NewSocket,RealString, ?MODULE),
                    case Http#http_request.method of
                        'CONNECT' ->
                            {ok, State#proxy{http_version=HTTPVersion,
                                             parse_status = connect, buffer=[],
                                             serversock=NewSocket}};
                        _ ->
                            {ok, State#proxy{http_version=HTTPVersion,
                                             parse_status = new, buffer=[],
                                             serversock=NewSocket}}
                    end;
                Length ->
                    CLength = list_to_integer(Length),
                    ?LOGF("HTTP Content-Length:~p~n",[CLength], ?DEB),
                    BodySize = length(Body),
                    if
                        BodySize == CLength ->  % end of response
                            {NewSocket,RelURL} = check_serversocket(ServerSocket,RequestURI,State#proxy.clientsock),
                            {ok,RealString,_Count} = regexp:gsub(NewString,RequestURI,RelURL),%FIXME: why not use relative_url ?
                            ts_client_proxy:send(NewSocket,RealString,?MODULE),
                            ?LOG("End of response, recording~n", ?DEB),
                            ts_proxy_recorder:dorecord({Http#http_request{body=Body}}),
                            {ok, State#proxy{http_version = HTTPVersion,
                                             parse_status = new, buffer=[],
                                             serversock=NewSocket}};
                        BodySize > CLength  ->
                            {error, bad_content_length};
                        true ->
                            {NewSocket,RelURL} = check_serversocket(ServerSocket,RequestURI,State#proxy.clientsock),
                            {ok,RealString,_Count} = regexp:gsub(NewString,RequestURI,RelURL), %FIXME: why not use relative_url ?
                            ts_client_proxy:send(NewSocket,RealString,?MODULE),
                            ?LOG("More data to come continue before recording~n", ?DEB),
                            {ok, State#proxy{http_version=HTTPVersion,
                                             content_length = CLength,
                                             body_size = BodySize,
                                             serversock=NewSocket,
                                             buffer = Http#http_request{body=Body },
                                             parse_status = body
                                            }
                            }
                    end
            end
    end;

parse(State=#proxy{parse_status=body, buffer=Http},_,ServerSocket,String) ->
	DataSize = length(String),
	?LOGF("HTTP Body size=~p ~n",[DataSize], ?DEB),
	Size = State#proxy.body_size + DataSize,
	CLength = State#proxy.content_length,
    ts_client_proxy:send(ServerSocket, String, ?MODULE),
    Buffer=lists:append(Http#http_request.body,String),
    %% Should be checked before
	case Size of 
		CLength -> % end of response
            ?LOG("End of response, recording~n", ?DEB),
            ts_proxy_recorder:dorecord( {Http#http_request{ body=Buffer }} ),
			{ok, State#proxy{body_size=0,parse_status=new, content_length=0,buffer=[]}};
		_ ->
            ?LOGF("Received ~p bytes of data, wait for ~p, continue~n", [Size,CLength],?DEB),
			{ok, State#proxy{body_size = Size, buffer = Http#http_request{body=Buffer}}}
	end;

parse(State=#proxy{parse_status=connect},_,ServerSocket,String) ->
    ?LOGF("Received data from client: ~s~n",[String],?DEB),
    ts_client_proxy:send(ServerSocket, String, ?MODULE),
    {ok, State}.


relative_url("CONNECT"++_Tail,_RequestURI,[])-> 
    {ok, []};
relative_url(NewString,RequestURI,RelURL)->
    [FullURL_noargs|_] = string:tokens(RequestURI,"?"),
    [RelURL_noargs|_]  = string:tokens(RelURL,"?"),
    {ok,RealString,_Count} = regexp:gsub(NewString,FullURL_noargs,RelURL_noargs),
    {ok, RealString}.

%%--------------------------------------------------------------------
%% Func: check_serversocket/2
%% Purpose: If the socket is not defined, or if the server is not the
%%          same, connect to the server as specified in URL
%% Returns: {Socket, RelativeURL (String)}
%%--------------------------------------------------------------------            
check_serversocket(Socket, "http://{" ++ Rest, ClientSock) ->
    check_serversocket(Socket, ts_config_http:parse_URL("https://"++Rest), ClientSock);
check_serversocket(Socket, "http://%7B" ++ Rest, ClientSock) -> %% for IE.
    check_serversocket(Socket, ts_config_http:parse_URL("https://"++Rest), ClientSock);
check_serversocket(Socket, URL, ClientSock) when list(URL)->
    check_serversocket(Socket, ts_config_http:parse_URL(URL), ClientSock);

check_serversocket(undefined, URL = #url{}, ClientSock) ->
    Port = ts_config_http:set_port(URL),
    ?LOGF("Connecting to ~p:~p ...~n", [URL#url.host, Port],?DEB),

    {ok, Socket} = connect(URL#url.scheme, URL#url.host,Port),

    ?LOGF("Connected to server ~p on port ~p (socket is ~p)~n",
          [URL#url.host,Port,Socket],?INFO),
    case URL#url.scheme of 
        connect ->
            ?LOGF("CONNECT: Send 'connection established' to client socket (~p)",[ClientSock],?DEB),
            ts_client_proxy:send(ClientSock, "HTTP/1.0 200 Connection established\r\nProxy-agent: tsung\r\n\r\n", ?MODULE),
            { Socket, [] };
        _ ->
            case URL#url.querypart of 
                []    -> {Socket, URL#url.path};
                Query -> {Socket, URL#url.path++"?"++Query}
            end
    end;
check_serversocket(Socket, URL=#url{host=Host}, _ClientSock) ->
    RealPort = ts_config_http:set_port(URL),
    {ok, RealIP} = inet:getaddr(Host,inet),
    case ts_client_proxy:peername(Socket) of
        {ok, {RealIP, RealPort}} -> % same as previous URL
            ?LOGF("Reuse socket ~p on URL ~p~n", [Socket, URL],?DEB),
            case URL#url.querypart of 
                []    -> {Socket, URL#url.path};
                Query -> {Socket, URL#url.path++"?"++Query}
            end;
        Other ->
            ?LOGF("New server configuration  (~p:~p, was ~p) on URL ~p~n", 
                  [RealIP, RealPort, Other, URL],?DEB),
            case Socket of 
                {sslsocket, _, _} -> ssl:close(Socket);
                _             -> gen_tcp:close(Socket)
            end,
            {ok, NewSocket} = connect(URL#url.scheme, Host,RealPort),
            case URL#url.querypart of 
                []    -> {NewSocket, URL#url.path};
                Query -> {NewSocket, URL#url.path++"?"++Query}
            end
    end.

connect(Scheme, Host, Port)->
    case Scheme of 
        https -> 
            {ok, _} = ssl:connect(Host,Port,
                                 [{active, once}]);
        _  -> 
            {ok, _} = gen_tcp:connect(Host,Port,
                                      [{active, once},
                                       {recbuf, ?tcp_buffer},
                                       {sndbuf, ?tcp_buffer}
                                      ])
    end.


%%--------------------------------------------------------------------
%% Func: record_http_request/2
%% Purpose: record request given State=#state_rec and Request=#http_request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
record_request(State=#state_rec{prev_host=Host, prev_port=Port, prev_scheme=Scheme},
                    #http_request{method  = Method, url = RequestURI,
                                  version = HTTPVersion,
                                  headers = ParsedHeader,body=Body}) ->
    
    FullURL = ts_utils:to_https({url, RequestURI}),

    {URL,NewPort,NewHost, NewScheme} = 
        case ts_config_http:parse_URL(FullURL) of 
            #url{path=RelURL,host=Host,port=Port,querypart=[],scheme=Scheme}->
                {RelURL, Port, Host, Scheme};
            #url{path=RelURL,host=Host,port=Port,querypart=Args,scheme=Scheme}->
                {RelURL++"?"++Args, Port, Host, Scheme};
            #url{path=RelURL,host=Host2,port=Port2,querypart=Args,scheme=Sc2}->
                {FullURL,Port2,Host2,Sc2 }
        end,
    Fd = State#state_rec.logfd,
    URL2 = ts_utils:export_text(URL),
    io:format(Fd,"<request><http url='~s' version='~s' ", [URL2, HTTPVersion]),
    case Body of 
        [] -> ok;
        _  -> 
            Body2 = ts_utils:export_text(Body),
            io:format(Fd," contents='~s' ", [Body2]) % must be a POST method
    end,

    %% Content-type recording (This is usefull for SOAP post for example):
    record_header(Fd,ParsedHeader,"content-type", "content_type='~s' "),
    record_header(Fd,ParsedHeader,"if_modified_since", "if_modified_since='~s' "),

    io:format(Fd,"method='~s'>", [Method]),

    record_header(Fd,ParsedHeader,"authorization",
                  "~n  <www_authenticate userid=~p passwd=~p />"),
    %% SOAP Support: Need to record use of the SOAPAction header
    record_header(Fd,ParsedHeader,"soapaction",
                  "~n  <soap action='~s'></soap>~n",
                  fun(A) -> string:strip(A,both,$") end ),

	io:format(Fd,"</http></request>~n",[]),

    {ok,State#state_rec{prev_port=NewPort,prev_host=NewHost,prev_scheme=NewScheme}}.

%%--------------------------------------------------------------------
%% Func: decode_basic_auth/1
%% Purpose: decode base64 encoded user passwd for basic authentication
%% Returns: {User, Passwd}
%%--------------------------------------------------------------------
decode_basic_auth(Base64)->
	AuthStr= httpd_util:decode_base64(Base64),
	Sep = string:chr(AuthStr,$:),
	{string:substr(AuthStr,1,Sep-1),string:substr(AuthStr,Sep+1)}.
	
%%--------------------------------------------------------------------
%% Func: record_header/3
%%--------------------------------------------------------------------
record_header(Fd, Headers, "authorization", Msg)->
    %% special case for authorization
    case httpd_util:key1search(Headers,"authorization") of
        "Basic " ++ Base64 ->
            {User,Passwd} = decode_basic_auth(Base64),
            io:format(Fd, Msg, [User,Passwd]);
		_ -> ok
    end;
record_header(Fd, Headers, HeaderName, Msg)->
    %% record Msg as it is given
    record_header(Fd, Headers,HeaderName, Msg, fun(A)->A end). 
%%--------------------------------------------------------------------
record_header(Fd, Headers,HeaderName, Msg, Fun)->
    case httpd_util:key1search(Headers,HeaderName) of
        undefined -> ok;
        Value     -> io:format(Fd,Msg,[Fun(Value)])
    end.

