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

%%% common functions used by http clients to:
%%%  - parse response from HTTP server
%%%  - set HTTP requests
%%%  - parse HTTP related stuff in XML config file

-module(ts_http_common).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("ts_profile.hrl").
-include("ts_http.hrl").

-include("ts_config.hrl").

-include_lib("xmerl/inc/xmerl.hrl").

-export([
         http_get/1,
         http_get_ifmodsince/1,
         http_post/1,
         set_msg/1, set_msg/2,
         parse/2,
         parse_URL/1,
         parse_config/2,
         set_port/1
		]).

%%----------------------------------------------------------------------
%% Func: http_get/1
%% Args: #http_request
%%----------------------------------------------------------------------
http_get(Req=#http_request{url=URL, version=Version, cookie=Cookie,
                           server_name = Host}) ->
	list_to_binary([?GET, " ", URL," ", "HTTP/", Version, ?CRLF, 
                    "Host: ", Host, ?CRLF,
                    user_agent(),
                    get_cookie(Cookie),
                    ?CRLF]).

%%----------------------------------------------------------------------
%% Func: http_get_ifmodsince/1
%% Args: #http_request
%%----------------------------------------------------------------------
http_get_ifmodsince(Req=#http_request{url=URL, version=Version, cookie=Cookie,
                                      get_ims_date=Date, server_name=Host}) ->
	list_to_binary([?GET, " ", URL," ", "HTTP/", Version, ?CRLF,
                    ["If-Modified-Since: ", Date, ?CRLF],
                    "Host: ", Host, ?CRLF,
                    user_agent(),
                    get_cookie(Cookie),
                    ?CRLF]).

%%----------------------------------------------------------------------
%% Func: http_post/1
%% Args: #http_request
%%----------------------------------------------------------------------
http_post(Req=#http_request{url=URL, version=Version, cookie=Cookie,
			    content_type=ContentType, body=Content, server_name=Host}) ->
	ContentLength=integer_to_list(size(Content)),
	?DebugF("Content Length of POST: ~p~n.", [ContentLength]),
	Headers = [?POST, " ", URL," ", "HTTP/", Version, ?CRLF,
               "Host: ", Host, ?CRLF,
               user_agent(),
               get_cookie(Cookie),
               "Content-Type: ", ContentType, ?CRLF,
               "Content-Length: ",ContentLength, ?CRLF,
               ?CRLF
              ],
	list_to_binary([Headers, Content ]).

%%----------------------------------------------------------------------
%% some HTTP headers functions
%%----------------------------------------------------------------------
user_agent() ->
	["User-Agent: ", ?USER_AGENT, ?CRLF].

get_cookie(none)    -> [];
get_cookie(Cookies) -> get_cookie(Cookies, []).

get_cookie([], Acc )   -> [lists:reverse(Acc), ?CRLF];
get_cookie([Cookie|Cookies], []) ->
    get_cookie(Cookies, [["Cookie: ", Cookie]]);
get_cookie([Cookie|Cookies], Acc) ->
    get_cookie(Cookies, [["; ", Cookie]|Acc]).

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
parse_config(Element = #xmlElement{name=http}, 
             Config=#config{curid= Id, session_tab = Tab,
                            sessions = [CurS |SList]}) ->
    Version  = ts_config:getAttr(Element#xmlElement.attributes, version),
    URL      = ts_config:getAttr(Element#xmlElement.attributes, url),
    Contents = ts_config:getAttr(Element#xmlElement.attributes, contents),
    %% Apache Tomcat applications need content-type informations to read post forms
    ContentType = ts_config:getAttr(Element#xmlElement.attributes,
                            content_type, "application/x-www-form-urlencoded"),
    Date     = ts_config:getAttr(Element#xmlElement.attributes, date),
    Method = case ts_config:getAttr(Element#xmlElement.attributes, method) of 
                 "GET"    -> get;
                 "GETIMS" -> getims;
                 "POST"   -> post;
                 Other ->
                     ?LOGF("Bad method ! ~p ~n",[Other],?ERR),
                     get
             end,
    ServerName = ts_config:get_default(Tab,http_server_name, server_name),
    Msg = set_msg(#http_request{url         = URL,
                                method      = Method,
                                version     = Version,
                                get_ims_date= Date,
                                server_name = ServerName,
								content_type= ContentType,
                                body        = list_to_binary(Contents)}, 0),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg}),
    lists:foldl( {ts_config, parse},
                 Config#config{},
                 Element#xmlElement.content);
%% Parsing default values
parse_config(Element = #xmlElement{name=default}, Conf = #config{session_tab = Tab}) ->
    case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "server_name" ->
            Val = ts_config:getAttr(Element#xmlElement.attributes, value),
            ets:insert(Tab,{{http_server_name, value}, Val})
    end,
    lists:foldl( {ts_config, parse}, Conf, Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    lists:foldl( {ts_config, parse}, Conf, Element#xmlElement.content);
%% Parsing non #xmlElement elements
parse_config(Element, Conf = #config{}) ->
    Conf.

%%----------------------------------------------------------------------
%% Func: parse/2
%% Args: Data, State
%% Returns: {NewState, Options for socket (list)}
%% Purpose: parse the response from the server and keep information
%%  about the response if State#state_rcv.session
%%----------------------------------------------------------------------
parse(Data, State) when (State#state_rcv.session)#http.status == none ->
	List = binary_to_list(Data),
	TotalSize = size(Data),
	{ok, Http, Tail} = parse_headers(#http{},List),
	ts_mon:add({ count, Http#http.status }),
	BodySize= length(Tail),
	CLength = Http#http.content_length,
	Close   = Http#http.close,
	Cookie  = lists:append([Http#http.cookie, State#state_rcv.dyndata]),
	if 
		CLength == 0, Http#http.status== 304 ->
			?Debug("HTTP Not modified~n"),
			{State#state_rcv{session= #http{}, ack_done = true,
							 datasize = TotalSize,
							 dyndata  = Cookie}, [], Close};
		CLength == 0, Http#http.chunk_toread == 0 ->
			case parse_chunked(Tail, State#state_rcv{session=Http}) of
				{NewState=#state_rcv{ack_done=false}, Opts} ->
					{NewState, Opts, false};
				{NewState, Opts} ->
					{NewState, Opts, Close}
			end;
		CLength == 0->
			?LOG("ERROR no content length !~n", ?INFO),
			ts_mon:add({ count, no_content_length }),
			{State#state_rcv{session= #http{}, ack_done = true,
							 datasize = TotalSize,
							 dyndata= Cookie}, [], Close};
		BodySize == CLength ->  % end of response
			{State#state_rcv{session= #http{}, ack_done = true,
							 datasize = BodySize,
							 dyndata= Cookie}, [], Close};
		BodySize > CLength  ->
			?LOGF("Error: HTTP Body (~p)> Content-Length (~p) !~n",
				  [BodySize, CLength], ?ERR),
			ts_mon:add({ count, http_bad_content_length }),
			{State#state_rcv{session= #http{}, ack_done = true,
							 datasize = TotalSize,
							 dyndata= Cookie}, [], Close};
		true -> %% need to read more data
			{State#state_rcv{session  = Http#http{ body_size=BodySize},
							 ack_done = false,
							 datasize = TotalSize,
							 dyndata  = Cookie},[],false}
	end;

%% FIXME: handle the case where the Headers are not complete in the first message
%% current connection
parse(Data, State=#state_rcv{session=Http}) when Http#http.chunk_toread >=0 ->
    case read_chunk_data(Data,State,Http#http.chunk_toread,Http#http.body_size) of
		{NewState=#state_rcv{ack_done=false}, NewOpts}->
            {NewState, NewOpts, false};
		{NewState, NewOpts}->
            {NewState, NewOpts, Http#http.close}
	end;

parse(Data, State) ->
    PreviousSize = State#state_rcv.datasize,
	DataSize = size(Data),
	?DebugF("HTTP Body size=~p ~n",[DataSize]),
    Http = State#state_rcv.session,
	CLength = Http#http.content_length,
	case Http#http.body_size + DataSize of 
		CLength -> % end of response
			{State#state_rcv{session= #http{}, ack_done = true, datasize = CLength},
			 [], Http#http.close};
		Size ->
			NewHttp = (State#state_rcv.session)#http{body_size = Size},
			{State#state_rcv{session= NewHttp, ack_done = false, 
                             datasize = DataSize+PreviousSize}, [], false}
	end.
												 
%%----------------------------------------------------------------------
%% Func: parse_chunked/2
%% Purpose: parse 'Transfer-Encoding: chunked' for HTTP/1.1
%% Returns: {NewState= record(state_rcv), SockOpts}
%%----------------------------------------------------------------------
parse_chunked(Body, State)->
    read_chunk(list_to_binary(Body), State, 0, 0).

%%----------------------------------------------------------------------
%% Func: read_chunk/4
%% Purpose: the real stuff for parsing chunks is here
%% Returns: {NewState= record(state_rcv), SockOpts}
%%----------------------------------------------------------------------
read_chunk(<<>>, State, Int, Acc) ->
    ?LOG("NO Data in chunk ! ~n", ?WARN),
	% FIXME: should we check if Headers has just been received and the
	% returns a new #http record ?
    { State, [] }; % read more data
%% this code has been inspired by inets/http_lib.erl
read_chunk(<<Char:1/binary, Data/binary>>, State, Int, Acc) ->
    case Char of
	<<C>> when $0=<C,C=<$9 ->
	    read_chunk(Data, State, 16*Int+(C-$0), Acc+1);
	<<C>> when $a=<C,C=<$f ->
	    read_chunk(Data, State, 16*Int+10+(C-$a), Acc+1);
	<<C>> when $A=<C,C=<$F ->
	    read_chunk(Data, State, 16*Int+10+(C-$A), Acc+1);
%	<<$;>> when Int>0 ->
%	    ExtensionList=read_chunk_ext_name(Data, State, [],[]),
%	    read_chunk_data(Data, State, Int+1,ExtensionList);
%	<<$;>> when Int==0 ->
%	    ExtensionList=read_chunk_ext_name(Data, State, [],[]),
%	    read_data_lf(),
	<<?CR>> when Int>0 ->
	    read_chunk_data(Data, State, Int+3, Acc+1);
	<<?CR>> when Int==0 -> %% should be the end of tranfer
			Cookie=lists:append([(State#state_rcv.session)#http.cookie,
                                 State#state_rcv.dyndata]),
            ?DebugF("Finish tranfer chunk ~p~n", [binary_to_list(Data)]),
            {State#state_rcv{session= #http{}, ack_done = true,
                             datasize = Acc, %% FIXME: is it the correct size?
                             dyndata= Cookie}, []};
	<<C>> when C==$ -> % Some servers (e.g., Apache 1.3.6) throw in
			   % additional whitespace...
	    read_chunk(Data, State, Int, Acc+1);
	_Other ->
            ?LOGF("Unexpected error while parsing chunk ~p~n", [_Other] ,?WARN),
			ts_mon:add({count, http_unexpected_chunkdata}),
            {State#state_rcv{session= #http{}, ack_done = true}, []}
    end.

%%----------------------------------------------------------------------
%% Func: read_chunk_data/4
%% Purpose: read 'Int' bytes of data
%% Returns: {NewState= record(state_rcv), SockOpts}
%%----------------------------------------------------------------------
read_chunk_data(Data, State, Int, Acc) when size(Data) > Int->
    ?DebugF("Read ~p bytes of chunk with size = ~p~n", [Int, size(Data)]),
    <<NewData:Int/binary, Rest/binary >> = Data,
    read_chunk(Rest, State,  0, Int + Acc);
read_chunk_data(Data, State, Int, Acc) -> % not enough data in buffer
    BodySize = size(Data),
	Cookie=(State#state_rcv.session)#http.cookie,
    ?DebugF("Partial chunk received (~p/~p)~n", [BodySize,Int]),
    NewHttp = (State#state_rcv.session)#http{chunk_toread   = Int-BodySize,
											 body_size      = BodySize + Acc},
    {State#state_rcv{session  = NewHttp,
					 ack_done = false, % continue to read data
                     datasize = BodySize + Acc,
                     dyndata  = Cookie},[]}.

%%----------------------------------------------------------------------
%% Func: get_cookie_val/1
%% Purpose: Separate cookie values from attributes
%%----------------------------------------------------------------------
get_cookie_val(Cookie) ->
    case string:tokens( Cookie, "; ") of 
        [CookieVal |CookieOtherAttrib] ->
            %% FIXME: handle path attribute
            %% several cookies can be set with a different path attribute
            CookieVal ;
        _Other -> % something wrong
            []
    end.
     	    
%%----------------------------------------------------------------------
%% Func: parse_URL/1
%% Returns: #url
%%----------------------------------------------------------------------
parse_URL("https://" ++ String) ->
    parse_URL(host, String, [], #url{scheme=https});
parse_URL("http://" ++ String) ->
    parse_URL(host, String, [], #url{scheme=http}).

%%----------------------------------------------------------------------
%% Func: parse_URL/4 (inspired by yaws_api.erl)
%% Returns: #url record
%%----------------------------------------------------------------------
% parse host
parse_URL(host, [], Acc, URL) -> % no path or port
    URL#url{host=lists:reverse(Acc), path= "/"};
parse_URL(host, [$/|Tail], Acc, URL) -> % path starts here
    parse_URL(path, Tail, "/", URL#url{host=lists:reverse(Acc)});
parse_URL(host, [$:|Tail], Acc, URL) -> % port starts here
    parse_URL(port, Tail, [], URL#url{host=lists:reverse(Acc)});
parse_URL(host, [H|Tail], Acc, URL) ->
    parse_URL(host, Tail, [H|Acc], URL);

% parse port
parse_URL(port,[], Acc, URL) ->
    URL#url{port=list_to_integer(lists:reverse(Acc)), path= "/"};
parse_URL(port,[$/|T], Acc, URL) ->
    parse_URL(path, T, "/", URL#url{port=list_to_integer(lists:reverse(Acc))});
parse_URL(port,[H|T], Acc, URL) ->
    parse_URL(port, T, [H|Acc], URL);

% parse path
parse_URL(path,[], Acc, URL) ->
    URL#url{path=lists:reverse(Acc)};
parse_URL(path,[$?|T], Acc, URL) ->
    URL#url{path=lists:reverse(Acc), querypart=T};
parse_URL(path,[H|T], Acc, URL) ->
    parse_URL(path, T, [H|Acc], URL).

%%----------------------------------------------------------------------
%% Func: set_msg/1 or /2 or /3
%% Returns: #message record
%% Purpose:
%% unless specified, the thinktime is an exponential random var.
%%----------------------------------------------------------------------
set_msg(HTTPRequest) ->
	set_msg(HTTPRequest, round(ts_stats:exponential(?messages_intensity))).

%% if the URL is full (http://...), we parse it and get server host,
%% port and scheme from the URL and override the global setup of the
%% server. These informations are stored in the #message record.
set_msg(HTTP=#http_request{url="http" ++ URL}, ThinkTime) -> % full URL
    URLrec = parse_URL("http" ++ URL),
    Path = URLrec#url.path ++ URLrec#url.querypart,
    Port = set_port(URLrec),
    Scheme = case URLrec#url.scheme of
                 http  -> gen_tcp;
                 https -> ssl
             end,
    set_msg(HTTP#http_request{url=Path}, ThinkTime,
            #message{ack  = parse,
                     host = URLrec#url.host,
                     scheme = Scheme,
                     port = Port});
%
set_msg(HTTPRequest, Think) -> % relative URL, use global host, port and scheme
    set_msg(HTTPRequest, Think, #message{ack = parse}).
            
set_msg(HTTPRequest, 0, Msg) -> % no thinktime, only wait for response
	Msg#message{ thinktime=infinity,
                 param = HTTPRequest };
set_msg(HTTPRequest, Think, Msg) -> % end of a page, wait before the next one
	Msg#message{ endpage   = true,
                 thinktime = Think,
                 param = HTTPRequest }.

%%--------------------------------------------------------------------
%% Func: set_port/1
%% Purpose: Returns port according to scheme if not already defined
%% Returns: PortNumber (integer)
%%--------------------------------------------------------------------
set_port(#url{scheme=https,port=undefined})  -> 443;
set_port(#url{scheme=http,port=undefined})   -> 80;
set_port(#url{port=Port}) when is_integer(Port) -> Port;
set_port(#url{port=Port}) -> integer_to_list(Port).


%%--------------------------------------------------------------------
%% Func: parse_headers/2
%% Purpose: Parse HTTP headers line by line
%% Returns: {ok, #http, Body}
%%--------------------------------------------------------------------
parse_headers(H, Tail) ->
    case get_line(Tail) of
	{line, Line, Tail2} ->
	    parse_headers(parse_line(Line, H), Tail2);
	{lastline, Line, Tail2} ->
	    {ok, parse_line(Line, H), Tail2}
    end.

%%--------------------------------------------------------------------
%% Func: parse_status/2
%% Purpose: Parse HTTP status
%% Returns: #http
%%--------------------------------------------------------------------
parse_status([A,B,C|Tail],  Http) ->
	Status=list_to_integer([A,B,C]),
	?DebugF("HTTP Status ~p~n",[Status]),
	Http#http{status=Status}.

%%--------------------------------------------------------------------
%% Func: parse_line/2
%% Purpose: Parse a HTTP header
%% Returns: #http
%%--------------------------------------------------------------------
parse_line("HTTP/1.1 " ++ TailLine, Http )->
	parse_status(TailLine, Http);
parse_line("HTTP/1.0 " ++ TailLine, Http )->
	parse_status(TailLine, Http#http{close=true});

parse_line("Content-Length: "++Tail, Http)->
	CL=list_to_integer(Tail),
	?DebugF("HTTP Content-Length ~p~n",[CL]),
	Http#http{content_length=CL};
parse_line("Connection: close"++Tail, Http)->
	Http#http{close=true};
parse_line("Transfer-Encoding: chunked"++Tail, Http)->
	?LOG("Chunked transfer encoding~n",?DEB),
	Http#http{chunk_toread=0};
parse_line("Transfer-Encoding:"++Tail, Http)->
	?LOGF("Unknown tranfer encoding ~p~n",[Tail],?NOTICE),
	Http;
parse_line("Set-Cookie: "++Tail, Http=#http{cookie=PrevCookies})->
	Cookie = get_cookie_val(Tail), %% FIXME: is it ok ?
	?DebugF("HTTP New cookie val ~p~n",[Cookie]),
	Http#http{cookie=lists:reverse([Cookie|PrevCookies])};
parse_line(Line,Http) ->
	?DebugF("Skip header ~p (Http record is ~p)~n",[Line,Http]),
	Http.

%% code taken from yaws
is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).
% ret: {line, Line, Trail} | {lastline, Line, Trail}
get_line(L) ->    
    get_line(L, []).
get_line("\r\n\r\n" ++ Tail, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n" ++ Tail, Cur) ->
    case is_nb_space(hd(Tail)) of
	true ->  %% multiline ... continue 
	    get_line(Tail, [$\n, $\r | Cur]);
	false ->
	    {line, lists:reverse(Cur), Tail}
    end;
get_line([H|T], Cur) ->
    get_line(T, [H|Cur]).
