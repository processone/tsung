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


-module(ts_http_common).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("../include/ts_profile.hrl").
-include("../include/ts_http.hrl").

-export([get_client/2,
		 get_client/3,
		 http_get/3,
		 http_post/3,
		 parse/2,
		 get_simple_client/2
		]).

%%----------------------------------------------------------------------
%% Func: get_client/2
%% Purpose: Generate a client session.
%% N and Id are currently not used (instead, data are taken from the
%%  the session server).
%% Returns: List
%%----------------------------------------------------------------------
get_client(N, Id) ->
	get_client(N, Id, ?http_req_filename).
get_client(N, Id, File) ->
	ts_req_server:read_sesslog(File),
	{ok, Session} = ts_req_server:get_next_session(),
	build_session([], Session, Id).

%%----------------------------------------------------------------------
%% Func: get_simple_client/2
%% Purpose: build an http session without reading inputs from a file
%% Returns: List
%% currently unused !
%%----------------------------------------------------------------------
get_simple_client(N, Id) ->
	ts_req_server:read(?http_req_filename),
	build_simplesession(N, [], Id).


%%----------------------------------------------------------------------
%% Func: build_session/3
%% Purpose: build a session; a session is a list of Pages where a Page 
%% is a list of 'URL'
%% Returns: List
%%----------------------------------------------------------------------
build_session(N, [FirstPage | Session] , Id) ->
	build_session(N, Session, FirstPage, [], Id).

%%----------------------------------------------------------------------
%% Func: build_session/5
%% Args: Thinktime (integer), Session, Page, FinalSession, Id
%%----------------------------------------------------------------------
build_session([], [], [Line], Session, Id) ->
	case parse_URL(Line) of 
		{error, Reason} ->
			?PRINTDEBUG("unexpected value while parsing sesslog ~p~n",[Reason],?ERR),
			abort;
		{URL, Think} ->
			lists:reverse([ set_msg(URL, Think) | Session ]) ;%% should we set think to 0 ?
		Else ->
			?PRINTDEBUG("unexpected value while parsing sesslog ~p~n",[Else],?ERR),
			abort
	end;
build_session(Think, [], [URL], Session, Id) ->
	%% the "efficiency guide" said that doing the reverse at the end
	%% is faster than using '++' during recursion
	lists:reverse([ set_msg(URL, Think) | Session ]) ;

%% single URL in a page, wait during thinktime
build_session([], [Next| PendingSession], [Line], Session, Id) ->
	case parse_URL(Line) of 
		{error, Reason} ->
			?PRINTDEBUG("unexpected value while parsing sesslog ~p~n",[Reason],?ERR),
			abort;
		{URL, Think} ->
			build_session([], PendingSession, Next, [set_msg(URL, Think) | Session], Id);
		Else ->
			?PRINTDEBUG("unexpected value while parsing sesslog ~p~n",[Else],?ERR),
			abort
	end;
%% Last URL in a page, wait during thinktime
build_session(Think, [Next| PendingSession], [URL], Session, Id) ->
	build_session([], PendingSession, Next, [set_msg(URL, Think) | Session], Id);

%% First URL in a page, don't wait (wait 1ms in reality).
build_session([], PendingSession, [Line | Page ], Session, Id) ->
	case parse_URL(Line) of 
		{error, Reason} ->
			abort;
		{URL, Think} ->
			build_session(Think, PendingSession, Page, [ set_msg(URL, 0) | Session ], Id);
		Else ->
			?PRINTDEBUG("unexpected value while parsing sesslog ~p~n",[Else],?ERR),
			abort
	end;

build_session(Think, PendingSession, [URL | Page ], Session, Id) ->
	build_session(Think, PendingSession, Page, [ set_msg(URL, 0) | Session ], Id);

build_session(N, PendingSession, Page, Session, Id) ->
	this_should_not_happen.


%%----------------------------------------------------------------------
%% Func: build_simplesession/3
%% This type of session is just a list of URL, no page (~burst).
%%----------------------------------------------------------------------
build_simplesession(0, Session, Id) ->
	Session;
build_simplesession(N, Session, Id) ->
	{ok, URL} = ts_req_server:get_random_req(),
	build_simplesession(N-1, [set_msg(URL) | Session], Id).

%%----------------------------------------------------------------------
%% Func: http_get/2
%% Are we caching this string ? We should !
%%----------------------------------------------------------------------
http_get(URL, 1.0, none) ->
	CR=io_lib:nl(),
	?GET ++ " " ++ URL ++" " ++ ?HTTP10 ++ CR ++ user_agent() ++ CR++CR;
http_get(URL, 1.0, Cookie) -> %%TODO 
	CR=io_lib:nl(),
	?GET ++ " " ++ URL ++" " ++ ?HTTP10 ++ CR ++ user_agent() ++ CR++CR;
http_get(URL, 1.1, none) ->
	CR=io_lib:nl(),
	?GET ++ " " ++ URL ++" " ++ ?HTTP11 ++ CR++ host() ++ CR++ user_agent() ++ CR++CR;
http_get(URL, 1.1, Cookie) -> %% TODO
	CR=io_lib:nl(),
	?GET ++ " " ++ URL ++" " ++ ?HTTP11 ++ CR++ host() ++ CR++ user_agent() ++ CR++CR.

http_post(URL, 1.0, none) ->
	CR=io_lib:nl(),
	?POST ++ " " ++ URL ++" " ++ ?HTTP10 ++ CR ++ user_agent() ++ CR++CR;
http_post(URL, 1.0, Cookie) -> %%TODO 
	CR=io_lib:nl(),
	?POST ++ " " ++ URL ++" " ++ ?HTTP10 ++ CR ++ user_agent() ++ CR++CR;
http_post(URL, 1.1, none) ->
	CR=io_lib:nl(),
	?POST ++ " " ++ URL ++" " ++ ?HTTP11 ++ CR++ host() ++ CR++ user_agent() ++ CR++CR;
http_post(URL, 1.1, Cookie) -> %% TODO
	CR=io_lib:nl(),
	?POST ++ " " ++ URL ++" " ++ ?HTTP11 ++ CR++ host() ++ CR++ user_agent() ++ CR++CR.

%%----------------------------------------------------------------------
%% some HTTP headers functions
%%----------------------------------------------------------------------
user_agent() ->
	"User-Agent: IDX-Tsunami".
host() ->
	"Host:" ++ ?server_name.

%%----------------------------------------------------------------------
%% Func: parse_URL/1
%%----------------------------------------------------------------------
parse_URL(Line) ->
	case regexp:split(Line, "(\s|\t)+") of 
		{ok, [URL | FieldList]} ->
			%% TODO: parse FieldList
			{URL, round(ts_stats:exponential(?messages_intensity))};
		{error, Reason} ->
			?PRINTDEBUG("Error while parsing session ~p~n",[Reason],?ERR),
			{error, Reason}
	end.
%% example:  /foo2.html method=POST contents='Post data'
%% TODO: a URL should be a record: #url{url , method, body, auth, cookie}

%%----------------------------------------------------------------------
%% Func: set_msg/1 or /2
%% unless specified, the thinktime is an exponential random var.
%%----------------------------------------------------------------------
set_msg(URL) ->
	set_msg(URL, round(ts_stats:exponential(?messages_intensity))).
set_msg(URL, 0) -> % no waiting time, only wait for response (ack=local)
	#message{ack = parse, 
			 thinktime=infinity,
			 param = #http_request {url= URL} };
set_msg(URL, Think) -> % end of a page, wait before the next one
	#message{ack = parse, 
			 endpage   = true,
			 thinktime = Think,
			 param = #http_request {url= URL} }.



%%----------------------------------------------------------------------
%% Func: parse/2
%%----------------------------------------------------------------------

%% new connection
parse(Data, State) when record(State#state_rcv.session, http), (State#state_rcv.session)#http.status == none ->
	List = binary_to_list(Data),
	%%	regexp:split is much less efficient ! 
	StartHeaders = string:str(List, "\r\n\r\n"),
	Headers = string:substr(List, 1, StartHeaders-1),
	[Status, ParsedHeader] = request_header(Headers),
	ts_mon:addcount({ Status }),
	case httpd_util:key1search(ParsedHeader,"content-length") of
		undefined ->
			?PRINTDEBUG("No content length ! ~p~n",[Headers], ?WARN),
			State#state_rcv{session= #http{}, ack_done = true, datasize = 0};
		Length ->
			CLength = list_to_integer(Length)+4,
			?PRINTDEBUG("HTTP Content-Length:~p~n",[CLength], ?DEB),
			HeaderSize = length(Headers),
			BodySize = size(Data)-HeaderSize,
			case BodySize of 
				CLength ->  % end of response
					State#state_rcv{session= #http{}, ack_done = true, datasize = BodySize};
				_ ->
					Http = #http{content_length = CLength,
								 status         = Status,
								 body_size      = BodySize},
					State#state_rcv{session = Http, ack_done = false, datasize = BodySize}
			end
	end;

%% TODO : handle the case where the Headers are not complete in the first message
%% current connection
parse(Data, State) when record(State#state_rcv.session, http) ->
	DataSize = size(Data),
	Size = (State#state_rcv.session)#http.body_size + DataSize,
	CLength = (State#state_rcv.session)#http.content_length,
	case Size of 
		CLength -> % end of response
			State#state_rcv{session= #http{}, ack_done = true, datasize = Size};
		_ ->
			Http = (State#state_rcv.session)#http{body_size = Size},
			State#state_rcv{session= Http, ack_done = false, datasize = Size}
	end;
parse(Data, State) ->
	parse(Data, State#state_rcv{session=#http{}}).
												 
			

%%----------------------------------------------------------------------
%% Func: request_header/1
%%----------------------------------------------------------------------
request_header(Header)->
    [ResponseLine|HeaderFields] = httpd_parse:split_lines(Header),
    ParsedHeader = httpd_parse:tagup_header(HeaderFields),
	[get_resp(ResponseLine), ParsedHeader].

%% return status code
get_resp(Line) ->
	StartStatus = string:str(Line, " "),
%%	HTTP = string:substr(Line, 1, StartStatus-1),
	Status = string:substr(Line, StartStatus+1, 3),
	list_to_integer(Status).

%"HTTP/1.1 200 OK\r\nDate: sdf\r\nContent-Length: 4\r\nKeep-Alive: sdfsdf\r\nConnection: \r\nContent-Type:\r\n\r\nDataqdlfkqjsdmflqksdjfmqslkdfjqmsdlfkqsjmf"
% réponse HTTP: 
% HTTP/1.1 200 OK\r\n
% Date: 
% Server: 
% Last-Modified: 
% ETag: 
% Accept-Ranges: 
% Content-Length: 
% Keep-Alive:
% Connection: 
% Content-Type:
% \r\n
% Data 




