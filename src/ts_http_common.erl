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
		 http_post/4,
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
%% rem: crash if parse_requestline returns an error
%%----------------------------------------------------------------------
build_session([], [], [Line], Session, Id) ->
	{ok, Req, Think} = parse_requestline(Line),
	lists:reverse([ set_msg(Req, Think) | Session ]); %% should we set think to 0 ?

build_session(Think, [], [Line], Session, Id) ->
	%% the "efficiency guide" said that doing the reverse at the end
	%% is faster than using '++' during recursion
	{ok, Req, _Think} = parse_requestline(Line),
	lists:reverse([ set_msg(Req, Think) | Session ]) ;

%% single URL in a page, wait during thinktime
build_session([], [Next| PendingSession], [Line], Session, Id) ->
	{ok, Req, Think} = parse_requestline(Line),
	build_session([], PendingSession, Next, [set_msg(Req, Think) | Session], Id);

%% Last URL in a page, wait during thinktime
build_session(Think, [Next| PendingSession], [Line], Session, Id) ->
	{ok, Req, _Think} = parse_requestline(Line),
	build_session([], PendingSession, Next, [set_msg(Req, Think) | Session], Id);

%% First URL in a page, don't wait (wait 1ms in reality).
build_session([], PendingSession, [Line | Page ], Session, Id) ->
	{ok, Req, Think} = parse_requestline(Line),
	build_session(Think, PendingSession, Page, [ set_msg(Req, 0) | Session ], Id);

build_session(Think, PendingSession, [Line | Page ], Session, Id) ->
	{ok, Req, _Think} = parse_requestline(Line),
	build_session(Think, PendingSession, Page, [ set_msg(Req, 0) | Session ], Id).


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
%% Func: http_get/3
%% Args: URL, HTTP Version, Cookie
%%----------------------------------------------------------------------
http_get(URL, Version, Cookie) ->
	lists:append([?GET, " ", URL," ", "HTTP/", Version, ?CR, 
				  protocol_headers(Version),
				  user_agent(),
				  get_cookie(Cookie),
				  ?CR]).

%%----------------------------------------------------------------------
%% Func: http_post/4
%% Args: URL, HTTP Version, Cookie, Content
%% TODO: Content-Type ?
%%----------------------------------------------------------------------
http_post(URL, Version, Cookie, Content) ->
	ContentLength=length(Content),
	lists:append([?POST, " ", URL," ", "HTTP/", Version, ?CR,
				  protocol_headers(Version),
				  user_agent(),
				  get_cookie(Cookie),
				  "Content-Length: ",Content,?CR,
				  ?CR,
				  Content]).

%%----------------------------------------------------------------------
%% some HTTP headers functions
%%----------------------------------------------------------------------
user_agent() ->
	lists:append(["User-Agent: ", ?USER_AGENT, ?CR]).

get_cookie([]) ->
	[];
get_cookie(none) ->
	[];
get_cookie(Cookie) ->
	lists:append(["Cookie: ", Cookie, ?CR]).

%%----------------------------------------------------------------------
%% set HTTP headers specific to the protocol version
%%----------------------------------------------------------------------
protocol_headers("1.1") ->
	lists:append(["Host: ", ?server_name, ?CR]);
protocol_headers("1.0") ->
	[].
	

%%----------------------------------------------------------------------
%% Func: parse_requestline/1
%% Args: Line (string)
%% Returns: {URL (string), thinktime (integer)} | {error, Reason} 
%% Purpose: parse a line from sesslog (cf. httperf man page)
%% Input example:  /foo2.html method=POST contents='Post data'
%%----------------------------------------------------------------------
parse_requestline(Line) ->
	case regexp:split(Line, "(\s|\t)+") of  %% slow ?
		{ok, [URL | FieldList]} ->
			Request = httperf2record(FieldList),
			{ok, Request#http_request{url=URL} , round(ts_stats:exponential(?messages_intensity)) };
		{error, Reason} ->
			?PRINTDEBUG("Error while parsing session ~p~n",[Reason],?ERR),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%% Func: token2record/2
%% Args: {Name, Value} , #http_request
%% Returns: #http_request
%% Purpose: set the value of record according to {tag, value}
%%----------------------------------------------------------------------
token2record({"think", Value}, Record) -> % currently not used 
	Record;
token2record({"method", Value}, Record) ->
	Record#http_request{method = Value};
token2record({"contents", Value}, Record) ->
	Record#http_request{body = Value};
token2record(Unknown, Record) ->
	?PRINTDEBUG("Unknown ~p~n",[Unknown],?NOTICE),
	Record.

%%----------------------------------------------------------------------
%% Func: httperf2record/1
%% Args: List
%% Returns: record 
%% Purpose: parse a list of ["tag=value"] from httperf sesslog file
%%----------------------------------------------------------------------
httperf2record([]) ->
	#http_request{};
httperf2record(List) ->
	httperf2record(List,#http_request{}).

%% Func: httperf2record/2 
httperf2record( [], Record) ->
	Record;
httperf2record( [Head | Tail], Record) ->
	case string:tokens( Head, "=") of 
		[Name, Value] -> 
			NewRecord=token2record({httpd_util:to_lower(Name), Value}, Record);
		[Val] -> 
			%% we assume (!) that there were some whitespace in the contents tag
			%% so append this string to it.
			PrevContents = Record#http_request.body,
			NewRecord=Record#http_request{body = PrevContents ++ " " ++ Val};
		[] ->
			NewRecord = Record
	end,
	httperf2record(Tail, NewRecord).



%%----------------------------------------------------------------------
%% Func: set_msg/1 or /2
%% Returns: #message record
%% Purpose:
%% unless specified, the thinktime is an exponential random var.
%%----------------------------------------------------------------------
set_msg(HTTPRequest) ->
	set_msg(HTTPRequest, round(ts_stats:exponential(?messages_intensity))).
set_msg(HTTPRequest, 0) -> % no thinktime, only wait for response
	#message{ack = parse, 
			 thinktime=infinity,
			 param = HTTPRequest };
set_msg(HTTPRequest, Think) -> % end of a page, wait before the next one
	#message{ack = parse, 
			 endpage   = true,
			 thinktime = Think,
			 param = HTTPRequest }.



%%----------------------------------------------------------------------
%% Func: parse/2
%% Args: Data, State
%% Returns: {NewState, Options for socket (list)}
%% Purpose: parse the response from the server and keep information
%%  about the response if State#state_rcv.session
%%----------------------------------------------------------------------

%% Experimental code, only used when {packet, http} is set 
parse({http_response, Socket, {VsnMaj, VsnMin}, Status, Data}, State) ->
	?PRINTDEBUG("HTTP Reponse: ~p ~p ~p ~p ~n", [VsnMaj, VsnMin, Status, Data], ?DEB),
	ts_mon:addcount({ Status }),
	Http = #http{ status = Status},
	{State#state_rcv{session=Http}, []};
parse({http_header, Socket, Num, 'Content-Length', _, Data}, State) ->
	CLength = list_to_integer(Data),
	?PRINTDEBUG("HTTP Content length: ~p ~p~n", [Num, CLength], ?DEB),
	OldHttp = State#state_rcv.session,
	NewHttp = OldHttp#http{content_length=CLength},
	{State#state_rcv{session=NewHttp}, []};
parse({http_header, Socket, Num, Header, _R, Data}, State) ->
	?PRINTDEBUG("HTTP Headers: ~p ~p ~p ~p ~n", [Num, Header, _R, Data], ?DEB),
	{State, []};
parse({http_eoh, Socket}, State) ->
	?PRINTDEBUG2("HTTP eoh: ~n", ?DEB),
	{State, [{packet, raw}]};
%% end of experimental code

%% new connection, headers parsing
parse(Data, State) when (State#state_rcv.session)#http.status == none ->
	List = binary_to_list(Data),
	%%	regexp:split is much less efficient ! 
	StartHeaders = string:str(List, "\r\n\r\n"),
	Headers = string:substr(List, 1, StartHeaders-1),
	[Status, ParsedHeader] = request_header(Headers),
	?PRINTDEBUG("HTTP Headers: ~p ~n", [ParsedHeader], ?DEB),
	Cookie = parse_cookie(ParsedHeader),
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
					{State#state_rcv{session= #http{}, ack_done = true, datasize = BodySize, dyndata= Cookie}, []};
				_ ->
					Http = #http{content_length = CLength,
								 status         = Status,
								 body_size      = BodySize},
					{State#state_rcv{session = Http, ack_done = false, datasize = BodySize, dyndata=Cookie},[]}
			end
	end;

%% TODO : handle the case where the Headers are not complete in the first message
%% current connection
parse(Data, State) ->
	DataSize = size(Data),
	?PRINTDEBUG("HTTP Body size=~p ~n",[DataSize], ?DEB),
	Size = (State#state_rcv.session)#http.body_size + DataSize,
	CLength = (State#state_rcv.session)#http.content_length,
	case Size of 
		CLength -> % end of response
			{State#state_rcv{session= #http{}, ack_done = true, datasize = Size},
			 []};
%			 [{packet, http}]}; %% testing purpose
		_ ->
			Http = (State#state_rcv.session)#http{body_size = Size},
			{State#state_rcv{session= Http, ack_done = false, datasize = Size}, []}
	end.
												 
			

%%----------------------------------------------------------------------
%% Func: request_header/1
%% Returns: [HTTPStatus (integer), Headers]
%% Purpose: parse HTTP headers. 
%%----------------------------------------------------------------------
request_header(Header)->
    [ResponseLine|HeaderFields] = httpd_parse:split_lines(Header),
    ParsedHeader = httpd_parse:tagup_header(HeaderFields),
	[get_status(ResponseLine), ParsedHeader].

%%----------------------------------------------------------------------
%% Func: get_status/1
%% Purpose: returns HTTP status code
%%----------------------------------------------------------------------
get_status(Line) ->
	StartStatus = string:str(Line, " "),
	Status = string:substr(Line, StartStatus+1, 3),
	list_to_integer(Status).

%%----------------------------------------------------------------------
%% Func: parse_cookie/1
%% Purpose: returns HTTP cookie code
%%----------------------------------------------------------------------
parse_cookie(ParsedHeader) ->
	case httpd_util:key1search(ParsedHeader,"set-cookie") of
		undefined ->
			[];
		Cookie ->
				case string:tokens( Cookie, "; ") of 
					[CookieVal |CookieOtherAttrib] ->
						% TODO handle path attribute
						% several cookies can be set with a different path attribute
						CookieVal ;
					_Other -> % something wrong
						[]
				end
	end.




