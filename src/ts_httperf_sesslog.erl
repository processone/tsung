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

%%% common functions used by http clients to parse httperf's style
%%% session file

-module(ts_httperf_sesslog).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("../include/ts_profile.hrl").
-include("../include/ts_http.hrl").

-export([get_client/2,
		 get_client/3,
		 get_simple_client/2,
         build_session/1
		]).

%%----------------------------------------------------------------------
%% Func: get_client/2
%% Purpose: Generate a client session.
%% N and Id are currently not used (instead, data are taken from the
%%  the session server).
%% Returns: List
%%----------------------------------------------------------------------
get_client(N, Id) ->
	get_client(N, Id, ?config(http_req_filename)).
get_client(N, Id, File) ->
	ts_req_server:read_sesslog(File),
	{ok, Session} = ts_req_server:get_next_session(),
    Session.
%%%	build_session([], Session, Id).

%%----------------------------------------------------------------------
%% Func: get_simple_client/2
%% Purpose: build an http session without reading inputs from a file
%% Returns: List
%% currently unused !
%%----------------------------------------------------------------------
get_simple_client(N, Id) ->
	ts_req_server:read(?config(http_req_filename)),
	build_simplesession(N, [], Id).


%%----------------------------------------------------------------------
%% Func: build_session/1
%% Purpose: build a session; a session is a list of Pages where a Page 
%% is a list of 'URL'. Put a fixed Id
%% Returns: List
%%----------------------------------------------------------------------
build_session(Session) ->
	build_session([], Session, 1). 

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
			{ok, Request#http_request{url=URL} , 
			 round(ts_stats:exponential(?messages_intensity)) };
		{error, Reason} ->
			?LOGF("Error while parsing session ~p~n",[Reason],?ERR),
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
    LowValue = list_to_atom(httpd_util:to_lower(Value)),
	Record#http_request{method = LowValue};
token2record({"contents", Value}, Record) ->
	Record#http_request{body = Value};
token2record(Unknown, Record) ->
	?LOGF("Unknown ~p~n",[Unknown], ?NOTICE),
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
    URLrec = ts_http_common:parse_URL("http" ++ URL),
    Path = URLrec#url.path ++ URLrec#url.querypart,
    case URLrec of
        #url{port = undefined, scheme= https} ->
            set_msg(HTTP#http_request{url=Path },
                    ThinkTime,
                    #message{ack  = parse,
                             host = URLrec#url.host,
                             scheme = ssl,
                             port = 443});
        #url{port = undefined, scheme= http} ->
            set_msg(HTTP#http_request{url=Path},
                    ThinkTime,
                    #message{ack  = parse,
                             host = URLrec#url.host,
                             scheme = gen_tcp,
                             port = 80});
        #url{scheme= http} ->
            set_msg(HTTP#http_request{url=Path},
                    ThinkTime,
                    #message{ack  = parse,
                             host = URLrec#url.host,
                             scheme = gen_tcp,
                             port = URLrec#url.port});
        #url{scheme= https} ->
            set_msg(HTTP#http_request{url=Path},
                    ThinkTime,
                    #message{ack  = parse,
                             host = URLrec#url.host,
                             scheme = ssl,
                             port = URLrec#url.port})
    end;
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


    



