%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%	 Author : Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%%  Created: 22 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
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

%%%-------------------------------------------------------------------
%%% File    : ts_proxy_recorder.erl
%%% Author  : Nicolas Niclausse <nniclausse@IDEALX.com>
%%% Description : 
%%%
%%% Created : 22 Dec 2003 by Nicolas Niclausse <nniclausse@IDEALX.com>
%%%-------------------------------------------------------------------

-module(ts_proxy_recorder).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_http.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/1, dorecord/1]).
-export([decode_basic_auth/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {log_file,    % logfile name
                logfd,       % logfile IODevice
                prev_port,   % previous port  
                prev_host,   % previous hostname 
                timestamp=0, % last request date
                thinktime_low = 1000 % dot not record thinktime less than this
                                     % value (msec)
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% Function: dorecord/1
%% Description: record a new request
%%--------------------------------------------------------------------
dorecord(Args) ->
    gen_server:cast(?MODULE,{record, Args}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init(Filename) ->
    Date = ts_utils:datestr(),
    %% add date to filename
    File= case regexp:gsub(Filename,"\.xml$", Date ++ ".xml") of %% "
        {ok, RealName, _ } -> RealName;
        _ ->  Date ++ "-" ++ Filename         
    end,    
    case file:open(File,write) of 
		{ok, Stream} ->
			?LOG("starting recorder~n",?NOTICE),
			{ok, #state{ log_file = File,
                         logfd    = Stream
					   }};
		{error, Reason} ->
			?LOGF("Can't open log file ~p! ~p~n",[File,Reason], ?ERR),
			{stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({record, endsession}, State) ->
    io:format(State#state.logfd,"</session>"),
    {noreply, State};

handle_cast({record, {HTTPRequest}}, State=#state{timestamp=0}) -> % first record
    Name= ts_utils:datestr(),
    io:format(State#state.logfd,"<session name='~s' popularity='1' "++
              " persistent='true' messages_ack='parse' type='ts_http'>~n",["rec"++Name]),
    {ok, NewState} = record_http_request(State, HTTPRequest),
    {noreply, NewState#state{timestamp=now()}};

handle_cast({record, {HTTPRequest}}, State) ->
    TimeStamp=now(),
    Elapsed = ts_utils:elapsed(State#state.timestamp,TimeStamp),
    if
        Elapsed < State#state.thinktime_low ->
            ?LOGF("skip too low thinktime, assuming it's an embedded object (~p)~n",
                  [Elapsed],?INFO);
        true ->
            io:format(State#state.logfd,"<thinktime value='~p'></thinktime>~n",
                      [round(Elapsed/1000)])
    end,
    {ok, NewState} = record_http_request(State, HTTPRequest),
    {noreply, NewState#state{timestamp=TimeStamp}};

handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    io:format(State#state.logfd,"</session>~n",[]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: record_http_request/2
%% Purpose: record request given State=#state and Request=#http_request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
record_http_request(State=#state{prev_host=Host, prev_port=Port},
                    Request=#http_request{method  = Method, url = RequestURI,
                                          version = "HTTP/" ++ HTTPVersion,
                                          headers = ParsedHeader,body=Body}) ->
    FullURL = case RequestURI of 
                  "http://{"++Rest -> "https://" ++ Rest;
                  _ -> RequestURI
              end,
                  
    {URL,NewPort,NewHost} = 
        case ts_http_common:parse_URL(FullURL) of 
              #url{path=RelURL, host=Host, port=Port,querypart=[]} ->
                {RelURL, Port, Host};
              #url{path=RelURL, host=Host, port=Port,querypart=Args} ->
                {RelURL++"?"++Args, Port, Host};
              #url{path=RelURL, host=Host2,port=Port2,querypart=Args } ->
                {FullURL,Port2,Host2 }
        end,
    Fd=State#state.logfd,
    URL2 = case regexp:gsub(URL,"&","&amp;") of  % FIXME: what if already &amp; ?
        {ok,NewURL,RepCount} -> NewURL;
        _ -> URL
    end,

    io:format(Fd,"<request><http url='~s' version='~s' ",
              [URL2, HTTPVersion]),
    case Body of 
        [] -> ok;
        _  -> io:format(Fd," contents='~s' ", [Body]) % must be a POST method
    end,
    case httpd_util:key1search(ParsedHeader,"if-modified-since") of 
        undefined ->
            io:format(Fd,"method='~s'>~n", [ Method]);
        Date ->
            io:format(Fd,"method='GETIMS' date='~s'>",[Date])
    end,
    case httpd_util:key1search(ParsedHeader,"authorization") of 
        "Basic "++Base64 ->
			{User,Passwd}=decode_basic_auth(Base64),
            io:format(Fd,"<www_authenticate userid=~p passwd=~p/>~n",[User,Passwd]);
		_ -> ok
    end,
	io:format(Fd,"</http></request>~n",[]),

    {ok, State#state{prev_port=NewPort,prev_host=NewHost}}.

%%--------------------------------------------------------------------
%% Func: decode_basic_auth/1
%% Purpose: decode base64 encoded user passwd for basic authentication
%% Returns: {User, Passwd}
%%--------------------------------------------------------------------
decode_basic_auth(Base64)->
	AuthStr= httpd_util:decode_base64(Base64),
	Sep = string:chr(AuthStr,$:),
	{string:substr(AuthStr,1,Sep-1),string:substr(AuthStr,Sep+1)}.
	
