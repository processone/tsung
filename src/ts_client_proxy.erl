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
%%% File    : ts_client_proxy.erl
%%% Author  : Nicolas Niclausse <nniclausse@idealx.com>
%%% Description : handle communication with client and server.
%%%
%%% Created : 22 Dec 2003 by Nicolas Niclausse <nniclausse@idealx.com>
%%%-------------------------------------------------------------------

-module(ts_client_proxy).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_http.hrl").

-define(lifetime, 120000).

%%--------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          clientsock,
          parse_status=new, %% http status = body|new|headers
          body_size=0,
          content_length=0,
          serversock
          }).
%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the gen_server with the socket given by the listener
%%--------------------------------------------------------------------
start(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

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
init([Socket]) ->
    {ok, #state{clientsock=Socket}}.

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
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
% client data, parse and send it to the server.
handle_info({tcp, ClientSock, String}, State) 
  when ClientSock == State#state.clientsock ->
    ok=inet:setopts(ClientSock,[{active, once}]),
    {ok, NewState}  = parse(State,ClientSock,State#state.serversock,String),
    {noreply, NewState, ?lifetime};

% server data, send it to the client
handle_info({tcp, ServerSock, String}, State) 
  when ServerSock == State#state.serversock ->
    ok=inet:setopts(ServerSock,[{active, once}]),
    gen_tcp:send(State#state.clientsock, String),
    {noreply, State, ?lifetime};

%%%%%%%%%%%% Errors and termination %%%%%%%%%%%%%%%%%%%

% Log who did close the connection, and exit.
handle_info({tcp_closed, Socket}, State=#state{serversock=Socket})->
    ?LOG("socket closed by server~n",?INFO),
    {noreply, State#state{serversock=undefined}, ?lifetime};

handle_info({tcp_closed, Socket}, State) ->
    ?LOG("socket closed by client~n",?INFO),
    {stop, normal, ?lifetime};

	
% Log properly who caused an error, and exit.
handle_info({tcp_error, Socket, Reason}, State) ->
    ?LOGF("error on socket ~p ~p~n",[Socket,Reason],?ERR),
    {stop, {error, sockname(Socket,State), Reason}, State};

handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(Info, State) ->
    {stop, unknown, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
%    ts_proxy_recorder:dorecord(endsession),
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
%% Function: sockname/2
%%           sockname(Socket,State)
%% Purpose: decides whether some socket is the client or the server
%% Description: State contains two fields, "serversock" and "clientsock".
%%              This function searches Socket among the two, and returns
%%              an appropriate atom among 'server', 'client' and 'unknown'.
%% Returns: Sockname
%% Types:   Sockname -> server | client | unknown
%%          State -> state_record()

sockname(Socket,State)->
    if 
        (Socket == State#state.serversock) -> server;
        (Socket == State#state.clientsock) -> client;
        true -> unknown
    end.

%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse HTTP request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State=#state{parse_status=Status},ClientSock,ServerSocket,String) 
  when Status == new ->
    StartHeaders = string:str(String, "\r\n\r\n"),
    Headers = string:substr(String, 1, StartHeaders-1),%FIXME: headers may not be complete
    {ok,[Method, RequestURI, HTTPVersion, RequestLine, ParsedHeader]} =
        httpd_parse:request_header(Headers),
    TotalSize= length(String),
    ?LOGF("StartHeaders = ~p, totalsize =~p~n",[StartHeaders, TotalSize],?DEB),
    case {httpd_util:key1search(ParsedHeader,"content-length"), TotalSize-StartHeaders} of
        {undefined, 3} -> % no body, everything received
            ts_proxy_recorder:dorecord({#http_request{method=Method,
                                                      url=RequestURI,
                                                      version=HTTPVersion,
                                                      headers=ParsedHeader}
                                        }),
            NewSocket = check_serversocket(ServerSocket,RequestURI),
            gen_tcp:send(NewSocket,String),
            {ok, State#state{parse_status = new,
                             serversock=NewSocket}};
        {undefined, Diff} ->
            {error, undefined};
        {Length, _} ->
            CLength = list_to_integer(Length)+4,
            ?LOGF("HTTP Content-Length:~p~n",[CLength], ?DEB),
            ts_proxy_recorder:dorecord({#http_request{method=Method,
                                                      url=RequestURI,
                                                      version=HTTPVersion,
                                                      headers=ParsedHeader}
                                       }),
            HeaderSize = length(Headers),
            BodySize = TotalSize-HeaderSize,
            if
                BodySize == CLength ->  % end of response
                    NewSocket = check_serversocket(ServerSocket,RequestURI),
                    gen_tcp:send(NewSocket,String),
                    {ok, State#state{parse_status = new,
                                     serversock=NewSocket}};
                BodySize > CLength  ->
                    {error, bad_content_length};
                true ->
                    NewSocket = check_serversocket(ServerSocket,RequestURI),
                    gen_tcp:send(NewSocket,String),
                    {ok, State#state{content_length = CLength,
                                body_size = TotalSize,
                                parse_status = body
                                    }}
            end
    
    end;

parse(State=#state{parse_status=Status},ClientSock,ServerSocket,String) 
  when Status == body ->
	DataSize = length(String),
	?LOGF("HTTP Body size=~p ~n",[DataSize], ?DEB),
	Size = State#state.body_size + DataSize,
	CLength = State#state.content_length,
    gen_tcp:send(ServerSocket, String),
	case Size of 
		CLength -> % end of response
			State#state{body_size=0,parse_status=new, content_length=0};
		_ ->
			State#state{body_size = Size}
	end.

%%--------------------------------------------------------------------
%% Func: check_serversocket/2
%% Purpose: If the socket is not defined, or if the server is not the
%%          same, connect to the server as specified in URL
%% Returns: Socket
%%--------------------------------------------------------------------            
check_serversocket(Socket, URL) when list(URL)->
    check_serversocket(Socket, ts_http_common:parse_URL(URL));
check_serversocket(undefined, URL) ->
    Port = set_port(URL),
    ?LOGF("Connecting to ~p:~p ...~n", [URL#url.host, Port],?DEB),
    {ok, Socket} = gen_tcp:connect(URL#url.host,Port,
                                   [{active, once}]),
    ?LOGF("Connected to server ~p on port ~p (socket is ~p)~n",
          [URL#url.host,Port,Socket],?INFO),
    Socket;
    
check_serversocket(Socket, URL=#url{port=Port,host=Host}) ->
    RealPort = set_port(URL),
    {ok, RealIP} = inet:getaddr(Host,inet),
    case inet:peername(Socket) of
        {ok, {RealIP, RealPort}} -> % same as previous URL
            ?LOGF("Reuse socket ~p on URL ~p~n", [Socket, URL],?INFO),
            Socket;
        Other ->
            gen_tcp:close(Socket),
            ?LOGF("New server configuration  (~p:~p, was ~p) on URL ~p~n", 
                  [RealIP, RealPort, Other, URL],?NOTICE),
            {ok, NewSocket} = gen_tcp:connect(Host,RealPort, [{active, once}]),
            NewSocket
    end.

%%--------------------------------------------------------------------
%% Func: set_port/1
%% Purpose: Returns port according to scheme if not already defined
%% Returns: PortNumber (integer)
%%--------------------------------------------------------------------
set_port(#url{scheme=https,port=undefined})  -> 443;
set_port(#url{scheme=http,port=undefined})   -> 80;
set_port(#url{port=Port}) when integer(Port) -> Port;
set_port(#url{port=Port}) -> integer_to_list(Port).
                
