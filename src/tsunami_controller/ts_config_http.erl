%%%
%%%  Copyright © IDEALX S.A.S. 2004
%%%
%%%	 Author : Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%%  Created: 20 Apr 2004 by Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
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
%%%  the two.

%%% common functions used by http clients to parse config

-module(ts_config_http).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-export([parse_config/2, parse_URL/1, set_port/1]).

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=http}, 
             Config=#config{curid = Id, session_tab = Tab, server = Server,
                            sessions = [CurS | _], dynvar=DynVar,
							subst    = SubstFlag, match=MatchRegExp}) ->
    Version  = ts_config:getAttr(Element#xmlElement.attributes, version),
    URL      = ts_config:getAttr(Element#xmlElement.attributes, url),
    Contents = ts_config:getAttr(Element#xmlElement.attributes, contents),
    UseProxy = ts_config:getAttr(atom, Element#xmlElement.attributes, use_server_as_proxy, false),
    %% Apache Tomcat applications need content-type informations to read post forms
    ContentType = ts_config:getAttr(string,Element#xmlElement.attributes,
                            content_type, "application/x-www-form-urlencoded"),
    Date     = ts_config:getAttr(string, Element#xmlElement.attributes, 
                                 'if_modified_since', undefined),
    Method = case ts_config:getAttr(Element#xmlElement.attributes, method) of 
                 "GET" -> get;
                 "POST"-> post;
                 Other ->
                     ?LOGF("Bad method ! ~p ~n",[Other],?ERR),
                     get
             end,
            
    Request = #http_request{url         = URL,
                            method      = Method,
                            version     = Version,
                            get_ims_date= Date,
                            content_type= ContentType,
                            body        = list_to_binary(Contents)},
    %% SOAP Support: Add SOAPAction header to the message
    Request2 = case lists:keysearch(soap,#xmlElement.name,
                                    Element#xmlElement.content) of
                   {value, SoapEl=#xmlElement{} } ->
                       SOAPAction  = ts_config:getAttr(SoapEl#xmlElement.attributes,
                                                       action),
                       Request#http_request{soap_action=SOAPAction};
                   _ -> 
                       Request
               end,
    PreviousHTTPServer = case ets:lookup(Tab, {http_server, CurS#session.id}) of 
                             [] -> [];
                             [{_Key,PrevServ}] -> PrevServ
                         end,
    Msg = case lists:keysearch(www_authenticate,#xmlElement.name,
                               Element#xmlElement.content) of
              {value, AuthEl=#xmlElement{} } ->
                  UserId  = ts_config:getAttr(string,AuthEl#xmlElement.attributes,
                                              userid, undefined),
                  Passwd  = ts_config:getAttr(string,AuthEl#xmlElement.attributes, 
                                              passwd, undefined),
                  NewReq=Request2#http_request{userid=UserId, passwd=Passwd},
                  set_msg(NewReq, 0, {SubstFlag, MatchRegExp, UseProxy, Server, PreviousHTTPServer, Tab, CurS#session.id} );
              _ ->
                  set_msg(Request2, 0, {SubstFlag, MatchRegExp, UseProxy, Server, PreviousHTTPServer, Tab, CurS#session.id} )
          end,

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id},Msg#ts_request{endpage=true,
                                                         dynvar_specs=DynVar}}),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=undefined},
                 Element#xmlElement.content);
%% Parsing default values
parse_config(Element = #xmlElement{name=default}, Conf = #config{session_tab = Tab}) ->
    case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "user_agent" ->
            Val = ts_config:getAttr(Element#xmlElement.attributes, value), %FIXME: useless
            lists:foldl( fun(A,B)->parse_config(A,B) end, Conf, Element#xmlElement.content)
    end,
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end, Conf, Element#xmlElement.content);
%% Parsing user_agent
parse_config(Element = #xmlElement{name=user_agent}, Conf = #config{session_tab = Tab}) ->
    Freq= ts_config:getAttr(integer,Element#xmlElement.attributes, frequency),
    [Val]= ts_config:getText(Element#xmlElement.content),
    ?LOGF("Get user agent: ~p ~p ~n",[Freq, Val],?WARN),
    Previous = case ets:lookup(Tab, {http_user_agent, value}) of 
                   [] ->
                       [];
                   [{Key,Old}] -> 
                       Old
               end,
    ets:insert(Tab,{{http_user_agent, value}, [{Freq, Val}|Previous]}),
    lists:foldl( fun(A,B)->parse_config(A,B) end, Conf, Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.


%%----------------------------------------------------------------------
%% Func: set_msg/3
%% Returns: #ts_request record
%% Purpose: build the #ts_request record from an #http_request,
%% thinktime and Substition def.
%%----------------------------------------------------------------------
%% if the URL is full (http://...), we parse it and get server host,
%% port and scheme from the URL and override the global setup of the
%% server. These informations are stored in the #ts_request record.
set_msg(HTTP=#http_request{url="http" ++ URL}, 
        ThinkTime, {SubstFlag, MatchRegExp, UseProxy, Server, _PrevHTTPServer, Tab, Id}) ->  % full URL
    URLrec = parse_URL("http" ++ URL),
    Path = set_query(URLrec),
    HostHeader = set_host_header(URLrec),
    Port = set_port(URLrec),
    Scheme = set_scheme(URLrec#url.scheme),
    ets:insert(Tab,{{http_server, Id}, {HostHeader}}),
    set_msg2(HTTP#http_request{url=Path, host_header = HostHeader}, ThinkTime,
            #ts_request{ack  = parse,
						subst = SubstFlag,
						match = MatchRegExp,
                        host = URLrec#url.host,
                        scheme = Scheme,
                        port = Port});

%% relative URL, no previous HTTP server (hence, first request in session)
set_msg(HTTPRequest, Think, {SubstFlag, MatchRegExp, false, Server, [],_Tab,_Id}) -> 
    Tmp= case Server#server.type of
             gen_tcp -> "http";
             ssl -> "https"
         end,
    URL =lists:append([Tmp, "://", Server#server.host, ":",
                      integer_to_list(Server#server.port),
                      HTTPRequest#http_request.url]),
    set_msg(HTTPRequest#http_request{url=URL}, Think, {SubstFlag, MatchRegExp, false, Server, [],_Tab,_Id});
    
%% relative URL, no previous HTTP server, use proxy, error !
set_msg(HTTPRequest, Think, {SubstFlag, MatchRegExp, true, _Server, [],_Tab,_Id}) -> 
    ?LOG("Need absolut URL when using a proxy ! Abort",?ERR),
    throw({error, badurl_proxy});
%% relative URL, no proxy
set_msg(HTTPRequest, Think, {SubstFlag, MatchRegExp, false, _Server, {HostHeader},_Tab,_Id}) -> 
%%% use global host, port and scheme (undefined value), it will be
%%% dynamicaly set during the run (using default server or previous
%%% one used in the current session)
    set_msg2(HTTPRequest#http_request{host_header= HostHeader}, Think,
             #ts_request{ack = parse, subst = SubstFlag, match = MatchRegExp });
set_msg(HTTPRequest, Think, {SubstFlag, MatchRegExp, true, Server, {HostHeader},_Tab,_Id}) -> % relative URL, use proxy
%%% use global host, port and scheme (undefined value), it will be
%%% dynamicaly set during the run (using default server or previous
%%% one used in the current session)
    set_msg2(HTTPRequest#http_request{host_header= HostHeader}, Think,
             #ts_request{ack = parse, subst = SubstFlag, match = MatchRegExp }).

%% Func: set_mgs2/3
%% Purpose: set param and thinktime in ts_request
%% Returns: ts_request
set_msg2(HTTPRequest, 0, Msg) -> % no thinktime, only wait for response
	Msg#ts_request{ thinktime=infinity,
                    param = HTTPRequest };
set_msg2(HTTPRequest, Think, Msg) -> % end of a page, wait before the next one
	Msg#ts_request{ endpage   = true,
                    thinktime = Think,
                    param = HTTPRequest }.

%%--------------------------------------------------------------------
%% Func: set_host_header/1
%%--------------------------------------------------------------------
set_host_header(#url{host=Host,scheme=https,port=undefined})  -> Host ++":443";
set_host_header(#url{host=Host,scheme=http,port=undefined})   -> Host;
set_host_header(#url{host=Host,port=Port}) when is_integer(Port) ->
    Host ++ ":" ++ integer_to_list(Port);
set_host_header(#url{host=Host,port=Port}) when is_list(Port) ->
    Host ++ ":" ++ Port.


%%--------------------------------------------------------------------
%% Func: set_port/1
%% Purpose: Returns port according to scheme if not already defined
%% Returns: PortNumber (integer)
%%--------------------------------------------------------------------
set_port(#url{scheme=https,port=undefined})  -> 443;
set_port(#url{scheme=http,port=undefined})   -> 80;
set_port(#url{port=Port}) when is_integer(Port) -> Port;
set_port(#url{port=Port}) -> integer_to_list(Port).

set_scheme(http) -> gen_tcp;
set_scheme(https) -> ssl.
    

set_query(URLrec = #url{querypart=""}) ->
    URLrec#url.path;
set_query(URLrec = #url{}) ->
    URLrec#url.path ++ "?" ++ URLrec#url.querypart.


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


