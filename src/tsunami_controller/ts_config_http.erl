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

-include_lib("xmerl/inc/xmerl.hrl").

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
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS |SList], dynvar=DynVar,
							subst    = SubstFlag, match=MatchRegExp}) ->
    Version  = ts_config:getAttr(Element#xmlElement.attributes, version),
    URL      = ts_config:getAttr(Element#xmlElement.attributes, url),
    Contents = ts_config:getAttr(Element#xmlElement.attributes, contents),
    %% Apache Tomcat applications need content-type informations to read post forms
    ContentType = ts_config:getAttr(Element#xmlElement.attributes,
                            content_type, "application/x-www-form-urlencoded"),
    Date     = ts_config:getAttr(Element#xmlElement.attributes, 
                                 'if_modified_since', undefined),
    Method = case ts_config:getAttr(Element#xmlElement.attributes, method) of 
                 "GET" -> get;
                 "POST"-> post;
                 Other ->
                     ?LOGF("Bad method ! ~p ~n",[Other],?ERR),
                     get
             end,
    ServerName = ts_config:get_default(Tab,http_server_name, server_name),
    Request = #http_request{url         = URL,
                            method      = Method,
                            version     = Version,
                            get_ims_date= Date,
                            server_name = ServerName,
                            content_type= ContentType,
                            body        = list_to_binary(Contents)},
    %% SOAP Support: Add SOAPAction header to the message
    Request2 = case lists:keysearch(soap,#xmlElement.name,
				    Element#xmlElement.content) of
              {value, SoapEl=#xmlElement{} } ->
                       SOAPAction  = ts_config:getAttr(SoapEl#xmlElement.attributes,
                                                       action, []),
                       Request#http_request{soap_action=SOAPAction};
                   _ -> 
                       Request
               end,
    Msg = case lists:keysearch(www_authenticate,#xmlElement.name,
                               Element#xmlElement.content) of
              {value, AuthEl=#xmlElement{} } ->
                  UserId  = ts_config:getAttr(AuthEl#xmlElement.attributes,
                                              userid, undefined),
                  Passwd  = ts_config:getAttr(AuthEl#xmlElement.attributes, 
                                              passwd, undefined),
                  NewReq=Request2#http_request{userid=UserId, passwd=Passwd},
                  set_msg(NewReq, 0, {SubstFlag, MatchRegExp} );
              _ ->
                  set_msg(Request2, 0, {SubstFlag, MatchRegExp} )
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
        "server_name" ->
            Val = ts_config:getAttr(Element#xmlElement.attributes, value),
            ets:insert(Tab,{{http_server_name, value}, Val})
    end,
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end, Conf, Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(Element, Conf = #config{}) ->
    Conf.


%%----------------------------------------------------------------------
%% Func: set_msg/1 or /3
%% Returns: #ts_request record
%% Purpose:
%% unless specified, the thinktime is an exponential random var.
%%----------------------------------------------------------------------
set_msg(HTTPRequest) ->
	set_msg(HTTPRequest, round(ts_stats:exponential(?messages_intensity)), false).

%% if the URL is full (http://...), we parse it and get server host,
%% port and scheme from the URL and override the global setup of the
%% server. These informations are stored in the #ts_request record.
set_msg(HTTP=#http_request{url="http" ++ URL}, 
        ThinkTime, {SubstFlag, MatchRegExp}) ->  % full URL
    URLrec = parse_URL("http" ++ URL),
    Path = URLrec#url.path ++ URLrec#url.querypart,
    Port = set_port(URLrec),
    Scheme = case URLrec#url.scheme of
                 http  -> gen_tcp;
                 https -> ssl
             end,
    set_msg2(HTTP#http_request{url=Path}, ThinkTime,
            #ts_request{ack  = parse,
						subst = SubstFlag,
						match = MatchRegExp,
                        host = URLrec#url.host,
                        scheme = Scheme,
                        port = Port});
%
set_msg(HTTPRequest, Think, {SubstFlag, MatchRegExp}) -> % relative URL, 
%%% use global host, port and scheme
    set_msg2(HTTPRequest, Think, #ts_request{ack = parse,
                                             subst = SubstFlag,
                                             match = MatchRegExp
                                            }).
            
set_msg2(HTTPRequest, 0, Msg) -> % no thinktime, only wait for response
	Msg#ts_request{ thinktime=infinity,
                    param = HTTPRequest };
set_msg2(HTTPRequest, Think, Msg) -> % end of a page, wait before the next one
	Msg#ts_request{ endpage   = true,
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
