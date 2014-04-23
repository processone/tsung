%%%  This code was developped by Zhihui Jiao(jzhihui521@gmail.com).
%%%
%%%  Copyright (C) 2013 Zhihui Jiao
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

-module(ts_config_mqtt).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_mqtt.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name = dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element, Conf);
parse_config(Element = #xmlElement{name = mqtt},
             Config = #config{curid = Id, session_tab = Tab,
                              sessions = [CurS | _], dynvar = DynVar,
                              subst = SubstFlag, match = MatchRegExp}) ->
    Type = ts_config:getAttr(atom, Element#xmlElement.attributes, type),
    CleanStart = ts_config:getAttr(atom, Element#xmlElement.attributes,
                                   clean_start, true),
    UserName = ts_config:getAttr(string, Element#xmlElement.attributes,
                                   username, undefined),
    Password = ts_config:getAttr(string, Element#xmlElement.attributes,
                                   password, undefined),
    KeepAlive = ts_config:getAttr(float_or_integer, Element#xmlElement.attributes,
                                  keepalive, 10),
    WillTopic = ts_config:getAttr(string, Element#xmlElement.attributes,
                                  will_topic, ""),
    WillQos = ts_config:getAttr(float_or_integer, Element#xmlElement.attributes,
                                will_qos, 0),
    WillMsg = ts_config:getAttr(string, Element#xmlElement.attributes,
                                will_msg, ""),
    WillRetain = ts_config:getAttr(atom, Element#xmlElement.attributes,
                                   will_retain, false),
    Topic = ts_config:getAttr(string, Element#xmlElement.attributes, topic, ""),
    Qos = ts_config:getAttr(float_or_integer, Element#xmlElement.attributes,
                            qos, 0),
    Retained = ts_config:getAttr(atom, Element#xmlElement.attributes,
                                 retained, false),
    RetainValue = case Retained of
        true -> 1;
        false -> 0
    end,
    Timeout = ts_config:getAttr(float_or_integer, Element#xmlElement.attributes,
                                timeout, 1),
    Payload = ts_config:getText(Element#xmlElement.content),

    Request = #mqtt_request{type = Type, clean_start = CleanStart,
                            keepalive = KeepAlive, will_topic = WillTopic,
                            will_qos = WillQos, will_msg = WillMsg,
                            will_retain = WillRetain, topic = Topic, qos = Qos,
                            retained = RetainValue, payload = Payload, username = UserName, password = Password},
    Ack = case {Type, Qos} of
        {publish, 0} -> no_ack;
        {disconnect, _} -> no_ack;
        _ -> parse
    end,
    Msg = #ts_request{ack = Ack,
                      endpage = true,
                      dynvar_specs  = DynVar,
                      subst   = SubstFlag,
                      match   = MatchRegExp,
                      param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    case Type of
        waitForMessages ->
            ets:insert(Tab, {{CurS#session.id, Id},
                             {thinktime, Timeout * 1000}});
        _ ->
            ets:insert(Tab, {{CurS#session.id, Id}, Msg })
    end,

    ?LOGF("request tab: ~p~n", [ets:match(Tab, '$1')], ?INFO),

    lists:foldl( fun(A, B)->ts_config:parse(A, B) end,
                 Config#config{dynvar = []},
                 Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.
