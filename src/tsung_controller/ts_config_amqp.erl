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

-module(ts_config_amqp).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_amqp.hrl").

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
parse_config(Element = #xmlElement{name = amqp},
             Config = #config{curid = Id, session_tab = Tab,
                              sessions = [CurS | _], dynvar = DynVar,
                              subst = SubstFlag, match = MatchRegExp}) ->
    initialize_options(Tab),

    TypeStr = ts_config:getAttr(string, Element#xmlElement.attributes, type),
    Type = list_to_atom(TypeStr),

    ReqList = case Type of
        %% connection.open request, we add all the requests to be done
        'connection.open' ->
            ['connect', 'connection.start_ok', 'connection.tune_ok',
             'connection.open'];
        'waitForConfirms' ->
            Timeout = ts_config:getAttr(float_or_integer,
                                        Element#xmlElement.attributes,
                                        timeout, 1),
            ets:insert(Tab, {{CurS#session.id, Id}, {thinktime, Timeout * 1000}}),
            [];
        'waitForMessages' ->
            Timeout = ts_config:getAttr(float_or_integer,
                                        Element#xmlElement.attributes,
                                        timeout, 60),
            ets:insert(Tab, {{CurS#session.id, Id}, {thinktime, Timeout * 1000}}),
            [];
        _ ->
            [Type]
    end,
    Result = lists:foldl(fun(RequestType, CurrId) ->
                {Ack, Request} = parse_request(Element, RequestType, Tab),
                Msg = #ts_request{ack = Ack,
                                  endpage = true,
                                  dynvar_specs  = DynVar,
                                  subst   = SubstFlag,
                                  match   = MatchRegExp,
                                  param   = Request},
                ets:insert(Tab, {{CurS#session.id, CurrId}, Msg}),
                CurrId + 1
        end, Id, ReqList),

    ResultId = case ReqList of
        [] -> Id;
        _ -> Result - 1
    end,
    
    ts_config:mark_prev_req(Id - 1, Tab, CurS),

    lists:foldl(fun(A, B) -> ts_config:parse(A, B) end,
                Config#config{dynvar = [], curid = ResultId},
                Element#xmlElement.content);

%% Parsing options
parse_config(Element = #xmlElement{name=option}, Conf = #config{session_tab = Tab}) ->
    NewConf = case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "username" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes,
                                    value,?AMQP_USER),
            ets:insert(Tab,{{amqp_username,value}, Val}),
            Conf;
        "password" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes,
                                    value,?AMQP_PASSWORD),
            ets:insert(Tab,{{amqp_password,value}, Val}),
            Conf;
        "heartbeat" ->
            Val = ts_config:getAttr(float_or_integer,
                                    Element#xmlElement.attributes, value, 600),
            ets:insert(Tab,{{amqp_heartbeat,value}, Val}),
            Conf
    end,
    lists:foldl(fun(A,B) -> ts_config:parse(A,B) end,
                NewConf, Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

parse_request(Element, Type = 'connection.open', _Tab) ->
    Vhost = ts_config:getAttr(string, Element#xmlElement.attributes, vhost, "/"),
    Request = #amqp_request{type = Type, vhost = Vhost},
    {parse, Request};
parse_request(_Element, Type = 'connection.start_ok', Tab) ->
    UserName = ts_config:get_default(Tab, amqp_username),
    Password = ts_config:get_default(Tab, amqp_password),
    Request = #amqp_request{type = Type, username = UserName,
                            password = Password},
    {parse, Request};
parse_request(_Element, Type = 'connection.tune_ok', Tab) ->
    HeartBeat = ts_config:get_default(Tab, amqp_heartbeat),
    Request = #amqp_request{type = Type, heartbeat = HeartBeat},
    {no_ack, Request};
parse_request(Element, Type = 'channel.open', _Tab) ->
    Channel = ts_config:getAttr(string, Element#xmlElement.attributes,
                                channel, "0"),
    Request = #amqp_request{type = Type, channel = Channel},
    {parse, Request};
parse_request(Element, Type = 'basic.publish', _Tab) ->
    Channel = ts_config:getAttr(string, Element#xmlElement.attributes,
                                channel, "1"),
    Exchange = ts_config:getAttr(string, Element#xmlElement.attributes,
                                 exchange, ""),
    RoutingKey = ts_config:getAttr(string, Element#xmlElement.attributes,
                                   routing_key, "/"),
    Size = ts_config:getAttr(float_or_integer, Element#xmlElement.attributes,
                             payload_size, 100),
    PersistentStr = ts_config:getAttr(string, Element#xmlElement.attributes,
                                      persistent, "false"),
    Payload = ts_config:getAttr(string, Element#xmlElement.attributes,
                                      payload, ""),
    Persistent = list_to_atom(PersistentStr),
    Request = #amqp_request{type = Type, channel = Channel, exchange = Exchange,
                            routing_key = RoutingKey, payload_size = Size,
                            payload = Payload, persistent = Persistent},
    {no_ack, Request};
parse_request(Element, Type = 'basic.consume', _Tab) ->
    Channel = ts_config:getAttr(string, Element#xmlElement.attributes,
                                channel, "1"),
    Queue = ts_config:getAttr(string, Element#xmlElement.attributes, queue, ""),
    AckStr = ts_config:getAttr(string,
                               Element#xmlElement.attributes, ack, "false"),
    Ack = list_to_atom(AckStr),
    Request = #amqp_request{type = Type, channel = Channel,
                            queue = Queue, ack = Ack},
    {parse, Request};
parse_request(Element, Type = 'basic.qos', _Tab) ->
    Channel = ts_config:getAttr(string, Element#xmlElement.attributes,
                                channel, "1"),
    PrefetchSize = ts_config:getAttr(float_or_integer,
                              Element#xmlElement.attributes, prefetch_size, 0),
    PrefetchCount = ts_config:getAttr(float_or_integer,
                              Element#xmlElement.attributes, prefetch_count, 0),
    Request = #amqp_request{type = Type, channel = Channel,
                            prefetch_size = PrefetchSize,
                            prefetch_count = PrefetchCount},
    {parse, Request};
parse_request(Element, Type, _Tab) ->
    Channel = ts_config:getAttr(string, Element#xmlElement.attributes,
                                channel, "1"),
    Request = #amqp_request{type = Type, channel = Channel},
    {parse, Request}.


initialize_options(Tab) ->
    case ts_config:get_default(Tab, amqp_initialized) of
        {undef_var, _} ->
            ets:insert_new(Tab,{{amqp_username,value}, ?AMQP_USER}),
            ets:insert_new(Tab,{{amqp_password,value}, ?AMQP_PASSWORD}),
            ets:insert_new(Tab,{{amqp_heartbeat,value}, 600});
        _Else ->
            ok
    end.
