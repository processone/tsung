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

-module(ts_config_websocket).

-vc('$Id$ ').
-author('jzhihui521@gmail.com').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_websocket.hrl").

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
parse_config(Element = #xmlElement{name = websocket},
             Config = #config{curid = Id, session_tab = Tab,
                              sessions = [CurS | _], dynvar = DynVar,
                              subst = SubstFlag, match = MatchRegExp}) ->
    Type = ts_config:getAttr(atom, Element#xmlElement.attributes, type),
    ValRaw = ts_config:getText(Element#xmlElement.content),
    Path = ts_config:getAttr(string, Element#xmlElement.attributes, path, "/"),
    Origin = ts_config:getAttr(string, Element#xmlElement.attributes, origin, ""),
    SubProtocols = ts_config:getAttr(string, Element#xmlElement.attributes, subprotocols, ""),
    Frame = ts_config:getAttr(string, Element#xmlElement.attributes, frame,
                              "binary"),

    Request = #websocket_request{data = ValRaw, type = Type, subprotos = SubProtocols,
                                 origin = Origin, path = Path, frame = Frame},

    Ack = case Type of
              message ->
                  ts_config:getAttr(atom, Element#xmlElement.attributes,
                                    ack, parse);
              _ ->
                  parse
          end,

    Msg = #ts_request{ack = Ack,
                      endpage = true,
                      dynvar_specs  = DynVar,
                      subst   = SubstFlag,
                      match   = MatchRegExp,
                      param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab, {{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A, B)->ts_config:parse(A, B) end,
                 Config#config{dynvar = []},
                 Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

