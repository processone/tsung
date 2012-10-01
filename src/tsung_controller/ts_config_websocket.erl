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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

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

    %%is this needed ?
    CleanStr = ts_utils:clean_str(ValRaw),
    Request = #websocket_request{data=CleanStr, type=Type, path=Path},

    Ack = case Type of
              connect ->
                  parse;
              _ ->
                  no_ack
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

