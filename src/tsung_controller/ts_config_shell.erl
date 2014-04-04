%%%
%%%  Copyright 2010 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 18 août 2010 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
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


-module(ts_config_shell).
-vc('$Id$ ').
-author('nicolas.niclausse@sophia.inria.fr').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

-include("ts_shell.hrl").

%% @spec parse_config(#xmlElement{}, Config::term()) -> NewConfig::term()
%% @doc Parses a tsung.xml configuration file xml element for this
%% protocol and updates the Config term.
%% @end
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=shell},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->

    Cmd  = ts_config:getAttr(string,Element#xmlElement.attributes, cmd),
    Args  = ts_config:getAttr(string,Element#xmlElement.attributes, args, ""),

    Request = #shell{command=Cmd,args=Args},
    Msg= #ts_request{ack     = parse,
                     endpage = true,
                     dynvar_specs  = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id},Msg}),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end, Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.
