%%%
%%%  Copyright 2011 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@inria.fr>
%%%  Created: 4 mai 2011 by Nicolas Niclausse <nicolas.niclausse@inria.fr>
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

-module(ts_config_job).
-vc('$Id$ ').
-author('nicolas.niclausse@inria.fr').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

-include("ts_job.hrl").

%% @spec parse_config(#xmlElement{}, Config::term()) -> NewConfig::term()
%% @doc Parses a tsung.xml configuration file xml element for this
%% protocol and updates the Config term.
%% @end
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=job},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->
    Request = #job{req       = ts_config:getAttr(atom,Element#xmlElement.attributes, req, submit),
                   type      = ts_config:getAttr(atom,Element#xmlElement.attributes, type, oar),
                   script    = ts_config:getAttr(string,Element#xmlElement.attributes, script),
                   notify_script = ts_config:getAttr(string,Element#xmlElement.attributes, notify_script),
                   walltime  = ts_config:getAttr(string,Element#xmlElement.attributes, walltime, "1:00:00"),
                   resources = ts_config:getAttr(string,Element#xmlElement.attributes, resources, ""),
                   queue     = ts_config:getAttr(string,Element#xmlElement.attributes, queue),
                   notify_port = ts_config:getAttr(integer_or_string,Element#xmlElement.attributes, notify_port),
                   jobid     = ts_config:getAttr(integer_or_string,Element#xmlElement.attributes, jobid, undefined),
                   name      = ts_config:getAttr(string,Element#xmlElement.attributes, name, "tsung"),
                   user      = ts_config:getAttr(string,Element#xmlElement.attributes, user, undefined),
                   options   = ts_config:getAttr(string,Element#xmlElement.attributes, options),
                   duration  = ts_config:getAttr(integer_or_string,Element#xmlElement.attributes, duration, 3600)
                   },
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
