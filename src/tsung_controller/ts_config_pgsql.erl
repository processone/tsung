%%%
%%%  Copyright © Nicolas Niclausse 2005
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 6 Nov 2005 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

%%% common functions used by pgsql clients to parse config

-module(ts_config_pgsql).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_pgsql.hrl").
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
parse_config(Element = #xmlElement{name=pgsql},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->

    {Ack,Request} =
        case ts_config:getAttr(atom, Element#xmlElement.attributes, type) of
            sql ->
                ValRaw = ts_config:getText(Element#xmlElement.content),
                SQL = list_to_binary(ts_utils:clean_str(ValRaw)),
                ?LOGF("Got SQL query: ~p~n",[SQL], ?NOTICE),
                {parse,#pgsql_request{sql=SQL, type= sql}};
            close ->
                {parse,#pgsql_request{type=close}};
            sync ->
                {parse,#pgsql_request{type=sync}};
            flush ->
                {parse,#pgsql_request{type=flush}};
            copydone ->
                {parse,#pgsql_request{type=copydone}};
            execute ->
                Portal = ts_config:getAttr(Element#xmlElement.attributes, name_portal),
                Limit = ts_config:getAttr(integer,Element#xmlElement.attributes, max_rows,0),
                {no_ack,#pgsql_request{type=execute,name_portal=Portal,max_rows=Limit}};
            parse ->
                Name = ts_config:getAttr(Element#xmlElement.attributes, name_prepared),
                Query = list_to_binary(ts_config:getText(Element#xmlElement.content)),
                Params=case ts_config:getAttr(Element#xmlElement.attributes, parameters) of
                           "" -> "";
                           P ->
                               lists:map(fun(S)-> list_to_integer(S) end, ts_utils:splitchar(P,$,))
                       end,
                {no_ack,#pgsql_request{type=parse,name_prepared=Name,equery=Query,parameters=Params}};
            bind ->
                Portal = ts_config:getAttr(Element#xmlElement.attributes, name_portal),
                Prep = ts_config:getAttr(Element#xmlElement.attributes, name_prepared),
                Formats = case ts_config:getAttr(Element#xmlElement.attributes, formats_results) of
                              "" -> "";
                              FR ->
                                  lists:map(fun(A)->list_to_atom(A) end,ts_utils:splitchar(FR,$,))
                          end,
                Params=case ts_config:getAttr(Element#xmlElement.attributes, parameters) of
                           "" -> "";
                           P ->
                               lists:map(fun("null")-> null;
                                            (A)     -> A end, ts_utils:split(P,","))
                       end,
                ParamsFormat = ts_config:getAttr(atom,Element#xmlElement.attributes, formats, none),
                {no_ack,#pgsql_request{type=bind,name_portal=Portal,name_prepared=Prep,
                                       formats=ParamsFormat,formats_results=Formats,parameters=Params}};
            copy ->
                Contents = case ts_config:getAttr(string, Element#xmlElement.attributes, contents_from_file) of
                               [] ->
                                   P=ts_config:getText(Element#xmlElement.content),
                                   list_to_binary(lists:map(fun(S)-> list_to_integer(S) end, ts_utils:splitchar(P,$,)));
                               FileName ->
                                   {ok, FileContent} = file:read_file(FileName),
                                   FileContent
                           end,
                {no_ack,#pgsql_request{type=copy,equery=Contents}};
            copyfail ->
                Str = ts_config:getAttr(string,Element#xmlElement.attributes, equery,undefined),
                {parse,#pgsql_request{type=copyfail,equery=list_to_binary(Str)}};
            describe ->
                Portal=ts_config:getAttr(string,Element#xmlElement.attributes, name_portal,undefined),
                Prep = ts_config:getAttr(string,Element#xmlElement.attributes, name_prepared,undefined),
                {no_ack,#pgsql_request{type=describe,name_portal=Portal,name_prepared=Prep}};
            authenticate ->
                Passwd  = ts_config:getAttr(Element#xmlElement.attributes, password),
                {parse,#pgsql_request{passwd=Passwd, type= authenticate}};
            connect ->
                Database  = ts_config:getAttr(Element#xmlElement.attributes, database),
                User  = ts_config:getAttr(Element#xmlElement.attributes, username),
                {parse,#pgsql_request{username=User, database=Database,  type=connect}}
        end,
    Msg= #ts_request{ack     = Ack,
                     endpage = true,
                     dynvar_specs  = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing options
%% parse_config(Element = #xmlElement{name=options}, Conf = #config{session_tab = Tab}) ->
%%     case ts_config:getAttr(Element#xmlElement.attributes, name) of
%%         "todo" ->
%%             Val = ts_config:getAttr(Element#xmlElement.attributes, value),
%%             ets:insert(Tab,{{http_use_server_as_proxy}, Val})
%%     end,
%%     lists:foldl( fun(A,B)->ts_config:parse(A,B) end, Conf, Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

