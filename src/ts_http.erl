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


-module(ts_http).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("ts_profile.hrl").
-include("ts_http.hrl").

-export([get_client/2, add_dynparams/2,
		 get_random_message/1,
         parse/2,
         parse_config/2,
         new_session/0]).


%%
new_session() ->
	#http{}.
%%
get_random_message(Req=#http_request{method=get}) ->
	ts_http_common:http_get(Req);

get_random_message(Req=#http_request{method=getims}) ->
	ts_http_common:http_get_ifmodsince(Req);

get_random_message(Req=#http_request{method=post}) ->
	ts_http_common:http_post(Req).

%%
get_client(N, Id) ->
	{ok, Session, Size, IP} = ts_config_server:get_next_session(),
    {Session, Size, IP}.

%%
parse(Data, State) ->
	ts_http_common:parse(Data, State).

%%
parse_config(Element, Conf) ->
	ts_http_common:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Func: add_dynparams/2
%%----------------------------------------------------------------------
add_dynparams(Param, DynData) ->
	Param#http_request{cookie=DynData}.


