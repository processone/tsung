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

-include("../include/ts_profile.hrl").
-include("../include/ts_http.hrl").

-export([get_client/2, get_random_message/1, parse/2, new_session/0]).


%%
new_session() ->
	#http{}.
%%
get_random_message(#http_request{url = URL, method=get, cookie=Cookie}) ->
	list_to_binary(ts_http_common:http_get(URL, ?http_version, Cookie));

get_random_message(#http_request{url = URL, method=method, cookie=Cookie, body= Body}) ->
	list_to_binary(ts_http_common:http_post(URL, ?http_version, Cookie, Body));

get_random_message(#http_request{url = URL}) ->
	list_to_binary(ts_http_common:http_get(URL, ?http_version, none)).

%%
get_client(N, Id) ->
	ts_http_common:get_client(N, Id).

%%
parse(Data, State) ->
	ts_http_common:parse(Data, State).


