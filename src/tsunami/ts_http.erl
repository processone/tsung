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

-export([init_dynparams/0,
		 add_dynparams/3,
		 get_message/1,
         parse/2,
         parse_config/2,
         new_session/0]).


%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
	#http{}.

%%----------------------------------------------------------------------
%% Function: get_message/21
%% Purpose: Build a message/request ,
%% Args:	#http_request
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req=#http_request{method=get}) ->
	ts_http_common:http_get(Req);

get_message(Req=#http_request{method=post}) ->
	ts_http_common:http_post(Req).

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: Parse the given data and return a new state
%% Args:	Data (binary)
%%			State (record)
%% Returns: NewState (record)
%%----------------------------------------------------------------------
parse(Data, State) ->
	ts_http_common:parse(Data, State).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
	ts_http_common:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/3
%% Purpose: add dynamic parameters to build the message
%%          this is used for ex. for Cookies in HTTP
%%----------------------------------------------------------------------
add_dynparams([],Param, Host) ->
	Param#http_request{server_name=Host};
add_dynparams(DynData, Param, Host) ->
	Param#http_request{cookie=DynData,server_name=Host}.

init_dynparams() ->
	[].


