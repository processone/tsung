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
		 add_dynparams/4,
		 get_message/1,
		 session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, "parse"|"no_ack"|"local", "true"|"false"} 
%%----------------------------------------------------------------------
session_defaults() ->
    %% we parse the server response, and continue if the tcp
    %% connection is closed
	{ok,"parse", "true"}.

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
	ts_config_http:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%          this is used for ex. for Cookies in HTTP
%%----------------------------------------------------------------------
add_dynparams(false, DynData, Param, Host) ->
    add_dynparams(DynData#dyndata.proto, Param, Host);
add_dynparams(Subst, DynData, Param, Host) ->
    NewParam = subst(Param, DynData#dyndata.dynvars),
    add_dynparams(DynData#dyndata.proto,NewParam, Host).
% Function: add_dynparams/3
add_dynparams(#http_dyndata{cookies=[]},Param, Host) ->
	Param#http_request{server_name=Host};
add_dynparams(#http_dyndata{cookies=DynData}, Param, Host) ->
	Param#http_request{cookie=DynData,server_name=Host}.

init_dynparams() ->
	#dyndata{proto=#http_dyndata{}}.


%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: Replace on the fly dynamic element of the HTTP request For
%%          the moment, we only do dynamic substitution in URL, body,
%%          userid, passwd, because we see no need for the other HTTP
%%          request parameters.
%%----------------------------------------------------------------------
subst(Req=#http_request{url=URL, body=Body, userid=UserId, passwd=Passwd}, DynData) ->
    Req#http_request{url    = ts_search:subst(URL, DynData),
					 body   = ts_search:subst(Body, DynData),
					 userid = ts_search:subst(UserId, DynData),
					 passwd = ts_search:subst(Passwd, DynData)}.
