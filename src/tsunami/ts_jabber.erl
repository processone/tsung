%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2004 IDEALX
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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

%%% File    : ts_jabber.erl
%%% Author  : Nicolas Niclausse <nniclausse@IDEALX.com>
%%% Purpose : 
%%% Created : 11 Jan 2004 by Nicolas Niclausse <nniclausse@IDEALX.com>

-module(ts_jabber).
-author('nniclausse@hyperion').

-include("ts_profile.hrl").
-include("ts_jabber.hrl").

-export([init_dynparams/0,
		 add_dynparams/4,
		 get_message/1,
		 session_defaults/0,
         subst/1,
         parse/2,
         parse_config/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, "parse"|"no_ack"|"local", "true"|"false"} 
%%----------------------------------------------------------------------
session_defaults() ->
	{ok,"local", "true"}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
	#jabber{}.
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request
%% Args:	#jabber
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req=#jabber{}) ->
	ts_jabber_common:get_message(Req).


%%----------------------------------------------------------------------
%% Function: parse/3
%% Purpose: Parse the given data and return a new state
%% Args:	Data (binary)
%%			State (record)
%% Returns: NewState (record)
%%----------------------------------------------------------------------
%% no parsing in jabber. use only ack
parse(Data, State) ->
	State.

%%
parse_config(Element, Conf) ->
	ts_config_jabber:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(Subst,[], Param, Host) ->
	Param;
add_dynparams(Subst,DynData, Param, Host) ->
	Param#jabber{id=DynData}.

init_dynparams() ->
	ts_user_server:get_idle().


%%----------------------------------------------------------------------
%% Function: subst/1
%% Purpose: Replace on the fly dynamic element of the HTTP request
%%          For the moment, this feature is not supported for the Jabber
%%          protocol
%%----------------------------------------------------------------------
subst(Req) ->
    Req.
