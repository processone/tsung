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

%%% -----------------------------------------------------------------------
%%% Purpose: API module for client sessions
%%% -----------------------------------------------------------------------

-module(ts_profile).
-created('Date: 2000/10/23 13:48:17 nniclausse Exp ').
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

%% API
-export([get_client/2, get_server/0, get_message/2, parse/3, add_dynparams/3,
		 thinktime/0, new_session/2]).

-include("../include/ts_profile.hrl").

%%----------------------------------------------------------------------
%% Function: get_server/0
%% Purpose: Get server parameters
%% Returns: tuple
%%----------------------------------------------------------------------
get_server() ->
    {?server_adr, ?server_port, ?server_protocol}.

%%----------------------------------------------------------------------
%% Function: new_session/2
%% Purpose: initialize session information (used by 'parse' clients)
%% Returns: record or []
%%----------------------------------------------------------------------
new_session(Module, parse) ->
	Module:new_session();
new_session(Module, _) ->  
	[].
%%----------------------------------------------------------------------
%% Function: get_client/2
%% Purpose: Generate a client session for a given protocol (Module).
%% Args:	Module (module name)
%%			Id
%% Returns: List of #message
%%----------------------------------------------------------------------

get_client(Module, Id)->
	?PRINTDEBUG("get_client called with args ~p ~p ~n",[Module,Id],?DEB),
    Module:get_client(?messages_number, Id).

%%----------------------------------------------------------------------
%% Function: get_message/2
%% Purpose: Build a message/request using a protocol implemented in Module,
%%		    and given some parameters
%% Args:	Module (term)
%%			Param (record)
%% Returns: binary
%%----------------------------------------------------------------------

get_message(Module, Param) ->
	?PRINTDEBUG("get_message called with args ~p ~p ~n",[Module,Param],?DEB),
    Module:get_random_message(Param).
%%TODO: utiliser le meme nom ? pourquoi ajouter random ?


%%----------------------------------------------------------------------
%% Function: parse/3
%% Purpose: Parse the given data and return a new state
%% Args:	Module (term)
%%			Data (binary)
%%			State (record)
%% Returns: NewState (record)
%%----------------------------------------------------------------------
parse(Module, Data, State) ->
	Module:parse(Data, State).


%%----------------------------------------------------------------------
%% Function: thinktime/0
%% Purpose: get a random sample for thinktime. this is used only by 
%%          dynamics clients
%% Args:	Module (term)
%% Returns: integer
%%----------------------------------------------------------------------
thinktime() ->
	round(ts_stats:exponential(?messages_intensity)). % hardcoded for now
%	Module:thinktime().


%%----------------------------------------------------------------------
%% Function: add_dynparams/3
%% Purpose: add dynamic parameters to build the message
%%          this is used for ex. for Cookies in HTTP
add_dynparams(Module, Param, DynData) ->
	Module:add_dynparams(Param, DynData).

