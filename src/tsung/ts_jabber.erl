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
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Purpose : 
%%% Created : 11 Jan 2004 by Nicolas Niclausse <nicolas@niclux.org>

-module(ts_jabber).
-author('nniclausse@hyperion').

-include("ts_profile.hrl").
-include("ts_jabber.hrl").

-export([init_dynparams/0,
		 add_dynparams/4,
		 get_message/1,
		 session_defaults/0,
         subst/2,
         parse/2,
         parse_config/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, "parse"|"no_ack"|"local", "true"|"false"} 
%%----------------------------------------------------------------------
session_defaults() ->
	{ok, true}.

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
parse(_Data, State) ->
	State.

%%
parse_config(Element, Conf) ->
	ts_config_jabber:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_Subst,[], Param, _Host) ->
	Param;
add_dynparams(false,#dyndata{proto=DynData}, Param, _Host) ->
	Param#jabber{id=DynData#jabber_dyndata.id};
add_dynparams(true,#dyndata{proto=JabDynData, dynvars=DynVars}, Param, _Host) ->
    ?DebugF("Subst in jabber msg (~p) with dyn vars ~p~n",[Param,DynVars]),
   NewParam = subst(Param, DynVars),
        updatejab(DynVars, NewParam#jabber{id = JabDynData#jabber_dyndata.id}).

init_dynparams() ->
    #dyndata{proto=#jabber_dyndata{id=ts_user_server:get_idle()}}.


%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: Replace on the fly dynamic element
%%----------------------------------------------------------------------
subst(Req=#jabber{data=Data}, DynData) ->
    Req#jabber{data=ts_search:subst(Data,DynData)}.


%%----------------------------------------------------------------------
%% Func: updatejab/2
%%  takes dyn vars and adds them to jabber record
%%  'nonce' used for sip-digest auth
%%  'sid' session-id used for digest auth
%%----------------------------------------------------------------------
updatejab([],Param) -> Param;
updatejab([{nonce, Val}|Rest], Param)->
   updatejab(Rest, Param#jabber{nonce = Val});
updatejab([{sid, Val}|Rest], Param)->
   updatejab(Rest, Param#jabber{sid = Val});
updatejab([_|Rest], Param)->
   updatejab(Rest, Param).

