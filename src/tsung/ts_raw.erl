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

-module(ts_raw).
-author('nniclausse@hyperion').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_raw.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         subst/2,
         parse/2,
         parse_bidi/2,
         dump/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok,true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(raw)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#raw{}) ->
    Buffer.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #raw{}.
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request
%% Args:    #jabber
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#raw{datasize=Size},S) when is_list(Size) ->
    get_message(#raw{datasize=list_to_integer(Size)},S);
get_message(#raw{datasize=Size},#state_rcv{session=S}) when is_integer(Size), Size > 0 ->
    BitSize = Size*8,
   {<< 0:BitSize >>,S} ;
get_message(#raw{data=Data},#state_rcv{session=S})->
    {list_to_binary(Data),S}.


%%----------------------------------------------------------------------
%% Function: parse/3
%% Purpose: Parse the given data and return a new state
%% Args:    Data (binary)
%%            State (record)
%% Returns: NewState (record)
%%----------------------------------------------------------------------
%% no parsing . use only ack
parse(_Data, State) ->
    State.

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

dump(A,B) ->
    ts_plugin:dump(A,B).

%%
parse_config(Element, Conf) ->
    ts_config_raw:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_,[], Param, _Host) ->
    Param;
add_dynparams(true, {DynVars, _Session}, OldReq, _Host) ->
    subst(OldReq, DynVars);
add_dynparams(_Subst, _DynData, Param, _Host) ->
    Param.

%%----------------------------------------------------------------------
%% Function: subst/1
%%----------------------------------------------------------------------
subst(Req=#raw{datasize=Size,data=Data},DynVars) ->
    Req#raw{datasize = ts_search:subst(Size, DynVars),
            data= ts_search:subst(Data, DynVars)}.
