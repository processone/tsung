%%%
%%%  Copyright 2009 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 20 août 2009 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
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

-module(ts_shell).
-vc('$Id: ts_erlang.erl,v 0.0 2009/08/20 16:31:58 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(ts_plugin).

-include("ts_profile.hrl").
-include("ts_shell.hrl").
-include_lib("kernel/include/file.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         dump/2,
         parse/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).


%%====================================================================
%% Data Types
%%====================================================================

%% @type dyndata() = #dyndata{proto=ProtoData::term(),dynvars=list()}.
%% Dynamic data structure
%% @end

%% @type server() = {Host::tuple(),Port::integer(),Protocol::atom()}.
%% Host/Port/Protocol tuple
%% @end

%% @type param() = {dyndata(), server()}.
%% Dynamic data structure
%% @end

%% @type hostdata() = {Host::tuple(),Port::integer()}.
%% Host/Port pair
%% @end

%% @type client_data() = binary() | closed.
%% Data passed to a protocol implementation is either a binary or the
%% atom closed indicating that the server closed the tcp connection.
%% @end

%%====================================================================
%% API
%%====================================================================

parse_config(El,Config) ->
     ts_config_shell:parse_config(El, Config).


%% @spec session_defaults() -> {ok, Persistent} | {ok, Persistent, Bidi}
%% Persistent = bool()
%% Bidi = bool()
%% @doc Default parameters for sessions of this protocol. Persistent
%% is true if connections are preserved after the underlying tcp
%% connection closes. Bidi should be true for bidirectional protocols
%% where the protocol module needs to reply to data sent from the
%% server. @end
session_defaults() ->
    {ok, true}. % not relevant for erlang type (?).

%% @spec new_session() -> State::term()
%% @doc Initialises the state for a new protocol session.
%% @end
new_session() ->
    #shell_sess{}.

%% @spec decode_buffer(Buffer::binary(),Session::record(shell)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#shell{}) ->
    Buffer. % nothing to do for shell


%% @spec add_dynparams(Subst, dyndata(), param(), hostdata()) -> {dyndata(), server()} | dyndata()
%% Subst = term()
%% @doc Updates the dynamic request data structure created by
%% {@link ts_protocol:init_dynparams/0. init_dynparams/0}.
%% @end
add_dynparams(false, {_DynVars, Session}, Param, HostData) ->
    add_dynparams(Session, Param, HostData);
add_dynparams(true, {DynVars, Session}, Param, HostData) ->
    NewParam = subst(Param, DynVars),
    add_dynparams(Session,NewParam, HostData).

add_dynparams(#shell_sess{}, Param, _HostData) ->
    Param.

%%----------------------------------------------------------------------
%% @spec subst(record(shell), term()) -> record(shell)
%% @doc  Replace on the fly dynamic element of the request. @end
%%----------------------------------------------------------------------
subst(Req=#shell{command=Cmd,args=Args}, DynVars) ->
    Req#shell{command=ts_search:subst(Cmd,DynVars),args=ts_search:subst(Args,DynVars)}.


dump(A,B) ->
    ts_plugin:dump(A,B).

%% @spec parse(Data::client_data(), State) -> {NewState, Opts, Close}
%% State = #state_rcv{}
%% Opts = proplist()
%% Close = bool()
%% @doc
%% Opts is a list of inet:setopts socket options. Don't change the
%% active/passive mode here as tsung will set {active,once} before
%% your options.
%% Setting Close to true will cause tsung to close the connection to
%% the server.
%% @end
parse({os, cmd, _Args, Res},State) when is_list(Res)->
    {State#state_rcv{ack_done=true,datasize=length(Res)}, [], false};
parse({os, cmd, _Args, Res},State) ->
    {State#state_rcv{ack_done=true,datasize=size(term_to_binary(Res))}, [], false}.

%% @spec parse_bidi(Data, State) -> {nodata, NewState} | {Data, NewState}
%% Data = client_data()
%% NewState = term()
%% State = term()
%% @doc Parse a block of data from the server. No reply will be sent
%% if the return value is nodata, otherwise the Data binary will be
%% sent back to the server immediately.
%% @end
parse_bidi(_Data, _State) ->
    erlang:error(dummy_implementation).

%% @spec get_message(record(shell),record(state_rcv)) -> {Message::term(),record(state_rcv)}
%% @doc Creates a new message to send to the connected server.
%% @end
get_message(#shell{command=Cmd, args=Args},#state_rcv{session=S}) ->
    Msg=Cmd++" "++Args ,
    {{os, cmd, [Msg], length(Msg) } , S}.


