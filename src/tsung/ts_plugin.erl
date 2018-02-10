%%%  Copyright (C) 2011 Nicolas Niclausse
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
%%%  Created : 3 Mar 2011 by Nicolas Niclausse <nicolas@niclux.org>

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_plugin).

-export([dump/2, parse_bidi/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{add_dynparams, 4},
     {get_message, 2},
     {session_defaults, 0},
     {dump, 2},
     {parse, 2},
     {parse_bidi, 2},
     {parse_config, 2},
     {decode_buffer, 2},
     {new_session, 0}];
behaviour_info(_Other) ->
    undefined.


%% @spec dump(protocol, {Request::term(),Session::term(), Id::integer(),
%%             Host::string(),DataSize::integer()}) -> ok
%% @doc It can be used to send specific data to the current plugin back to ts_mon
%% @end
dump(_Type,_Data) ->
    ok.

%% @spec parse_bidi(Data::binary(),State::record(state_rcv)) ->
%%   {NewData::binary()|nodata, NewState::record(state_rcv), think|continue}
%% @doc Parse a block of data from the server. No reply will be sent
%% if the return value is nodata, otherwise the Data binary will be
%% sent back to the server immediately. If the last argument is
%% 'think', it will continue to wait; if it's 'continue', it will
%% handle the next action (request, thinktime, ...)
%% @end
parse_bidi(_Data, State) ->
    {nodata, State, think}.
