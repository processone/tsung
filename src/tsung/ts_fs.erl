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

-module(ts_fs).
-vc('$Id: ts_erlang.erl,v 0.0 2009/08/20 16:31:58 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behavior(ts_plugin).

-include("ts_macros.hrl").
-include("ts_profile.hrl").
-include("ts_fs.hrl").
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
     ts_config_fs:parse_config(El, Config).


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
    #fs_session{}.

%% @spec decode_buffer(Buffer::binary(),Session::record(fs)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#fs_session{}) ->
    Buffer.


%% @spec add_dynparams(Subst, dyndata(), param(), hostdata()) -> {dyndata(), server()} | dyndata()
%% Subst = term()
%% @doc Updates the dynamic request data structure created by
%% {@link ts_protocol:init_dynparams/0. init_dynparams/0}.
%% @end
add_dynparams(false, {_,Session}, Param, HostData) ->
    add_dynparams(Session, Param, HostData);
add_dynparams(true, {DynVars,Session}, Param, HostData) ->
    NewParam = subst(Param, DynVars),
    add_dynparams(Session,NewParam, HostData).

add_dynparams(#fs_session{position=Pos,iodev=IODevice}, Req=#fs{}, _HostData) when is_integer(Pos)->
    Req#fs{position=Pos,iodev=IODevice};
add_dynparams(#fs_session{}, Param, _HostData) ->
    Param.

%%----------------------------------------------------------------------
%% @spec subst(record(fs), dynvars:term()) -> record(fs)
%% @doc Replace on the fly dynamic element of the request.
%% @end
%%----------------------------------------------------------------------
subst(Req=#fs{path=Path,size=Size,dest=Dest}, DynVars) ->
    Req#fs{path=ts_search:subst(Path,DynVars),dest=ts_search:subst(Dest,DynVars), size=ts_search:subst(Size,DynVars)}.

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
parse({file, open, _Args, {ok,IODevice}},State=#state_rcv{session=S}) ->
    NewDyn=S#fs_session{iodev=IODevice,position=0},
    {State#state_rcv{ack_done=true,datasize=0,session=NewDyn}, [], false};
parse({file, open, [Path,_], {error,Reason}},State) ->
    ?LOGF("error while opening file: ~p(~p)~n",[Path, Reason],?ERR),
    ts_mon_cache:add({count,error_fs_open}),
    {State#state_rcv{ack_done=true,datasize=0}, [], false};
parse({file, close, [_IODevice], ok},State=#state_rcv{session=S}) ->
    NewDyn=S#fs_session{iodev=undefined,position=0},
    {State#state_rcv{ack_done=true,datasize=0,session=NewDyn}, [], false};
parse({file, close, [_IODevice], {error,Reason}}, State) ->
    ?LOGF("error while closing file: ~p~n",[Reason],?ERR),
    ts_mon_cache:add({count,error_fs_close}),
    {State#state_rcv{ack_done=true,datasize=0}, [], false};

parse({file, pread, [_IODev,Pos,Size], {ok,_Data}},State=#state_rcv{session=S,datasize=DataSize}) ->
    NewDyn=S#fs_session{position=Pos+Size},
    {State#state_rcv{ack_done=true,datasize=DataSize+Size,session=NewDyn}, [], false};
parse({file, pread, [_IODev,_Pos,Size], eof},State=#state_rcv{session=S,datasize=DataSize}) ->
    NewDyn=S#fs_session{position=0},
    {State#state_rcv{ack_done=true,datasize=DataSize+Size,session=NewDyn}, [], false};
parse({file, pread, [_IODev,_Pos,_Size], {error,Reason}},State) ->
    ?LOGF("error while reading file: ~p~n",[Reason],?ERR),
    ts_mon_cache:add({count,error_fs_pread}),
    {State#state_rcv{ack_done=true,datasize=0}, [], false};

parse({file, write_file, _Args, ok},State) ->
    {State#state_rcv{ack_done=true,datasize=0}, [], false};
parse({file, write_file, [Path,_], {error,Reason}},State) ->
    ?LOGF("error while writing file: ~p (~p)~n",[Path, Reason],?ERR),
    ts_mon_cache:add({count,error_fs_write}),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, pwrite, [_IODev,Pos,Data], ok},State=#state_rcv{session=S}) ->
    NewDyn=S#fs_session{position=Pos+length(Data)},
    {State#state_rcv{ack_done=true,datasize=0,session=NewDyn}, [], false};
parse({file, pwrite, Args, {error,Reason}},State) ->
    ?LOGF("error while writing file: ~p (~p)~n",[Args, Reason],?ERR),
    ts_mon_cache:add({count,error_fs_pwrite}),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};

parse({file, del_dir, [_Path], ok},State) ->
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, del_dir, [Path], {error,Reason}},State) ->
    ?LOGF("error while delete directory: ~p (~p)~n",[Path, Reason],?ERR),
    ts_mon_cache:add({count,error_fs_del_dir}),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};

parse({file, make_dir, [_Path], ok},State) ->
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, make_dir, [Path], {error, eexist} },State) ->
    ?LOGF("error while creating diretory: ~p already exists~n",[Path],?NOTICE),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, make_dir, [Path], {error,Reason}},State) ->
    ?LOGF("error while creating diretory: ~p (~p)~n",[Path, Reason],?ERR),
    ts_mon_cache:add({count,error_fs_mkdir}),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};

parse({file, make_symlink, _Args, ok},State) ->
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, make_symlink, [_Existing, New], {error, eexist} },State) ->
    ?LOGF("error while creating symlink: ~p already exists~n",[New],?NOTICE),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, make_symlink, [Existing, New], {error,Reason}},State) ->
    ?LOGF("error while creating symlink: ~p to ~p (~p)~n",[Existing, New, Reason],?ERR),
    ts_mon_cache:add({count,error_fs_mksymlink}),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};

parse({file, delete, [_Path], ok},State) ->
    {State#state_rcv{ack_done=true, datasize=0}, [], false};
parse({file, delete, [Path], {error,Reason}},State) ->
    ?LOGF("error while deleting file: ~p (~p)~n",[Path, Reason],?ERR),
    {State#state_rcv{ack_done=true, datasize=0}, [], false};

parse({file, read_file_info, [_Path], {ok, _FileInfo}},State) ->
    %% which value should we use for datasize ?
    {State#state_rcv{ack_done=true,datasize=0}, [], false};
parse({file, read_file_info, [Path], {error,Reason}},State) ->
    ?LOGF("error while running stat file: ~p (~p)~n",[Path,Reason],?ERR),
    ts_mon_cache:add({count,error_fs_stat}),
    {State#state_rcv{ack_done=true,datasize=0}, [], false};

parse({ts_utils, read_file_raw, [_Path], {ok,_Res,Size}},State) ->
    {State#state_rcv{ack_done=true,datasize=Size}, [], false};
parse({ts_utils, read_file_raw, [Path], {error,Reason}},State) ->
    ?LOGF("error while reading file: ~p(~p)~n",[Path,Reason],?ERR),
    ts_mon_cache:add({count,error_fs_read}),
    {State#state_rcv{ack_done=true,datasize=0}, [], false}.

%% @spec parse_bidi(Data, State) -> {nodata, NewState} | {Data, NewState}
%% Data = client_data()
%% NewState = term()
%% State = term()
%% @doc Parse a block of data from the server. No reply will be sent
%% if the return value is nodata, otherwise the Data binary will be
%% sent back to the server immediately.
%% @end
parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

dump(A,B) ->
    ts_plugin:dump(A,B).

%% @spec get_message(record(),record(state_rcv)) -> {term(),record(state_rcv)}
%% @doc Creates a new message to send to the connected server.
%% @end
get_message(R,#state_rcv{session=S}) ->
    {get_message2(R),S}.

get_message2(#fs{command=read, path=Path}) ->
    {ts_utils,read_file_raw,[Path],0};
get_message2(#fs{command=read_chunk, iodev=IODevice,position=Loc, size=Size}) when is_integer(Loc)->
    {file,pread,[IODevice,Loc,Size],0};
get_message2(#fs{command=write_chunk, iodev=IODevice,position=Loc, size=Size}) when is_integer(Loc)->
    {file,pwrite,[IODevice,Loc,ts_utils:urandomstr(Size)],Size};
get_message2(#fs{command=open, mode=read,path=Path,position=Loc}) when is_integer(Loc)->
    {file,open,[Path,[read,raw,binary]],0};
get_message2(#fs{command=open, mode=write,path=Path,position=Loc}) when is_integer(Loc)->
    {file,open,[Path,[write,raw,binary]],0};
get_message2(#fs{command=close, iodev=IODevice}) ->
    {file,close,[IODevice],0};
get_message2(#fs{command=delete, path=Path}) ->
    {file,delete,[Path],0};
get_message2(#fs{command=del_dir, path=Path}) ->
    {file,del_dir,[Path],0};
get_message2(#fs{command=make_dir, path=Path}) ->
    {file,make_dir,[Path],0};
get_message2(#fs{command=make_symlink, path=Existing, dest=New}) ->
    {file,make_symlink,[Existing, New],0};
get_message2(#fs{command=stat, path=Path}) ->
    {file,read_file_info,[Path],0};
get_message2(#fs{command=write,path=Path, size=Size}) ->
    {file,write_file,[Path,ts_utils:urandomstr(Size),[raw]],Size}.


