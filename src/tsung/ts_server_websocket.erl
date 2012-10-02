%%%
%%%  Copyright 2010 © ProcessOne
%%%
%%%  Author : Eric Cestari <ecestari@mac.com>
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
%%%  the two.


-module (ts_server_websocket).

-export([ connect/3, send/3, close/1, set_opts/2, protocol_options/1,
          normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_websocket.hrl").

-record(state, {parent, socket = none, accept, host, port, path, opts, version,
                buffer = <<>>, state = not_connected, subprotos = []}).

-record(ws_config, {path, version = "13"}).

protocol_options(#proto_opts{tcp_rcv_size = Rcv, tcp_snd_size = Snd,
                             websocket_path = Path}) ->
    [#ws_config{path = Path},
     binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd},
     {keepalive, true} %% FIXME: should be an option
    ].

connect(Host, Port, Opts) ->
    Parent = self(),

    [WSConfig | TcpOpts] = Opts,
    Path = WSConfig#ws_config.path,
    Version = WSConfig#ws_config.version,

    Pid = spawn_link(
            fun() ->
                    loop(#state{parent = Parent, host = Host, port = Port,
                                opts = TcpOpts, path = Path, version = Version})
            end),
    {ok, Pid}.

loop(#state{socket = none, host = Host, path = Path, port = Port, opts = Opts,
            version = Version, subprotos = SubProtocol,
            state = not_connected} = State)->
    {ok, Socket} = gen_tcp:connect(Host, Port, opts_to_tcp_opts(Opts)),
    {Handshake, Accept} = websocket:handshake_request(Host, Path, 
                                                      SubProtocol, Version),
    gen_tcp:send(Socket, Handshake),
    loop(State#state{socket = Socket, accept = Accept,
                     state = waiting_handshake});

loop(#state{parent = Parent, socket = Socket, accept = Accept,
            state = waiting_handshake} = State)->
    receive
        {tcp, Socket, Data}->
            CheckResult = websocket:check_handshake(Data, Accept),
            case CheckResult of
                ok ->
                    ?Debug("handshake success: ~n"),
                    inet:setopts(Socket, [{active, once}]),
                    loop(State#state{state = connected});
                {error, Reason} ->
                    ?DebugF("handshake fail: ~p~n", [Reason]),
                    Parent ! {gen_ts_transport, self(), error, Reason}
            end;
        {tcp_closed, Socket}->
            ?LOGF("tcp closed:~p~n", [Socket], ?ERR),
            Parent ! {gen_ts_transport, self(), closed};
        {tcp_error, Socket, Error}->
            ?LOGF("tcp error:~p~n", [Socket], ?ERR),
            Parent ! {gen_ts_transport, self(), error, Error}
    end;

loop(#state{parent = Parent, socket = Socket, state = connected} = State)->
    receive
        {send, Data, Ref} ->
            EncodedData = websocket:encode_binary(Data),
            gen_tcp:send(Socket, EncodedData),
            Parent ! {ok, Ref},
            loop(State);
        close ->
            EncodedData = websocket:encode_close(<<"close">>),
            gen_tcp:send(Socket, EncodedData),
            gen_tcp:close(Socket);
        {set_opts, Opts} ->
            inet:setopts(Socket, Opts),
            loop(State);
        {tcp, Socket, Data}->
            DecodeResult = websocket:decode(Data),
            case DecodeResult of
                {close, Reason} ->
                    ?DebugF("receive close from server: ~p~n", [Reason]),
                    Parent ! {gen_ts_transport, self(), closed};
                {Result, none} ->
                    ?DebugF("receive from server: ~p~n", [Result]),
                    Parent ! {gen_ts_transport, self(), Result},
                    loop(State#state{buffer = <<>>});
                {Result, Left} ->
                    ?DebugF("receive from server: ~p~n", [Result]),
                    Parent ! {gen_ts_transport, self(), Result},
                    loop(State#state{buffer = Left})
            end;
        {tcp_closed, Socket}->
            Parent ! {gen_ts_transport, self(), closed};
        {tcp_error, Socket, Error}->
            Parent ! {gen_ts_transport, self(), error, Error};
        E -> ?LOGF("Message:~p~n", [E], ?WARN)
    end.

opts_to_tcp_opts(Opts) -> Opts.

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->
    ?DebugF("sending to server: ~p~n",[Data]),
    Ref = make_ref(),
    Socket ! {send, Data, Ref},
    MonitorRef = erlang:monitor(process,Socket),
    receive
        {'DOWN', MonitorRef, _Type, _Object, _Info} ->
            {error, no_ws_connection};
        {ok, Ref} ->
            erlang:demonitor(MonitorRef),
            ok
    after
        30000 ->
            erlang:demonitor(MonitorRef),
            {error, timeout}
    end.

close(Socket) ->
    Socket ! close.

%% set_opts/2 -> socket()
set_opts(Socket, Opts) ->
    Socket ! {set_opts, Opts},
    Socket.

normalize_incomming_data(_Socket, X) ->
    %% nothing to do here, ts_websocket uses a special process to handle 
    %%http requests,the incoming data is already delivered to 
    %%ts_client as {gen_ts_transport, ..} instead of gen_tcp | ssl
    X.
