-module(ts_tcp).

-export([ connect/3, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").

protocol_options(#proto_opts{tcp_rcv_size=Rcv, tcp_snd_size=Snd}) ->
    [binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd},
     {keepalive, true} %% FIXME: should be an option
    ].
%% -> {ok, Socket}
connect(Host, Port, Opts) ->
    gen_tcp:connect(Host, Port, opts_to_tcp_opts(Opts)).

opts_to_tcp_opts(Opts) -> Opts.

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->   
    gen_tcp:send(Socket, Data).

close(Socket) ->
    gen_tcp:close(Socket).

% set_opts/2 -> socket()
set_opts(Socket, Opts) ->
    inet:setopts(Socket, Opts),
    Socket.
   

normalize_incomming_data(Socket, {tcp, Socket, Data}) ->
    {gen_ts_transport, Socket, Data};
normalize_incomming_data(Socket, {tcp_closed, Socket}) ->
    {gen_ts_transport, Socket, closed};
normalize_incomming_data(Socket, {tcp_error, Socket, Error}) ->
    {gen_ts_transport, Socket, error, Error};
normalize_incomming_data(_Socket, X) ->
    X. %%Other, non gen_tcp packet.


