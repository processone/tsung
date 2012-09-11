-module(ts_ssl).

-export([ connect/3,connect/2, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").


protocol_options(#proto_opts{ssl_ciphers=negociate}) ->
    [binary, {active, once} ];
protocol_options(#proto_opts{ssl_ciphers=Ciphers}) ->
    ?DebugF("cipher is ~p~n",[Ciphers]),
    [binary, {active, once}, {ciphers, Ciphers} ].

%% -> {ok, Socket}
connect(Host, Port, Opts) ->
    ssl:connect(Host, Port, opts_to_tcp_opts(Opts)).

connect(Socket, Opts)->
    ssl:connect(Socket,  opts_to_tcp_opts(Opts)).

opts_to_tcp_opts(Opts) -> Opts.

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->
    ssl:send(Socket, Data).

close(none)   -> ok;
close(Socket) ->
    ssl:close(Socket).

% set_opts/2 -> socket()
set_opts(Socket, Opts) ->
    ssl:setopts(Socket, Opts),
    Socket.

normalize_incomming_data(Socket, {ssl, Socket, Data}) ->
    {gen_ts_transport, Socket, Data};
normalize_incomming_data(Socket, {ssl_closed, Socket}) ->
    {gen_ts_transport, Socket, closed};
normalize_incomming_data(Socket, {ssl_error, Socket, Error}) ->
    {gen_ts_transport, Socket, error, Error};
normalize_incomming_data(_Socket, X) ->
    X. %%Other, non gen_tcp packet.


