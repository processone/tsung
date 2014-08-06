-module(ts_ssl).

-export([ connect/2, connect/3, connect/4, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").


protocol_options(#proto_opts{ssl_ciphers=Ciphers, certificate = Cert,
                             is_first_connect = First, reuse_sessions =Reuse}) when First or not Reuse->
    [binary, {active, once}, {reuse_sessions, false} ] ++ Cert ++ set_ciphers(Ciphers);
protocol_options(#proto_opts{ssl_ciphers=Ciphers, certificate = Cert}) ->
    [binary, {active, once}] ++ Cert ++ set_ciphers(Ciphers).

set_ciphers(negociate)-> [];
set_ciphers(Ciphers)  -> [{ciphers, Ciphers}].

%% -> {ok, Socket}
connect(Host, Port, Opts) when is_list(Host) ->
    connect(Host, Port, opts_to_tcp_opts(Opts), infinity);

connect(Socket, Opts, ConnectTimeout) ->
    ssl:connect(Socket, opts_to_tcp_opts(Opts), ConnectTimeout).

connect(Host, Port, Opts, ConnectTimeout) ->
    ssl:connect(Host, Port, opts_to_tcp_opts(Opts), ConnectTimeout).

connect(Socket, Opts) ->
    connect(Socket, Opts, infinity).


opts_to_tcp_opts(Opts) -> Opts.

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->
    ssl:send(Socket, Data).

close(none)   -> ok;
close(Socket) ->
    ssl:close(Socket).

% set_opts/2 -> socket()
set_opts(none,  _Opts) -> none;
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


