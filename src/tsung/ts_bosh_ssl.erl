-module(ts_bosh_ssl).

-export([ connect/3, send/3, close/1, set_opts/2, protocol_options/1 ]).

-behaviour(gen_ts_transport).

%% This is exactly like ts_bosh, but using ssl instead of plain connections.
%% It is easier (fewer tsung modifications required) to have two separate modules, 
%% and delegate from here to the original.
connect(Host, Port, Opts) ->
    ts_bosh:connect(Host, Port, Opts, ssl).

send(Pid, Data, _Opts) ->
    ts_bosh:send(Pid, Data, _Opts).

close(Pid) ->
    ts_bosh:close(Pid).

set_opts(Pid, _Opts) ->
    ts_bosh:set_opts(Pid, _Opts).

protocol_options(_P) ->
    ts_bosh:protocol_options(_P).


