-module(ts_tcp).

-export([ connect/3, send/3, close/1, set_opts/2, protocol_options/1 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").

%% TODO:
%%      Refactor thhe gen_ts_transport API so we don't need to use a separate process
%%      here only to transform the incomming data packets tuple.

protocol_options(#proto_opts{tcp_rcv_size=Rcv, tcp_snd_size=Snd}) ->
    [binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd},
     {keepalive, true} %% FIXME: should be an option
    ].

    

connect(Host, Port, Opts) ->
    case gen_tcp:connect(Host, Port, opts_to_tcp_opts(Opts)) of
        {ok, Socket} ->
            ParentPid = self(),
            Pid = spawn_link(fun() -> enter_loop(Socket, ParentPid) end),
            ok = gen_tcp:controlling_process(Socket, Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

opts_to_tcp_opts(Opts) -> Opts.

send(Socket, Data, _Opts) when is_pid(Socket) ->   
    Socket ! {send, Data},
    ok.

close(Socket) ->
    Socket ! close.


set_opts(Socket, Opts) ->
    Socket ! {set_opts, Opts},
    Socket.
   
enter_loop(Socket, ParentPid) ->
    process_flag(trap_exit, true),
    loop(Socket, ParentPid).

loop(Socket, ParentPid) ->
    receive
        {'EXIT', ParentPid, _Reason} ->
            gen_tcp:close(Socket);   
                %% terminate (even 'normal' termination of the 
                %%            parent must trigger this process to terminate)
        {tcp, Socket, Data} ->
            ParentPid ! {gen_ts_transport, self(), Data},
            loop(Socket, ParentPid);
        {tcp_closed, Socket} ->
            ParentPid ! {gen_ts_transport, self(), closed};
        {tcp_error, Socket, Error} ->
            ParentPid ! {gen_ts_transport, self(), error, Error };
        {send, Data} ->
            gen_tcp:send(Socket, Data),
            %% TODO: ack the sender, so it is blocked in the same
            %%       way that in tsung before introducing this process
            loop(Socket, ParentPid);
        {set_opts, Opts} ->
            inet:setopts(Socket, Opts),
            loop(Socket, ParentPid);
        close ->
            gen_tcp:close(Socket)
    end.
