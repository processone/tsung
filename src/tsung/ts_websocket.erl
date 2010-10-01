-module (ts_websocket).

-export([ connect/3, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").

-record (state, {parent, host, port, opts, socket=none, buffer = none, state=not_connected}).

protocol_options(#proto_opts{tcp_rcv_size=Rcv, tcp_snd_size=Snd}) ->
    [binary,
     {active, once},
     {recbuf, Rcv},
     {sndbuf, Snd},
     {keepalive, true} %% FIXME: should be an option
    ].

connect(Host, Port, Opts) ->
    Parent = self(),
    Pid = spawn_link(fun()-> loop(#state{parent = Parent, host=Host,port = Port, opts = Opts}) end),
    {ok, Pid}.
    
loop(#state{socket=none, state=not_connected, host=Host, port=Port, opts=Opts} = State)->
  {ok, Socket} = gen_tcp:connect(Host, Port, opts_to_tcp_opts(Opts)),
  Handshake = list_to_binary(["GET /xmpp HTTP/1.1\r\n",
              "Host: ",Host,"\r\n",
              "Connection: Upgrade\r\n",
              "Sec-WebSocket-Key2: 12998 5 Y3 1  .P00\r\n",
              "Upgrade: WebSocket\r\n",
              "Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5\r\n",
              "Origin: http://",Host,"\r\n\r\n",
              "^n:ds[4U"]),
  gen_tcp:send(Socket,Handshake),
  loop(State#state{socket=Socket, state=waiting_handshake});
  
loop(#state{parent = Parent, state=waiting_handshake, socket = Socket}=State)->
  receive
    {tcp, Socket, Data}->
      inet:setopts(Socket, [{active, once}]),
      loop(State#state{state=connected});
    {tcp_closed, Socket}->
      Parent ! {gen_ts_transport, self(), closed};
    {tcp_error, Socket, Error}->
      Parent ! {gen_ts_transport, self(), error, Error}
  end;

loop(#state{parent=Parent, state=connected, socket=Socket}=State)->
  receive
    {send, Data, Ref} ->
      gen_tcp:send(Socket, iolist_to_binary([0,Data,255])),
      Parent ! {ok, Ref},
      loop(State);
    close ->
      gen_tcp:close(Socket);
    {set_opts, Opts} ->
      inet:setopts(Socket, Opts),
      loop(State);
    {tcp, Socket, Data}->
      handle_data(Data, State);
    {tcp_closed, Socket}->
      Parent ! {gen_ts_transport, self(), closed};
    {tcp_error, Socket, Error}->
      Parent ! {gen_ts_transport, self(), error, Error};
    E -> erlang:display({ "Message:", E})
  end.
  
opts_to_tcp_opts(Opts) -> Opts.

%% send/3 -> ok | {error, Reason}
send(Socket, Data, _Opts)  ->
  %erlang:display({ "sending to server:", binary_to_list(Data)}),   
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

% set_opts/2 -> socket()
set_opts(Socket, Opts) ->
    Socket ! {set_opts, Opts},
    Socket.
   

normalize_incomming_data(_Socket, X) -> 
  X. %% nothing to do here, ts_websocket uses a special process to handle http requests, 
     %% the incoming data is already delivered to ts_client as {gen_ts_transport, ..} instead of gen_tcp | ssl

% Buffering and data handling
handle_data(<<0,T/binary>>, #state{buffer=none}=State) ->
	handle_data(T, State#state{buffer= <<>>});

handle_data(<<>>, #state{socket = Socket, buffer=none}=State) ->
  loop(State#state{buffer=none});

handle_data(<<255,T/binary>>, #state{parent = Parent, buffer=L}=State) ->
  erlang:display({ "sending to client:", binary_to_list(iolist_to_binary(L))}),
	Parent ! {gen_ts_transport, self(), iolist_to_binary(L)},
	handle_data(T,  State#state{buffer=none});

handle_data(<<H/utf8,T/binary>>, #state{ buffer=L}=State)->
	handle_data(T, State#state{ buffer=iolist_to_binary([L, H])});

handle_data(<<>>, #state{socket = Socket} = State) ->
  loop(State).
