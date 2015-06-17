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
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_bosh).

-export([ connect/4, send/3, close/1, set_opts/2, protocol_options/1, normalize_incomming_data/2 ]).

-export([connect/5]).  %% used for ts_bosh_ssl sessions.

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(CONTENT_TYPE, "text/xml; charset=utf-8").
-define(VERSION, "1.8").
-define(WAIT, 60). %1 minute
-define(HOLD, 1). %only 1 request pending

-define(MAX_QUEUE_SIZE, 5). %% at most 5 messages queued, after that close the connection.
                            %% In practice we never had more than 1 pending packet, as we are blocking
                            %% the client process until we sent the packet.  But I keep this functionality in place,
                            %% in case we decide to do the sending() of data asynchronous.

-record(state, {
          host,
          path,
          port,
          % {Host::string(), Port:integer(), Path::string(), Ssl::bool()}
          domain = undefined,
          sid,
          rid,
          parent_pid,
          max_requests, %TODO: use this, now fixed on 2
          queue = [], %% stanzas that have been queued because we reach the limit of requets
          open = [],
          free = [],
          local_ip,
          local_port,
          session_state = fresh, %% fresh | normal | closing
          pending_ref,
          connect_timeout = 20 * 1000,
          type  %% 'tcp' | 'ssl'
         }).

normalize_incomming_data(_Socket, X) ->
    X. %% nothing to do here, ts_bosh uses a special process to handle http requests,
       %% the incoming data is already delivered to ts_client as {gen_ts_transport, ..} instead of gen_tcp | ssl

connect(Host, Port, Opts, Timeout) ->
    connect(Host, Port, Opts, Timeout, tcp).

connect(Host, Port, Opts, Timeout, Type) when Type =:= 'tcp' ; Type =:= 'ssl' ->
    Parent = self(),
    [BoshPath | OtherOpts] = Opts,
    Pid = spawn(fun() -> loop(Host, Port, BoshPath, OtherOpts, Type, Parent, Timeout) end),
    ?DebugF("connect ~p ~p ~p ~p ~p",[Host, Port, BoshPath, self(), Pid]),
    {ok, Pid}.

extract_domain("to='" ++ Rest) ->
    lists:takewhile(fun(C) -> C =/= $' end, Rest);
extract_domain([_|Rest]) -> extract_domain(Rest).

send(Pid, Data, _Opts) ->
    Ref = make_ref(),
    Msg = case Data of
              <<"<?xml", Rest/binary>> ->  %%HACK: use this to detect stream start (or restarts)
                  Domain =  extract_domain(binary_to_list(Rest)),
                  {stream, Domain, Ref};
              <<"</stream:stream>", _/binary>> -> %%Use this to detect stream end
                  {stream, terminate, Ref};
              _ ->
                  {send, Data, Ref}
          end,
    Pid ! Msg,
    MonitorRef = erlang:monitor(process,Pid),
    receive
        {'DOWN', MonitorRef, _Type, _Object, _Info} ->
            {error, no_bosh_connection};
        {ok, Ref} ->
            erlang:demonitor(MonitorRef, [flush]),
            ok
    after
        30000 ->
            erlang:demonitor(MonitorRef, [flush]),
            {error, timeout}
    end.

close(Pid) when is_pid(Pid)->
    Pid ! close;
close(none) ->
    ?LOG("close: no pid",?DEB);
close(Pid) ->
    ?LOGF("close: bad argument: ~p, should be a pid",[Pid],?ERR).

set_opts(Pid, _Opts) ->
    Pid.

protocol_options(#proto_opts{bosh_path = BoshPath}) ->
    [BoshPath].

loop(Host, Port, Path, Opts, Type, Parent, Timeout) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    _MonitorRef = erlang:monitor(process,Parent),
    loop(#state{session_state = fresh,
               port = Port,
               path = Path,
               parent_pid = Parent,
               host = Host,
               local_ip = proplists:get_value(ip, Opts, undefined),
               local_port = proplists:get_value(port, Opts, undefined),
               type = Type,
               connect_timeout = Timeout
              }).

loop(#state{parent_pid = ParentPid} = State) ->
    ?DebugF("loop: wait for message free:~p open:~p",[State#state.free, State#state.open]),
    receive
        {'DOWN', _MonitorRef, _Type, _Object, _Info} ->  %%parent terminates
            ok;
        {'EXIT', ParentPid, _Reason} -> %%even 'normal' terminates this
            ok;
        close ->
            ok;
        {send, Data, Ref}  ->
            case  do_send(State, Data) of
                {sent, NewState} ->
                    ParentPid ! {ok, Ref},
                    loop(NewState);
                {queued, #state{queue =Q} = NewState} when length(Q) < ?MAX_QUEUE_SIZE ->  %%do not return yet..
                    loop(NewState#state{pending_ref = Ref});
                {queued, NewState} -> %% we reach the max allowed queued messages.. close the connection.
                    ?LOGF("Client reached max bosh requests queue size: ~p. Closing session",
                          [length(NewState#state.queue)], ?ERR),
                    ts_mon:add({count, error_bosh_maxqueued}),
                    ParentPid ! {ok, Ref},
                    ParentPid ! {gen_ts_transport, self(), closed}
            end;
        {stream, terminate, Ref} ->
            #state{host = Host,
                   path = Path,
                   sid = Sid,
                   rid = Rid,
                   type = Type} = State,
            {NewState, Socket} = new_socket(State, once),
            ok = make_raw_request(Type, Socket, Host, Path, close_stream_msg(Sid, Rid)),
            ParentPid ! {ok, Ref},
            loop(NewState#state{session_state = closing, open = [{Socket, Rid+1}|NewState#state.open]});
        {stream, Domain, Ref} when State#state.domain == undefined ->
            NewState = do_connect(State, Domain),
            ParentPid ! {ok, Ref},
            loop(NewState);
        {stream, _Domain, Ref} -> %%here we must do a reset
            NewState = do_reset(State),
            ParentPid ! {ok, Ref},
            loop(NewState);
        {Tag, Socket, {http_response, Vsn, 200, "OK"}} when Tag == 'http' ; Tag == 'ssl'->
            ?Debug("loop: http response received"),
            case do_receive_http_response(State, Socket, Vsn) of
                {ok, NewState} ->
                    loop(NewState);
                terminate ->
                    if
                        State#state.session_state  /= 'closing' ->
                            ts_mon:add({count, error_bosh_terminated}),
                            ?LOG("Session terminated by server", ?INFO);
                        true ->
                            ok
                    end,
                    State#state.parent_pid ! {gen_ts_transport, self(), closed}
            end;
        {Close, Socket} when Close == tcp_closed ; Close == 'ssl_closed' ->
            ?LOG("loop: close",?DEB),
            case lists:keymember(Socket, 1, State#state.open) of
                true ->
                    %%ERROR, a current request is closed
                    ?LOG("Open request closed by server", ?ERR),
                    ts_mon:add({count, error_bosh_socket_closed}),
                    State#state.parent_pid ! {gen_ts_transport, self(), closed};
                false ->
                    %% A HTTP persistent connection, currently not in use, is closed by the server.
                    %% We can continue without trouble, just remove it, it will be reopened when needed.
                    loop(State#state{free = lists:delete(Socket, State#state.free)})
            end;
        {Tag, _Socket, {http_response, _Vsn, ResponseCode, _StatusLine}} when Tag == 'http' ; Tag == 'ssl' ->
            State#state.parent_pid ! {gen_ts_transport, self(), error, list_to_atom(integer_to_list(ResponseCode))};
        Unexpected ->
            ?LOGF("Bosh process received unexpected message: ~p", [Unexpected], ?ERR),
            State#state.parent_pid ! {gen_ts_transport, self(), error, unexpected_data}
    end.

do_receive_http_response(State, Socket, Vsn) ->
    #state{open = Open,
           sid = Sid,
           rid = Rid,
           queue = Queue,
           host = Host,
           path = Path,
           type = Type,
           parent_pid = ParentPid} = State,
    {ok, {{200, "OK"}, Hdrs, Resp}} = read_response(Type, Socket, Vsn, {200, "OK"}, [], <<>>, httph),
    ts_mon:add({ sum, size_rcv, iolist_size([ [if is_atom(H) -> atom_to_list(H); true -> H end, V] ||
                                                {H,V} <- Hdrs])}), %% count header size

    {_El = #xmlElement{name = body,
                       attributes = Attrs,
                       content = Content}, []}= xmerl_scan:string(binary_to_list(Resp)),
    case get_attr(Attrs, type) of
        "terminate" ->
            terminate;
        _R ->
            NewOpen = lists:keydelete(Socket, 1, Open),
            NewState2  = if
                             NewOpen == [] andalso State#state.session_state =:= 'normal' ->
                                 socket_setopts(Type, Socket, [{packet, http}, {active, once}]),
                                 ?DebugF("make empty request for normal session state ~p ~p ~p queue:~p", [Type,Socket,Rid, Queue]),
                                 ok = make_empty_request(Type, Socket,Sid, Rid, Queue, Host, Path),
                                 case length(Queue) of
                                     0 -> ok;
                                     _ ->
                                         ParentPid ! {ok, State#state.pending_ref}
                                         %% we just sent the pending packet, wakeup the client
                                 end,
                                 State#state{open = [{Socket, Rid}], rid = Rid +1, queue = []};
                             length(NewOpen) == 1 andalso length(State#state.queue) > 0 ->
                                 %%there are pending packet, sent it if the RID is ok, otherwise wait
                                 case NewOpen of
                                     [{_, R}] when (Rid - R) =< 1 ->
                                         socket_setopts(Type, Socket, [{packet, http}, {active, once}]),
                                         ok = make_empty_request(Type, Socket,Sid, Rid, Queue, Host, Path),
                                         ParentPid ! {ok, State#state.pending_ref},
                                         %% we just sent the pending packet, wakeup the client
                                         State#state{open = [{Socket, Rid}], rid = Rid +1, queue = []};
                                     _ ->
                                         NewState = return_socket(State, Socket),
                                         NewState#state{open = NewOpen}
                                 end;
                             true ->
                                 NewState = return_socket(State, Socket),
                                 NewState#state{open = NewOpen}
                         end,
            case Content of
                [] ->
                    %%empty response, do not bother the ts_client process with this
                    %% (so Noack/Bidi won't count this bosh specific thing, only async stanzas)
                    %% since ts_client don't see this, we need to count the size received
                    ts_mon:add({ sum, size_rcv, iolist_size(Resp)});
                _ ->
                    ParentPid ! {gen_ts_transport, self(), Resp}
            end,
            {ok, NewState2}
    end.

do_connect(#state{type = Type, host = Host, path = Path, parent_pid = ParentPid} = State, Domain) ->
    ?DebugF("do_connect ~p",[State]),
    Rid = 1000 + random:uniform(100000),
    %%Port= proplists:get_value(local_port, Options, undefined),
    NewState = State#state{
            domain = Domain,
            rid = Rid,
            open = [],
            queue = [],
            free = []
            },
    {NewState2, Socket} = new_socket(NewState, false),
    ok = make_raw_request(Type, Socket, Host, Path, create_session_msg(Rid, Domain, ?WAIT, ?HOLD)),
    {ok, {{200, "OK"}, Hdrs, Resp}} = read_response(Type, Socket, nil, nil, [], <<>>, http),
    ts_mon:add({ sum, size_rcv, iolist_size([ [if is_atom(H) -> atom_to_list(H); true -> H end, V] ||
                    {H,V} <- Hdrs])}), %% count header size

    NewState3 = return_socket(NewState2, Socket),
    {_El = #xmlElement{name = body,
                       attributes = Attrs,
                       content = _Content}, []} = xmerl_scan:string(binary_to_list(Resp)),
    ParentPid ! {gen_ts_transport, self(), Resp},
    NewState3#state{rid = Rid +1,
                    open = [],
                    sid = get_attr(Attrs, sid),
                    max_requests = 2
                   }.

do_reset(State) ->
    ?DebugF("do_reset free: ~p open:~p",[State#state.free,State#state.open]),
    #state{sid = Sid,
           rid = Rid,
           host = Host,
           path = Path,
           domain = Domain,
           type = Type} =  State,
    {NewState, Socket} = new_socket(State, once),
    ok = make_raw_request(Type, Socket, Host, Path, restart_stream_msg(Sid, Rid, Domain)),
    NewState#state{session_state = normal, rid = Rid +1, open = [{Socket, Rid}|State#state.open]}.

get_attr([], _Name) -> undefined;
get_attr([#xmlAttribute{name = Name, value = Value}|_], Name) -> Value;
get_attr([_|Rest], Name) -> get_attr(Rest, Name).

do_send(State, Data) ->
   #state{open = Open,
          rid = Rid,
          sid = Sid,
          host = Host,
          type = Type,
          path = Path,
          queue = Queue} = State,
    ?LOGF("do_send, rid:~p open:~p free:~p", [Rid,Open, State#state.free],?DEB),
    Result = if
                Open == []  -> send;
                true ->
                        Min = lists:min(lists:map(fun({_S,R}) -> R end, Open)),
                        if
                                (Rid -Min) =< 1 ->
                                        send;
                                true ->
                                        queue
                        end
            end,
     case Result of
         send ->
              {NewState, Socket} = new_socket(State, once),
              ok = make_request(Type, Socket, Sid, Rid, Queue, Host, Path, Data),
              {sent, NewState#state{rid = Rid +1, open = [{Socket, Rid}|Open], queue = Queue}};
         queue ->
                Queue = State#state.queue,
                NewQueue =  [Data|Queue],
               {queued, State#state{queue = NewQueue}}
    end.

make_empty_request(Type, Socket, Sid, Rid, Queue, Host, Path) ->
    StanzasText = lists:reverse(Queue),
    ?LOGF("make empty request ~p ~p ~p", [Type,Socket,Rid],?DEB),
    Body = stanzas_msg(Sid, Rid, StanzasText),
    make_request(Type, Socket, Host, Path, Body, iolist_size(StanzasText)).

make_raw_request(Type, Socket, Host, Path, Body) ->
    make_request(Type, Socket, Host, Path, Body, 0).

make_request(Type, Socket, Sid, Rid, Queue, Host, Path, Packet) ->
    StanzasText = lists:reverse([Packet|Queue]),
    ?LOGF("make  request ~p ~p ~p ~p", [Type,Socket,Rid, StanzasText],?DEB),
    Body = stanzas_msg(Sid, Rid, StanzasText),
    make_request(Type, Socket, Host, Path, Body, iolist_size(StanzasText)).
make_request(Type, Socket,Host, Path, Body, OriginalSize) ->
     ts_mon:add({count, bosh_http_req}),
     Hdrs = [{"Content-Type", ?CONTENT_TYPE}, {"keep-alive", "true"}],
     Request = format_request(Path, "POST", Hdrs, Host, Body),
     ok = socket_send(Type, Socket, Request),
     ts_mon:add({ sum, size_sent, iolist_size(Request) - OriginalSize}).
     %% add the http overhead. The size of the stanzas are already counted by ts_client code.


new_socket(State = #state{free = [Socket | Rest], type = Type}, Active) ->
        socket_setopts(Type, Socket, [{active, Active}, {packet, http}]),
        {State#state{free = Rest}, Socket};
new_socket(State = #state{type = Type, host = Host, port = Port, local_ip = LocalIp, local_port = LocalPort, connect_timeout=Timeout}, Active) ->
    Options = case LocalIp of
                  undefined -> [{active, Active}, {packet, http}];
                  _ ->  case LocalPort of
                            undefined -> [{active, Active}, {packet, http},{ip, LocalIp}];
                            _ ->
                                {ok, LPort} = ts_config_server:get_user_port(LocalIp),
                                [{active, Active}, {packet, http},{ip, LocalIp}, {port, LPort}]
                        end
    end,
    {ok, Socket} = socket_connect(Type, Host, Port,  Options, Timeout),
    ts_mon:add({count, bosh_http_conn}),
    {State, Socket}.

return_socket(State, Socket) ->
    socket_setopts(State#state.type, Socket, [{active, once}]),
    %%receive data from it, we want to know if something happens
    State#state{free = [Socket | State#state.free]}.

create_session_msg(Rid, To, Wait, Hold) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind'"
       " content='text/xml; charset=utf-8'",
       " ver='1.8'"
       " to='", To, "'",
       " rid='", integer_to_list(Rid), "'"
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " xmpp:version='1.0'",
       " wait='", integer_to_list(Wait), "'"
       " hold='", integer_to_list(Hold), "'/>"].

stanzas_msg(Sid, Rid, Text) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'"
       " sid='", Sid, "'>", Text, "</body>"].

restart_stream_msg(Sid, Rid, Domain) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'",
       " sid='", Sid, "'",
       " xmpp:restart='true'",
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " to='", Domain, "'",
       "/>"].

close_stream_msg(Sid, Rid) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'",
       " sid='", Sid, "'",
       " type='terminate'",
       " xmlns:xmpp='urn:xmpp:xbosh'",
       "/>"].

read_response(Type, Socket, Vsn, Status, Hdrs, Body, PacketType)  when PacketType == http ; PacketType == httph->
    socket_setopts(Type, Socket, [{packet, PacketType}, {active, false}]),
    case socket_recv(Type, Socket, 0) of
        {ok, {http_response, NewVsn, StatusCode, Reason}} ->
            NewStatus = {StatusCode, Reason},
            read_response(Type, Socket, NewVsn, NewStatus, Hdrs, Body, httph);
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {Name, Value},
            read_response(Type, Socket, Vsn, Status, [Header | Hdrs], Body, httph);
        {ok, http_eoh} ->
            socket_setopts(Type, Socket, [{packet, raw}, binary]),
            {NewBody, NewHdrs} = read_body(Type, Vsn, Hdrs, Socket),
            Response = {Status, NewHdrs, NewBody},
            {ok, Response};
        {error, closed} ->
            erlang:error(closed);
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_body(Type, _Vsn, Hdrs, Socket) ->
    % Find out how to read the entity body from the request.
    % * If we have a Content-Length, just use that and read the complete
    %   entity.
    % * If Transfer-Encoding is set to chunked, we should read one chunk at
    %   the time
    % * If neither of this is true, we need to read until the socket is
    %   closed (AFAIK, this was common in versions before 1.1).
    case proplists:get_value('Content-Length', Hdrs, undefined) of
        undefined ->
                throw({no_content_length, Hdrs});
        ContentLength ->
            read_length(Type, Hdrs, Socket, list_to_integer(ContentLength))
    end.

read_length(Type, Hdrs, Socket, Length) ->
    case socket_recv(Type, Socket, Length) of
        {ok, Data} ->
            {Data, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

%% @spec (Path, Method, Headers, Host, Body) -> Request
%% Path = iolist()
%% Method = atom() | string()
%% Headers = [{atom() | string(), string()}]
%% Host = string()
%% Body = iolist()
format_request(Path, Method, Hdrs, Host, Body) ->
    [
        Method, " ", Path, " HTTP/1.1\r\n",
        format_hdrs(add_mandatory_hdrs(Method, Hdrs, Host, Body), []),
        Body
    ].

%% spec normalize_method(AtomOrString) -> Method
%%   AtomOrString = atom() | string()
%%   Method = string()
%% doc
%% Turns the method in to a string suitable for inclusion in a HTTP request
%% line.
%% end
%-spec normalize_method(atom() | string()) -> string().
%normalize_method(Method) when is_atom(Method) ->
%    string:to_upper(atom_to_list(Method));
%normalize_method(Method) ->
%    Method.

format_hdrs([{Hdr, Value} | T], Acc) ->
    NewAcc = [
        Hdr, ":", Value, "\r\n" | Acc
    ],
    format_hdrs(T, NewAcc);
format_hdrs([], Acc) ->
    [Acc, "\r\n"].

add_mandatory_hdrs(Method, Hdrs, Host, Body) ->
    add_host(add_content_length(Method, Hdrs, Body), Host).

add_content_length("POST", Hdrs, Body) ->
    add_content_length(Hdrs, Body);
add_content_length("PUT", Hdrs, Body) ->
    add_content_length(Hdrs, Body);
add_content_length(_, Hdrs, _) ->
    Hdrs.

add_content_length(Hdrs, Body) ->
    case proplists:get_value("content-length", Hdrs, undefined) of
        undefined ->
            ContentLength = integer_to_list(iolist_size(Body)),
            [{"Content-Length", ContentLength} | Hdrs];
        _ -> % We have a content length
            Hdrs
    end.

add_host(Hdrs, Host) ->
    case proplists:get_value("host", Hdrs, undefined) of
        undefined ->
            [{"Host", Host } | Hdrs];
        _ -> % We have a host
            Hdrs
    end.



socket_connect(tcp, Host, Port, Options, Timeout) ->
    gen_tcp:connect(Host, Port, Options, Timeout);
socket_connect(ssl, Host, Port, Options, Timeout) ->
    %% First connect using tcp, and then upgrades.  The local ip and port directives seems to not work if
    %% the socket is opened directly as ssl.
%    {ForConnection, ForSSL} = lists:partition(fun({ip, _}) -> true; ({port, _}) -> true; (_) -> false end, Options),
%    {ok, S} = gen_tcp:connect(Host, Port, [{active, false}|ForConnection], Timeout),
%    ssl:connect(S, ForSSL, Timeout).
%   ?LOGF("Connect ~p", [ForSSL], ?ERR),
     ssl:connect(Host, Port, [{ssl_imp, new}|Options], Timeout).


socket_send(tcp, Socket, Data) ->
    gen_tcp:send(Socket, Data);
socket_send(ssl, Socket, Data) ->
    ssl:send(Socket, Data).

socket_recv(tcp, Socket, Len) ->
    gen_tcp:recv(Socket, Len);
socket_recv(ssl, Socket, Len) ->
    ssl:recv(Socket, Len).

% Not used
%socket_close(tcp, Socket) ->
%    gen_tcp:close(Socket);
%socket_close(ssl, Socket) ->
%    ssl:close(Socket).

socket_setopts(tcp, Socket, Opts) ->
    inet:setopts(Socket, Opts);
socket_setopts(ssl, Socket, Opts) ->
    ssl:setopts(Socket, Opts).
