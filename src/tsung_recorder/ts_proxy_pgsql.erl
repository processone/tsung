%%%
%%%  Copyright (C) Nicolas Niclausse 2005
%%%
%%%  Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 09 Nov 2005 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
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

-module(ts_proxy_pgsql).
-vc('$Id$ ').
-author('Nicolas.Niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_pgsql.hrl").
-include("ts_recorder.hrl").


-export([parse/4, record_request/2, socket_opts/0, gettype/0]).

-export([rewrite_serverdata/1]).
-export([rewrite_ssl/1]).

%%--------------------------------------------------------------------
%% Func: socket_opts/0
%%--------------------------------------------------------------------
socket_opts() -> [].

%%--------------------------------------------------------------------
%% Func: gettype/0
%%--------------------------------------------------------------------
gettype() -> "ts_pgsql".

%%--------------------------------------------------------------------
%% Func: rewrite_serverdata/1
%%--------------------------------------------------------------------
rewrite_serverdata(Data)->{ok, Data}.

%%--------------------------------------------------------------------
%% Func: rewrite_ssl/1
%%--------------------------------------------------------------------
rewrite_ssl(Data)->{ok, Data}.

%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse PGSQL request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State=#proxy{parse_status=Status},_,_SSocket,String=[0,0,0,8,4,210,22,47]) when Status==new ->
    ?LOG("SSL req: ~n",?DEB),
    Socket = connect(undefined),
    ts_client_proxy:send(Socket, String, ?MODULE),
    {ok, State#proxy{buffer=[],serversock = Socket }};
parse(State=#proxy{parse_status=Status},_,ServerSocket,String) when Status==new ->
    Data = list_to_binary(String),
    <<PacketSize:32/integer, StartupPacket/binary>> = Data,
    ?LOGF("Received data from client: size=~p [~p]~n",[PacketSize, StartupPacket],?DEB),
    <<ProtoMaj:16/integer, ProtoMin:16/integer, Data2/binary>> = StartupPacket,
    ?LOGF("Received data from client: proto maj=~p min=~p~n",[ProtoMaj, ProtoMin],?DEB),
    Res= pgsql_util:split_pair_rec(Data2),
    case get_db_user(Res) of
        #pgsql_request{database=undefined} ->
            ?LOGF("Received data from client: split = ~p~n",[Res],?DEB),
            Socket = connect(ServerSocket),
            ts_client_proxy:send(Socket, Data, ?MODULE),
            {ok, State#proxy{buffer=[], serversock = Socket} };
        Req ->
            ?LOGF("Received data from client: split = ~p~n",[Res],?DEB),
            ts_proxy_recorder:dorecord({Req#pgsql_request{type=connect}}),
            Socket = connect(ServerSocket),
            ts_client_proxy:send(Socket, Data, ?MODULE),
            {ok, State#proxy{parse_status=open, buffer=[], serversock = Socket} }
    end;
parse(State=#proxy{},_,ServerSocket,String) ->
    NewString = lists:append(State#proxy.buffer, String),
    Data = list_to_binary(NewString),
    ?LOGF("Received data from client: ~p~n",[Data],?DEB),
    NewState =  process_data(State,Data),
    ts_client_proxy:send(ServerSocket, String, ?MODULE),
    {ok,NewState}.



process_data(State,<< >>) ->
    State;
process_data(State,RawData = <<Code:8/integer, Size:4/integer-unit:8, Tail/binary>>) ->
    ?LOGF("PGSQL: received [~p]  size=~p Pckt size= ~p ~n",[Code, Size, size(Tail)],?DEB),
    RealSize = Size-4,
    case RealSize =< size(Tail) of
        true ->
            << Packet:RealSize/binary, Data/binary >> = Tail,
            NewState=case decode_packet(Code, Packet) of
                         {sql, SQL} ->
                             SQLStr= binary_to_list(SQL),
                             ?LOGF("sql = ~s~n",[SQLStr],?DEB),
                             ts_proxy_recorder:dorecord({#pgsql_request{type=sql, sql=SQLStr}}),
                             State#proxy{buffer=[]};
                         terminate ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=close}}),
                             State#proxy{buffer=[]};
                         {password, Password} ->
                             PwdStr= binary_to_list(Password),
                             ?LOGF("password = ~s~n",[PwdStr],?DEB),
                             ts_proxy_recorder:dorecord({#pgsql_request{type=authenticate, passwd=PwdStr}}),
                             State#proxy{buffer=[]};
                         {parse,{StringName,StringQuery,NParams,Params} } ->
                             %%TODO: record
                             State;
                         {portal,{PortalName,StringQuery,NParams,Params} } ->
                             %%TODO: record
                             State;
                         sync ->
                             %%TODO: record
                             State;
                         {describe,{Type,Name} } ->
                             %%TODO: record
                             State;
                         {execute,{NamePortal,MaxParams} } ->
                             %%TODO: record
                             State
                     end,
            process_data(NewState,Data);
        false ->
            ?LOG("need more~n",?DEB),
            State#proxy{buffer=RawData}
    end.

get_db_user(Arg) ->
    get_db_user(Arg,#pgsql_request{}).

get_db_user([], Req)-> Req;
get_db_user([{"user",User}| Rest], Req)->
    get_db_user(Rest,Req#pgsql_request{username=User});
get_db_user([{"database",DB}| Rest], Req) ->
    get_db_user(Rest,Req#pgsql_request{database=DB});
get_db_user([_| Rest], Req) ->
    get_db_user(Rest,Req).

decode_packet($Q, Data)->
    Size= size(Data)-1,
    <<SQL:Size/binary, 0:8 >> = Data,
    {sql, SQL};
decode_packet($p, Data) ->
    Size= size(Data)-1,
    <<Password:Size/binary, 0:8 >> = Data,
    {password, Password};
decode_packet($X, _) ->
    terminate;
decode_packet($D,  << Type:1/binary, Name/binary >>) -> %describe
    ?LOGF("Extended protocol: describe ~s ~p~n",[Type,Name], ?DEB),
    {describe,{Type,Name}};
decode_packet($S,  Data) -> %sync
    sync;
decode_packet($E,  Data) -> %execute
    {NamePortal,PortalSize} = pgsql_util:to_string(Data),
    S1=PortalSize+1,
    << _:S1/binary, MaxParams:32/integer >> = Data,
    ?LOGF("Extended protocol: execute ~p ~p~n",[NamePortal,MaxParams], ?DEB),
    {execute,{NamePortal,MaxParams}};
decode_packet($B, Data) -> %bind
    [NamePortal, StringQuery | Tail] = binary:split(Data,<<0>>,[global,trim]),
    Size = size(NamePortal)+size(StringQuery)+2,
    << _:Size/binary, NParams:16/integer,Params/binary >> = Data,
    ?LOGF("Extended protocol: bind ~p ~p ~p~n",[NamePortal,StringQuery,NParams], ?DEB),
    {portal,{NamePortal,StringQuery,NParams,Params}};
decode_packet($P, Data) -> % parse
    [StringName, StringQuery | Tail] = binary:split(Data,<<0>>,[global,trim]),
    Size = size(StringName)+size(StringQuery)+2,
    << _:Size/binary, NParams:16/integer,Params/binary >> = Data,
    ?LOGF("Extended protocol: parse ~p ~p ~p~n",[StringName,StringQuery,NParams], ?DEB),
    {parse,{StringName,StringQuery,NParams,Params}}.


%%--------------------------------------------------------------------
%% Func: record_request/2
%% Purpose: record request given State=#state_rec and Request=#pgsql_request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
record_request(State=#state_rec{logfd=Fd},
               #pgsql_request{type=connect, username=User, database=DB})->
    io:format(Fd,"<request><pgsql type='connect' database='~s' username='~s'/>", [DB,User]),
    io:format(Fd,"</request>~n",[]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=sql, sql=SQL})->
    io:format(Fd,"<request> <pgsql type='sql'><![CDATA[~s]]></pgsql>", [SQL]),
    io:format(Fd,"</request>~n",[]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=close})->
    io:format(Fd,"<request><pgsql type='close'/></request>", []),
    {ok,State};
record_request(State=#state_rec{logfd=Fd},
               #pgsql_request{type = authenticate , passwd  = Pass }) ->

    Fd = State#state_rec.logfd,
    io:format(Fd,"<request><pgsql type='authenticate' password='~s'>", [Pass]),
    io:format(Fd,"</pgsql></request>~n",[]),
    {ok,State}.


connect(undefined) ->
    {ok, Socket} = gen_tcp:connect(?config(pgsql_server),?config(pgsql_port),
                                   [{active, once},
                                    {recbuf, ?tcp_buffer},
                                    {sndbuf, ?tcp_buffer}
                                   ]),
    ?LOGF("ok, connected  ~p~n",[Socket],?DEB),
    Socket;
connect(Socket) -> Socket.
