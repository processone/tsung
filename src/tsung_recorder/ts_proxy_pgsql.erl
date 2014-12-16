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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_proxy_pgsql).
-vc('$Id$ ').
-author('Nicolas.Niclausse@niclux.org').

-include("ts_macros.hrl").
-include("ts_pgsql.hrl").
-include("ts_recorder.hrl").


-export([parse/4, record_request/2, socket_opts/0, gettype/0]).
-export([client_close/2]).

-export([rewrite_serverdata/1]).
-export([rewrite_ssl/1]).

%%--------------------------------------------------------------------
%% Func: socket_opts/0
%%--------------------------------------------------------------------
socket_opts() -> [binary].

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
%% Func: client_close/2
%%--------------------------------------------------------------------
client_close(_Socket,State)->
    ts_proxy_recorder:dorecord({#pgsql_request{type=close}}),
    State.

%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse PGSQL request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State=#proxy{parse_status=Status},_,_SSocket,Data= << 0,0,0,8,4,210,22,47 >>) when Status==new ->
    ?LOG("SSL req: ~n",?DEB),
    Socket = connect(undefined),
    ts_client_proxy:send(Socket, Data, ?MODULE),
    {ok, State#proxy{buffer= << >>,serversock = Socket }};
parse(State=#proxy{parse_status=Status},_,ServerSocket,Data) when Status==new ->
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
            {ok, State#proxy{buffer= <<>>, serversock = Socket} };
        Req ->
            ?LOGF("Received data from client: split = ~p~n",[Res],?DEB),
            ts_proxy_recorder:dorecord({Req#pgsql_request{type=connect}}),
            Socket = connect(ServerSocket),
            ts_client_proxy:send(Socket, Data, ?MODULE),
            {ok, State#proxy{parse_status=open, buffer= <<>>, serversock = Socket} }
    end;
parse(State=#proxy{},_,ServerSocket,Data) ->
    NewData = << (State#proxy.buffer)/binary, Data/binary >>,
    ?LOGF("Received data from client: ~p~n",[NewData],?DEB),
    NewState =  process_data(State,NewData),
    ts_client_proxy:send(ServerSocket, Data, ?MODULE),
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
                             State#proxy{buffer= <<>>};
                         terminate ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=close}}),
                             State#proxy{buffer= <<>>};
                         {password, Password} ->
                             PwdStr= binary_to_list(Password),
                             ?LOGF("password = ~s~n",[PwdStr],?DEB),
                             ts_proxy_recorder:dorecord({#pgsql_request{type=authenticate, passwd=PwdStr}}),
                             State#proxy{buffer= <<>>};
                         {parse,{<< >>,StringQuery,Params} } ->
                             %% TODO: handle Parameters if defined
                             ts_proxy_recorder:dorecord({#pgsql_request{type=parse,equery=StringQuery,
                                                                       parameters=Params}}),
                             State#proxy{buffer= <<>>};
                         {parse,{StringName,StringQuery,Params} } ->
                             %% TODO: handle Parameters if defined
                             ts_proxy_recorder:dorecord({#pgsql_request{type=parse,name_prepared=StringName,
                                                                        parameters=Params,
                                                                        equery=StringQuery}}),
                             State#proxy{buffer= <<>>};
                         {bind,{Portal,StringQuery,Params, ParamsFormat,ResFormats} } ->
                             R={#pgsql_request{type=bind, name_prepared=StringQuery,
                                               name_portal=Portal,
                                               parameters=Params,formats=ParamsFormat,
                                               formats_results=ResFormats}},
                             ts_proxy_recorder:dorecord(R),
                             State#proxy{buffer= <<>>};
                         {copy, CopyData} ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=copy,equery=CopyData}}),
                             State#proxy{buffer= <<>>};
                         copydone ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=copydone}}),
                             State#proxy{buffer= <<>>};
                         {copyfail,Msg} ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=copyfail,equery=Msg}}),
                             State#proxy{buffer= <<>>};
                         {describe,{<<"S">>,Name} } ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=describe,name_prepared=Name}}),
                             State#proxy{buffer= <<>>};
                         {describe,{<<"P">>,Name} } ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=describe,name_portal=Name}}),
                             State#proxy{buffer= <<>>};
                         {execute,{NamePortal,Max} } ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=execute,name_portal=NamePortal,max_rows=Max}}),
                             State#proxy{buffer= <<>>};
                         sync ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=sync}}),
                             State#proxy{buffer= <<>>};
                         flush ->
                             ts_proxy_recorder:dorecord({#pgsql_request{type=flush}}),
                             State#proxy{buffer= <<>>}
                     end,
            process_data(NewState,Data);
        false ->
            ?LOG("need more~n",?DEB),
            State#proxy{buffer=RawData}
    end;
process_data(State,RawData) ->
    ?LOG("need more~n",?DEB),
    State#proxy{buffer=RawData}.

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
    case Name of
        << 0 >> -> {describe,{Type,[]}};
        Bin ->     {describe,{Type,Bin}}
    end;
decode_packet($S, _Data) -> %sync
    sync;
decode_packet($H, _Data) -> %flush
    flush;
decode_packet($E, Data) -> %execute
    {NamePortal,PortalSize} = pgsql_util:to_string(Data),
    S1=PortalSize+1,
    << _:S1/binary, MaxParams:32/integer >> = Data,
    ?LOGF("Extended protocol: execute ~p ~p~n",[NamePortal,MaxParams], ?DEB),
    case MaxParams of
        0   -> {execute,{NamePortal,unlimited}};
        Val -> {execute,{NamePortal,Val}}
    end;
decode_packet($d, Data) -> %copy
    ?LOGF("Extended protocol: copy ~p~n",[Data], ?DEB),
    {copy, Data};
decode_packet($c, _) -> %copy-complete
    ?LOG("Extended protocol: copydone~n", ?DEB),
    copydone;
decode_packet($f, Data) -> %copy-fail
    ?LOGF("Extended protocol: copy failure~p~n", [Data],?DEB),
    {copyfail, Data};
decode_packet($B, Data) -> %bind
    [NamePortal, StringQuery | _] = split(Data,<<0>>,[global,trim]),
    Size = size(NamePortal)+size(StringQuery)+2,
    << _:Size/binary, NParamsFormat:16/integer,Tail1/binary >> = Data,
    SizeParamsFormat=2*NParamsFormat, % 16 bits
    << Formats:SizeParamsFormat/binary, NParams:16/integer, Tail2/binary>> = Tail1,
    ParamsFormat = case {NParamsFormat,Formats} of
                        {0,_}                   -> none;
                        {1,<< 0:16/integer >> } -> text;
                        {1,<< 1:16/integer >> } -> binary;
                        _                       -> auto
                    end,
    {Params,<< _NFormatRes:16/integer,FormatsResBin/binary >> }=get_params(NParams,Tail2,[]),
    ResFormats=get_params_format(FormatsResBin,[]),
    ?LOGF("Extended protocol: bind ~p ~p ~p ~p ~p~n",[NamePortal,StringQuery,Params,ParamsFormat,ResFormats ], ?DEB),
    {bind,{NamePortal,StringQuery,Params,ParamsFormat,ResFormats}};
decode_packet($P, Data) -> % parse
    [StringName, StringQuery | _] = split(Data,<<0>>,[global,trim]),
    Size = size(StringName)+size(StringQuery)+2,
    << _:Size/binary, NParams:16/integer,ParamsBin/binary >> = Data,
    Params=get_params_int(NParams,ParamsBin,[]),
    ?LOGF("Extended protocol: parse ~p ~p ~p~n",[StringName,StringQuery,Params], ?DEB),
    {parse,{StringName,StringQuery,Params}}.


get_params_format(<<>>,Acc) ->
    lists:reverse(Acc);
get_params_format(<<0:16/integer,Tail/binary>>,Acc) ->
    get_params_format(Tail,[text|Acc]);
get_params_format(<<1:16/integer,Tail/binary>>,Acc) ->
    get_params_format(Tail,[binary|Acc]).

get_params(0,Tail,Acc) ->
    {lists:reverse(Acc), Tail};
get_params(N,<<-1:32/integer-signed,Tail/binary>>,Acc) ->
    get_params(N-1,Tail,['null'|Acc]);
get_params(N,<<Size:32/integer,S:Size/binary,Tail/binary>>,Acc) ->
    get_params(N-1,Tail,[S|Acc]).

get_params_int(0,_,Acc) ->
    lists:reverse(Acc);
get_params_int(N,<<Val:32/integer,Tail/binary>>,Acc) ->
    get_params_int(N-1,Tail,[Val|Acc]).


split(Bin,Pattern,Options)->
    binary:split(Bin,Pattern,Options).

%%--------------------------------------------------------------------
%% Func: record_request/2
%% Purpose: record request given State=#state_rec and Request=#pgsql_request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
record_request(State=#state_rec{logfd=Fd, plugin_state=connected},
               #pgsql_request{type=connect, username=User, database=DB})->
    %% connect request while already connected
    ?LOG("PGSQL: connect request but we are already connected ! record a close request first ~n", ?WARN),
    io:format(Fd,"<!-- close forced by Tsung; maybe several clients were using the recorder ?-->~n <request><pgsql type='close'/></request>~n", []),
    io:format(Fd,"<request><pgsql type='connect' database='~s' username='~s'/>", [DB,User]),
    io:format(Fd,"</request>~n",[]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd},
               #pgsql_request{type=connect, username=User, database=DB})->
    io:format(Fd,"<request><pgsql type='connect' database='~s' username='~s'/>", [DB,User]),
    io:format(Fd,"</request>~n",[]),
    {ok,State#state_rec{plugin_state=connected }};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=sql, sql=SQL})->
    io:format(Fd,"<request> <pgsql type='sql'><![CDATA[~s]]></pgsql>", [SQL]),
    io:format(Fd,"</request>~n",[]),
    {ok,State};
record_request(State=#state_rec{plugin_state=undefined}, #pgsql_request{type=close})->
    %% not connected, don't record
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=close})->
    io:format(Fd,"<request><pgsql type='close'/></request>~n", []),
    {ok,State#state_rec{plugin_state=undefined}};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=sync})->
    io:format(Fd,"<request><pgsql type='sync'/></request>~n", []),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=flush})->
    io:format(Fd,"<request><pgsql type='flush'/></request>~n", []),
    {ok,State};
record_request(State=#state_rec{logfd=Fd,ext_file_id=Id}, #pgsql_request{type=copy,equery=Bin}) when size(Bin) > 1024->
    FileName=ts_utils:append_to_filename(State#state_rec.log_file,".xml","-"++integer_to_list(Id)++".bin"),
    ok = file:write_file(FileName,Bin),
    io:format(Fd,"<request><pgsql type='copy' contents_from_file='~s'/></request>~n", [FileName]),
    {ok,State#state_rec{ext_file_id=Id+1} };
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=copy,equery=Bin}) ->
    Str=ts_utils:join(",",binary_to_list(Bin)),
    io:format(Fd,"<request><pgsql type='copy'>~s</pgsql></request>~n", [Str]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=copyfail,equery=Bin}) ->
    Str=binary_to_list(Bin),
    io:format(Fd,"<request><pgsql type='copyfail' query='~s'/></request>~n", [Str]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=copydone}) ->
    io:format(Fd,"<request><pgsql type='copydone'/></request>~n", []),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=describe,name_prepared=undefined,name_portal=Val}) ->
    io:format(Fd,"<request><pgsql type='describe' name_portal='~s'/></request>~n", [Val]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=describe,name_portal=undefined,name_prepared=Val}) ->
    io:format(Fd,"<request><pgsql type='describe' name_prepared='~s'/></request>~n", [Val]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=parse,name_portal=undefined,
                                                          name_prepared=undefined,equery=Query,parameters=Params})->
    ParamsStr=ts_utils:join(",",Params),
    io:format(Fd,"<request><pgsql type='parse' parameters='~s'><![CDATA[~s]]></pgsql></request>~n", [Query,ParamsStr]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=parse,name_portal=undefined,
                                                          name_prepared=Val,equery=Query,parameters=Params})->
    ParamsStr=ts_utils:join(",",Params),
    io:format(Fd,"<request><pgsql type='parse' name_prepared='~s' parameters='~s'><![CDATA[~s]]></pgsql></request>~n", [Val,ParamsStr,Query]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=parse,name_portal=Portal,
                                                          parameters=Params,
                                                          name_prepared=Prep,equery=Query})->
    ParamsStr=ts_utils:join(",",Params),
    io:format(Fd,"<request><pgsql type='parse' name_portal='~s' name_prepared='~s' parameters='~s'><![CDATA[~s]]></pgsql></request>~n", [Portal,Prep,ParamsStr,Query]),
    {ok,State};

record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=bind,name_portal = <<>>,name_prepared=Val, parameters=[],formats=ParamsFormat, formats_results=ResFormats})->
    ResFormatsStr=ts_utils:join(",",ResFormats),
     io:format(Fd,"<request><pgsql type='bind' name_prepared='~s' formats='~s' formats_results='~s' /></request>~n", [Val,ParamsFormat,ResFormatsStr]),
     {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=bind,
                                                          name_portal=Portal,
                                                          name_prepared=Prep,
                                                          parameters=Params,
                                                          formats=ParamsFormat,
                                                          formats_results=ResFormats})->
    ParamsStr=ts_utils:join(",",Params),
    ResFormatsStr=ts_utils:join(",",ResFormats),
    io:format(Fd,"<request><pgsql type='bind' name_portal='~s' name_prepared='~s' formats='~s' formats_results='~s' parameters='~s'/></request>~n",
              [Portal,Prep,ParamsFormat,ResFormatsStr,ParamsStr]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=execute,name_portal=[],max_rows=unlimited})->
    io:format(Fd,"<request><pgsql type='execute'/></request>~n", []),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=execute,name_portal=[],max_rows=Max})->
    io:format(Fd,"<request><pgsql type='execute' max_rows='~p'/></request>~n", [Max]),
    {ok,State};
record_request(State=#state_rec{logfd=Fd}, #pgsql_request{type=execute,name_portal=Portal,max_rows=Max})->
    io:format(Fd,"<request><pgsql type='execute' name_portal='~p' max_rows='~p'/></request>~n", [Portal,Max]),
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
                                   ]++ socket_opts()),
    ?LOGF("ok, connected  ~p~n",[Socket],?DEB),
    Socket;
connect(Socket) -> Socket.
