%%%-------------------------------------------------------------------
%%% File    : ts_test_pgsql.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 10 Apr 2008 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_pgsql).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_pgsql.hrl").
-include("ts_recorder.hrl").

-include_lib("eunit/include/eunit.hrl").

test()->
    ok.


utils_md5_test()->
    myset_env(),
    Password="sesame",
    User="benchmd5",
     Salt= << 54,195,212,197 >>,
     Hash= list_to_binary(["md5967c89f451d1d504a1f02fc69fb65cb5",0]),
    PacketSize= 4+size(Hash),
    Bin= <<$p,PacketSize:32/integer, Hash/binary>>,
    ?assertMatch(Bin,  pgsql_proto:encode_message(pass_md5, {User,Password,Salt} ) ).

extended_test()->
    Data= << 80,0,0,0,75,115,99,117,49,0,100,101,99,108,97,114,101,32,115,99,117,49,32,99,117,114,
             115,111,114,32,119,105,116,104,32,104,111,108,100,32,102,111,114,32,115,101,108,101,
             99,116,32,67,79,85,78,84,40,42,41,32,102,114,111,109,32,32,32,98,114,46,97,104,32,0,0,
             0,83,0,0,0,4 >>,
    myset_env(0),
    Result=ts_proxy_pgsql:process_data(#proxy{},Data),
    ?assertMatch(#proxy{}, Result).


myset_env()->
    myset_env(0).
myset_env(Val)->
    application:set_env(stdlib,debug_level,Val).

