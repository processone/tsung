%%%-------------------------------------------------------------------
%%% File    : ts_test_recorder.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 Mar 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_config).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.
read_config_http_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/http_simple.xml")).
read_config_http2_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/http_distributed.xml")).
read_config_pgsql_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/pgsql.xml")).
read_config_jabber_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./examples/jabber.xml")).

config_get_session_test() ->
    myset_env(),
    ts_user_server:start([]),
    ts_config_server:start_link(["/tmp"]),
    ok = ts_config_server:read_config("./examples/http_setdynvars.xml"),
    {ok, {Session,Size,IP,Server} }  = ts_config_server:get_next_session("localhost"),
    ?assertMatch(1, Session#session.id).


read_config_badpop_test() ->
    myset_env(),
    ts_user_server:start([]),
    {ok, Config} = ts_config:read("./src/test/badpop.xml"),
    ?assertMatch({error,[{error,{bad_sum,_,_}}]}, ts_config_server:check_config(Config)).

config_minmax_test() ->
    myset_env(),
    {ok, {Session,Size,IP,Server} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    ?assertMatch({thinktime,{range,2000,4000}}, ts_config_server:get_req(Id,7)).

config_minmax2_test() ->
    myset_env(),
    {ok, {Session,Size,IP,Server} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    {thinktime, Req} = ts_config_server:get_req(Id,7),
    Ref=ts_client:set_thinktime(Req),
    receive
        {timeout,Ref2,end_thinktime} -> ok
    end,
    ?assertMatch(Ref, Ref2).

config_thinktime_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks.xml"),
    {ok, {Session,Size,IP,Server} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    {thinktime, Req=2000} = ts_config_server:get_req(Id,5),
    {thinktime, 2000} = ts_config_server:get_req(Id,7),
    Ref=ts_client:set_thinktime(Req),
    receive
        {timeout,Ref2,end_thinktime} -> ok
    end,
    ?assertMatch(Ref, Ref2).

config_thinktime2_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks2.xml"),
    {ok, {Session,Size,IP,Server} }  = ts_config_server:get_next_session("localhost"),
    Id = Session#session.id,
    {thinktime, Req} = ts_config_server:get_req(Id,5),
    Ref=ts_client:set_thinktime(Req),
    receive
        {timeout,Ref2,end_thinktime} -> ok
    end,
    random:seed(), % reinit seed for others tests
    ?assertMatch({random,1000}, Req).

myset_env()->
    myset_env(0).
myset_env(Level)->
    application:set_env(stdlib,debug_level,Level),
    application:set_env(stdlib,thinktime_override,"false"),
    application:set_env(stdlib,thinktime_random,"false").
