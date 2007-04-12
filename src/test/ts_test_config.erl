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
read_config_pgsql_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/pgsql.xml")).
read_config_jabber_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./examples/jabber.xml")).


myset_env()->
    application:set_env(stdlib,debug_level,0),
    application:set_env(stdlib,thinktime_override,"false"),
    application:set_env(stdlib,thinktime_random,"false").
