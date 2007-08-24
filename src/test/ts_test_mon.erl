%%%-------------------------------------------------------------------
%%% File    : ts_test_mon.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 24 August 2007 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_mon).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

procnet_test()->
    myset_env(),
    ?assertMatch({10106167,2609645},
                 ts_os_mon:get_os_data(packets, {unix, linux}, "./src/test/procnetdev_test.txt")).

procnet_7chars_test()->
    myset_env(),
    ?assertMatch({10106167,2609645},
                 ts_os_mon:get_os_data(packets, {unix, linux}, "./src/test/procnetdev_test7chars.txt")).


myset_env()->
    application:set_env(stdlib,debug_level,0).

