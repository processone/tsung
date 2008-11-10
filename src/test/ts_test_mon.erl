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
-include("ts_os_mon.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

sample_update_test()->
    myset_env(),
    Val=ts_stats_mon:update_stats(sample,[],50),
    Val2=ts_stats_mon:update_stats(sample,Val,20),
    ?assertMatch([35.0,450.0,50,20,2,0,0],Val2).

sample_counter_update_test()->
    myset_env(),
    Val=ts_stats_mon:update_stats(sample_counter,[],10),
    Val2=ts_stats_mon:update_stats(sample_counter,Val,60),
    Val3=ts_stats_mon:update_stats(sample_counter,Val2,80),
    ?assertMatch([35.0,450.0,50,20,2,0,0,90],Val3).

sample_counter2_update_test()->
    myset_env(),
    Val=ts_stats_mon:update_stats(sample_counter,[],10),
    Val2=ts_stats_mon:update_stats(sample_counter,Val,60),
    Val3=ts_stats_mon:update_stats(sample_counter,Val2,40),
    ?assertMatch([50,0,50,50,1,0,0,40],Val3).

sample_counter3_update_test()->
    myset_env(),
    Val=ts_stats_mon:update_stats(sample_counter,[],10),
    Val2=ts_stats_mon:update_stats(sample_counter,Val,60),
    Val3=ts_stats_mon:update_stats(sample_counter,Val2,0),
    ?assertMatch([50,0,50,50,1,0,0,60],Val3).

procnet_test()->
    myset_env(),
    ?assertMatch({10106167,2609645},
                 ts_os_mon_erlang:get_os_data(packets, {unix, linux}, "./src/test/procnetdev_test.txt")).

procnet_7chars_test()->
    myset_env(),
    ?assertMatch({10106167,2609645},
                 ts_os_mon_erlang:get_os_data(packets, {unix, linux}, "./src/test/procnetdev_test7chars.txt")).

get_plugins_test()->
    Dict= dict:from_list([{1,{snmp, 3 }},
                          {2,{snmp, 3 }},
                          {3,{snmp, 3 }},
                          {4,{erlang,3 }},
                          {5,{munin,3 }},
                          {6,{munin,3}}]),
    ?assertMatch([erlang, munin,snmp], ts_os_mon:get_all_plugins(#os_mon{pids=Dict})).
get_plugins_empty_test()->
    ?assertMatch([], ts_os_mon:get_all_plugins(#os_mon{pids=dict:new()})).

myset_env()->
    application:set_env(stdlib,debug_level,0).

