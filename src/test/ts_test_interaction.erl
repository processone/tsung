%%%-------------------------------------------------------------------
%%% File    : ts_test_interaction.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 28 Aug 2012 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_interaction).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").

-include_lib("eunit/include/eunit.hrl").


test()->
    ok.

notify_test()->
    myset_env(0),
    ts_interaction_server:start(),
    ts_interaction_server:send({chat,now()}),
    ts_interaction_server:notify({'receive',chat,self()}),
    ts_interaction_server:rcv({chat,now()}),
    Res = receive
              Data ->
                  erlang:display(["received 1",Data]),
                  ok
          after 1000 ->
                  timeout
          end,
    ?assertMatch(ok, Res ).

notify_to_test()->
    myset_env(0),
    ts_interaction_server:notify({send,chat2,self()}),
    ts_interaction_server:send({chat2,now()}),
    Res = receive
              Data ->
                  erlang:display(["received 2",Data]),
                  ok
          after 2000 ->
                  timeout
          end,
    ?assertMatch(ok, Res ).

myset_env()->
    myset_env(0).
myset_env(Val)->
    application:set_env(stdlib,dumpstats_interval,550),
    application:set_env(stdlib,mon_file,"test-mon.log"),
    application:set_env(stdlib,debug_level,Val).

