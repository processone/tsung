%%%-------------------------------------------------------------------
%%% File    : ts_test_recorder.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 Mar 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_file_server).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CSVSIZE,10000).


test()->
    ok.

config_file_server1_test()->
    myset_env(),
    ts_file_server:start(),
    ts_file_server:read([{default,"./src/test/test_file_server.csv"},
                         {user,"./src/test/test_file_server2.csv"} ]),
    ?assertMatch({ok,<< "username1;glop;">> }, ts_file_server:get_next_line()).

config_file_server2_test()->
    myset_env(),
    ?assertMatch({ok,<< "username2;;" >> }, ts_file_server:get_next_line()).


config_file_server3_test()->
    myset_env(),
    ?assertMatch({ok,<< "user1;sesame">> }, ts_file_server:get_next_line(user)).

config_file_server4_test()->
    myset_env(),
    ?assertMatch({ok,<< "username3;glop4;">> }, ts_file_server:get_next_line()).

config_file_server_dynfun_test()->
    myset_env(),
    ?assertMatch( << "username1;glop;">>, ts_file_server:get_next_line({self(), {}})).


config_file_server_huge_test()->
    myset_env(),
    ts_file_server:stop(),
    ts_file_server:start(),
    CSV=lists:foldl(fun(I,Acc)-> IStr=integer_to_list(I),
                       [Acc,"user",IStr,";passwd",IStr,"\n"]
              end, [],lists:seq(1,?CSVSIZE)),
    File="./src/test/usersdb.csv",
    file:write_file(File,list_to_binary(CSV)),
    {Time1, Out }  = timer:tc(ts_file_server, read, [[{default,File}]]),
     erlang:display([?CSVSIZE," read_file:", Time1]),
    ?assertMatch(ok, Out).

config_file_server_huge_get_random_test()->
    {Time2, Out } = timer:tc( lists, foreach, [ fun(_)-> ts_file_server:get_random_line() end,lists:seq(1,?CSVSIZE)]),
     erlang:display([?CSVSIZE," get all lines (random):", Time2]),
    ?assertMatch(ok, Out ).

config_file_server_huge_get_next_test()->
    {Time2, Out } = timer:tc( lists, foreach, [ fun(_)-> ts_file_server:get_next_line() end,lists:seq(1,?CSVSIZE)]),
     erlang:display([?CSVSIZE," get all lines:", Time2]),
    ?assertMatch(ok, Out ).

config_file_server_cycle_test()->
    myset_env(),
    ts_file_server:stop(),
    ts_file_server:start(),
    ts_file_server:read([{default,"./src/test/test_file_server.csv"}]),
    ts_file_server:get_next_line(),
    ts_file_server:get_next_line(),
    ts_file_server:get_next_line(),
    ?assertMatch({ok,<< "username1;glop;">> }, ts_file_server:get_next_line()).

config_file_server_all_test()->
    myset_env(),
    ?assertMatch({ok,[<< "username1;glop;">> ,<< "username2;;">> ,<< "username3;glop4;">> ]}, ts_file_server:get_all_lines()).

file_to_list_test()->
    Val = [ "username1;glop;",  "username2;;" , "username3;glop4;" ],
    ?assertMatch({ok, Val},ts_utils:file_to_list("./src/test/test_file_server.csv")).

split_test()->
    ?assertEqual([<<"username3" >>, <<"glop4">>, <<>>], ts_utils:split(<< "username3;glop4;">>, <<";">>)).

split2_test()->
    ?assertEqual([<< >>], ts_utils:split(<< "">>, <<";">>)).

myset_env()->
    myset_env(0).
myset_env(V)->
    application:set_env(stdlib,file_server_timeout,30000),
    application:set_env(stdlib,debug_level,V),
    application:set_env(stdlib,thinktime_override,"false"),
    application:set_env(stdlib,thinktime_random,"false").
