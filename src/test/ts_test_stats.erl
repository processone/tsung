%%%-------------------------------------------------------------------
%%% File    : ts_test_stats.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 12 Jul 2011 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_stats).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ts_profile.hrl").
-include_lib("ts_config.hrl").

set_dynvar_random_test() ->
    Min=1,
    Max=10,
    R=lists:map(fun(_)->ts_stats:uniform(Min,Max) end, lists:seq(1,1000)),
    ?assertEqual(Max,lists:max(R)),
    ?assertEqual(Min,lists:min(R)).
