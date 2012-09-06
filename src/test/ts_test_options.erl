%% ts_test_rate.erl
%% @author Nicolas Niclausse
%% @doc Test for options like rate limiting feature
%% created on 2011-03-14
-module(ts_test_options).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


test() ->
    ok.

rate1_test() ->
    R=10000 div 1000,
    B=15000,
    T0={0,0,0},
    T1={0,10,0},
    P1=14000,
    Res = ts_client:token_bucket(R,B,0,T0,P1,T1,false),
    ?assertEqual({1000,0},Res).

rate2_test() ->
    R=10000 div 1000,
    B=15000,
    T0={0,0,0},
    T1={0,10,0},
    T2={0,11,0},
    P1=14000,
    P2=14000,
    {S2,0} = ts_client:token_bucket(R,B,0,T0,P1,T1,false),
    Res = ts_client:token_bucket(R,B,S2,T1,P2,T2,false),
    ?assertEqual({0,300},Res).

