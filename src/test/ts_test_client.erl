%%%-------------------------------------------------------------------
%%% File    : ts_test_client.erl
%%% Author  : Rodolphe Quiédeville <rodolphe@quiedeville.org>
%%% Description :
%%%
%%% Created : 7 Oct 2013 by Rodolphe Quiédeville <rodolphe@quiedeville.org>
%%%-------------------------------------------------------------------
-module(ts_test_client).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

eq_test() ->
    ?assertEqual(false, ts_client:rel('eq',5,4)),
    ?assertEqual(true, ts_client:rel('eq',4,4)).

eq_float_test() ->
    ?assertEqual(false, ts_client:rel('eq',5.0,4.0)),
    ?assertEqual(true, ts_client:rel('eq',4.0,4.0)).

eq_mix_test() ->
    ?assertEqual(false, ts_client:rel('eq',5.0,4.0)),
    ?assertEqual(true, ts_client:rel('eq',4.0,4.0)).

eq_alist_test() ->
    ?assertEqual(false, ts_client:rel('eq',"5",4)),
    ?assertEqual(true, ts_client:rel('eq',"4",4)).

eq_blist_test() ->
    ?assertEqual(false, ts_client:rel('eq',<<"5">>,"4")),
    ?assertEqual(true, ts_client:rel('eq',<<"4">>,"4")).

eq_binary_test() ->
    ?assertEqual(false, ts_client:rel('eq',<<"5">>,"4")),
    ?assertEqual(true, ts_client:rel('eq',<<"4">>,"4")),
    ?assertEqual(false, ts_client:rel('eq',"5",<<"4">>)),
    ?assertEqual(true, ts_client:rel('eq',"4",<<"4">>)).

eq_aatom_test() ->
    ?assertEqual(false, ts_client:rel('eq', foo, <<"foobar">>)),
    ?assertEqual(true, ts_client:rel('eq', foo, <<"foo">>)).

eq_batom_test() ->
    ?assertEqual(false, ts_client:rel('eq',<<"barfoo">>, foobar)),
    ?assertEqual(true, ts_client:rel('eq',<<"foobar">>, foobar)).

neq_int_test() ->
    ?assertEqual(true, ts_client:rel('neq',5,4)),
    ?assertEqual(false, ts_client:rel('neq',4,4)).

neq_list_test() ->
    ?assertEqual(true, ts_client:rel('neq',"e","4")),
    ?assertEqual(false, ts_client:rel('neq',"4","4")).

neq_binary_test() ->
    ?assertEqual(true, ts_client:rel('neq',<<"ed">>,<<"4">>)),
    ?assertEqual(false, ts_client:rel('neq',<<"4">>,<<"4">>)).

need_jump_while_test()->
    ?assertEqual(true, ts_client:need_jump('while',true)),
    ?assertEqual(false, ts_client:need_jump('while',false)).

need_jump_until_test()->
    ?assertEqual(false, ts_client:need_jump('until',true)),
    ?assertEqual(true, ts_client:need_jump('until',false)).

need_jump_if_test()->
    ?assertEqual(false, ts_client:need_jump('if',true)),
    ?assertEqual(true, ts_client:need_jump('if',false)).

binary_to_num_int_test()->
    ?assertEqual(100, ts_client:binary_to_num(<<"100">>)).

binary_to_num_float_test()->
    ?assertEqual(100.1, ts_client:binary_to_num(<<"100.1">>)).

binary_to_num_float_neg_test()->
    ?assertEqual(-3.14, ts_client:binary_to_num(<<"-3.14">>)).

gt_int_test()->
    ?assertEqual(true, ts_client:rel('gt',<<"2">>,<<"3">>)),
    ?assertEqual(true, ts_client:rel('gt',<<"-2">>,<<"-1">>)),
    ?assertEqual(false, ts_client:rel('gt',<<"2">>,<<"2">>)),
    ?assertEqual(false, ts_client:rel('gt',<<"22">>,<<"3">>)).

gt_float_test()->
    ?assertEqual(true, ts_client:rel('gt',<<"2.0">>,<<"3.1">>)),
    ?assertEqual(false, ts_client:rel('gt',<<"2.0">>,<<"2.0">>)),
    ?assertEqual(false, ts_client:rel('gt',<<"22.1">>,<<"3.0">>)).

lt_int_test()->
    ?assertEqual(false, ts_client:rel('lt',<<"2">>,<<"3">>)),
    ?assertEqual(false, ts_client:rel('lt',<<"2">>,<<"2">>)),
    ?assertEqual(true, ts_client:rel('lt',<<"22">>,<<"3">>)).

lt_float_test()->
    ?assertEqual(false, ts_client:rel('lt',<<"2.0">>,<<"3.1">>)),
    ?assertEqual(false, ts_client:rel('lt',<<"2.0">>,<<"2.0">>)),
    ?assertEqual(true, ts_client:rel('lt',<<"22.1">>,<<"3.0">>)).

gte_int_test()->
    ?assertEqual(true, ts_client:rel('gte',<<"2">>,<<"3">>)),
    ?assertEqual(true, ts_client:rel('gte',<<"2">>,<<"2">>)),
    ?assertEqual(false, ts_client:rel('gte',<<"22">>,<<"3">>)).

gte_float_test()->
    ?assertEqual(true, ts_client:rel('gte',<<"2.0">>,<<"3.1">>)),
    ?assertEqual(true, ts_client:rel('gte',<<"-2.0">>,<<"-1.31">>)),
    ?assertEqual(true, ts_client:rel('gte',<<"2.0">>,<<"2.0">>)),
    ?assertEqual(false, ts_client:rel('gte',<<"22.1">>,<<"3.0">>)).

lte_int_test()->
    ?assertEqual(false, ts_client:rel('lte',<<"2">>,<<"3">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"-2">>,<<"-3">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"2">>,<<"2">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"22">>,<<"3">>)).

lte_float_test()->
    ?assertEqual(false, ts_client:rel('lte',<<"2.0">>,<<"3.1">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"-2.0">>,<<"-3.1">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"2.0">>,<<"2.0">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"-2.0">>,<<"-2.0">>)),
    ?assertEqual(true, ts_client:rel('lte',<<"22.1">>,<<"3.0">>)).
