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


