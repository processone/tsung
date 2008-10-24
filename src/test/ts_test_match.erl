%%%-------------------------------------------------------------------
%%% File    : ts_test_search.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : unit tests for ts_search module
%%%
%%% $Id: ts_test_search.erl 904 2008-10-08 08:16:38Z nniclausse $
%%%-------------------------------------------------------------------
-module(ts_test_match).

-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("ts_profile.hrl").
-include_lib("ts_config.hrl").

-define(MAX_COUNT,42).
-define(COUNT,5).

test()->
    ok.
match_abort_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=match}],Data, {?COUNT,?MAX_COUNT})).

match_abort_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(0, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=match}],Data, {?COUNT,?MAX_COUNT})).

nomatch_abort_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(0, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

nomatch_abort_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=abort, 'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

nomatch_continue_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

nomatch_continue_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

match_continue_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=match}],Data, {?COUNT,?MAX_COUNT})).

match_continue_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=continue, 'when'=match}],Data, {?COUNT,?MAX_COUNT})).

nomatch_loop_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT+1, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1,'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

nomatch_loop_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1,'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

match_loop_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1,'when'=match}],Data, {?COUNT,?MAX_COUNT})).

match_loop_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT+1, ts_search:match([#match{regexp="Erreur", do=loop, max_loop=?COUNT, loop_back=0, sleep_loop=1, 'when'=match}],Data, {?COUNT,?MAX_COUNT})).


nomatch_restart_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?MAX_COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT,'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

nomatch_restart_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT,'when'=nomatch}],Data, {?COUNT,?MAX_COUNT})).

match_restart_ok_test() ->
    myset_env(),
    Data="C'est n'est pas une chaine de caractere",
    ?assertMatch(?COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT,'when'=match}],Data, {?COUNT,?MAX_COUNT})).

match_restart_nok_test() ->
    myset_env(),
    Data="Ceci est une Erreur",
    ?assertMatch(?MAX_COUNT, ts_search:match([#match{regexp="Erreur", do=restart, max_restart=?COUNT, 'when'=match}],Data, {?COUNT,?MAX_COUNT})).

myset_env()->
    myset_env(0).
myset_env(Level)->
    application:set_env(stdlib,debug_level,Level).

