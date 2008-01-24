%%%-------------------------------------------------------------------
%%% File    : ts_test_search.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : unit tests for ts_search module
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(ts_test_search).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ts_profile.hrl").
-include_lib("ts_config.hrl").


-define(FORMDATA,"<input type=\"hidden\" name=\"jsf_tree_64\" id=\"jsf_tree_64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\">").

test()->
    ok.
parse_dyn_var_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{'jsf_tree_64',"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{'jsf_tree_64', Regexp} ],list_to_binary(Data))).

parse_dyn_var2_test() ->
    myset_env(),
    Data="<input type=\"hidden\" name=\"tree64\" id=\"tree64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\">",
    StrName="tree64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{tree64,"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA"}], ts_search:parse_dynvar([{tree64, Regexp }],list_to_binary(Data))).

parse_dyn_var3_test() ->
    myset_env(),
    Data="<hidden name=\"random\" value=\"42\"></form>",
    StrName="random",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{random,"42"}], ts_search:parse_dynvar([{random, Regexp }],list_to_binary(Data))).

parse_dyn_var4_test() ->
    myset_env(),
    Data="<hidden name='random' value='42'></form>",
    StrName="random",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    ?assertMatch([{random,"42"}], ts_search:parse_dynvar([{random, Regexp }],list_to_binary(Data))).

parse_subst1_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_REGEXP_DYNVAR_BEGIN++ StrName ++?DEF_REGEXP_DYNVAR_END,%'
    [{Name,Value}] = ts_search:parse_dynvar([{'jsf_tree_64', Regexp }],list_to_binary(Data)),
    ?assertMatch("H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA", ts_search:subst("%%_jsf_tree_64%%",[{Name,Value}])).

myset_env()->
    application:set_env(stdlib,debug_level,0).
