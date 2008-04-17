%%%-------------------------------------------------------------------
%%% File    : ts_test_search.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : unit tests for ts_search module
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(ts_test_search).

-compile(export_all).

-export([marketplace/1,namespace/1,sessionBucket/1, new/1]).

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

parse_extract_fun1_test() ->
    myset_env(),
    Data="/echo?symbol=%%ts_test_search:new%%",
    ?assertMatch("/echo?symbol=IBM", ts_search:subst(Data,[])).

parse_extract_fun2_test() ->
    myset_env(),
    Data="/stuff/%%ts_test_search:namespace%%/%%ts_test_search:marketplace%%/%%ts_test_search:sessionBucket%%/01/2000?keyA1=dataA1&amp;keyB1=dataB1",
    ?assertMatch("/stuff/namespace2/6/91/01/2000?keyA1=dataA1&amp;keyB1=dataB1", ts_search:subst(Data,[])).

dynvars_urandom_test() ->
    myset_env(),
    ?assertMatch(["qxvmvtglimieyhemzlxc"],ts_client:set_dynvars(urandom,{string,20},[toto],[])).

dynvars_urandom_neg_test() ->
    myset_env(),
    ?assertError(function_clause,ts_client:set_dynvars(urandom,{string,-3},[toto],[])).

dynvars_urandom2_test() ->
    myset_env(),
    ?assertMatch(["qxvmvtglimieyhemzlxc","qxvmvtglimieyhemzlxc"],ts_client:set_dynvars(urandom,{string,20},[toto,tutu],[])).

dynvars_random_test() ->
    myset_env(),
    [String] = ts_client:set_dynvars(random,{string,20},[toto],[]),
    ?assertMatch(20,length(String)).

dynvars_random2_test() ->
    myset_env(),
    [String,String2] = ts_client:set_dynvars(random,{string,20},[toto,titi],[]),
    ?assertMatch({20,20},{length(String),length(String2)}).


myset_env()->
    myset_env(0).
myset_env(Level)->
    application:set_env(stdlib,debug_level,Level).

new({Pid, DynData}) ->
    case random:uniform(3) of
        1 -> "IBM";
        2 -> "MSFT";
        3 -> "RHAT"
    end.

marketplace({Pid,DynData}) ->
    integer_to_list( random:uniform(7) ).

namespace({Pid,DynData}) ->
    "namespace" ++ integer_to_list(random:uniform(3)).

sessionBucket({Pid,DynData}) ->
    case random:uniform(96) of
        96 -> "00";
        X when X < 10  -> "0" ++ integer_to_list( X );
        X -> integer_to_list( X )
    end.
