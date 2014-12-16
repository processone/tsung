%% ==========================================================================
%% File    : ts_test_utils.erl
%% Author  : Rodolphe Quiédeville <rodolphe@quiedeville.org>
%% Description :
%%
%% Created : 17 Oct 2013 by Rodolphe Quiédeville
%% ==========================================================================
-module(ts_test_utils).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

add_time_test() ->
    ?assertEqual({1382,29907,875287}, ts_utils:add_time({1382,29904,875287},3)),
    %% have to check this second test, maybe 1383 instead of 1392
    Old = {1382,999999,875287},
    New = ts_utils:add_time(Old,3),
    ?assertEqual(3*1000*1000, timer:now_diff(New, Old)).

node_to_hostname_test() ->
    ?assertEqual({ok, "foo"}, ts_utils:node_to_hostname('bar@foo')).

to_lower_test()->
    ?assertEqual("foo",ts_utils:to_lower("Foo")),
    ?assertEqual("foo",ts_utils:to_lower("FOO")).

mkey1search_atom_test()->
    Data = [{foo,bar},{foo,caps},{bar,foo},{foo,caps}],
    ?assertEqual([bar,caps,caps],ts_utils:mkey1search(Data,foo)).

mkey1search_empty_test()->
    %% the key does not exists
    Data = [{foo,bar},{foo,caps},{bar,foo}],
    ?assertEqual(undefined,ts_utils:mkey1search(Data,foobar)).

mkey1search_string_test()->
    Data = [{"foo","bar"},{"foo","caps"},{"bar","foo"}],
    ?assertEqual(["bar","caps"],ts_utils:mkey1search(Data,"foo")).

datestr_test()->
    ?assertEqual(["2013","10","17",45,"19","41"],ts_utils:datestr({{2013,10,17},{19,41,29}})).

export_text_test()->
    ?assertEqual("foo",ts_utils:export_text("foo")).

export_text_bin_test()->
    ?assertEqual("foo",ts_utils:export_text(<<"foo">>)).

export_text_escape_test()->
    ?assertEqual("fo&amp;o",ts_utils:export_text(<<"fo&o">>)),
    ?assertEqual("A &gt; B",ts_utils:export_text(<<"A > B">>)),
    ?assertEqual("&apos;B&apos;",ts_utils:export_text("'B'")),
    ?assertEqual("&quot;B&quot;",ts_utils:export_text("\"B\"")),
    ?assertEqual("&lt; A",ts_utils:export_text(<<"< A">>)).


pmap_test()->
    F = fun(X) ->X + 1 end,
    L = [1,2,4,12,6,2,7,9,2,10],
    Res = lists:map(F,L),
    ResP = ts_utils:pmap(F,L),
    ?assertEqual(ResP, Res).

pmapn_test()->
    F = fun(X) ->X + 1 end,
    L = [1,2,4,12,6,2,7,9,2,10],
    Res = lists:map(F,L),
    ResP = ts_utils:pmap(F,L,3),
    ?assertEqual(ResP, Res),
    ResP2 = ts_utils:pmap(F,L,8),
    ?assertEqual(ResP2, Res).

pmapn_big_test()->
    F = fun(X) ->X + 1 end,
    L = lists:duplicate(1000, 42),
    Res = lists:map(F,L),
    ResP = ts_utils:pmap(F,L,10),
    ?assertEqual(ResP, Res).

filtermap_test()->
    Fun = fun(X) -> case X > 1 of 
                        true -> {true, X + 1}; 
                        _ -> false 
                    end 
          end,
    ResP = ts_utils:filtermap(Fun, [1,2,3]),
    Res = [3, 4],
    ?assertEqual(ResP, Res).

