%%%-------------------------------------------------------------------
%%% File    : ts_test_recorder.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 June 2007 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_proxy).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_http.hrl").
-include("ts_recorder.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

relative_url_test()->
    myset_env(),
     String= "foo http://www.glop.com/bar/foo.html foo bar",
     AbsURI="http://www.glop.com/bar/foo.html?toto=bar",
     RelURL="/bar/foo.html?toto=bar",
    ?assertMatch({ok,"foo /bar/foo.html foo bar"},
                 ts_proxy_http:relative_url(false,String,AbsURI,RelURL)).

%% parse_http_test()->
%%     myset_env(),
%%     ?assertMatch({ok,""},
%%                  ts_proxy_http:parse(State,ClientSocket,Socket,Data)).

myset_env()->
    application:set_env(stdlib,debug_level,0).

