%%%-------------------------------------------------------------------
%%% File    : ts_test_recorder.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 20 Mar 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_config).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("xmerl.hrl").
-include("ts_http.hrl").

test()->
    ok.

popularity_test() ->
    ?assertError({"can't mix probabilites and weights",10,10}, ts_config:get_popularity(10,10,undefined,100)),
    ?assertError({"can't use probability when using weight"}, ts_config:get_popularity(10,-1,true,100)),
    ?assertError({"can't use weights when using probabilities"}, ts_config:get_popularity(-1,10,false,100)),
    ?assertEqual({10,false,110}, ts_config:get_popularity(10,-1,false,100)),
    ?assertEqual({10,true,110}, ts_config:get_popularity(-1,10,true,100)),
    ?assertEqual({30,false,60}, ts_config:get_popularity(30,-1,false,30)),
    ?assertError({"must set weight or probability in session"} , ts_config:get_popularity(-1,-1,undefined,100)),
    ?assertError({"can't mix probabilites and weights",0,0}, ts_config:get_popularity(0,0,true,100)),
    ?assertError({"can't mix probabilites and weights",0,0}, ts_config:get_popularity(0,0,false,100)),
    ?assertEqual({0,true,100}, ts_config:get_popularity(-1,0,true,100)),
    ?assertEqual({0,false,100}, ts_config:get_popularity(0,-1,false,100)).

read_config_http_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/http_simple.xml",".")).
read_config_http2_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/http_distributed.xml",".")).
read_config_pgsql_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./examples/pgsql.xml",".")).
read_config_jabber_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./examples/jabber.xml",".")).

read_config_jabber_muc_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./examples/jabber_muc.xml",".")).

read_config_xmpp_muc_test() ->
    myset_env(),
    ts_user_server:start([]),
    ?assertMatch({ok, Config}, ts_config:read("./src/test/xmpp-muc.xml",".")).

config_get_session_test() ->
    myset_env(0),
    ts_user_server:start([]),
    ts_config_server:start_link(["/tmp"]),
    ok = ts_config_server:read_config("./examples/http_setdynvars.xml"),
    {ok, Session=#session{userid=1,dump=full} }  = ts_config_server:get_next_session({"localhost",1}),
    ?assertEqual(1, Session#session.id).

config_get_session_size_test() ->
    myset_env(),
    {ok, Session=#session{userid=2} }  = ts_config_server:get_next_session({"localhost",1}),
    ?assertEqual(13, Session#session.size).


read_config_badpop_test() ->
    myset_env(),
    ts_user_server:start([]),
    {ok, Config} = ts_config:read("./src/test/badpop.xml","."),
    ?assertMatch({error,{bad_sum,_,_}}, ts_config_server:check_config( ts_config_server:compute_popularities(Config))).


read_config_thinkfirst_test() ->
    myset_env(),
    ?assertMatch({ok, Config}, ts_config:read("./src/test/thinkfirst.xml",".")).


config_minmax_test() ->
    myset_env(),
    {ok, Session=#session{userid=3} }  = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    ?assertMatch({thinktime,{range,2000,4000}}, ts_config_server:get_req(Id,7)).

config_minmax2_test() ->
    myset_env(),
    {ok, Session=#session{userid=4} }  = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    {thinktime, Req} = ts_config_server:get_req(Id,7),
    Think=ts_client:set_thinktime(Req),
    Resp = receive
         Data-> Data
    end,
    ?assertMatch({timeout,_,end_thinktime}, Resp).

config_thinktime_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks.xml"),
    {ok, Session=#session{userid=5} }  = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    {thinktime, Req=2000} = ts_config_server:get_req(Id,5),
    {thinktime, 2000} = ts_config_server:get_req(Id,7),
    Think=ts_client:set_thinktime(Req),
    Resp = receive
         Data-> Data
    end,
    ?assertMatch({timeout,_,end_thinktime}, Resp).


config_thinktime2_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks2.xml"),
    {ok, Session=#session{userid=6} }  = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    {thinktime, Req} = ts_config_server:get_req(Id,5),
    Ref=ts_client:set_thinktime(Req),
    receive
        {timeout,Ref2,end_thinktime} -> ok
    end,
    random:seed(), % reinit seed for others tests
    ?assertMatch({random,1000}, Req).

read_config_tag_noexclusion_test() ->
    %% no exclusion all request will be played
    myset_env(),
    ok = ts_config_server:read_config("./examples/http_tag.xml"),
    {ok, Session=#session{userid=7} } = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    ReqRef = #http_request{url="/img/excluded.png"},
    {ts_request,parse,false,[],[],Req,_,_,_,_} = ts_config_server:get_req(Id,2),
    ?assertEqual(ReqRef#http_request.url, Req#http_request.url).

read_config_tag_one_test() ->
    %% one tag defined
    %% exclude urls tagged as 'landing'
    myset_env(),
    application:set_env(stdlib,exclude_tag,"landing"),
    ok = ts_config_server:read_config("./examples/http_tag.xml"),
    {ok, Session=#session{userid=8} } = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    ReqRef = #http_request{url="/img/excluded.gif"},
    {ts_request,parse,false,[],[],Req,_,_,_,_} = ts_config_server:get_req(Id,2),
    ?assertEqual(ReqRef#http_request.url, Req#http_request.url).

read_config_tag_two_test() ->
    %% two tag defined
    %% exclude urls tagged as 'landing' and 'gif'
    myset_env(),
    application:set_env(stdlib,exclude_tag,"gif,landing"),
    ok = ts_config_server:read_config("./examples/http_tag.xml"),
    {ok, Session=#session{userid=9} } = ts_config_server:get_next_session({"localhost",1}),
    Id = Session#session.id,
    ReqRef = #http_request{url="/img/not-excluded.png"},
    {ts_request,parse,false,[],[],Req,_,_,_,_} = ts_config_server:get_req(Id,2),
    ?assertEqual(ReqRef#http_request.url, Req#http_request.url).

config_arrivalrate_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks.xml"),
    {ok, {[Phase1,Phase2, Phase3],_,_} }  = ts_config_server:get_client_config("localhost"),
    RealDur = 10 * 60 * 1000,
    RealNU  = 1200,
    RealIntensity  = 2 / 1000,
    ?assertEqual({RealIntensity,RealNU,RealDur}, Phase1),
    ?assertEqual({RealIntensity/60, RealNU div 60,RealDur}, Phase2),
    ?assertEqual({RealIntensity/3600,12,RealDur*36}, Phase3).

config_interarrival_test() ->
    myset_env(),
    ok = ts_config_server:read_config("./examples/thinks2.xml"),
    {ok, {[Phase1,Phase2, Phase3],_,_} }  = ts_config_server:get_client_config("localhost"),
    RealDur = 10 * 60 * 1000,
    RealNU  = 1200,
    RealIntensity  = 2 / 1000,
    ?assertEqual({RealIntensity,RealNU,RealDur}, Phase1),
    ?assertEqual({RealIntensity/60,RealNU div 60,RealDur}, Phase2),
    ?assertEqual({RealIntensity/3600,12,RealDur*36}, Phase3).

read_config_maxusers_test() ->
    read_config_maxusers({5,15},10,"./src/test/thinkfirst.xml").

read_config_maxusers({MaxNumber1,MaxNumber2},Clients,File) ->
    myset_env(),
    C=lists:map(fun(A)->"client"++integer_to_list(A) end, lists:seq(1,Clients)),
    ts_config_server:read_config("./src/test/thinkfirst.xml"),
    {M1,M2} = lists:unzip(lists:map(fun(X)->
                          {ok,{[{_,Max,_},{_,Max2,_}],_,_}} = ts_config_server:get_client_config(X),
                          {Max,Max2}
                  end,  C)),
    [Head1|_]=M1,
    [Head2|_]=M2,
    ?assertEqual(1, Head1),
    ?assertEqual(1, Head2),
    ?assert(lists:min(M1) >= 0),
    ?assert(lists:min(M2) >= 0),
    ?assertEqual(lists:sum(M1), MaxNumber1),
    ?assertEqual(lists:sum(M2), MaxNumber2).

read_config_static_test() ->
    myset_env(),
    C=lists:map(fun(A)->"client"++integer_to_list(A) end, lists:seq(1,10)),
    M = lists:map(fun(X)->
                          {ok,Res,_} = ts_config_server:get_client_config(static,X),
                          ?LOGF("X: ~p~n",[length(Res)],?ERR),
                          length(Res)
                  end,  C),
    ?assertEqual(lists:sum(M) , 5).

cport_list_node_test() ->
    List=['tsung1@toto',
          'tsung3@titi',
          'tsung2@toto',
          'tsung7@titi',
          'tsung6@toto',
          'tsung4@tutu'],
    Rep =  ts_config_server:get_one_node_per_host(List),
    ?assertEqual(['tsung1@toto', 'tsung3@titi', 'tsung4@tutu'], lists:sort(Rep)).


ifalias_test() ->
    Res=ts_ip_scan:get_intf_aliases("lo"),
    ?assertEqual([{127,0,0,1}],Res).

ifalias2_test() ->
    {ok, L}=ts_utils:file_to_list("src/test/ifcfg.out"),
    Out=ts_ip_scan:get_intf_aliases(L,"eth0",[],[]),
    Res=lists:foldl(fun(A,L) -> [{192,168,76,A}|L] end, [],lists:seq(183,190)),
    ?assertEqual(Out,Res).

ifalias_ip_test() ->
    {ok, L}=ts_utils:file_to_list("src/test/ipcfg.out"),
    Out=ts_ip_scan:get_ip_aliases(L,[]),
    Res=lists:foldl(fun(A,L) -> [{192,12,0,A}|L] end, [],lists:seq(1,12)),
    ?assertEqual(Out,Res).

encode_test() ->
    Encoded="ts_encoded_47myfilepath_47toto_47titi_58sdfsdf_45sdfsdf_44aa_47",
    Str="/myfilepath/toto/titi:sdfsdf-sdfsdf,aa/",
    ?assertEqual(Encoded,ts_config_server:encode_filename(Str)).

decode_test() ->
    Encoded="ts_encoded_47myfilepath_47toto_47titi_58sdfsdf_45sdfsdf_44aa_47",
    Str="/myfilepath/toto/titi:sdfsdf-sdfsdf,aa/",
    ?assertEqual(Str,ts_config_server:decode_filename(Encoded)).

concat_atoms_test() ->
    ?assertEqual('helloworld', ts_utils:concat_atoms(['hello','world'])).


int_or_string_test() ->
     ?assertEqual(123, ts_config:getAttr(integer_or_string,[#xmlAttribute{name=to,value="123"}],to)).
int_or_string2_test() ->
    ?assertEqual("%%_toto%%", ts_config:getAttr(integer_or_string,[#xmlAttribute{name=to,value="%%_toto%%"}],to)).
int_test() ->
    ?assertEqual(100, ts_config:getAttr(integer,[#xmlAttribute{name=to,value="100"}],to)).

launcher_empty_test() ->
    Intensity=10,
    Users=2,
    Duration=25,
    Res=ts_launcher:wait_static({static,0},#launcher{nusers=0,phase_duration=300,phases=[{Intensity,Users,Duration}]}),
    ?assertMatch({next_state,launcher,#launcher{phases = [],
                                                nusers = Users,
                                                phase_nusers = Users,
                                                phase_duration=Duration,
                                                phase_start = _,
                                                intensity = Intensity},_},Res).


wildcard_test() ->
    Names = ["foo1", "foo2", "bar", "barfoo", "foobar", "foo", "fof","glop"],
    ?assertEqual(["foo1", "foo2", "foobar", "foo"], ts_utils:wildcard("foo*",Names)),
    ?assertEqual(["foo1", "foo2"], ts_utils:wildcard("foo?",Names)),
    ?assertEqual(["foobar"], ts_utils:wildcard("foo*r",Names)).

myset_env()->
    myset_env(0).
myset_env(Level)->
    catch  ts_user_server_sup:start_link() ,
    application:set_env(stdlib,debug_level,Level),
    application:set_env(stdlib,warm_time,1000),
    application:set_env(stdlib,thinktime_value,"5"),
    application:set_env(stdlib,thinktime_override,"false"),
    application:set_env(stdlib,thinktime_random,"false"),
    application:set_env(stdlib,exclude_tag,"").
