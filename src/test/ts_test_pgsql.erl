%%%-------------------------------------------------------------------
%%% File    : ts_test_pgsql.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created : 10 Apr 2008 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_pgsql).

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_pgsql.hrl").
-include("ts_recorder.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(PARSEBIN,<< 115,99,117,49,0,100,101,99,108,97,
                    114,101,32,115,99,117,49,32,99,117,114,115,111,
                    114,32,119,105,116,104,32,104,111,108,100,32,102,
                    111,114,32,115,101,108,101,99,116,32,98,114,110,
                    95,99,100,44,32,112,114,101,118,95,112,114,95,
                    100,116,44,32,99,117,114,114,95,112,114,95,100,
                    116,44,32,110,101,120,116,95,112,114,95,100,116,
                    44,32,98,114,110,95,110,109,44,32,98,114,110,95,
                    97,100,100,114,49,44,32,98,114,110,95,97,100,100,
                    114,50,44,32,98,114,110,95,97,100,100,114,51,44,
                    32,99,111,109,112,95,110,109,44,32,99,97,115,104,
                    95,97,99,44,32,105,98,116,95,103,114,112,95,99,
                    100,44,32,98,97,110,107,95,99,100,44,32,108,111,
                    103,95,112,97,116,104,44,32,99,111,95,98,114,110,
                    95,99,100,32,102,114,111,109,32,32,32,98,114,110,
                    32,32,119,104,101,114,101,32,98,114,110,46,98,
                    114,110,95,99,100,32,61,32,36,49,0,0,1,0,0,4,18 >>).

test()->
    ok.


utils_md5_test()->
    myset_env(),
    Password="sesame",
    User="benchmd5",
     Salt= << 54,195,212,197 >>,
     Hash= list_to_binary(["md5967c89f451d1d504a1f02fc69fb65cb5",0]),
    PacketSize= 4+size(Hash),
    Bin= <<$p,PacketSize:32/integer, Hash/binary>>,
    ?assertMatch(Bin,  pgsql_proto:encode_message(pass_md5, {User,Password,Salt} ) ).

extended_test()->
    Data= << 80,0,0,0,75,115,99,117,49,0,100,101,99,108,97,114,101,32,115,99,117,49,32,99,117,114,
             115,111,114,32,119,105,116,104,32,104,111,108,100,32,102,111,114,32,115,101,108,101,
             99,116,32,67,79,85,78,84,40,42,41,32,102,114,111,109,32,32,32,98,114,46,97,104,32,0,0,
             0,83,0,0,0,4 >>,
    Result=ts_proxy_pgsql:process_data(#proxy{},Data),
    ?assertMatch(#proxy{}, Result).

extended2_test()->
    Data = <<66,0,0,0,28, 0, 115,99,117,49,0, 0,1, 0,0, 0,1, 0,0,0,4, 78,68,83,66,
             0,1,0,0,
             68,0,0,0,6,80,0,69,0,0,0,9,0,0,0,0,0,83,0,0,0,4>>,
    Result=ts_proxy_pgsql:process_data(#proxy{},Data),
    ?assertMatch(#proxy{}, Result).

extended3_test()->
    Data = <<0, 99,117,51,0, 0,10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,10,
             0,0,0,26, 50,48,48,56,45,49,50,45,48,53,32,48,57,58,49,57,58,48,48,46,48,48,48,48,48,48,
             0,0,0,26, 50,48,49,49,45,48,56,45,50,51,32,48,56,58,52,55,58,48,48,46,48,48,48,48,48,48,
             0,0,0,10, 49,51,52,52,51,55,32,32,32,32,
             0,0,0,1,  69,
             0,0,0,5,  75,78,32,32,32,
             0,0,0,35, 75,79,78,84,69,78,65,32,78,65,83,73,79,78,65,76,32,66,72,68,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
             0,0,0,1,  89,
             0,0,0,1,  89,
             255,255,255,255,
             0,0,0,5,  75,78,32,32,32,
             0,1, 0,0 >>,
    Result=ts_proxy_pgsql:decode_packet($B,Data),
    Portal = <<>>,
    Statement = <<"cu3">>,
    Params= [<<"2008-12-05 09:19:00.000000">>,
                                    <<"2011-08-23 08:47:00.000000">>,
                                    <<"134437    ">>,
                                    <<"E">>,
                                    <<"KN   ">>,
                                    <<"KONTENA NASIONAL BHD               ">>,
                                    <<"Y">>,
                                    <<"Y">>,
                                    'null',
                                    <<"KN   ">>],
    Bind = {bind, {Portal, Statement, Params , auto, [text]}},
    ?assertEqual(Bind, Result).

extended_parse_test()->
    Prep   = <<"scu1">>,
    Query  = <<"declare scu1 cursor with hold for select brn_cd, prev_pr_dt, curr_pr_dt, next_pr_dt, brn_nm, brn_addr1, brn_addr2, brn_addr3, comp_nm, cash_ac, ibt_grp_cd, bank_cd, log_path, co_brn_cd from   brn  where brn.brn_cd = $1">>,
    Result = ts_proxy_pgsql:decode_packet($P,?PARSEBIN),
    ?assertMatch({parse,{Prep, Query,[1042]}}, Result).
    %% {ok,Dev}=file:open("/tmp/toto.erl.log",[write]),
    %% State=#state_rec{logfd=Dev},
    %% Rec = #pgsql_request{type=parse, parameters=[1042], name_prepared=Prep, equery=Query},
    %% ?assertMatch({ok,State}, ts_proxy_pgsql:record_request(State,Rec)).

encode_parse_test()->
    Prep   = <<"scu1">>,
    Query  = <<"declare scu1 cursor with hold for select brn_cd, prev_pr_dt, curr_pr_dt, next_pr_dt, brn_nm, brn_addr1, brn_addr2, brn_addr3, comp_nm, cash_ac, ibt_grp_cd, bank_cd, log_path, co_brn_cd from   brn  where brn.brn_cd = $1">>,
    Bin=?PARSEBIN,
    Res= << 80,0,0,0,234,Bin/binary>>,
    Rep=pgsql_proto:encode_message(parse,{Prep,Query,[1042]}),
    ?assertEqual(Res,Rep).

encode_parse2_test()->
    Rep=pgsql_proto:encode_message(parse,{<< >>,<< >>,[]}),
    ?assertEqual( << 80,0,0,0,8,0,0,0,0 >> ,Rep).


subst_parameters_test()->
    myset_env(),
    Proto=#pgsql_session{},
    DynVars=ts_dynvars:new(param,"42"),
    Params=["%%_param%%","1"],
    Req=ts_pgsql:add_dynparams(true,{DynVars,Proto},
                               #pgsql_request{type=bind,name_portal="",name_prepared="P0_10", formats=none,formats_results=[text],parameters=Params},
                               {"pgsql.org",5432,gen_tcp}),
    Str=[66,0,0,0,30,0,80,48,95,49,48,0,0,0,0,2,0,0,0,2,52,50,0,0,0,1,49,0,1,0,0],
    {Res,_}=ts_pgsql:get_message(Req,#state_rcv{}),
   ?assertEqual(Str, binary_to_list(Res)).

subst_parameters2_test()->
    myset_env(),
    Proto=#pgsql_session{},
    Params=[42,1],
    Req=ts_pgsql:add_dynparams(true,{[], Proto},
                               #pgsql_request{type=bind,name_portal="",name_prepared="P0_10", formats=none,formats_results=[text],parameters=Params},
                               {"pgsql.org",5432,gen_tcp}),
    Str=[66,0,0,0,30,0,80,48,95,49,48,0,0,0,0,2,0,0,0,2,52,50,0,0,0,1,49,0,1,0,0],
    {Res,_}=ts_pgsql:get_message(Req,#state_rcv{}),
   ?assertEqual(Str, binary_to_list(Res)).



myset_env()->
    myset_env(0).
myset_env(Val)->
    application:set_env(stdlib,debug_level,Val).

