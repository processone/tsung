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


-define(MANY,20).

-define(FORMDATA,"<input type=\"hidden\" name=\"jsf_tree_64\" id=\"jsf_tree_64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\">").



test()->
    ok.


parse_dyn_var_jsonpath_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [23,45]}",
    JSONPath = "titi[1]",
    ?assertEqual([{'myvar',45}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath2_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [23,45]}",
    JSONPath = "titi[3]",
    ?assertEqual([{'myvar',<< >>}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath3_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[?name=bar].val",
    ?assertEqual([{'myvar',42}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath4_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[?name=void].val",
    ?assertEqual([{'myvar', << >>}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath5_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"status\": \"foo\"}, {\"val\": 42, \"status\": \"OK\"}, {\"val\": 48, \"status\": \"OK\"}]}",
    JSONPath = "titi[?status=OK].val",
    ?assertEqual([{'myvar',[42,48]}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath6_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[*].val",
    ?assertEqual([{'myvar',[123,42]}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath7_test() ->
    myset_env(),
    Data = "\r\n\r\n {
    \"menu\": {
        \"id\": \"file\",
        \"value\": \"File\",
        \"popup\": {
            \"name\": \"glop\",
            \"menuitem\": [
                { \"value\": \"New\", \"onclick\": \"CreateNewDoc()\" },
                { \"value\": \"Open\", \"onclick\": \"OpenDoc()\" },
                { \"value\": \"Close\", \"onclick\": \"CloseDoc()\" }
            ]
                }
                }
        }",
    JSONPath = "menu.popup.name",
    ?assertEqual([{'myvar', << "glop" >>}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))),
    JSONPathTab = "menu.popup.menuitem[0].value",
    ?assertEqual([{'myvar', << "New" >>}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPathTab} ],list_to_binary(Data))).

parse_dyn_var_jsonpath_int_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[?val=123].name",
    ?assertEqual([{'myvar',<<"foo">>}], ts_search:parse_dynvar([{jsonpath,'myvar', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath_xmpp_test() ->
    myset_env(),
    Data="{\n  \"status\": \"terminated\",\n  \"uid\": \"944370dc04adbee1792732e01097e618af97cc27\",\n  \"updated_at\": 1282660758,\n  \"nodes\": [\n    \"suno-12\",\n    \"suno-13\"\n  ],\n  \"created_at\": 1282660398,\n  \"environment\": \"lenny-x64-big\",\n  \"result\": {\n    \"suno-13\": {\n      \"last_cmd_stdout\": \"\",\n      \"last_cmd_stderr\": \"\",\n      \"cluster\": \"suno\",\n      \"ip\": \"192.168.1.113\",\n      \"last_cmd_exit_status\": 0,\n      \"current_step\": null,\n      \"state\": \"OK\"\n    },\n    \"suno-12\": {\n      \"last_cmd_stdout\": \"\",\n      \"last_cmd_stderr\": \"\",\n      \"cluster\": \"suno\",\n      \"ip\": \"192.168.1.112\",\n      \"last_cmd_exit_status\": 0,\n      \"current_step\": null,\n      \"state\": \"OK\"\n    }\n  },\n  \"site_uid\": \"sophia\",\n  \"notifications\": [\n    \"xmpp:joe@foo.bar/tsung\"\n  ],\n  \"user_uid\": \"joe\"\n}",
    JSONPath = "nodes",
    ?assertMatch([{'nodes',[<<"suno-12">>,<<"suno-13">>]}], ts_search:parse_dynvar([{jsonpath,'nodes', JSONPath} ],list_to_binary(Data))).

parse_dyn_var_jsonpath_struct_test() ->
    myset_env(),
    Data="{\"accessToken\":{\"id\":\"78548a96-cadd-48c0-b7d6-4ff3b81f10cc\",\"lists\":[\"testlist1\"],\"token\":\"rTgdC3f7uJ/Smg3s4b9va2KW5GdPkRHtwYNgWbvwhensgOSf2/wan95VPDiXKnAAGilsZlpw/Td4bs/OPeVeYg==\",\"scope\":[\"GET_ME\",\"WRITE_ACCESS\"]},\"accessTokenSignature\":\"gWAL+zvDcQjqLmNdSwcG/TOWyta5g==\"}",
    JSONPath = "accessToken",
    ?assertMatch([{'nodes',  << "{\"id\":\"78548a96-cadd-48c0-b7d6-4ff3b81f10cc\",\"lists\":[\"testlist1\"],\"token\":\"rTgdC3f7uJ/Smg3s4b9va2KW5GdPkRHtwYNgWbvwhensgOSf2/wan95VPDiXKnAAGilsZlpw/Td4bs/OPeVeYg==\",\"scope\":[\"GET_ME\",\"WRITE_ACCESS\"]}" >> }], ts_search:parse_dynvar([{jsonpath,'nodes', JSONPath} ],list_to_binary(Data))).


parse_dyn_var_xpath_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body>"++?FORMDATA++"</body></html>",
    XPath = "//input[@name='jsf_tree_64']/@value",
    ?assertMatch([{'jsf_tree_64',[<< "H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA" >>]}], ts_search:parse_dynvar([{xpath,'jsf_tree_64', XPath} ],list_to_binary(Data))).


parse_dyn_var_xpath_with_scripttag_test() ->
    myset_env(),
    Data= "\r\n\r\n<html><head><script type=\"text/javascript\"> "
          " A = B <= C </script>"
          "</head><body>"++?FORMDATA++"</body></html>",
    XPath = "//input[@name='jsf_tree_64']/@value",
    ?assertMatch([{'jsf_tree_64', [<< "H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA" >>] }], ts_search:parse_dynvar([{xpath,'jsf_tree_64', XPath} ],list_to_binary(Data))).



parse_dyn_var_xpath2_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body><input type=\"hidden\" name=\"tree64\" id=\"tree64\" value=\"H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA\"></body></html>",
    XPath = "//input[@name='tree64']/@value",
    ?assertMatch([{tree64,[<< "H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA" >>]}], ts_search:parse_dynvar([{xpath,tree64, XPath }],list_to_binary(Data))).


parse_dyn_var_xpath3_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body><hidden name=\"random\" value=\"42\"></form></body></html>",
    XPath = "//hidden[@name='random']/@value",
    ?assertMatch([{random,[<<"42">>]}], ts_search:parse_dynvar([{xpath, random, XPath }],list_to_binary(Data))).


parse_dyn_var_xpath4_test() ->
    myset_env(),
    Data="\r\n\r\n<html><body><hidden name='random' value='42'></form></body>/<html>",
    XPath = "//hidden[@name='random']/@value",
    ?assertMatch([{random,[<<"42">>]}], ts_search:parse_dynvar([{xpath, random, XPath }],list_to_binary(Data))).


parse_dyn_var_many_re_test() ->
    myset_env(),
    {Data, Res}= setdata(?MANY,binary),
    RegexpFun = fun(A) -> {re,list_to_atom(A), ?DEF_RE_DYNVAR_BEGIN++ A ++?DEF_RE_DYNVAR_END} end,%'
    B=lists:map(fun(A)->"random"++integer_to_list(A) end, lists:seq(1,?MANY)),
    C=lists:map(RegexpFun, B),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[C,list_to_binary(Data)]),
    erlang:display([?MANY," re:", Time]),
    ?assertEqual(Res, Out).

parse_dyn_var_many_xpath_test() ->
    myset_env(),
    {Data, Res}= setdata(?MANY,binarylist),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "//input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_xpath_explicit_test() ->
    myset_env(),
    {Data, Res}= setdata(?MANY,binarylist),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "/html/body/form/input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath_explicit:", Time]),
    ?assertMatch(Res, Out).


parse_dyn_var_many_big_re_test() ->
    myset_env(),
    {Data, Res}= setdata_big(?MANY,binary),
    RegexpFun = fun(A) -> {re,list_to_atom(A), ?DEF_RE_DYNVAR_BEGIN++ A ++?DEF_RE_DYNVAR_END} end,%'
    B=lists:map(fun(A)->"random"++integer_to_list(A) end, lists:seq(1,?MANY)),
    C=lists:map(RegexpFun, B),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[C,list_to_binary(Data)]),
    erlang:display([?MANY," re_big:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_big_xpath_test() ->
    myset_env(),
    {Data, Res}= setdata_big(?MANY,binarylist),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "//input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath_big:", Time]),
    ?assertMatch(Res, Out).

parse_dyn_var_many_big_xpath_explicit_test() ->
    myset_env(),
    {Data, Res}= setdata_big(?MANY,binarylist),
    B=lists:map(fun(A)->{xpath, list_to_atom("random"++integer_to_list(A)),
                         "/html/body/form/input[@type='hidden'][@name='random"++integer_to_list(A)++"']/@value"} end, lists:seq(1,?MANY)),
    {Time, Out}=timer:tc( ts_search,parse_dynvar,[B,list_to_binary(Data)]),
    erlang:display([?MANY," xpath_explicit_big:", Time]),
    ?assertMatch(Res, Out).

setdata(N) ->
    setdata(N,list).
setdata(N,Type) ->
    {"\r\n\r\n<html><body><form>"++lists:flatmap(fun(A)->
                                           AI=integer_to_list(A),["<input type='hidden' name='random",AI,"'"," value='value",AI,"'>"] end,lists:seq(1,N))
++"</form></body>/<html>",  lists:reverse(lists:map(fun(A)->{list_to_atom("random"++integer_to_list(A)) , format_result("value"++integer_to_list(A),Type)} end, lists:seq(1,N)))}.

setdata_big(N) ->
    setdata_big(N, list).
setdata_big(N, Type) ->
    Head = "<head><title>ABCDERFDJSJS</title><script type='text/javascript'> "
           "Some javascript code</script><link rel='shortcut icon' "
           " href='favicon.ico' type='image/x-icon'></head>",
    Fields = lists:flatmap(fun(A)->
                           AI=integer_to_list(A),
                           ["<input type='hidden' name='random",AI,"'"," value='value",AI,"'>"]
                           end,lists:seq(1,N)),
    Form = "<form>" ++ Fields ++ "</form>",
    Content = "<div><p>This is a some random content</p>"
              "<ul><li>item1<li>item2</ul>"
              "<p>Some more text... not too big really...</p>"
              "<p>More text inside a paragraph element</p> "
              "</div>",
    HTML ="\r\n\r\n<html>" ++ Head ++ "<body>" ++ Content ++ Content ++ Form ++ Content ++ "</body></html>",
    {HTML,lists:reverse(lists:map(fun(A)->{list_to_atom("random"++integer_to_list(A)) , format_result("value"++integer_to_list(A),Type)} end, lists:seq(1,N)))}.


format_result(Data,binarylist) ->
    [list_to_binary(Data)];
format_result(Data,binary) ->
    list_to_binary(Data);
format_result(Data,_) ->
    Data.

parse_re_decode_test() ->
    myset_env(),
    Data= << "<input name=\"jsf_tree_64\" value=\"&apos;foo&amp;bar&apos;\">" >>,
    StrName="jsf_tree_64",
    Regexp = ?DEF_RE_DYNVAR_BEGIN++ StrName ++?DEF_RE_DYNVAR_END,%'
    [{Name,Value}] = ts_search:parse_dynvar([{re, 'jsf_tree_64', Regexp, fun ts_utils:conv_entities/1 }],Data),
    ?assertEqual("'foo&bar'", ts_search:subst("%%_jsf_tree_64%%",[{Name,Value}])).

parse_subst1_re_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_RE_DYNVAR_BEGIN++ StrName ++?DEF_RE_DYNVAR_END,%'
    [{Name,Value}] = ts_search:parse_dynvar([{re, 'jsf_tree_64', Regexp }],list_to_binary(Data)),
    ?assertMatch("H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA", ts_search:subst("%%_jsf_tree_64%%",[{Name,Value}])).

parse_subst2_re_test() ->
    myset_env(),
    Data="<HTML>
  <HEAD>
    <meta http-equiv='Content-Type' content='text/html; charset=ISO-8859-1'>
    <META NAME='author' CONTENT='Test'>
    <TITLE>4DOM version 0.10.2</TITLE>
  </HEAD>
  <BODY>
    <H1>4DOM version 0.10.2</H1>
    <H3></H3>
    <P>4Suite <A HREF='http://4Suite.org/getdoc.epy?file=4Suite/'>http://FourThought.com/4Suite<
/A>
    </P>
",
    Regexp = "<TITLE>(.*)</TITLE>",
    [{Name,Value}] = ts_search:parse_dynvar([{re, 'title', Regexp }],list_to_binary(Data)),
    ?assertMatch("4DOM version 0.10.2", ts_search:subst("%%_title%%",[{Name,Value}])).

parse_extract_fun1_test() ->
    myset_env(),
    Data="/echo?symbol=%%ts_test_search:new%%",
    ?assertMatch("/echo?symbol=MSFT", ts_search:subst(Data,[])).

parse_extract_fun2_test() ->
    myset_env(),
    Data="/stuff/%%ts_test_search:namespace%%/%%ts_test_search:marketplace%%/%%ts_test_search:sessionBucket%%/01/2000?keyA1=dataA1&amp;keyB1=dataB1",
    ?assertMatch("/stuff/namespace1/5/58/01/2000?keyA1=dataA1&amp;keyB1=dataB1", ts_search:subst(Data,[])).

parse_subst_var_fun_test() ->
    myset_env(),
    Data=?FORMDATA,
    StrName="jsf_tree_64",
    Regexp = ?DEF_RE_DYNVAR_BEGIN++ StrName ++?DEF_RE_DYNVAR_END,%'
    [{Name,Value}] = ts_search:parse_dynvar([{re, 'jsf_tree_64', Regexp }],list_to_binary(Data)),
    ?assertMatch("H4sIAAAAAAAAAK1VS2/TQBBeo+kalCKAA-MSFT", ts_search:subst("%%_jsf_tree_64%%-%%ts_test_search:new%%",[{Name,Value}])).

parse_subst_badregexp_sid_test() ->
    myset_env(),
    Data="HTTP/1.1 200 OK\r\nServer: nginx/0.7.65\r\nDate: Fri, 05 Feb 2010 08:13:29 GMT\r\nContent-Type: text/xml; charset=utf-8\r\nConnection: keep-alive\r\nContent-Length: 373\r\n\r\n<body polling=\"10\" ver=\"1.6\" secure=\"true\" wait=\"20\" requests=\"2\" hold=\"1\" sid=\"5bfd2b59-3144-4e62-993b-d05d2ae3bee9\" xmpp:version=\"1.0\" xmlns:stream=\"http://etherx.jabber.org/streams\" authid=\"b65b29eb-99c0-4afd-8f97-d6d20f4ddba2\" maxpause=\"10\" from=\"tigase-test\" inactivity=\"10\" ack=\"2995502128855\" xmlns:xmpp=\"urn:xmpp:xbosh\" xmlns=\"http://jabber.org/protocol/httpbind\"/>",
    Regexp = "sid=\".*?\"",
    [{Name,Value}] = ts_search:parse_dynvar([{re, sid, Regexp }],list_to_binary(Data)),
    ?assertEqual({sid,<<"">>},{Name,Value}).

parse_subst_regexp_sid_test() ->
    myset_env(),
    Data="HTTP/1.1 200 OK\r\nServer: nginx/0.7.65\r\nDate: Fri, 05 Feb 2010 08:13:29 GMT\r\nContent-Type: text/xml; charset=utf-8\r\nConnection: keep-alive\r\nContent-Length: 373\r\n\r\n<body polling=\"10\" ver=\"1.6\" secure=\"true\" wait=\"20\" requests=\"2\" hold=\"1\" sid=\"5bfd2b59-3144-4e62-993b-d05d2ae3bee9\" xmpp:version=\"1.0\" xmlns:stream=\"http://etherx.jabber.org/streams\" authid=\"b65b29eb-99c0-4afd-8f97-d6d20f4ddba2\" maxpause=\"10\" from=\"tigase-test\" inactivity=\"10\" ack=\"2995502128855\" xmlns:xmpp=\"urn:xmpp:xbosh\" xmlns=\"http://jabber.org/protocol/httpbind\"/>",
    Regexp = "sid=\"([^\"]*)\"",
    [{Name,Value}] = ts_search:parse_dynvar([{re, sid, Regexp }],list_to_binary(Data)),
    ?assertEqual({sid,<<"5bfd2b59-3144-4e62-993b-d05d2ae3bee9">>},{Name,Value}).


dynvars_urandom_test() ->
    myset_env(),
    ?assertMatch([<<"qxvmvtglimieyhemzlxc">>],ts_client:set_dynvars(urandom,{string,20},[toto],[],{},[])).

dynvars_urandom_neg_test() ->
    myset_env(),
    ?assertError(function_clause,ts_client:set_dynvars(urandom,{string,-3},[toto],[],{},[])).

dynvars_urandom2_test() ->
    myset_env(),
    ?assertMatch([<<"qxvmvtglimieyhemzlxc">>,<<"qxvmvtglimieyhemzlxc">>],ts_client:set_dynvars(urandom,{string,20},[toto,tutu],[],{},[])).

dynvars_random_test() ->
    myset_env(),
    [String] = ts_client:set_dynvars(random,{string,20},[toto],[],{},[]),
    ?assertMatch(20,length(binary_to_list(String))).

dynvars_random2_test() ->
    myset_env(),
    [String,String2] = ts_client:set_dynvars(random,{string,20},[toto,titi],[],{},[]),
    ?assertMatch({20,20},{length(binary_to_list(String)),length(binary_to_list(String2))}).

dynvars_jsonpath_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": 123, \"name\": \"foo\"}, {\"val\": 42, \"name\": \"bar\"}]}",
    JSONPath = "titi[?name=bar].val",
    Dynvars=ts_dynvars:new(data,Data),
    ?assertEqual(42,ts_client:set_dynvars(jsonpath,{JSONPath,data},[toto],Dynvars,{},[])).

dynvars_jsonpath2_test() ->
    myset_env(),
    Data="{\"accessToken\":{\"id\":\"78548a96-cadd-48c0-b7d6-4ff3b81f10cc\",\"lists\":[\"testlist1\"],\"token\":\"rTgdC3f7uJ/Smg3s4b9va2KW5GdPkRHtwYNgWbvwhensgOSf2/wan95VPDiXKnAAGilsZlpw/Td4bs/OPeVeYg==\",\"scope\":[\"GET_ME\",\"WRITE_ACCESS\"]},\"accessTokenSignature\":\"gWAL+zvDcQjqLmNdSwcG/TOWyta5g==\"}",
    JSONPath = "accessToken",
    JSONPath2 = "accessTokenSignature",
    Dynvars=ts_dynvars:new(data,Data),
    Res = << "{\"id\":\"78548a96-cadd-48c0-b7d6-4ff3b81f10cc\",\"lists\":[\"testlist1\"],\"token\":\"rTgdC3f7uJ/Smg3s4b9va2KW5GdPkRHtwYNgWbvwhensgOSf2/wan95VPDiXKnAAGilsZlpw/Td4bs/OPeVeYg==\",\"scope\":[\"GET_ME\",\"WRITE_ACCESS\"]}" >>,
    ?assertEqual(Res,ts_client:set_dynvars(jsonpath,{JSONPath,data},[toto],Dynvars,{},[])),
    ?assertEqual(<< "gWAL+zvDcQjqLmNdSwcG/TOWyta5g==" >>,ts_client:set_dynvars(jsonpath,{JSONPath2,data},[toto],Dynvars,{},[])).

dynvars_jsonpath3_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [42, 666] }",
    JSONPath = "titi[1]",
    Dynvars=ts_dynvars:new(data,Data),
    ?assertEqual(666,ts_client:set_dynvars(jsonpath,{JSONPath,data},[toto],Dynvars,{},[])).

dynvars_jsonpath4_test() ->
    myset_env(),
    Data="\r\n\r\n{\"titi\": [{\"val\": [ 123, 231 ] , \"name\": \"foo\"}, {\"val\":  [ 13, 23 ], \"name\": \"bar\"}]}",
    JSONPath = "titi[?name=bar].val[0]",
    Dynvars=ts_dynvars:new(data,Data),
    ?assertEqual(13,ts_client:set_dynvars(jsonpath,{JSONPath,data},[toto],Dynvars,{},[])).

dynvars_file_test() ->
    myset_env(),
    ts_file_server:stop(),
    ts_file_server:start(),
    ts_file_server:read([{default,"./src/test/test_file_server.csv"}]),
    ?assertMatch([<<"username1">>,<<"glop">>, << >>],ts_client:set_dynvars(file,{iter,default,<< ";" >>},[],[],{},[])).

dynvars_file_pipe_test() ->
    myset_env(),
    ts_file_server:stop(),
    ts_file_server:start(),
    ts_file_server:read([{default,"./src/test/test_file_server_pipe.csv"}]),
    ?assertMatch([<<"conv%2F99%2F589%2Finfo.txt">>,<<"99">> ,<<"589">>],ts_client:set_dynvars(file,{iter,default,<< "|" >>},[],[],{},[])).



%%TODO: out of order..
%parse_dynvar_xpath_collection_test() ->
%    myset_env(),
%    Data="<html><body>"
%        " <img src=img0'>"
%         "<div><img src='img1'> "
%         "      <img src='img2'> "
%         " </div> "
%         " <img src='img3'> "
%         " <img src='img4'></body></html>",
%    XPath = "//img/@src",
%    Tree = mochiweb_html:parse(list_to_binary(Data)),
%    R = mochiweb_xpath:execute(XPath,Tree),
%    erlang:display(R),
%    Expected = [<<"img0">>,<<"img1">>,<<"img2">>,<<"img3">>,<<"img4">>],
%    ?assertMatch(Expected, R).


parse_dynvar_xpath_single_test() ->
   myset_env(),
   Data="<html><body>"
        "<div> "
        "      <img src='img2'> "
        " </div> "
        " <img src='img3'> "
        " <a href='/index.html'></body></html>",
   XPath = "//a/@href",
   Tree = mochiweb_html:parse(list_to_binary(Data)),
   R = mochiweb_xpath:execute(XPath,Tree),
   erlang:display(R),
   Expected = [<<"/index.html">>],
   ?assertEqual(Expected, R).

filter_re_include_test() ->
    ?assertEqual(["/toto"], ts_client:filter({ok,["http://toto/", "/toto", "mailto:bidule"]}, {true, "^/.*"})).

filter_re_exclude_test() ->
    ?assertEqual(["http://toto/", "mailto:bidule"], ts_client:filter({ok,["http://toto/", "/toto", "mailto:bidule"]}, {false,"^/.*"})).

extract_body_test() ->
    Data = << "HTTP header\r\nHeader: value\r\n\r\nbody\r\n" >>,
    ?assertEqual(<< "body\r\n" >>, ts_search:extract_body(Data)).

extract_body_nohttp_test() ->
    Data = << "random\r\nstuff" >>,
    ?assertEqual(Data, ts_search:extract_body(Data)).

badarg_re_test() ->
    Data = << "Below this line, is 1000 repeated lines">>,
    Regexp = "is (\\d+) repeated lines",
    {ok,Regexp2}=re:compile(Regexp),
    ?assertEqual([{lines, <<"1000">>}], ts_search:parse_dynvar([{re, 'lines', Regexp2 }],Data)).

myset_env()->
    myset_env(0).
myset_env(Level)->
    application:set_env(stdlib,debug_level,Level).

new({Pid, DynData}) ->
    "MSFT".

marketplace({Pid,DynData}) ->
    "5".

namespace({Pid,DynData}) ->
    "namespace1".

sessionBucket({Pid,DynData}) ->
    "58".
