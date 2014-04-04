%%%
%%%  Copyright © Nicolas Niclausse 2007
%%%
%%%	 Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 17 Mar 2007 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

-module(ts_test_http).
-vc('$Id: ts_test_jabber.erl 768 2007-11-15 11:01:01Z mremond $ ').
-author('Nicolas.Niclausse@niclux.org').

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_config.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->ok.

ipv6_url_test() ->
    URL=ts_config_http:parse_URL("http://[2178:2:5:0:28f:0:3]:8080/toto.php?titi=[43]"),
    ?assertMatch(#url{path="/toto.php",port=8080,host="2178:2:5:0:28f:0:3",scheme=http}, URL).

ipv6_url2_test() ->
    S=ts_config_http:server_to_url(#server{host="2178:2:5:0:28f:0:3",port=80,type=gen_tcp} ),
    ?assertEqual("http://[2178:2:5:0:28f:0:3]", S).

ipv6_url3_test() ->
    S=ts_config_http:server_to_url(#server{host="[2178:2:5:0:28f:0:3]",port=80,type=gen_tcp} ),
    ?assertEqual("http://[2178:2:5:0:28f:0:3]", S).

ipv6_url4_test() ->
    S=ts_config_http:server_to_url(#server{host="2178:2:5:0:28f:0:3",port=8080,type=gen_tcp} ),
    ?assertEqual("http://[2178:2:5:0:28f:0:3]:8080", S).

ipv4_url_test() ->
    URL=ts_config_http:parse_URL("http://127.0.0.1:8080/"),
    ?assertMatch(#url{path="/",port=8080,host="127.0.0.1",scheme=http}, URL).

subst_url_test() ->
    DynVars=ts_dynvars:new('image', "/images/my image with spaces.png"),
    Req=ts_http:subst(true,#http_request{url="%%_image%%"}, DynVars),
    ?assertEqual("/images/my%20image%20with%20spaces.png", Req#http_request.url).

subst_full_url_test() ->
    myset_env(),
    URL="http://myserver/%%_path%%",
    Proto=#http{user_agent="Firefox"},
    DynVars=ts_dynvars:new(path,"bidule/truc"),
    {Req,_}=ts_http:add_dynparams(true,{DynVars, Proto} ,
                                  #http_request{url=URL},
                                  {"erlang.org",80,ts_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: myserver\r\nUser-Agent: Firefox\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).

subst_redirect_test()->
    myset_env(),
    URL="%%_redirect%%",
    Cookie="toto=bar; path=/; domain=erlang.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"erlang.org",[]),
    Proto=#http{session_cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(redirect,"http://erlang.org/bidule/truc"),
    {Req,_}=ts_http:add_dynparams(true,{DynVars, Proto} ,
                                  #http_request{url=URL},
                                  {"erlang.org",80,ts_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: erlang.org\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).


subst_ipv6_host_test()->
    URL="/%%_dynpath%%",
    Proto=#http{user_agent="Firefox"},
    DynVars=ts_dynvars:new(dynpath,"bidule/truc"),
    Rep=ts_http:add_dynparams(true,{DynVars, Proto},
                                  #http_request{url=URL},
                                  {"2178:2:5:0:28f:0:3",80,ts_tcp6}),
    {Res,_}=ts_http:get_message(Rep,#state_rcv{}),
    OK = << "GET /bidule/truc HTTP/1.1\r\nHost: [2178:2:5:0:28f:0:3]\r\nUser-Agent: Firefox\r\n\r\n" >>,
    ?assertEqual(OK, Res).

subst_ipv6_host2_test()->
    URL="/%%_dynpath%%",
    Proto=#http{user_agent="Firefox"},
    DynVars=ts_dynvars:new(dynpath,"bidule/truc"),
    Rep=ts_http:add_dynparams(true,{DynVars, Proto},
                                  #http_request{url=URL},
                                  {"::1",8080,ts_tcp6}),
    {Res,_}=ts_http:get_message(Rep,#state_rcv{}),
    OK = << "GET /bidule/truc HTTP/1.1\r\nHost: [::1]:8080\r\nUser-Agent: Firefox\r\n\r\n" >>,
    ?assertEqual(OK, Res).

subst_redirect_proto_test()->
    myset_env(),
    URL="%%_redirect%%",
    Cookie="toto=bar; path=/; domain=erlang.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"erlang.org",[]),
    Proto=#http{session_cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(redirect,"http://erlang.org/bidule/truc"),
    Rep=ts_http:add_dynparams(true,{DynVars, Proto},
                                  #http_request{url=URL},
                                  {"erlang.org",80,ts_tcp6}),
    ?assertMatch({_,{"erlang.org",80,ts_tcp6}}, Rep).

subst_cookie_test()->
    myset_env(),
    URL="/bidule/truc",
    Cookie="bar=%%_foovar%%; path=/; domain=erlang.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"erlang.org",[]),
    Proto=#http{user_agent="Firefox"},
    DynVars=ts_dynvars:new(foovar,"foo"),
    Req=ts_http:add_dynparams(true,{DynVars, Proto},
                                  #http_request{url=URL,cookie=Cookies},
                                  {"erlang.org",80,ts_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: erlang.org\r\nUser-Agent: Firefox\r\nCookie: bar=foo\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).

cookie_subdomain_test()->
    myset_env(),
    URL="/bidule/truc",
    Cookie="toto=bar; path=/; domain=.domain.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"domain.org",[]),
    Proto=#http{session_cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(),
    Req=ts_http:add_dynparams(false,{DynVars, Proto},
                                  #http_request{url=URL},
                                  {"www.domain.org",80,ts_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: www.domain.org\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).

cookie_dotdomain_test()->
    myset_env(),
    URL="/bidule/truc",
    Cookie="toto=bar; path=/; domain=.www.domain.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"www.domain.org",[]),
    Proto=#http{session_cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(),
    Req=ts_http:add_dynparams(false,{DynVars, Proto},
                                  #http_request{url=URL},
                                  {"www.domain.org",80, ts_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: www.domain.org\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).



add_cookie_samekey_samedomain_test()->
    myset_env(),
    Cookie1="RMID=732423sdfs73242; path=/; domain=.example.net",
    Cookie2="RMID=42; path=/; domain=.example.net",
    Val1=#cookie{key="RMID",value="732423sdfs73242",domain=".example.net",path="/"},
    Val2=#cookie{key="RMID",value="42",domain=".example.net",path="/"},
    Cookies=ts_http_common:add_new_cookie(Cookie1,"foobar.com",[]),
    %% same domain, second cookie should erase the first one
    Res=ts_http_common:add_new_cookie(Cookie2,"foobar.com",Cookies),
    ?assertMatch([Val2],Res).

add_cookie_replace_key_default_domain_test()->
    myset_env(),
    Cookie1="RMID=732423sdfs73242; path=/; ",
    Cookie2="RMID=42; path=/; domain=.example.net",
    Val2=#cookie{key="RMID",value="42",domain=".example.net",path="/"},
    Cookies=ts_http_common:add_new_cookie(Cookie1,"example.net",[]),
    %% same domain, second cookie should erase the first one
    Res=ts_http_common:add_new_cookie(Cookie2,"foobar.com",Cookies),
    ?assertEqual([Val2],Res).

set_cookie_test()->
    myset_env(),
    Cookie="RMID=732423sdfs73242; path=/; domain=.foobar.com",
    Val="Cookie: RMID=732423sdfs73242\r\n",
    Cookies=ts_http_common:add_new_cookie(Cookie,"www.foobar.com",[]),
    ?assertEqual(Val,lists:flatten(ts_http_common:set_cookie_header({Cookies,"www.foobar.com","/toto.html"}))).

add_cookie_test()->
    myset_env(),
    Cookie1="RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/; domain=.example.net",
    Cookie2="ID=42; path=/; domain=.example.net",
    Val1=#cookie{key="RMID",value="732423sdfs73242",domain=".example.net",path="/",expires="Fri, 31-Dec-2010 23:59:59 GMT"},
    Val2=#cookie{key="ID",value="42",domain=".example.net",path="/"},
    Cookies=ts_http_common:add_new_cookie(Cookie1,"foobar.com",[]),
    ?assertEqual([Val2,Val1],ts_http_common:add_new_cookie(Cookie2,"foobar.com",Cookies)).

add_cookie_samekey_nodomain_test()->
    myset_env(),
    Cookie1="RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/; domain=.example.net",
    Cookie2="RMID=42; path=/; domain=.foobar.net",
    Val1=#cookie{key="RMID",value="732423sdfs73242",domain=".example.net",path="/",expires="Fri, 31-Dec-2010 23:59:59 GMT"},
    Val2=#cookie{key="RMID",value="42",domain=".foobar.net",path="/"},
    Cookies=ts_http_common:add_new_cookie(Cookie1,"foobar.com",[]),
    %% two different domains, two cookies
    ?assertEqual([Val2,Val1],ts_http_common:add_new_cookie(Cookie2,"foobar.com",Cookies)).

add_cookie_samekey_nodomain_req_test()->
    myset_env(),
    URL="/bidule/truc",
    Cookie1="RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/; domain=.example.net",
    Cookie2="RMID=42; path=/; domain=.foobar.net",
    Cookies1=ts_http_common:add_new_cookie(Cookie1,"",[]),
    Cookies = ts_http_common:add_new_cookie(Cookie2,"",Cookies1),
    Proto=#http{session_cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(),
    Req=ts_http:add_dynparams(false,{DynVars, Proto},
                                  #http_request{url=URL},
                                  {"www.foobar.net",80, ts_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: www.foobar.net\r\nUser-Agent: Firefox\r\nCookie: RMID=42\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).

chunk_header_ok1_test()->
    Rep=ts_http_common:parse_line("transfer-encoding: chunked\r\n",#http{},[]),
    ?assertMatch(#http{chunk_toread=0}, Rep).
chunk_header_ok2_test()->
    Rep=ts_http_common:parse_line("transfer-encoding: Chunked\r\n",#http{},[]),
    ?assertMatch(#http{chunk_toread=0}, Rep).
chunk_header_ok3_test()->
    Rep=ts_http_common:parse_line("transfer-encoding:chunked\r\n",#http{},[]),
    ?assertMatch(#http{chunk_toread=0}, Rep).
chunk_header_bad_test()->
    Rep=ts_http_common:parse_line("transfer-encoding: cheddar\r\n",#http{},[]),
    ?assertMatch(#http{chunk_toread=-1}, Rep).

parse_304_test() ->
    Res = <<"HTTP/1.1 304 Not Modified\r\nDate: Fri, 24 Aug 2012 07:49:37 GMT\r\nServer: Apache/2.2.16 (Debian)\r\nETag: \"201ad-10fb-473ae23fb0600\"\r\nVary: Accept-Encoding\r\n\r\n">>,
    State=#state_rcv{session=#http{user_agent="Firefox"}},
    {Rep, [], false } =ts_http:parse(Res,State),
    ?assertMatch(#http{user_agent="Firefox",status={none,304}, partial=false}, Rep#state_rcv.session).

split_body_test() ->
    Data = << "HTTP header\r\nHeader: value\r\n\r\nbody\r\n" >>,
    ?assertEqual({<< "HTTP header\r\nHeader: value" >>, << "body\r\n" >>}, ts_http:split_body(Data)).

split_body2_test() ->
    Data = << "HTTP header\r\nHeader: value\r\n\r\nbody\r\n\r\nnewline in body\r\n" >>,
    ?assertEqual({<< "HTTP header\r\nHeader: value" >>, << "body\r\n\r\nnewline in body\r\n" >>}, ts_http:split_body(Data)).

split_body3_test() ->
    Data = << "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\n19\r\nbody\r\n\r\nnewline in body\r\n\r\n" >>,
    ?assertEqual({<< "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked" >>, << "19\r\nbody\r\n\r\nnewline in body\r\n\r\n" >>}, ts_http:split_body(Data)).

decode_buffer_test() ->
    Data = << "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\n19\r\nbody\r\n\r\nnewline in body\r\n0\r\n\r\n" >>,
    ?assertEqual(<< "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\nbody\r\n\r\nnewline in body\r\n" >>, ts_http:decode_buffer(Data, #http{chunk_toread=-2})).

decode_buffer2_test() ->
    Data = << "HTTP header\r\nHeader: value\r\n\r\nbody\r\n\r\nnewline in body\r\n" >>,
    ?assertEqual(<< "HTTP header\r\nHeader: value\r\n\r\nbody\r\n\r\nnewline in body\r\n" >>, ts_http:decode_buffer(Data, #http{chunk_toread=-1}) ).

decode_buffer3_test() ->
    Data = << "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\n17\r\nbody\r\n\r\nnewline in body\r\n3\r\nabc\r\n0\r\n\r\n" >>,
    ?assertEqual(<< "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\nbody\r\n\r\nnewline in bodyabc" >>, ts_http:decode_buffer(Data, #http{chunk_toread=-2})).

compress_chunk_test()->
    <<A:10/binary, B/binary>> = zlib:gzip("sesame ouvre toi"),
    Data1 = << "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\nA\r\n" >>,
    Data2= <<"1A\r\n" >>,
    Data3= <<"0\r\n\r\n" >>,
    Data= <<Data1/binary, A/binary, Data2/binary, B/binary, Data3/binary>>,
    ?assertEqual(<< "HTTP header\r\nHeader: value\r\nTransfer-Encoding: chunked\r\n\r\nsesame ouvre toi" >>, ts_http:decode_buffer(Data, #http{chunk_toread=-2, compressed={false,gzip}})).

authentication_basic_test()->
    Base="QWxhZGRpbjpvcGVuIHNlc2FtZQ==",
    ?assertEqual(["Authorization: Basic ",Base,?CRLF], ts_http_common:authenticate(#http_request{userid="Aladdin", auth_type="basic",passwd="open sesame"})).

authentication_digest1_test()->
    OK="Authorization: Digest username=\"Mufasa\", realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", uri=\"/dir/index.html\", response=\"6629fae49393a05397450978507c4ef1\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\", qop=\"auth\", nc=00000001, cnonce=\"0a4f113b\"\r\n",

    Req=#http_request{userid="Mufasa", auth_type="digest",passwd="Circle Of Life",
                      realm ="testrealm@host.com", url="/dir/index.html",
                      digest_qop    = "auth",
                      digest_nonce  = "dcd98b7102dd2f0e8b11d0f600bfb0c093",
                      digest_nc = "00000001",
                      digest_cnonce = "0a4f113b",
                      digest_opaque = "5ccc069c403ebaf9f0171e9517f40e41"},
    ?assertEqual(OK, lists:flatten(ts_http_common:authenticate(Req))).


oauth_test()->
    myset_env(),
    Data = <<"HTTP/1.1 200 OK\r\nDate: Mon, 10 Sep 2012 12:26:35 GMT\r\nServer: Apache/2.2.17 (Debian)\r\nX-Powered-By: PHP/5.3.3-7\r\nContent-Length: 55\r\nContent-Type: text/html\r\n\r\noauth_token=requestkey&oauth_token_secret=requestsecret">>,

    ?assertMatch([{'token',<< "requestsecret" >>}], ts_search:parse_dynvar([{re,'token', "oauth_token_secret=([^&]*)"} ],Data)),
    ?assertMatch([{'token',<< "requestkey" >>}], ts_search:parse_dynvar([{re,'token', "oauth_token=([^&]*)"} ],Data)).


set_msg_dyn_test() ->
    URL = "http://jm-11:%%_myport%%/%%_myurl%%",
    Subst =true,
    Res = ts_config_http:set_msg(#http_request{url= URL},
            {Subst, undefined, false, [#server{host="myserver", port=99, type="tcp"}], "myserver", ets:new(fake,[]), 1}),
    ?assertMatch(#http_request{url=URL}, Res#ts_request.param).

set_msg_test() ->
    URL   = "http://server:8080/path%%bla%%",
    Subst = false,
    Res   = ts_config_http:set_msg(#http_request{url= URL},
                                   {Subst, undefined, false, [#server{host="myserver", port=99, type="tcp"}], "myserver", ets:new(fake,[]), 1}),
    ?assertMatch(#http_request{url="/path%%bla%%",host_header= "server:8080"}, Res#ts_request.param),
    ?assertMatch(#ts_request{host="server", port=8080, scheme = ts_tcp}, Res).

set_msg2_test() ->
    URL   = "http://server:8080/path%%",
    Subst = true,
    Res   = ts_config_http:set_msg(#http_request{url= URL},
                                   {Subst, undefined, false, [#server{host="myserver", port=99, type="tcp"}], "myserver", ets:new(fake,[]), 1}),
    ?assertMatch(#http_request{url="/path%%",host_header= "server:8080"}, Res#ts_request.param),
    ?assertMatch(#ts_request{host="server", port=8080, scheme = ts_tcp}, Res).

 myset_env()->
    myset_env(0).
 myset_env(N)->
    application:set_env(stdlib,debug_level,N).
