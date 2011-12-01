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
    Req=ts_http:subst(#http_request{url="%%_image%%"}, DynVars),
    ?assertEqual("/images/my%20image%20with%20spaces.png", Req#http_request.url).

subst_redirect_test()->
    myset_env(),
    URL="%%_redirect%%",
    Cookie="toto=bar; path=/; domain=erlang.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"erlang.org",[]),
    Proto=#http_dyndata{cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(redirect,"http://erlang.org/bidule/truc"),
    {Req,_}=ts_http:add_dynparams(true,#dyndata{proto=Proto,dynvars=DynVars},
                                  #http_request{url=URL},
                                  {"erlang.org",80,gen_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: erlang.org\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertMatch(Str, binary_to_list(Res)).

subst_redirect_proto_test()->
    myset_env(),
    URL="%%_redirect%%",
    Cookie="toto=bar; path=/; domain=erlang.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"erlang.org",[]),
    Proto=#http_dyndata{cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(redirect,"http://erlang.org/bidule/truc"),
    Rep=ts_http:add_dynparams(true,#dyndata{proto=Proto,dynvars=DynVars},
                                  #http_request{url=URL},
                                  {"erlang.org",80,gen_tcp6}),
    ?assertMatch({_,{"erlang.org",80,gen_tcp6}}, Rep).

cookie_subdomain_test()->
    myset_env(),
    URL="/bidule/truc",
    Cookie="toto=bar; path=/; domain=.domain.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"domain.org",[]),
    Proto=#http_dyndata{cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(),
    Req=ts_http:add_dynparams(false,#dyndata{proto=Proto,dynvars=DynVars},
                                  #http_request{url=URL},
                                  {"www.domain.org",80,gen_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: www.domain.org:80\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
    {Res,_}=ts_http:get_message(Req,#state_rcv{}),
    ?assertEqual(Str, binary_to_list(Res)).

cookie_dotdomain_test()->
    myset_env(),
    URL="/bidule/truc",
    Cookie="toto=bar; path=/; domain=.www.domain.org",
    Cookies=ts_http_common:add_new_cookie(Cookie,"www.domain.org",[]),
    Proto=#http_dyndata{cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(),
    Req=ts_http:add_dynparams(false,#dyndata{proto=Proto,dynvars=DynVars},
                                  #http_request{url=URL},
                                  {"www.domain.org",80, gen_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: www.domain.org:80\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
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
    Proto=#http_dyndata{cookies=Cookies,user_agent="Firefox"},
    DynVars=ts_dynvars:new(),
    Req=ts_http:add_dynparams(false,#dyndata{proto=Proto,dynvars=DynVars},
                                  #http_request{url=URL},
                                  {"www.foobar.net",80, gen_tcp}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: www.foobar.net:80\r\nUser-Agent: Firefox\r\nCookie: RMID=42\r\n\r\n",
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

 myset_env()->
    myset_env(0).
 myset_env(N)->
    application:set_env(stdlib,debug_level,N).
