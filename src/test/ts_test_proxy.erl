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

relative_url2_test()->
    myset_env(),
     String= "foo http://www.glop.com/(;-)/foo.html foo bar",
     AbsURI="http://www.glop.com/(;-)/foo.html?toto=bar",
     RelURL="/(;-)/foo.html?toto=bar",
    ?assertMatch({ok,"foo /(;-)/foo.html foo bar"},
                 ts_proxy_http:relative_url(false,String,AbsURI,RelURL)).

rewrite_http_none_test()->
     myset_env(),
    Data="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Content-Length: 30
<html><head></head><body>
<h1>http://foo.bar/toto1</h1>
</body></html>
",
     ?assertMatch({ok,Data},
                  ts_utils:from_https(Data)).

rewrite_http_test()->
     myset_env(),
    Data="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Content-Length: 30
<html><head></head><body>
<h1>https://foo.bar/toto</h1>
</body></html>
",
    NewData="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Content-Length: 30
<html><head></head><body>
<h1>http://-foo.bar/toto</h1>
</body></html>
",
    {ok,Res}=ts_utils:from_https(Data),
     ?assertEqual(list_to_binary(NewData),iolist_to_binary(Res) ).

rewrite_http_location_test()->
     myset_env(),
    Data="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Location: https://foo.bar/
Content-Length: 30
<html><head></head><body>
<h1>https://foo.bar/toto or https://foo.bar/glop</h1>
</body></html>
",
    NewData="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Location: http://-foo.bar/
Content-Length: 30
<html><head></head><body>
<h1>http://-foo.bar/toto or http://-foo.bar/glop</h1>
</body></html>
",
    {ok, Res}=ts_utils:from_https(Data),
     ?assertEqual(list_to_binary(NewData),iolist_to_binary(Res) ).

rewrite_http_location_nourl_test()->
     myset_env(),
    Data="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Location: https://foo.bar/
Content-Length: 30
<html><head></head><body>
</body></html>
",
    NewData="HTTP/1.1 200 OK
Server: Apache/2.0.46 (White Box)
Location: http://-foo.bar/
Content-Length: 30
<html><head></head><body>
</body></html>
",
    {ok, Res} = ts_utils:from_https(Data),
    ?assertEqual(list_to_binary(NewData), iolist_to_binary(Res)).


rewrite_http_body_test()->
    myset_env(),
    Data="sqdfqsdflqkfnmqlskfqd http://-foobar.foo42.fr\r\n",
    NewData="sqdfqsdflqkfnmqlskfqd https://foobar.foo42.fr\r\n",
    {ok, Res} = ts_utils:to_https({request,{body,Data}}),
     ?assertEqual(list_to_binary(NewData), iolist_to_binary(Res)).

rewrite_http_encode_test()->
     myset_env(),
    Data="GET http://-foobar.foo42.fr/ HTTP/1.1\r\nHost: -foobar.foo42.fr\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\n\r\n",
    NewData="GET https://foobar.foo42.fr/ HTTP/1.1\r\nHost: foobar.foo42.fr\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\n\r\n",
    {ok, Res} = ts_utils:to_https({request,Data}),
     ?assertEqual(list_to_binary(NewData), iolist_to_binary(Res)).


rewrite_http_encode2_test()->
     myset_env(),
    Data="GET http://gforge-qualif.foo.fr/ HTTP/1.1\r\nHost: gforge-qualif.foo.fr\r\nUser-Agent: Mozilla/5.0 (X11; U; Linux x86_64; fr; rv:1.9.1.6) Gecko/20100107 Fedora/3.5.6-1.fc12 Firefox/3.5.6\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: fr,en-us;q=0.7,en;q=0.3\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 300\r\nProxy-Connection: keep-alive\r\nPragma: no-cache\r\nCache-Control: no-cache\r\n\r\n",
    NewData="GET http://gforge-qualif.foo.fr/ HTTP/1.1\r\nHost: gforge-qualif.foo.fr\r\nUser-Agent: Mozilla/5.0 (X11; U; Linux x86_64; fr; rv:1.9.1.6) Gecko/20100107 Fedora/3.5.6-1.fc12 Firefox/3.5.6\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: fr,en-us;q=0.7,en;q=0.3\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 300\r\nProxy-Connection: keep-alive\r\nPragma: no-cache\r\nCache-Control: no-cache\r\n\r\n",
    {ok, Res} = ts_utils:to_https({request,Data}),
    ?assertEqual(list_to_binary(NewData), iolist_to_binary(Res)).

rewrite_http_encode3_test()->
     myset_env(),
    Data="GET http://-secure.foo.com/ HTTP/1.1\r\nHost: -secure.com\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0b9) Gecko/20100101 Firefox/4.0b9\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: fr,en-us;q=0.7,en;q=0.3\r\nAccept-Encoding: gzip, deflate\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nProxy-Connection: keep-alive\r\n\r\n",
    NewData="GET https://secure.foo.com/ HTTP/1.1\r\nHost: secure.com\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:2.0b9) Gecko/20100101 Firefox/4.0b9\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: fr,en-us;q=0.7,en;q=0.3\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\nKeep-Alive: 115\r\nProxy-Connection: keep-alive\r\n\r\n",
    {ok, Res} = ts_utils:to_https({request,Data}),
     ?assertEqual(list_to_binary(NewData), iolist_to_binary(Res)).

rewrite_webdav_test()->
    myset_env(),
    Data = "REPORT /tsung/!svn/vcc/default HTTP/1.1\r\nUser-Agent: SVN/1.4.4 (r25188) neon/0.25.5\r\nConnection: TE\r\nTE: trailers\r\nContent-Length: 172\r\nContent-Type: text/xml\r\nAccept-Encoding: svndiff1;q=0.9,svndiff;q=0.8\r\nAccept-Encoding: gzip\r\nAccept-Encoding: gzip\r\n\r\n<S:update-report send-all=\"true\" xmlns:S=\"svn:\"><S:src-path>http://-svn.process-one.net/tsung/trunk/examples</S:src-path><S:entry rev=\"816\" ></S:entry></S:update-report>",
    NewData="REPORT /tsung/!svn/vcc/default HTTP/1.1\r\nUser-Agent: SVN/1.4.4 (r25188) neon/0.25.5\r\nConnection: TE\r\nTE: trailers\r\nContent-Length: 172\r\nContent-Type: text/xml\r\nAccept-Encoding: svndiff1;q=0.9,svndiff;q=0.8\r\n\r\n<S:update-report send-all=\"true\" xmlns:S=\"svn:\"><S:src-path>https://svn.process-one.net/tsung/trunk/examples</S:src-path><S:entry rev=\"816\" ></S:entry></S:update-report>",
    {ok, Res} = ts_utils:to_https({request,Data}),
     ?assertEqual(list_to_binary(NewData), iolist_to_binary(Res)).

rewrite_http_encode_post_test()->
    myset_env(),
    Data="POST http://-foobar.foo42.fr/ HTTP/1.1\r\nHost: -foobar.foo42.fr\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,;q=0.7Content-Type: application/x-www-form-urlencoded\r\nContent-Length: 24\r\n\r\nuname=admin&upass=*****",
    NewData="POST https://foobar.foo42.fr/ HTTP/1.1\r\nHost: foobar.foo42.fr\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,;q=0.7Content-Type: application/x-www-form-urlencoded\r\nContent-Length: 24\r\n\r\nuname=admin&upass=*****",
    {ok,Res}=ts_utils:to_https({request,Data}),
    ?assertEqual(list_to_binary(NewData),iolist_to_binary(Res)).


%% parse_http_test()->
%%     myset_env(),
%%     ?assertMatch({ok,""},
%%                  ts_proxy_http:parse(State,ClientSocket,Socket,Data)).

myset_env(Val)->
    application:set_env(stdlib,debug_level,Val).
myset_env()->
    myset_env(0).

