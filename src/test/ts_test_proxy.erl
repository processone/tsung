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
Content-Length: 33
<html><head></head><body>
<h1>http://ssl-foo.bar/toto</h1>
</body></html>
",
     ?assertMatch({ok,NewData},
                  ts_utils:from_https(Data)).

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
Location: http://ssl-foo.bar/
Content-Length: 36
<html><head></head><body>
<h1>http://ssl-foo.bar/toto or http://ssl-foo.bar/glop</h1>
</body></html>
",
     ?assertMatch({ok,NewData},
                  ts_utils:from_https(Data)).

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
Location: http://ssl-foo.bar/
Content-Length: 30
<html><head></head><body>
</body></html>
",
     ?assertMatch({ok,NewData},
                  ts_utils:from_https(Data)).

rewrite_http_encode_test()->
     myset_env(),
    Data="GET http://ssl-foobar.foo42.fr/ HTTP/1.1\r\nHost: ssl-foobar.foo42.fr\r\nAccept-Encoding: gzip,deflate\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\n\r\n",
    NewData="GET https://foobar.foo42.fr/ HTTP/1.1\r\nHost: foobar.foo42.fr\r\nAccept-Charset: ISO-8859-15,utf-8;q=0.7,*;q=0.7\r\n\r\n",
     ?assertMatch({ok,NewData},
                  ts_utils:to_https({request,Data})).

rewrite_webdav_test()->
     myset_env(),
    Data = "REPORT /tsung/!svn/vcc/default HTTP/1.1\r\nUser-Agent: SVN/1.4.4 (r25188) neon/0.25.5\r\nConnection: TE\r\nTE: trailers\r\nContent-Length: 172\r\nContent-Type: text/xml\r\nAccept-Encoding: svndiff1;q=0.9,svndiff;q=0.8\r\nAccept-Encoding: gzip\r\nAccept-Encoding: gzip\r\n\r\n<S:update-report send-all=\"true\" xmlns:S=\"svn:\"><S:src-path>http://ssl-svn.process-one.net/tsung/trunk/examples</S:src-path><S:entry rev=\"816\" ></S:entry></S:update-report>",
    NewData="REPORT /tsung/!svn/vcc/default HTTP/1.1\r\nUser-Agent: SVN/1.4.4 (r25188) neon/0.25.5\r\nConnection: TE\r\nTE: trailers\r\nContent-Length: 169\r\nContent-Type: text/xml\r\nAccept-Encoding: svndiff1;q=0.9,svndiff;q=0.8\r\n\r\n<S:update-report send-all=\"true\" xmlns:S=\"svn:\"><S:src-path>https://svn.process-one.net/tsung/trunk/examples</S:src-path><S:entry rev=\"816\" ></S:entry></S:update-report>",
     ?assertMatch({ok,NewData},
                  ts_utils:to_https({request,Data})).




%% parse_http_test()->
%%     myset_env(),
%%     ?assertMatch({ok,""},
%%                  ts_proxy_http:parse(State,ClientSocket,Socket,Data)).

myset_env()->
    application:set_env(stdlib,debug_level,0).

