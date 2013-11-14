%% Copyright (c) 2008-2009 Tim Fletcher <http://tfletcher.com/>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(oauth_http).

-export([get/1, post/2, put/2, response_params/1, response_body/1, response_code/1]).

-type http_status() :: {string(), integer(), string()}.

-spec get(string()) -> {ok, {Status::http_status(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
get(URL) ->
  request(get, {URL, []}).

-spec post(string(), term()) -> {ok, {Status::http_status(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
post(URL, Data) ->
  request(post, {URL, [], "application/x-www-form-urlencoded", Data}).

-spec put(string(), term()) -> {ok, {Status::http_status(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
put(URL, Data) ->
  request(put, {URL, [], "application/x-www-form-urlencoded", Data}).

-spec request(httpc:method(), tuple()) -> {ok, {Status::http_status(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
request(Method, Request) ->
  httpc:request(Method, Request, [{autoredirect, false}], []).

-spec response_params({http_status(), [{string(), string()}], string()}) -> [{string(), string()}].
response_params(Response) ->
  oauth_uri:params_from_string(response_body(Response)).

-spec response_body({http_status(), [{string(), string()}], string()}) -> string().
response_body({{_, _, _}, _, Body}) ->
  Body.

-spec response_code({http_status(), [{string(), string()}], string()}) -> integer().
response_code({{_, Code, _}, _, _}) ->
  Code.
