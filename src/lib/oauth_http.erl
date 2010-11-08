-module(oauth_http).

-export([get/1, post/2, response_params/1, response_body/1, response_code/1]).

-type http_status() :: {string(), integer(), string()}.

-spec get(string()) -> {ok, {Status::http_status(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
get(URL) ->
  request(get, {URL, []}).

-spec post(string(), term()) -> {ok, {Status::http_status(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
post(URL, Data) ->
  request(post, {URL, [], "application/x-www-form-urlencoded", Data}).

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
