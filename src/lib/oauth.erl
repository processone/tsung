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

-module(oauth).

-export(
  [ get/5
  , header/1
  , post/5
  , put/5
  , signature/5
  , signature_base_string/3
  , signed_params/6
  , token/1
  , token_secret/1
  , uri/2
  , verify/6
  ]).

-spec get(string(), [proplists:property()], oauth_client:consumer(), string(), string()) -> {ok, {Status::tuple(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
get(URL, ExtraParams, Consumer, Token, TokenSecret) ->
  SignedParams = signed_params("GET", URL, ExtraParams, Consumer, Token, TokenSecret),
  oauth_http:get(uri(URL, SignedParams)).

-spec post(string(), [proplists:property()], oauth_client:consumer(), string(), string()) -> {ok, {Status::tuple(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
post(URL, ExtraParams, Consumer, Token, TokenSecret) ->
  SignedParams = signed_params("POST", URL, ExtraParams, Consumer, Token, TokenSecret),
  oauth_http:post(URL, oauth_uri:params_to_string(SignedParams)).

-spec put(string(), [proplists:property()], oauth_client:consumer(), string(), string()) -> {ok, {Status::tuple(), Headers::[{string(), string()}], Body::string()}} | {error, term()}.
put(URL, ExtraParams, Consumer, Token, TokenSecret) ->
  SignedParams = signed_params("PUT", URL, ExtraParams, Consumer, Token, TokenSecret),
  oauth_http:put(URL, oauth_uri:params_to_string(SignedParams)).

-spec uri(string(), [proplists:property()]) -> string().
uri(Base, []) ->
  Base;
uri(Base, Params) ->
  lists:concat([Base, "?", oauth_uri:params_to_string(Params)]).

-spec header([{string(), string()}]) -> {string(), string()}.
header(Params) ->
  {"Authorization", "OAuth " ++ oauth_uri:params_to_header_string(Params)}.

-spec token([proplists:property()]) -> string().
token(Params) ->
  proplists:get_value("oauth_token", Params).

-spec token_secret([proplists:property()]) -> string().
token_secret(Params) ->
  proplists:get_value("oauth_token_secret", Params).

-spec verify(string(), string(), string(), [proplists:property()], oauth_client:consumer(), string()) -> boolean().
verify(Signature, HttpMethod, URL, Params, Consumer, TokenSecret) ->
  case signature_method(Consumer) of
    plaintext ->
      oauth_plaintext:verify(Signature, consumer_secret(Consumer), TokenSecret);
    hmac_sha1 ->
      BaseString = signature_base_string(HttpMethod, URL, Params),
      oauth_hmac_sha1:verify(Signature, BaseString, consumer_secret(Consumer), TokenSecret);
    rsa_sha1 ->
      BaseString = signature_base_string(HttpMethod, URL, Params),
      oauth_rsa_sha1:verify(Signature, BaseString, consumer_secret(Consumer))
  end.

-spec signed_params(string(), string(), [proplists:property()], oauth_client:consumer(), string(), string()) -> [{string(), string()}].
signed_params(HttpMethod, URL, ExtraParams, Consumer, Token, TokenSecret) ->
  Params = token_param(Token, params(Consumer, ExtraParams)),
  [{"oauth_signature", signature(HttpMethod, URL, Params, Consumer, TokenSecret)}|Params].

-spec signature(string(), string(), [proplists:property()], oauth_client:consumer(), string()) -> string().
signature(HttpMethod, URL, Params, Consumer, TokenSecret) ->
  case signature_method(Consumer) of
    plaintext ->
      oauth_plaintext:signature(consumer_secret(Consumer), TokenSecret);
    hmac_sha1 ->
      BaseString = signature_base_string(HttpMethod, URL, Params),
      oauth_hmac_sha1:signature(BaseString, consumer_secret(Consumer), TokenSecret);
    rsa_sha1 ->
      BaseString = signature_base_string(HttpMethod, URL, Params),
      oauth_rsa_sha1:signature(BaseString, consumer_secret(Consumer))
  end.

-spec signature_base_string(string(), string(), [proplists:property()]) -> string(). 
signature_base_string(HttpMethod, URL, Params) ->
  NormalizedURL = oauth_uri:normalize(URL),
  NormalizedParams = oauth_uri:params_to_string(lists:sort(Params)),
  oauth_uri:calate("&", [HttpMethod, NormalizedURL, NormalizedParams]).

token_param("", Params) ->
  Params;
token_param(Token, Params) ->
  [{"oauth_token", Token}|Params].

params(Consumer, Params) ->
  Nonce = base64:encode_to_string(crypto:rand_bytes(32)), % cf. ruby-oauth
  params(Consumer, oauth_unix:timestamp(), Nonce, Params).

params(Consumer, Timestamp, Nonce, Params) ->
  [ {"oauth_version", "1.0"}
  , {"oauth_nonce", Nonce}
  , {"oauth_timestamp", integer_to_list(Timestamp)}
  , {"oauth_signature_method", signature_method_string(Consumer)}
  , {"oauth_consumer_key", consumer_key(Consumer)}
  | Params
  ].

signature_method_string(Consumer) ->
  case signature_method(Consumer) of
    plaintext ->
      "PLAINTEXT";
    hmac_sha1 ->
      "HMAC-SHA1";
    rsa_sha1 ->
      "RSA-SHA1"
  end.

signature_method(_Consumer={_, _, Method}) ->
  Method.

consumer_secret(_Consumer={_, Secret, _}) ->
  Secret.

consumer_key(_Consumer={Key, _, _}) ->
  Key.
