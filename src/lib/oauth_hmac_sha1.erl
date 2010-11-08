-module(oauth_hmac_sha1).

-export([signature/3, verify/4]).

-spec signature(string(), string(), string()) -> string().
signature(BaseString, CS, TS) ->
  Key = oauth_uri:calate("&", [CS, TS]),
  base64:encode_to_string(crypto:sha_mac(Key, BaseString)).

-spec verify(string(), string(), string(), string()) -> boolean().
verify(Signature, BaseString, CS, TS) ->
  Signature =:= signature(BaseString, CS, TS).
