-module(oauth_plaintext).

-export([signature/2, verify/3]).

-spec signature(string(), string()) -> string().
signature(CS, TS) ->
  oauth_uri:calate("&", [CS, TS]).

-spec verify(string(), string(), string()) -> boolean().
verify(Signature, CS, TS) ->
  Signature =:= signature(CS, TS).
