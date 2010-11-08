-module(oauth_rsa_sha1).

-export([signature/2, verify/3]).

-include_lib("public_key/include/public_key.hrl").

-spec signature(string(), string()) -> string().
signature(BaseString, PrivateKeyPath) ->
	{ok, Contents} = file:read_file(PrivateKeyPath),
  [Info] = public_key:pem_decode(Contents),
  PrivateKey = public_key:pem_entry_decode(Info),
  base64:encode_to_string(public_key:sign(list_to_binary(BaseString), PrivateKey)).

-spec verify(string(), string(), term()) -> boolean().
verify(Signature, BaseString, PublicKey) ->
  public_key:verify_signature(to_binary(BaseString), sha, base64:decode(Signature), public_key(PublicKey)).

to_binary(Term) when is_list(Term) ->
  list_to_binary(Term);
to_binary(Term) when is_binary(Term) ->
  Term.

public_key(Path) when is_list(Path) ->
	{ok, Contents} = file:read_file(Path),
  [{cert, DerCert, not_encrypted}] = public_key:pem_decode(Contents),
  {ok, Cert} = public_key:pkix_decode_cert(DerCert, otp),
  public_key(Cert);
public_key(#'OTPCertificate'{tbsCertificate=Cert}) ->
  public_key(Cert);
public_key(#'OTPTBSCertificate'{subjectPublicKeyInfo=Info}) ->
  public_key(Info);
public_key(#'OTPSubjectPublicKeyInfo'{subjectPublicKey=Key}) ->
  Key.
