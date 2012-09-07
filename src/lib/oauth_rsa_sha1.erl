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

-module(oauth_rsa_sha1).

-export([signature/2, verify/3]).

-include_lib("public_key/include/public_key.hrl").

-spec signature(string(), string()) -> string().
signature(BaseString, PrivateKeyPath) ->
	{ok, Contents} = file:read_file(PrivateKeyPath),
  [Info] = public_key:pem_decode(Contents),
  PrivateKey = public_key:pem_entry_decode(Info),
  base64:encode_to_string(public_key:sign(list_to_binary(BaseString), sha, PrivateKey)).

-spec verify(string(), string(), term()) -> boolean().
verify(Signature, BaseString, PublicKey) ->
  public_key:verify(to_binary(BaseString), sha, base64:decode(Signature), public_key(PublicKey)).

to_binary(Term) when is_list(Term) ->
  list_to_binary(Term);
to_binary(Term) when is_binary(Term) ->
  Term.

public_key(Path) when is_list(Path) ->
	{ok, Contents} = file:read_file(Path),
  [{'Certificate', DerCert, not_encrypted}] = public_key:pem_decode(Contents),
  public_key( public_key:pkix_decode_cert(DerCert, otp));
public_key(#'OTPCertificate'{tbsCertificate=Cert}) ->
  public_key(Cert);
public_key(#'OTPTBSCertificate'{subjectPublicKeyInfo=Info}) ->
  public_key(Info);
public_key(#'OTPSubjectPublicKeyInfo'{subjectPublicKey=Key}) ->
  Key.
