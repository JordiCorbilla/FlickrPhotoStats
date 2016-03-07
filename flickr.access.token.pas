// Copyright (c) 2015, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit flickr.access.token;

interface

type
  IAccessToken = interface
    function GenerateRequestAccessToken() : string;
  end;

  TAccessToken = class(TInterfacedObject, IAccessToken)
    FOauth_verifier: string;
    FOauth_consumer_key: string;
    FOauth_token : string;
    FConsumerSecret: string;
    FTokenSecret: string;
    function GetOauth_verifier() : string;
    function GetOauth_consumer_key() : string;
    function GetConsumerSecret() : string;
    function GetTokenSecret() : string;
    procedure SetOauth_verifier(const Value: string);
    procedure SetOauth_consumer_key(const Value: string);
    function GetOauth_token() : string;
    procedure SetConsumerSecret(const Value: string);
    procedure SetTokenSecret(const Value: string);
    procedure SetOauth_token(const Value: string);
    class function New(const Oauth_verifier : string; const Oauth_consumer_key : string; const Oauth_token : string; ConsumerSecret : string; TokenSecret : string): IAccessToken; static;
    function GenerateRequestAccessToken() : string;
    Constructor Create(Oauth_verifier : string; Oauth_consumer_key : string; Oauth_token : string; ConsumerSecret : string; TokenSecret : string);
  end;

const
		OAuthConsumerKey = 'oauth_consumer_key';
		OAuthCallbackKey = 'oauth_callback';
		OAuthVersionKey = 'oauth_version';
		OAuthSignatureMethodKey = 'oauth_signature_method';
		OAuthSignatureKey = 'oauth_signature';
		OAuthTimestampKey = 'oauth_timestamp';
    OAuthVerifierKey = 'oauth_verifier';
		OAuthNonceKey = 'oauth_nonce';
		OAuthTokenKey = 'oauth_token';
		OAuthTokenSecretKey = 'oauth_token_secret';
    OAuthVersion = '1.0';
    HMACSHA1SignatureType = 'HMAC-SHA1';
    PlainTextSignatureType = 'PLAINTEXT';
    RSASHA1SignatureType = 'RSA-SHA1';

implementation

uses
   IdHash, IdHMAC, IdHMACSHA1, Windows, IdHashMessageDigest, IdCoderMIME, IdURI, HTTPApp, flickr.signature, NetEncoding;

{ TAccessToken }

constructor TAccessToken.Create(Oauth_verifier, Oauth_consumer_key, Oauth_token: string; ConsumerSecret : string; TokenSecret : string);
begin
  SetOauth_verifier(Oauth_verifier);
  SetOauth_consumer_key(Oauth_consumer_key);
  SetOauth_token(Oauth_token);
  SetConsumerSecret(ConsumerSecret);
  SetTokenSecret(TokenSecret);
end;

function TAccessToken.GenerateRequestAccessToken: string;
var
  baseURL, paramURL, encodedURL, returnURL : string;
  ConsumerSecret : string;
  TokenSecret : string;
  timeStamp : string;
begin
  //Generate request access token needs to generate:

  //https://www.flickr.com/services/oauth/access_token
  //?oauth_consumer_key=653e7a6ecc1d528c516cc8f92cf98611
  //&oauth_nonce=37026218
  //&oauth_signature_method=HMAC-SHA1
  //&oauth_timestamp=1305586309
  //&oauth_token=72157626737672178-022bbd2f4c2f3432
  //&oauth_verifier=5d1b96a26b494074
  //&oauth_version=1.0

  //this one goes after
  //&oauth_signature=UD9TGXzrvLIb0Ar5ynqvzatM58U%3D

  baseURL := 'https://www.flickr.com/services/oauth/access_token';

  baseURL := String(TNetEncoding.URL.Encode(baseURL));
  timeStamp := TSignature.getTimeStamp();
  paramURL := OAuthConsumerKey+'=' + GetOauth_consumer_key;
  paramURL := paramURL + '&'+OAuthNonceKey+'=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&'+OAuthSignatureMethodKey+'=' + HMACSHA1SignatureType;
  paramURL := paramURL + '&'+OAuthTimestampKey+'=' + timeStamp;
  paramURL := paramURL + '&'+OAuthTokenKey+'=' + GetOauth_token;
  paramURL := paramURL + '&'+OAuthVerifierKey+'=' + GetOauth_verifier;
  paramURL := paramURL + '&'+OAuthVersionKey+'=' + OAuthVersion;

  paramURL := String(TNetEncoding.URL.Encode(paramURL));

  //Encode this to get the signature
  encodedURL := 'GET&' + baseURL + '&' + paramURL;

  //Example encoded URL:
  //'GET&https%3A%2F%2Fwww.flickr.com%2Fservices%2Foauth%2Faccess_token
  //oauth_consumer_key%3D0edf6f13dc6309c822b59ae8bb783df6%26
  //oauth_nonce%3D1CC5A4B05607CA5378CBF06EAB2C927F%26
  //oauth_signature_method%3DHMAC-SHA1%26
  //oauth_timestamp%3D1420675285%26
  //oauth_token%3D72157626737672178
  //oauth_verifier=5d1b96a26b494074
  //oauth_version%3D1.0'

  ConsumerSecret := String(TNetEncoding.URL.Encode(GetConsumerSecret()));
  TokenSecret := String(TNetEncoding.URL.Encode(GetTokenSecret()));

  ConsumerSecret := ConsumerSecret + '&' + TokenSecret;

  returnURL := 'https://www.flickr.com/services/oauth/access_token';
  returnURL := returnURL + '?'+OAuthConsumerKey+'=' + GetOauth_consumer_key();
  returnURL := returnURL + '&'+OAuthNonceKey+'=' + TSignature.getOAuthNonce(timeStamp);
  returnURL := returnURL + '&'+OAuthSignatureMethodKey+'=' + HMACSHA1SignatureType;
  returnURL := returnURL + '&'+OAuthTimestampKey+'=' + timeStamp;
  returnURL := returnURL + '&'+OAuthTokenKey+'=' + GetOauth_token;
  returnURL := returnURL + '&'+OAuthVerifierKey+'=' + GetOauth_verifier;
  returnURL := returnURL + '&'+OAuthVersionKey+'=' + OAuthVersion;
  returnURL := returnURL + '&'+OAuthSignatureKey+'=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);

  //Example returnURL
  //https://www.flickr.com/services/oauth/access_token?
  //oauth_consumer_key=0edf6f13dc6309c822b59ae8bb783df6&
  //oauth_nonce=E596FAAF34570F692EE8EEE99AE77C93&
  //oauth_signature_method=HMAC-SHA1&
  //oauth_timestamp=1423410957&
  //oauth_token=72157650641006186-2b053586e148c2e7&
  //oauth_verifier=b50d59c25f7cf591&
  //oauth_version=1.0&
  //oauth_signature=cho7fqBJhrnXGeOFU63yFEnHC6M%3D

  result := returnURL;
end;

function TAccessToken.GetOauth_consumer_key: string;
begin
  result :=  FOauth_consumer_key;
end;

function TAccessToken.GetOauth_token: string;
begin
  result :=  FOauth_token;
end;

function TAccessToken.GetOauth_verifier: string;
begin
  result :=  FOauth_verifier;
end;

function TAccessToken.GetTokenSecret: string;
begin
  result := FTokenSecret;
end;

function TAccessToken.GetConsumerSecret: string;
begin
  result := FConsumerSecret;
end;

class function TAccessToken.New(const Oauth_verifier, Oauth_consumer_key, Oauth_token: string; ConsumerSecret : string; TokenSecret : string): IAccessToken;
begin
  Result := TAccessToken.Create(Oauth_verifier, Oauth_consumer_key, Oauth_token, ConsumerSecret, TokenSecret);
end;

procedure TAccessToken.SetOauth_consumer_key(const Value: string);
begin
  FOauth_consumer_key := value;
end;

procedure TAccessToken.SetOauth_token(const Value: string);
begin
  FOauth_token := Value;
end;

procedure TAccessToken.SetOauth_verifier(const Value: string);
begin
  FOauth_verifier := value;
end;

procedure TAccessToken.SetTokenSecret(const Value: string);
begin
  FTokenSecret := value;
end;

procedure TAccessToken.SetConsumerSecret(const Value: string);
begin
  FConsumerSecret := Value;
end;

end.
