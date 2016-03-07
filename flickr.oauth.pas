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

unit flickr.oauth;

interface

uses
  SysUtils, IdGlobal;

type
  IOAuth = interface
    function GenerateRequestTokenQuery() : string;
  end;

  TOAuth = class(TInterfacedObject, IOAuth)
  private
    FApiKey: string;
    FSecret: string;
    function GetApiKey() : string;
    function GetSecret() : string;
    procedure SetApiKey(const Value: string);
    procedure SetSecret(const Value: string);
  public
    property ApiKey : string read GetApiKey write SetApiKey;
    property Secret : string read GetSecret write SetSecret;
    class function New(const ApiKey : string; const Secret : string): IOAuth; static;
    Constructor Create(ApiKey : string; Secret : string);
    function GenerateRequestTokenQuery() : string;
  end;

const
		OAuthConsumerKeyKey = 'oauth_consumer_key';
		OAuthCallbackKey = 'oauth_callback';
		OAuthVersionKey = 'oauth_version';
		OAuthSignatureMethodKey = 'oauth_signature_method';
		OAuthSignatureKey = 'oauth_signature';
		OAuthTimestampKey = 'oauth_timestamp';
		OAuthNonceKey = 'oauth_nonce';
		OAuthTokenKey = 'oauth_token';
		OAuthTokenSecretKey = 'oauth_token_secret';
    OAuthVersion = '1.0';
    OAuthCallback = 'http%3A%2F%2Fwww.example.com';
    HMACSHA1SignatureType = 'HMAC-SHA1';
    PlainTextSignatureType = 'PLAINTEXT';
    RSASHA1SignatureType = 'RSA-SHA1';

implementation

uses
   IdHash, IdHMAC, IdHMACSHA1, Windows, IdHashMessageDigest, IdCoderMIME, IdURI, HTTPApp, flickr.signature, NetEncoding;

{ TOauth }

constructor TOAuth.Create(ApiKey, Secret: string);
begin
  SetApiKey(ApiKey);
  SetSecret(Secret);
end;

function TOauth.GetApiKey: string;
begin
  result := FApiKey;
end;

function TOauth.GetSecret: string;
begin
  result := FSecret;
end;

procedure TOauth.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TOauth.SetSecret(const Value: string);
begin
  FSecret := Value;
end;

class function TOauth.New(const ApiKey : string; const Secret : string): IOAuth;
begin
  Result := TOauth.Create(ApiKey, Secret);
end;

function TOAuth.GenerateRequestTokenQuery: string;
var
  baseURL, paramURL, encodedURL, returnURL : string;
  ConsumerSecret : string;
  TokenSecret : string;
  timeStamp : string;
begin
  baseURL := 'https://www.flickr.com/services/oauth/request_token';

  baseURL := String(TNetEncoding.URL.Encode(baseURL));
  timeStamp := TSignature.getTimeStamp();
  paramURL := OAuthCallbackKey+'=' + OAuthCallback;
  paramURL := paramURL + '&'+OAuthConsumerKeyKey+'=' + GetApiKey();
  paramURL := paramURL + '&'+OAuthNonceKey+'=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&'+OAuthSignatureMethodKey+'=' + HMACSHA1SignatureType;
  paramURL := paramURL + '&'+OAuthTimestampKey+'=' + timeStamp;
  paramURL := paramURL + '&'+OAuthVersionKey+'=' + OAuthVersion;

  paramURL := String(TNetEncoding.URL.Encode(paramURL));

  //Encode this to get the signature
  encodedURL := 'GET&' + baseURL + '&' + paramURL;

  //Example encoded URL:
  //'GET&https%3A%2F%2Fwww.flickr.com%2Fservices%2Foauth%2Frequest_token
  //&oauth_callback%3Dhttp%253A%252F%252Fwww.example.com%26
  //oauth_consumer_key%3D0edf6f13dc6309c822b59ae8bb783df6%26
  //oauth_nonce%3D1CC5A4B05607CA5378CBF06EAB2C927F%26
  //oauth_signature_method%3DHMAC-SHA1%26
  //oauth_timestamp%3D1420675285%26
  //oauth_version%3D1.0'

  ConsumerSecret := String(TNetEncoding.URL.Encode(GetSecret()));
  TokenSecret := ''; //First time is empty

  ConsumerSecret := ConsumerSecret + '&' + TokenSecret;

  returnURL := 'https://www.flickr.com/services/oauth/request_token';
  returnURL := returnURL + '?'+OAuthCallbackKey+'=' + OAuthCallback;
  returnURL := returnURL + '&'+OAuthConsumerKeyKey+'=' + GetApiKey();
  returnURL := returnURL + '&'+OAuthNonceKey+'=' + TSignature.getOAuthNonce(timeStamp);
  returnURL := returnURL + '&'+OAuthSignatureMethodKey+'=' + HMACSHA1SignatureType;
  returnURL := returnURL + '&'+OAuthTimestampKey+'=' + timeStamp;
  returnURL := returnURL + '&'+OAuthVersionKey+'=' + OAuthVersion;
  returnURL := returnURL + '&'+OAuthSignatureKey+'=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);

  //Example returnURL
  //https://www.flickr.com/services/oauth/request_token?
  //oauth_callback=http%3A%2F%2Fwww.example.com&
  //oauth_consumer_key=0edf6f13dc6309c822b59ae8bb783df6&
  //oauth_nonce=0FB8FE2EA930B94952540A52072786DB&
  //oauth_signature_method=HMAC-SHA1&
  //oauth_timestamp=1420675228&
  //oauth_version=1.0&
  //oauth_signature=4L7C8QdsvykGqhTYw7Q%2F%2Bx5g9WI%3D

  result := returnURL;
end;

end.
