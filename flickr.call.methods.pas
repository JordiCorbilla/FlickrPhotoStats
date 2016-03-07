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
unit flickr.call.methods;

interface

uses
  Generics.collections, flickr.lib.options.agent;

type
  ICallMethod = interface
    function getURLmethod(method : string) : string;
    function getURLmethodParams(method : string; params : TDictionary<string, string>) : string;
  end;

  TCallMethod = Class(TinterfacedObject, ICallMethod)
  private
    FApi_key : string;
    FAuth_token : string;
    FSecret : string;
    FToken_Secret : string;
  public
    class function New(optionsAgent : IOptionsAgent): ICallMethod;
    constructor Create(optionsAgent : IOptionsAgent);
    function getURLmethodParams(method : string; params : TDictionary<string, string>) : string;
    function getURLmethod(method : string) : string;
  End;

implementation

uses
  flickr.signature, HTTPApp, NetEncoding;

{ TCallMethod }

constructor TCallMethod.Create(optionsAgent : IOptionsAgent);
begin
  FApi_key := optionsAgent.flickrApiKey;
  FAuth_token := optionsAgent.userToken;
  FSecret := optionsAgent.secret;
  FToken_secret := optionsAgent.userTokenSecret;
end;

function TCallMethod.getURLmethod(method: string): string;
var
  baseURL, paramURL, encodedURL, returnURL : string;
  ConsumerSecret : string;
  TokenSecret : string;
  timeStamp : string;
begin
  //Generate request access token needs to generate:

  baseURL := 'https://api.flickr.com/services/rest';

  baseURL := String(TNetEncoding.URL.Encode(baseURL));
  timeStamp := TSignature.getTimeStamp();
  paramURL := 'format=rest';
  paramURL := paramURL + '&method=' + method;
  paramURL := paramURL + '&oauth_consumer_key=' + Fapi_key;
  paramURL := paramURL + '&oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&oauth_signature_method=HMAC-SHA1';
  paramURL := paramURL + '&oauth_timestamp=' + timeStamp;
  paramURL := paramURL + '&oauth_token=' + Fauth_token;
  paramURL := paramURL + '&oauth_version=1.0';

  paramURL := String(TNetEncoding.URL.Encode(paramURL));

  //Encode this to get the signature
  encodedURL := 'GET&' + baseURL + '&' + paramURL;

  //Example encoded URL:

  ConsumerSecret := String(TNetEncoding.URL.Encode(Fsecret));
  TokenSecret := String(TNetEncoding.URL.Encode(Ftoken_secret));

  ConsumerSecret := ConsumerSecret + '&' + TokenSecret;

  returnURL := 'https://api.flickr.com/services/rest';
  returnURL := returnURL + '?oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  returnURL := returnURL + '&format=rest';
  returnURL := returnURL + '&oauth_consumer_key=' + Fapi_key;
  returnURL := returnURL + '&oauth_timestamp=' + timeStamp;
  returnURL := returnURL + '&oauth_signature_method=HMAC-SHA1';
  returnURL := returnURL + '&oauth_version=1.0';
  returnURL := returnURL + '&oauth_token=' + Fauth_token;
  returnURL := returnURL + '&oauth_signature=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);
  returnURL := returnURL + '&method=' + method;

  result := returnURL;
end;

function TCallMethod.getURLmethodParams(method: string; params: TDictionary<string, string>): string;
var
  baseURL, paramURL, encodedURL, returnURL : string;
  ConsumerSecret : string;
  TokenSecret : string;
  timeStamp : string;
  keys : string;
begin
  //Generate request access token needs to generate:

  baseURL := 'https://api.flickr.com/services/rest';

  baseURL := String(TNetEncoding.URL.Encode(baseURL));
  timeStamp := TSignature.getTimeStamp();
  paramURL := 'format=rest';
  paramURL := paramURL + '&method=' + method;
  paramURL := paramURL + '&oauth_consumer_key=' + Fapi_key;
  paramURL := paramURL + '&oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&oauth_signature_method=HMAC-SHA1';
  paramURL := paramURL + '&oauth_timestamp=' + timeStamp;
  paramURL := paramURL + '&oauth_token=' + Fauth_token;
  paramURL := paramURL + '&oauth_version=1.0';
  for keys in params.keys do
  begin
    paramURL := paramURL + '&' + keys + '=' + params.Items[keys];
  end;

  paramURL := String(TNetEncoding.URL.Encode(paramURL));

  //Encode this to get the signature
  encodedURL := 'GET&' + baseURL + '&' + paramURL;

  //Example encoded URL:

  ConsumerSecret := String(TNetEncoding.URL.Encode(Fsecret));
  TokenSecret := String(TNetEncoding.URL.Encode(Ftoken_secret));

  ConsumerSecret := ConsumerSecret + '&' + TokenSecret;

  returnURL := 'https://api.flickr.com/services/rest';
  returnURL := returnURL + '?oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  returnURL := returnURL + '&format=rest';
  returnURL := returnURL + '&oauth_consumer_key=' + Fapi_key;
  returnURL := returnURL + '&oauth_timestamp=' + timeStamp;
  returnURL := returnURL + '&oauth_signature_method=HMAC-SHA1';
  returnURL := returnURL + '&oauth_version=1.0';
  returnURL := returnURL + '&oauth_token=' + Fauth_token;
  returnURL := returnURL + '&oauth_signature=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);
  for keys in params.keys do
  begin
    returnURL := returnURL + '&' + keys + '=' + params.Items[keys];
  end;
  returnURL := returnURL + '&method=' + method;

  result := returnURL;
end;

class function TCallMethod.New(optionsAgent : IOptionsAgent): ICallMethod;
begin
  result := TCallMethod.create(optionsAgent);
end;

end.
