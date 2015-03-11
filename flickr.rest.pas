// Copyright (c) 2014, Jordi Corbilla
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

unit flickr.rest;

interface

type
  IFlickrRest = interface
    function getFavorites(api_key: string; photo_id: string): string;
    function getInfo(api_key: string; photo_id: string): string;
    function getPhotos(api_key: string; user_id: string; page: string; per_page: string): string;
    function getPhotoSets(api_key: string; user_id: string; page: string; per_page: string): string;
    function getGroups(api_key: string; page: string; per_page: string; auth_token : string; secret : string): string;
    function getTestLogin(api_key: string; auth_token : string; secret : string; token_secret : string): string;
  end;

  TFlickrRest = class(TInterfacedObject, IFlickrRest)
  private
  public
    function getFavorites(api_key: string; photo_id: string): string;
    function getInfo(api_key: string; photo_id: string): string;
    function getPhotos(api_key: string; user_id: string; page: string; per_page: string): string;
    function getPhotoSets(api_key: string; user_id: string; page: string; per_page: string): string;
    function getGroups(api_key: string; page: string; per_page: string; auth_token : string; secret : string): string;
    function getTestLogin(api_key: string; auth_token : string; secret : string; token_secret : string): string;
    class function New(): IFlickrRest;
  end;

implementation

uses
  flickr.signature, HTTPApp;

{ TFlickrRest }

function TFlickrRest.getFavorites(api_key, photo_id: string): string;
begin
  Result := 'https://api.flickr.com/services/rest/?method=flickr.photos.getFavorites&api_key=' + api_key + '&photo_id=' + photo_id;
end;

function TFlickrRest.getGroups(api_key, page, per_page: string; auth_token : string; secret : string): string;
var
  url : string;
  signature : string;
begin
  //Generate signature
  url := secret +'api_key'+api_key+ 'auth_token' +auth_token;
  url := url + 'formatrest';
  url := url + 'methodflickr.groups.pools.getgroups';
  url := url + 'page' + page + 'per_page' + per_page;
  signature := TSignature.api_sig(url);

  //Example
  //https://api.flickr.com/services/rest/?
  //method=flickr.groups.pools.getgroups&
  //api_key=0edf6f13dc6309c822b59ae8bb783df6&
  //page=1&
  //per_page=500&
  //format=rest&
  //auth_token=72157639942921845-e4f73de08dc774e6&
  //api_sig=477fdb1c77194a6e185e0fd99da04868

  Result := 'https://api.flickr.com/services/rest/?method=flickr.groups.pools.getGroups&api_key=' + api_key + '&page=' + page + '&per_page=' + per_page + '&format=rest&auth_token=' +auth_token + '&api_sig=' + signature;
end;

function TFlickrRest.getInfo(api_key, photo_id: string): string;
begin
  Result := 'https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=' + api_key + '&photo_id=' + photo_id;
end;

function TFlickrRest.getPhotos(api_key, user_id, page, per_page: string): string;
begin
  Result := 'https://api.flickr.com/services/rest/?method=flickr.people.getPhotos&api_key=' + api_key + '&user_id=' + user_id + '&page=' + page + '&per_page=' + per_page;
end;

function TFlickrRest.getPhotoSets(api_key, user_id, page, per_page: string): string;
begin
  Result := 'https://api.flickr.com/services/rest/?method=flickr.photosets.getList&api_key=' + api_key + '&user_id=' + user_id + '&page=' + page + '&per_page=' + per_page;
end;

function TFlickrRest.getTestLogin(api_key: string; auth_token : string; secret : string; token_secret : string): string;
var
  baseURL, paramURL, encodedURL, returnURL : string;
  ConsumerSecret : string;
  TokenSecret : string;
  timeStamp : string;
begin
  //Generate request access token needs to generate:

  baseURL := 'https://api.flickr.com/services/rest';

  baseURL := String(HTTPEncode(AnsiString(baseURL)));
  timeStamp := TSignature.getTimeStamp();
  paramURL := 'format=rest';
  paramURL := paramURL + '&method=flickr.test.login';
  paramURL := paramURL + '&oauth_consumer_key=' + api_key;
  paramURL := paramURL + '&oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&oauth_signature_method=HMAC-SHA1';
  paramURL := paramURL + '&oauth_timestamp=' + timeStamp;
  paramURL := paramURL + '&oauth_token=' + auth_token;
  paramURL := paramURL + '&oauth_version=1.0';

  paramURL := String(HTTPEncode(AnsiString(paramURL)));

  //Encode this to get the signature
  encodedURL := 'GET&' + baseURL + '&' + paramURL;

  //Example encoded URL:

  ConsumerSecret := String(HTTPEncode(AnsiString(secret)));
  TokenSecret := String(HTTPEncode(AnsiString(token_secret)));

  ConsumerSecret := ConsumerSecret + '&' + TokenSecret;

  returnURL := 'https://api.flickr.com/services/rest';
  returnURL := returnURL + '?oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  returnURL := returnURL + '&format=rest';
  returnURL := returnURL + '&oauth_consumer_key=' + api_key;
  returnURL := returnURL + '&oauth_timestamp=' + timeStamp;
  returnURL := returnURL + '&oauth_signature_method=HMAC-SHA1';
  returnURL := returnURL + '&oauth_version=1.0';
  returnURL := returnURL + '&oauth_token=' + auth_token;
  returnURL := returnURL + '&oauth_signature=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);
  returnURL := returnURL + '&method=flickr.test.login';

  result := returnURL;
end;

class function TFlickrRest.New: IFlickrRest;
begin
  Result := Create;
end;

end.
