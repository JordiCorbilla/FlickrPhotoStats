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

unit flickr.rest;

interface

type
  IFlickrRest = interface
    function getFavorites(api_key: string; photo_id: string): string;
    function getInfo(api_key: string; photo_id: string): string;
    function getAllContexts(api_key: string; photo_id: string): string;
    function getPhotos(api_key: string; user_id: string; page: string; per_page: string): string;
    function getPhotoSets(api_key: string; user_id: string; page: string; per_page: string): string;
    function getGroups(api_key: string; page: string; per_page: string; auth_token : string; secret : string; token_secret : string): string;
    function getTestLogin(api_key: string; auth_token : string; secret : string; token_secret : string): string;
    function getPoolsAdd(api_key: string; auth_token : string; secret : string; token_secret : string; photoId : string; groupId : string): string;
    function getPhotoSetsAdd(api_key: string; auth_token : string; secret : string; token_secret : string; photoId : string; photosetId : string): string;
  end;

  TFlickrRest = class(TInterfacedObject, IFlickrRest)
  private
  public
    function getFavorites(api_key: string; photo_id: string): string;
    function getInfo(api_key: string; photo_id: string): string;
    function getAllContexts(api_key: string; photo_id: string): string;
    function getPhotos(api_key: string; user_id: string; page: string; per_page: string): string;
    function getPhotoSets(api_key: string; user_id: string; page: string; per_page: string): string;
    function getGroups(api_key: string; page: string; per_page: string; auth_token : string; secret : string; token_secret : string): string;
    function getTestLogin(api_key: string; auth_token : string; secret : string; token_secret : string): string;
    function getPoolsAdd(api_key: string; auth_token : string; secret : string; token_secret : string; photoId : string; groupId : string): string;
    function getPhotoSetsAdd(api_key: string; auth_token : string; secret : string; token_secret : string; photoId : string; photosetId : string): string;
    class function New(): IFlickrRest;
  end;

implementation

uses
  flickr.signature, HTTPApp, flickr.call.methods, generics.collections, SysUtils;

{ TFlickrRest }

function TFlickrRest.getAllContexts(api_key, photo_id: string): string;
begin
  //Example respone
  //<?xml version="1.0" encoding="utf-8" ?>
  //<rsp stat="ok">
  //  <set title="Jovanka Book 2015" id="72157651649699422" primary="16773770727" secret="0f76e44e9b" server="7608" farm="8" view_count="76" comment_count="0" count_photo="28" count_video="0" />
  //  <set title="Girona 2015" id="72157651574426626" primary="16348853954" secret="9808817507" server="8712" farm="9" view_count="58" comment_count="0" count_photo="28" count_video="0" />
  //  <pool title="girls" url="/groups/14845145@N00/pool/" id="14845145@N00" iconserver="0" iconfarm="0" members="7667" pool_count="160981" />
  //  <pool title="The Photographer's Club" url="/groups/photographers-club/pool/" id="15166189@N00" iconserver="1" iconfarm="1" members="9814" pool_count="456459" />
  //  <pool title="Emotions" url="/groups/emo/pool/" id="16141209@N00" iconserver="3" iconfarm="1" members="29563" pool_count="490116" />
  //  <pool title="Amateur Digital Photography/Help and Ideas" url="/groups/racedog/pool/" id="26058684@N00" iconserver="2" iconfarm="1" members="16876" pool_count="545297" />
  //  <pool title="35mm - Photography" url="/groups/35mmphotography/pool/" id="27044956@N00" iconserver="0" iconfarm="0" members="2349" pool_count="70519" />
  //  <pool title="Portraiture Photography" url="/groups/potrait/pool/" id="29496069@N00" iconserver="2" iconfarm="1" members="50360" pool_count="1322308" />
  //  <pool title="Autoportrait" url="/groups/autoportrait/pool/" id="30042135@N00" iconserver="5559" iconfarm="6" members="11015" pool_count="158579" />
  //  <pool title="Experimental Dream" url="/groups/experimental_dream/pool/" id="32002189@N00" iconserver="4" iconfarm="1" members="8606" pool_count="204859" />
  //</xml>

  Result := 'https://api.flickr.com/services/rest/?method=flickr.photos.getAllContexts&api_key=' + api_key + '&photo_id=' + photo_id;
end;

function TFlickrRest.getFavorites(api_key, photo_id: string): string;
begin
  Result := 'https://api.flickr.com/services/rest/?method=flickr.photos.getFavorites&api_key=' + api_key + '&photo_id=' + photo_id;
end;

function TFlickrRest.getGroups(api_key, page, per_page: string; auth_token : string; secret : string; token_secret : string): string;
var
  params : TDictionary<string, string>;
begin
  params := TDictionary<String, String>.create;
  params.Add('page', page);
  params.Add('per_page', per_page);
  Result := TCallMethod.New(api_key, auth_token, secret, token_secret).getURLmethodParams('flickr.groups.pools.getGroups', params);
  params.Free;
end;

function TFlickrRest.getInfo(api_key, photo_id: string): string;
begin
  //Example response:
  //<?xml version="1.0" encoding="utf-8" ?>
  //<rsp stat="ok">
  //  <photo id="16964370019" secret="eb030cd3af" server="7659" farm="8" dateuploaded="1429047216" isfavorite="0" license="0" safety_level="0" rotation="0" originalsecret="a0c335bb0f" originalformat="jpg" views="9024" media="photo">
  //    <owner nsid="96100496@N05" username="Jordi Corbilla Photography" realname="Jordi Corbilla" location="London, United Kingdom" iconserver="3894" iconfarm="4" path_alias="jordicorbillaphotography" />
  //    <title>Portrait in the forest.</title>
  //    <description>One of those portraits that won't let you indifferent. The sun was going down and it was casting a harsh light on the background. I had to push the camera a bit to get the correct exposure and get the model sharp. The results are great as usual.
  //
  //&lt;b&gt;Thank you all for your appreciation.&lt;/b&gt;
  //
  //Follow me on:
  //&lt;a href=&quot;https://www.facebook.com/JordiCorbillaPhotography&quot; rel=&quot;nofollow&quot;&gt;Facebook&lt;/a&gt;
  //&lt;a href=&quot;https://500px.com/JordiCorbillaPhotography&quot; rel=&quot;nofollow&quot;&gt;500px&lt;/a&gt;
  //&lt;a href=&quot;http://www.viewbug.com/member/JordiCorbillaPhotography&quot; rel=&quot;nofollow&quot;&gt;Viewbug&lt;/a&gt;
  //&lt;a href=&quot;https://ello.co/jordicorbillaphotography&quot; rel=&quot;nofollow&quot;&gt;ello&lt;/a&gt;
  //&lt;a href=&quot;http://instagram.com/thunderjordi&quot; rel=&quot;nofollow&quot;&gt;Instragram&lt;/a&gt;
  //&lt;a href=&quot;https://www.flickr.com/photos/jordicorbillaphotography&quot;&gt;flickr&lt;/a&gt;
  //
  //
  //&lt;b&gt;© 2015 Jordi Corbilla - All Rights Reserved.
  //Do not use any of my images
  //without permission.&lt;/b&gt;</description>
  //    <visibility ispublic="1" isfriend="0" isfamily="0" />
  //    <dates posted="1429047216" taken="2015-04-02 18:29:55" takengranularity="0" takenunknown="0" lastupdate="1429444762" />
  //    <permissions permcomment="3" permaddmeta="2" />
  //    <editability cancomment="1" canaddmeta="1" />
  //    <publiceditability cancomment="1" canaddmeta="0" />
  //    <usage candownload="1" canblog="1" canprint="1" canshare="1" />
  //    <comments>26</comments>
  //    <notes />
  //    <people haspeople="1" />
  //    <tags>
  //      <tag id="96095156-16964370019-2854" author="96100496@N05" authorname="Jordi Corbilla Photography" raw="model" machine_tag="0">model</tag>
  //      <tag id="96095156-16964370019-19621" author="96100496@N05" authorname="Jordi Corbilla Photography" raw="brazilian" machine_tag="0">brazilian</tag>
  //      <tag id="96095156-16964370019-2994" author="96100496@N05" authorname="Jordi Corbilla Photography" raw="nikon" machine_tag="0">nikon</tag>
  //      <tag id="96095156-16964370019-20596702" author="96100496@N05" authorname="Jordi Corbilla Photography" raw="D7000" machine_tag="0">d7000</tag>
  //      <tag id="96095156-16964370019-7974" author="96100496@N05" authorname="Jordi Corbilla Photography" raw="85mm" machine_tag="0">85mm</tag>
  //    </tags>
  //    <location latitude="41.987391" longitude="2.816448" accuracy="16" context="0" place_id="KFKinoNTW7kMYmzQyg" woeid="29369505">
  //      <locality place_id="KFKinoNTW7kMYmzQyg" woeid="29369505">Germans Sabat</locality>
  //      <county place_id="SDvoojFQULyclWg1oQ" woeid="12602125">Gerona</county>
  //      <region place_id="buY2USBQUL8KB8CtqQ" woeid="12578034">Catalonia</region>
  //      <country place_id="oVKj1ohTUb5V1poSBg" woeid="23424950">Spain</country>
  //    </location>
  //    <geoperms ispublic="0" iscontact="1" isfriend="0" isfamily="0" />
  //    <urls>
  //      <url type="photopage">https://www.flickr.com/photos/jordicorbillaphotography/16964370019/</url>
  //    </urls>
  //  </photo>
  //</rsp>

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

function TFlickrRest.getPhotoSetsAdd(api_key, auth_token, secret, token_secret, photoId, photosetId: string): string;
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
  photosetId := photosetId.Replace('@', '%40');
  paramURL := paramURL + '&method=flickr.photosets.addPhoto';
  paramURL := paramURL + '&oauth_consumer_key=' + api_key;
  paramURL := paramURL + '&oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&oauth_signature_method=HMAC-SHA1';
  paramURL := paramURL + '&oauth_timestamp=' + timeStamp;
  paramURL := paramURL + '&oauth_token=' + auth_token;
  paramURL := paramURL + '&oauth_version=1.0';
  paramURL := paramURL + '&photo_id=' + photoId;
  paramURL := paramURL + '&photoset_id=' + photosetId;

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
  returnURL := returnURL + '&photoset_id=' + photosetId;
  returnURL := returnURL + '&photo_id=' + photoId;
  returnURL := returnURL + '&oauth_consumer_key=' + api_key;
  returnURL := returnURL + '&oauth_timestamp=' + timeStamp;
  returnURL := returnURL + '&oauth_signature_method=HMAC-SHA1';
  returnURL := returnURL + '&oauth_version=1.0';
  returnURL := returnURL + '&oauth_token=' + auth_token;
  returnURL := returnURL + '&oauth_signature=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);
  returnURL := returnURL + '&method=flickr.photosets.addPhoto';

  result := returnURL;
end;

function TFlickrRest.getPoolsAdd(api_key, auth_token, secret, token_secret, photoId, groupId: string): string;
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
  groupId := groupId.Replace('@', '%40');
  paramURL := paramURL + '&group_id=' + groupId;
  paramURL := paramURL + '&method=flickr.groups.pools.add';
  paramURL := paramURL + '&oauth_consumer_key=' + api_key;
  paramURL := paramURL + '&oauth_nonce=' + TSignature.getOAuthNonce(timeStamp);
  paramURL := paramURL + '&oauth_signature_method=HMAC-SHA1';
  paramURL := paramURL + '&oauth_timestamp=' + timeStamp;
  paramURL := paramURL + '&oauth_token=' + auth_token;
  paramURL := paramURL + '&oauth_version=1.0';
  paramURL := paramURL + '&photo_id=' + photoId;

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
  returnURL := returnURL + '&group_id=' + groupId;
  returnURL := returnURL + '&photo_id=' + photoId;
  returnURL := returnURL + '&oauth_consumer_key=' + api_key;
  returnURL := returnURL + '&oauth_timestamp=' + timeStamp;
  returnURL := returnURL + '&oauth_signature_method=HMAC-SHA1';
  returnURL := returnURL + '&oauth_version=1.0';
  returnURL := returnURL + '&oauth_token=' + auth_token;
  returnURL := returnURL + '&oauth_signature=' + TSignature.getOAuthSignature(encodedURL, ConsumerSecret);
  returnURL := returnURL + '&method=flickr.groups.pools.add';

  result := returnURL;
end;

function TFlickrRest.getTestLogin(api_key: string; auth_token : string; secret : string; token_secret : string): string;
begin
  result := TCallMethod.New(api_key, auth_token, secret, token_secret).getURLmethod('flickr.test.login');
end;

class function TFlickrRest.New: IFlickrRest;
begin
  Result := Create;
end;

end.
