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

unit flickr.signature;

interface

uses
   IdHash, IdHMAC, IdHMACSHA1, Windows, IdHashMessageDigest, IdCoderMIME, IdURI, HTTPApp, SysUtils, IdGlobal, NetEncoding;

type
  TSignature = class(TObject)
    class function getTimeStamp(): string;
    class function DateTimeToUnixEpoch(date: TDateTime): Longint;
    class function getOAuthNonce(timeStamp: string): string; static;
    class function Base64Encode(const Input: TIdBytes): string; static;
    class function EncryptHMACSha1(Input, AKey: string): TIdBytes; static;
    class function getOAuthSignature(encodedURL, consumerTokenSecret: string): string; static;
    class function api_sig(url : string): string;
  end;

const
    UnixDateTimeStamps : TDateTime = 25569; //25569, which is the number of days between 1 January 1900 and 1 January 1970

implementation

{ TSignature }

class function TSignature.getTimeStamp: string;
begin
  Result := IntToStr(DateTimeToUnixEpoch(Now));
end;

class function TSignature.DateTimeToUnixEpoch(date: TDateTime): Longint;
var
  timeZone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(timeZone); //this returns UTC format
  date := date + (timeZone.Bias / 1440);
  Result := Trunc((date - UnixDateTimeStamps) * 86400);
end;

class function TSignature.getOAuthNonce(timeStamp : string): string;
var
  md5: TIdHashMessageDigest;
begin
  md5 := TIdHashMessageDigest5.Create;
  Result := md5.HashStringAsHex(timeStamp);
  md5.Free;
end;

class function TSignature.api_sig(url: string): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := idmd5.HashStringAsHex(url,IndyTextEncoding_OSDefault()).ToLower;
  finally
    idmd5.Free;
  end;
end;

class function TSignature.Base64Encode(const Input: TIdBytes): string;
begin
  result := String(TNetEncoding.URL.Encode(TIdEncoderMIME.EncodeBytes(Input)));
end;

class function TSignature.EncryptHMACSha1(Input, AKey: string): TIdBytes;
var
  hmacSha1 : TIdHMACSHA1;
begin
  hmacSha1 := TIdHMACSHA1.Create;
  hmacSha1.Key := IndyTextEncoding_UTF8.GetBytes(AKey);
  Result := hmacSha1.HashValue(IndyTextEncoding_UTF8.GetBytes(Input));
  hmacSha1.Free;
end;

class function TSignature.getOAuthSignature(encodedURL : string; consumerTokenSecret : string): string;
begin
  Result := Base64Encode(EncryptHMACSha1(encodedURL, consumerTokenSecret));
end;

end.
