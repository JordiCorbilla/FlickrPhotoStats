// Copyright (c) 2015-2016, Jordi Corbilla
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

unit flickr.lib.parse;

interface

uses
  IdHTTP, IdIOHandler, IdIOHandlerStream, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdGlobal,
  System.SysUtils, System.Variants, System.Classes;

type
  TParseAnalyticsAPI = Class(TObject)
    class function Active() : boolean;
    class function UpdateClient(id : string; photos : integer; views : integer; likes : integer; comments : integer; version: string) : boolean;
  End;

implementation

{ TParseAnalyticsAPI }

// Example parse.com
//curl -X POST \
//  -H "X-Parse-Application-Id: At6aTGG3g32CIyOo8PiGxhNagGrSp3UObhTaOv4T" \
//  -H "X-Parse-REST-API-Key: uA27SEsZOirgYWuUFzuqTHnuWm32qXKIJgWflTQ9" \
//  -H "Content-Type: application/json" \
//  -d '{
//      }' \
//  https://api.parse.com/1/events/AppOpened

class function TParseAnalyticsAPI.Active: boolean;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  JsonToSend: TStringStream;
begin
  JsonToSend := TStringStream.Create('{}');
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.CustomHeaders.Values['X-Parse-Application-Id'] := 'At6aTGG3g32CIyOo8PiGxhNagGrSp3UObhTaOv4T';
      IdHTTP.Request.CustomHeaders.Values['X-Parse-REST-API-Key'] := 'uA27SEsZOirgYWuUFzuqTHnuWm32qXKIJgWflTQ9';
      IdHTTP.Request.ContentType := 'application/json';
      response := IdHTTP.Post('https://api.parse.com/1/events/AppOpened', JsonToSend);
      response := response.Replace(AnsiChar(#10), '');
      result := (response = '{}');
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
    JsonToSend.Free;
  end;
end;

class function TParseAnalyticsAPI.UpdateClient(id: string; photos, views, likes, comments: integer; version: string): boolean;
var
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response : string;
  JsonToSend: TStringStream;
begin
  JsonToSend := TStringStream.Create('{"version":"'+version+'","Id":"'+id+'","photos":'+photos.ToString+',"views":'+views.ToString+',"likes":'+likes.ToString+',"comments":'+comments.ToString+'}');
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      IdHTTP.Request.Connection := 'Keep-Alive';
      IdIOHandler.SSLOptions.Method := sslvSSLv23;
      IdHTTP.Request.CustomHeaders.Clear;
      IdHTTP.Request.CustomHeaders.Values['X-Parse-Application-Id'] := 'At6aTGG3g32CIyOo8PiGxhNagGrSp3UObhTaOv4T';
      IdHTTP.Request.CustomHeaders.Values['X-Parse-REST-API-Key'] := 'uA27SEsZOirgYWuUFzuqTHnuWm32qXKIJgWflTQ9';
      IdHTTP.Request.ContentType := 'application/json';
      response := IdHTTP.Post('https://api.parse.com/1/classes/Instances', JsonToSend);
      response := response.Replace(AnsiChar(#10), '');
      result := (response.Contains('createdAt'));
    finally
      IdHTTP.Free;
    end;
  finally
    IdIOHandler.Free;
    JsonToSend.Free;
  end;
end;

end.
