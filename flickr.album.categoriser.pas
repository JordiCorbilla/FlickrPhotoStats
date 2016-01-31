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

unit flickr.album.categoriser;

interface

uses
  flickr.repository, flickr.lib.options, SysUtils, flickr.photos, flickr.rest, flickr.lib.options.agent, flickr.lib.response;

type
  TLogProcedure = reference to procedure(value : string);

  TAlbumCategoriser = Class(TObject)
    class procedure AutoAdd(repository: IFlickrRepository; options: IOptions; optionsEmail: IOptionsAgent; log : TLogProcedure);
  End;

implementation

uses
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, IdIOHandler, IdIOHandlerStream, IdIOHandlerSocket, IdIOHandlerStack,
  IdSSL, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, IdGlobal;

{ TAlbumCategoriser }

class procedure TAlbumCategoriser.AutoAdd(repository: IFlickrRepository; options: IOptions; optionsEmail: IOptionsAgent; log : TLogProcedure);
var
  i, j: integer;
  photo: IPhoto;
  value: string;
  urlAdd: string;
  timedout: boolean;
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  response: string;
begin
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    IdHTTP.IOHandler := IdIOHandler;

    for i := 0 to repository.photos.Count - 1 do
    begin
      for j := 0 to options.AlbumViewsID.Count - 1 do
      begin
        photo := repository.photos[i];
        value := options.AlbumViews[j].Replace('.', '');
        if (photo.getTotalViewsDay() >= value.ToInteger) then
        begin
          if not photo.inAlbum(options.AlbumViewsID[j]) then
          begin
            urlAdd := TFlickrRest.New().getPhotoSetsAdd(optionsEmail.flickrApiKey, optionsEmail.userToken, optionsEmail.secret, optionsEmail.userTokenSecret, photo.Id, options.AlbumViewsID[j]);
            timedout := false;
            while (not timedout) do
            begin
              try
                response := IdHTTP.Get(urlAdd);
                response := TResponse.filter(response);
                log(response + ' ' + photo.Title + ' -> ' + value);
                timedout := true;
              except
                on e: exception do
                begin
                  sleep(200);
                  timedout := false;
                end;
              end;
            end;
          end;
        end;
      end;

      for j := 0 to options.AlbumLikesID.Count - 1 do
      begin
        photo := repository.photos[i];
        value := options.AlbumLikes[j].Replace('.', '');
        if (photo.getTotalLikesDay() >= value.ToInteger) then
        begin
          if not photo.inAlbum(options.AlbumLikesID[j]) then
          begin
            urlAdd := TFlickrRest.New().getPhotoSetsAdd(optionsEmail.flickrApiKey, optionsEmail.userToken, optionsEmail.secret, optionsEmail.userTokenSecret, photo.Id, options.AlbumLikesID[j]);
            timedout := false;
            while (not timedout) do
            begin
              try
                response := IdHTTP.Get(urlAdd);
                response := TResponse.filter(response);
                log(response + ' ' + photo.Title + ' -> ' + value);
                timedout := true;
              except
                on e: exception do
                begin
                  sleep(200);
                  timedout := false;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    IdIOHandler.Free;
    IdHTTP.Free;
  end;
end;

end.
