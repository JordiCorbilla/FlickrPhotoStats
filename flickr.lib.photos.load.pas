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

unit flickr.lib.photos.load;

interface

uses
  Generics.collections, XMLIntf,
  flickr.rest, System.SysUtils, flickr.lib.options.agent;

type
  TPhotoLoader = class(TObject)
    class function load(optionsAgent : IOptionsAgent): TList<String>;
  end;

implementation

uses
  flickr.http.lib;

{ TPhotoLoader }

class function TPhotoLoader.load(optionsAgent : IOptionsAgent): TList<String>;
var
  iXMLRootNode4: IXMLNode;
  pages, title, id, ispublic, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  PhotoList: TList<String>;
begin
  PhotoList := TList<string>.Create();
  THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotos('1', '500'), procedure (iXMLRootNode : IXMLNode)
    begin
      pages := iXMLRootNode.attributes['pages'];
      total := iXMLRootNode.attributes['total'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photo>
      numTotal := total.ToInteger();
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photo' then
        begin
          id := iXMLRootNode4.attributes['id'];
          ispublic := iXMLRootNode4.attributes['ispublic'];
          title := iXMLRootNode4.attributes['title'];
          if ispublic = '1' then
          begin
            PhotoList.Add(id);
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

  // Load the remaining pages
  numPages := pages.ToInteger;
  for i := 2 to numPages do
  begin
    THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotos(i.ToString, '500'), procedure (iXMLRootNode : IXMLNode)
    begin
      pages := iXMLRootNode.attributes['pages'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photo>
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photo' then
        begin
          id := iXMLRootNode4.attributes['id'];
          ispublic := iXMLRootNode4.attributes['ispublic'];
          title := iXMLRootNode4.attributes['title'];
          if ispublic = '1' then
          begin
            PhotoList.Add(id);
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);
  end;

  result := PhotoList;
end;

end.
