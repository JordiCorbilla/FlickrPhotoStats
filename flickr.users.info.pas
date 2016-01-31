// Copyright (c) 2016, Jordi Corbilla
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

unit flickr.users.info;

interface

uses
  SysUtils, XMLIntf, flickr.rest,
  flickr.xml.helper, flickr.lib.options.agent;

type
  TUserInfo = class(TObject)
    class function getLocation(user_id : string; optionsAgent : IOptionsAgent): string;
    class function getStreamViews(user_id : string; optionsAgent : IOptionsAgent) : integer;
  end;

implementation

uses
  flickr.http.lib;

{ TUserInfo }

class function TUserInfo.getLocation(user_id : string; optionsAgent : IOptionsAgent) : string;
var
  iXMLRootNode4: IXMLNode;
  location : string;
begin
  location := '';
  THttpRest.Post(TFlickrRest.New(optionsAgent).getUserInfo(user_id), procedure (iXMLRootNode : IXMLNode)
    begin
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <details>
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'location' then
        begin
          location := TXMLHelper.new(iXMLRootNode4.NodeValue).getString;
          break;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);
  result := location;
end;

class function TUserInfo.getStreamViews(user_id : string; optionsAgent : IOptionsAgent) : integer;
var
  iXMLRootNode4, iXMLRootNode5: IXMLNode;
  ifbreak: Boolean;
  views : integer;
begin
  views := 0;
  THttpRest.Post(TFlickrRest.New(optionsAgent).getUserInfo(user_id), procedure (iXMLRootNode : IXMLNode)
    begin
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <details>
      ifbreak := false;
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photos' then
        begin
          iXMLRootNode5 := iXMLRootNode4.ChildNodes.First;
          while iXMLRootNode5 <> nil do
          begin
            if iXMLRootNode5.NodeName = 'views' then
            begin
              views := TXMLHelper.new(iXMLRootNode5.NodeValue).getInt;
              ifbreak := true;
              break;
            end;
            iXMLRootNode5 := iXMLRootNode5.NextSibling;
          end;
        end;
        if ifbreak then
          break;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);
  result := views;
end;

end.
