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
  SysUtils, IdHTTP, IdIOHandler, IdIOHandlerStream,
  IdIOHandlerSocket, IdIOHandlerStack, IDGlobal,
  IdSSL, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, flickr.rest,
  flickr.xml.helper;

type
  TUserInfo = class(TObject)
    class function getLocation(user_id : string; api_key: string; auth_token : string; secret : string; token_secret : string): string;
    class procedure getStreamViews(user_id : string; api_key: string; auth_token : string; secret : string; token_secret : string);
  end;

implementation

{ TUserInfo }

class function TUserInfo.getLocation(user_id : string; api_key: string; auth_token : string; secret : string; token_secret : string) : string;
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  xmlDocument: IXMLDocument;
  timedout: Boolean;
  location : string;
begin
  location := '';
  IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  IdIOHandler.ReadTimeout := IdTimeoutInfinite;
  IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
  xmlDocument := TXMLDocument.Create(nil);
  IdHTTP := TIdHTTP.Create(nil);
  try
    IdHTTP.IOHandler := IdIOHandler;
    timedout := false;
    while (not timedout) do
    begin
      try
        response := IdHTTP.Get(TFlickrRest.New().getUserInfo(api_key, user_id, auth_token, secret, token_secret));
        timedout := true;
      except
        on e: exception do
        begin
          sleep(200);
          timedout := false;
        end;
      end;
    end;

    xmlDocument.LoadFromXML(response);
    iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
    iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
    iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <person>
    iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <details>
    while iXMLRootNode4 <> nil do
    begin
      if iXMLRootNode4.NodeName = 'location' then
      begin
        location := TXMLHelper.new(iXMLRootNode4.NodeValue).getString;
        break;
      end;
      iXMLRootNode4 := iXMLRootNode4.NextSibling;
    end;
  finally
    IdIOHandler.Free;
    IdHTTP.Free;
    xmlDocument := nil;
  end;

  result := location;
end;

class procedure TUserInfo.getStreamViews(user_id : string; api_key: string; auth_token : string; secret : string; token_secret : string);
begin

end;

end.
