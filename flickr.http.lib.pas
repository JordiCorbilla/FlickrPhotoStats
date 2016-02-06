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

unit flickr.http.lib;

interface

uses
  XMLDoc, xmldom, XMLIntf, msxmldom, IdHTTP, IdIOHandler, IdIOHandlerStream,
  IdIOHandlerSocket, IdIOHandlerStack, IDGlobal, Activex,
  IdSSL, IdSSLOpenSSL, flickr.lib.procedures;

type
  TNodeProcedure = reference to procedure(iXMLRootNode: IXMLNode);

  THttpRest = class(TObject)
    class procedure Post(getString : string; node : TNodeProcedure); overload;
    class procedure Post(getString : string; messageFilter : boolean; responseFilter : boolean; node : TNodeProcedure); overload;
    class procedure Post(getString : string; arguments : string; log : TLogProcedure) overload;
  end;

implementation

uses
  SysUtils, flickr.lib.response, flickr.lib.logging;

{ THttpRest }

class procedure THttpRest.Post(getString: string; node: TNodeProcedure);
begin
  Post(getString, false, false, node);
end;

class procedure THttpRest.Post(getString: string; messageFilter, responseFilter: boolean; node: TNodeProcedure);
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3: IXMLNode;
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  xmlDocument: IXMLDocument;
  timedout: Boolean;
  initialResponse : string;
  i: Integer;
begin
  CoInitialize(nil);
  try
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
          response := IdHTTP.Get(getString);
          timedout := true;
        except
          on e: exception do
          begin
            sleep(200);
            timedout := false;
            if messageFilter then
              if e.Message.ToLower.Contains('unauthorized') then
                timedout := true;
          end;
        end;
      end;
      //found in one of the xml
      initialResponse := response;
      response := response.Replace('’', '');
      for i := 0 to 31 do
        response := response.Replace(Char(i), '');
      //response := response.Replace(Char(10), '');
      //response := response.Replace(Char(9), '');
      response := response.Replace('&gt;', '');
      response := response.Replace('&lt;', '');
      response := response.Replace('gt;', '');
      response := response.Replace('lt;', '');
      response := response.Replace('“', '');
      response := response.Replace('”', '');
      response := response.Replace('/B&amp;', '');
      response := response.Replace('B&amp;', '');
      response := response.Replace('/b&amp;', '');
      response := response.Replace('b&amp;', '');
      response := response.Replace('&amp;', '');

      // found in the xml
      if responseFilter then
      begin
        if response.Contains('<rsp stat="ok">') then
        begin
          xmlDocument.LoadFromXML(response);
          iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
          iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
          iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photo>
          node(iXMLRootNode3);
        end;
      end
      else
      begin
        try
          xmlDocument.LoadFromXML(response);
        except
          TLogger.LogFile(initialResponse);
          TLogger.LogFile(response);
        end;
        iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
        iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
        iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photo>
        node(iXMLRootNode3);
      end;
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
      xmlDocument := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

class procedure THttpRest.Post(getString : string; arguments : string; log : TLogProcedure);
var
  response: string;
  IdHTTP: TIdHTTP;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  timedout: Boolean;
begin
  CoInitialize(nil);
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      timedout := false;
      while (not timedout) do
      begin
        try
          response := IdHTTP.Get(getString);
          response := TResponse.filter(response);
          log(response + ' ' + arguments);
          timedout := true;
        except
          on e: exception do
          begin
            sleep(200);
            timedout := false;
          end;
        end;
      end;
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
    end;
  finally
    CoUninitialize;
  end;
end;

end.
