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

unit flickr.lib.email;

interface

uses
  SysUtils, IdMessage, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient,
  IdSMTPBase, IdSMTP, IdText, System.Classes;

type
  TFlickrEmail = class(TObject)
    class procedure Send(toAddress : string; text: string);
    class procedure SendHTML(toAddress : string; text: TStrings);
  end;

implementation

uses
  System.IniFiles, flickr.lib.options.email;

{ TFlickrEmail }

class procedure TFlickrEmail.Send(toAddress : string; text: string);
var
  IdSMTP1: TIdSMTP;
  IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
  IdMessage1: TIdMessage;
  options : IOptionsEmail;
begin
  options := TOptionsEmail.New.Load;
  IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  IdSMTP1 := TIdSMTP.Create(nil);
  IdMessage1 := TIdMessage.Create(nil);
  try
    IdSSLIOHandlerSocketOpenSSL1.Destination := options.Server + ':' + options.Port.ToString;
    IdSSLIOHandlerSocketOpenSSL1.Host := options.Server;
    IdSSLIOHandlerSocketOpenSSL1.Port := options.Port;
    IdSMTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;
    IdSMTP1.Host := options.Server;
    IdSMTP1.Password := options.Password;
    IDSMTP1.Port := options.Port;
    IdSMTP1.Username := options.User;
    IdSMTP1.UseTLS := utUseExplicitTLS;

    IdSSLIOHandlerSocketOpenSSL1.Destination :=  options.Server + ':' + options.Port.ToString;
    IdSSLIOHandlerSocketOpenSSL1.Host := options.Server;
    IdSSLIOHandlerSocketOpenSSL1.Port := IDSMTP1.Port;

    IdSMTP1.Connect;
    IdMessage1.From.Address := options.User;
    IdMessage1.Recipients.EMailAddresses := toAddress;
    IdMessage1.Subject := 'This is your Flickr Analytics update';
    IdMessage1.Body.Text := text;
    IdSMTP1.Send(IdMessage1);
    IdSMTP1.Disconnect;
  finally
    IdSSLIOHandlerSocketOpenSSL1.free;
    IdSMTP1.free;
    IdMessage1.free;
  end;
end;

class procedure TFlickrEmail.SendHTML(toAddress: string; text: TStrings);
var
  IdSMTP1: TIdSMTP;
  IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
  IdMessage1: TIdMessage;
  htmtext : TIdText;
  options : IOptionsEmail;
begin
  options := TOptionsEmail.New.Load;
  IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  IdSMTP1 := TIdSMTP.Create(nil);
  IdMessage1 := TIdMessage.Create(nil);
  try
    IdSSLIOHandlerSocketOpenSSL1.Destination := options.Server + ':' + options.Port.ToString;
    IdSSLIOHandlerSocketOpenSSL1.Host := options.Server;
    IdSSLIOHandlerSocketOpenSSL1.Port := options.Port;
    IdSMTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;
    IdSMTP1.Host := options.Server;
    IdSMTP1.Password := options.Password;
    IDSMTP1.Port := options.Port;
    IdSMTP1.Username := options.User;
    IdSMTP1.UseTLS := utUseExplicitTLS;

    IdSSLIOHandlerSocketOpenSSL1.Destination :=  options.Server + ':' + options.Port.ToString;
    IdSSLIOHandlerSocketOpenSSL1.Host := options.Server;
    IdSSLIOHandlerSocketOpenSSL1.Port := IDSMTP1.Port;

    IdSMTP1.Connect;

    IdMessage1.From.Address := options.User;
    IdMessage1.Recipients.EMailAddresses := toAddress;
    IdMessage1.Subject := 'This is your Flickr Analytics update';

    htmtext := TIdText.Create(IdMessage1.MessageParts, text);
    htmtext.ContentType := 'text/html';

    IdSMTP1.Send(IdMessage1);
    IdSMTP1.Disconnect;
  finally
    IdSSLIOHandlerSocketOpenSSL1.free;
    IdSMTP1.free;
    IdMessage1.free;
  end;
end;

end.
