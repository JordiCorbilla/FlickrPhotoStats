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
  IdSMTPBase, IdSMTP;

type
  TFlickrEmail = class(TObject)
    class procedure Send(toAddress : string; text: string);
    class procedure LoadOptions(var server : string; var port : string; var user : string; var password : string);
  end;

implementation

uses
  System.IniFiles;

{ TFlickrEmail }

class procedure TFlickrEmail.LoadOptions(var server, port, user, password: string);
var
  inifile : Tinifile;
begin
  inifile := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'FlickrAnalyticsEmail.ini');
  try
    server := inifile.ReadString('System', 'Server', 'smtp.gmail.com');
    port := inifile.ReadString('System', 'Port', '587');
    user := inifile.ReadString('System', 'user', 'flickrphotoanalytics@gmail.com');
    password := inifile.ReadString('System', 'Password', '');

  finally
    inifile.Free;
  end;
end;

class procedure TFlickrEmail.Send(toAddress : string; text: string);
var
  IdSMTP1: TIdSMTP;
  IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
  IdMessage1: TIdMessage;
  Server, Port, User, Password : string;
begin
  LoadOptions(Server, Port, User, Password);
  IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  IdSMTP1 := TIdSMTP.Create(nil);
  IdMessage1 := TIdMessage.Create(nil);
  try
    IdSSLIOHandlerSocketOpenSSL1.Destination := Server + ':' + Port;
    IdSSLIOHandlerSocketOpenSSL1.Host := Server;
    IdSSLIOHandlerSocketOpenSSL1.Port := Port.ToInteger;
    IdSMTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;
    IdSMTP1.Host := Server;
    IdSMTP1.Password := Password;
    IDSMTP1.Port := Port.ToInteger;
    IdSMTP1.Username := User;
    IdSMTP1.UseTLS := utUseExplicitTLS;

    IdSSLIOHandlerSocketOpenSSL1.Destination :=  Server + ':' + Port;
    IdSSLIOHandlerSocketOpenSSL1.Host := Server;
    IdSSLIOHandlerSocketOpenSSL1.Port := IDSMTP1.Port;

    IdSMTP1.Connect;
    IdMessage1.From.Address := User;
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

end.
