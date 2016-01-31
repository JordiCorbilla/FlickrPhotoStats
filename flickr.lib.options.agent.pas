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

unit flickr.lib.options.agent;

interface

type
  IOptionsAgent = interface
    procedure Setpassword(const Value: string);
    procedure Setport(const Value: integer);
    procedure Setserver(const Value: string);
    procedure Setuser(const Value: string);
    procedure SetuserToken(const Value: string);
    procedure SetuserTokenSecret(const Value: string);
    procedure SetflickrApiKey(const Value: string);
    procedure Setsecret(const Value: string);
    function Getserver() : string;
    function Getport() : integer;
    function Getuser() : string;
    function Getpassword() : string;
    function GetuserToken() : string;
    function GetuserTokenSecret() : string;
    function GetflickrApiKey() : string;
    function Getsecret() : string;
    function GetflickrUserId() : string;
    procedure SetflickrUserId(const Value: string);
    procedure SetAppId(const Value: string);
    function GetAppId() : string;
    property server : string read Getserver write Setserver;
    property port : integer read Getport write Setport;
    property user : string read Getuser write Setuser;
    property password : string read Getpassword write Setpassword;
    property userToken : string read GetuserToken write SetuserToken;
    property userTokenSecret : string read GetuserTokenSecret write SetuserTokenSecret;
    property flickrApiKey : string read GetflickrApiKey write SetflickrApiKey;
    property secret : string read Getsecret write Setsecret;
    property flickrUserId : string read GetflickrUserId write SetflickrUserId;
    property AppId : string read GetAppId write SetAppId;
    function Load() : IOptionsAgent;
    procedure Save();
  end;

  TOptionsAgent = Class(TInterfacedObject, IOptionsAgent)
  private
    FuserToken: string;
    Fport: integer;
    Fpassword: string;
    Fuser: string;
    FuserTokenSecret: string;
    Fserver: string;
    Fsecret: string;
    FflickrApiKey: string;
    FflickrUserId: string;
    FAppId: string;
    procedure Setpassword(const Value: string);
    procedure Setport(const Value: integer);
    procedure Setserver(const Value: string);
    procedure Setuser(const Value: string);
    procedure SetuserToken(const Value: string);
    procedure SetuserTokenSecret(const Value: string);
    procedure SetflickrApiKey(const Value: string);
    procedure Setsecret(const Value: string);
    function Getserver() : string;
    function Getport() : integer;
    function Getuser() : string;
    function Getpassword() : string;
    function GetuserToken() : string;
    function GetuserTokenSecret() : string;
    function GetflickrApiKey() : string;
    function Getsecret() : string;
    function GetflickrUserId() : string;
    procedure SetflickrUserId(const Value: string);
    procedure SetAppId(const Value: string);
    function GetAppId() : string;
  public
    property server : string read Getserver write Setserver;
    property port : integer read Getport write Setport;
    property user : string read Getuser write Setuser;
    property password : string read Getpassword write Setpassword;
    property userToken : string read GetuserToken write SetuserToken;
    property userTokenSecret : string read GetuserTokenSecret write SetuserTokenSecret;
    property flickrApiKey : string read GetflickrApiKey write SetflickrApiKey;
    property secret : string read Getsecret write Setsecret;
    property flickrUserId : string read GetflickrUserId write SetflickrUserId;
    property AppId : string read GetAppId write SetAppId;
    class function New(): IOptionsAgent;
    function Load() : IOptionsAgent;
    procedure Save();
  End;

implementation

uses
  System.inifiles, SysUtils, flickr.lib.encoding;

{ TOptionsEmail }

function TOptionsAgent.GetAppId: string;
begin
  result := FAppId;
end;

function TOptionsAgent.GetflickrApiKey: string;
begin
  result := FflickrApiKey;
end;

function TOptionsAgent.GetflickrUserId: string;
begin
  result := FflickrUserId;
end;

function TOptionsAgent.Getpassword: string;
begin
  result := FPassword;
end;

function TOptionsAgent.Getport: integer;
begin
  result := FPort;
end;

function TOptionsAgent.Getsecret: string;
begin
  result := Fsecret;
end;

function TOptionsAgent.Getserver: string;
begin
  result := FServer;
end;

function TOptionsAgent.Getuser: string;
begin
  result := FUser;
end;

function TOptionsAgent.GetuserToken: string;
begin
  result := FUserToken;
end;

function TOptionsAgent.GetuserTokenSecret: string;
begin
  result := FUserTokenSecret;
end;

function TOptionsAgent.Load: IOptionsAgent;
var
  inifile : Tinifile;
begin
  inifile := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'FlickrAnalyticsAgent.ini');
  try
    FuserToken := THelper.Decode(inifile.ReadString('System', 'UserToken', ''));
    Fport := inifile.ReadInteger('System', 'Port', 0);
    Fpassword := THelper.Decode(inifile.ReadString('System', 'Password', ''));
    Fuser := THelper.Decode(inifile.ReadString('System', 'User', ''));
    FuserTokenSecret := THelper.Decode(inifile.ReadString('System', 'UserTokenSecret', ''));
    Fserver := THelper.Decode(inifile.ReadString('System', 'Server', ''));
    FflickrApiKey := THelper.Decode(inifile.ReadString('System', 'FlickrApiKey', ''));
    FSecret := THelper.Decode(inifile.ReadString('System', 'Secret', ''));
    FFlickrUserId := THelper.Decode(inifile.ReadString('System', 'FlickrUserId', ''));
    FAppId := THelper.Decode(inifile.ReadString('System', 'AppId', ''));
  finally
    inifile.Free;
  end;
  result := self;
end;

class function TOptionsAgent.New: IOptionsAgent;
begin
  result := Create;
end;

procedure TOptionsAgent.Save;
var
  iniFile : TInifile;
begin
  inifile := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'FlickrAnalyticsAgent.ini');
  try
    inifile.WriteString('System', 'UserToken', THelper.Encode(FuserToken));
    inifile.WriteInteger('System', 'Port', Fport);
    inifile.WriteString('System', 'Password', THelper.Encode(Fpassword));
    inifile.WriteString('System', 'User', THelper.Encode(Fuser));
    inifile.WriteString('System', 'UserTokenSecret', THelper.Encode(FuserTokenSecret));
    inifile.WriteString('System', 'Server', THelper.Encode(Fserver));
    inifile.WriteString('System', 'FlickrApiKey', THelper.Encode(FflickrApiKey));
    inifile.WriteString('System', 'Secret', THelper.Encode(FSecret));
    inifile.WriteString('System', 'FlickrUserId', THelper.Encode(FFlickrUserId));
    inifile.WriteString('System', 'AppId', THelper.Encode(FAppId));
  finally
    inifile.Free;
  end;
end;

procedure TOptionsAgent.SetAppId(const Value: string);
begin
  FAppId := Value;
end;

procedure TOptionsAgent.SetflickrApiKey(const Value: string);
begin
  FflickrApiKey := Value;
end;

procedure TOptionsAgent.SetflickrUserId(const Value: string);
begin
  FflickrUserId := Value;
end;

procedure TOptionsAgent.Setpassword(const Value: string);
begin
  Fpassword := Value;
end;

procedure TOptionsAgent.Setport(const Value: integer);
begin
  Fport := Value;
end;

procedure TOptionsAgent.Setsecret(const Value: string);
begin
  Fsecret := Value;
end;

procedure TOptionsAgent.Setserver(const Value: string);
begin
  Fserver := Value;
end;

procedure TOptionsAgent.Setuser(const Value: string);
begin
  Fuser := Value;
end;

procedure TOptionsAgent.SetuserToken(const Value: string);
begin
  FuserToken := Value;
end;

procedure TOptionsAgent.SetuserTokenSecret(const Value: string);
begin
  FuserTokenSecret := Value;
end;

end.
