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

unit frmAuthentication;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.OleCtrls, SHDocVw, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack,
  IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, System.ImageList;

type
  TfrmAuthenticate = class(TForm)
    WebBrowser1: TWebBrowser;
    ImageList1: TImageList;
    Panel1: TPanel;
    btnClose: TButton;
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Button3: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    NavigationUrl : String;
    authenticated : boolean;
  end;

var
  frmAuthenticate: TfrmAuthenticate;

implementation

uses
  System.StrUtils, frmFlickrStatsMain, flickr.access.token;

{$R *.dfm}

procedure TfrmAuthenticate.btnCloseClick(Sender: TObject);
var
  response: string;
  oauth_token: string;
  oauth_verifier: string;
  OAccessTokenUrl: string;
  fullname: string;
  oauth_token_secret: string;
  user_nsid: string;
  username: string;
begin
  // 'http://www.example.com/?oauth_token=72157648370759854-32e3740fbaef246b&oauth_verifier=d46f58e4a5780b25'
  response := WebBrowser1.LocationURL;
  frmFlickrMain.Log('response url ' + response);
  response := response.Replace('http://www.example.com/?', '');
  response := response.Replace('oauth_token', '');
  response := response.Replace('oauth_verifier', '');

  // '=72157648370759854-32e3740fbaef246b&=d46f58e4a5780b25'
  oauth_token := AnsiLeftStr(response, AnsiPos('&', response));
  // =72157648370759854-32e3740fbaef246b&
  response := AnsiRightStr(response, length(response) - length(oauth_token));
  // =d46f58e4a5780b25
  oauth_verifier := response;

  // Clean the parameters
  oauth_token := oauth_token.Replace('=', '').Replace('&', '');
  oauth_verifier := oauth_verifier.Replace('=', '').Replace('&', '');
  frmFlickrMain.Log('oauth_token= ' + oauth_token);
  frmFlickrMain.Log('oauth_verifier= ' + oauth_verifier);

  // Now we have the request token
  // we need to exchange it for an Access token
  OAccessTokenUrl := TAccessToken.New(oauth_verifier, frmFlickrMain.apikey.text, oauth_token, frmFlickrMain.secret.text, frmFlickrMain.userTokenSecret).GenerateRequestAccessToken();
  frmFlickrMain.Log('Calling OAuth URL ' + OAccessTokenUrl);
  response := IdHTTP1.Get(OAccessTokenUrl);
  frmFlickrMain.Log('OAuth URL response ' + response);

  // Example response
  // fullname=Jordi%20Corbilla&
  // oauth_token=72157639942921845-e4f73de08dc774e6&
  // oauth_token_secret=3eefb68a488fbb63&
  // user_nsid=96100496%40N05&
  // username=Jordi%20Corbilla%20Photography

  response := response.Replace('fullname', '');
  response := response.Replace('oauth_token', '');
  response := response.Replace('_secret', '');
  response := response.Replace('user_nsid', '');
  response := response.Replace('username', '');

  fullname := AnsiLeftStr(response, AnsiPos('&', response));
  response := AnsiRightStr(response, length(response) - length(fullname));

  oauth_token := AnsiLeftStr(response, AnsiPos('&', response));
  response := AnsiRightStr(response, length(response) - length(oauth_token));

  oauth_token_secret := AnsiLeftStr(response, AnsiPos('&', response));
  response := AnsiRightStr(response, length(response) - length(oauth_token_secret));

  user_nsid := AnsiLeftStr(response, AnsiPos('&', response));
  response := AnsiRightStr(response, length(response) - length(user_nsid));

  username := response;

  fullname := fullname.Replace('=', '').Replace('&', '');
  oauth_token := oauth_token.Replace('=', '').Replace('&', '');
  oauth_token_secret := oauth_token_secret.Replace('=', '').Replace('&', '');
  user_nsid := user_nsid.Replace('=', '').Replace('&', '');
  username := username.Replace('=', '').Replace('&', '');

  frmFlickrMain.Log('fullname= ' + fullname);
  frmFlickrMain.Log('oauth_token= ' + oauth_token);
  frmFlickrMain.Log('oauth_token_secret= ' + oauth_token_secret);
  frmFlickrMain.Log('user_nsid= ' + user_nsid);
  frmFlickrMain.Log('username= ' + username);

  frmFlickrMain.userToken := oauth_token;
  frmFlickrMain.userTokenSecret := oauth_token_secret;
  frmFlickrMain.optionsAgent.userToken := oauth_token;
  frmFlickrMain.optionsAgent.userTokenSecret := oauth_token_secret;
  frmFlickrMain.Button10.Enabled := true;
  frmFlickrMain.btnGetGroups.Enabled := true;
  frmFlickrMain.Button5.Enabled := true;
  frmFlickrMain.btnAddPhotos.Enabled := true;
  frmFlickrMain.btnRemovePhoto.Enabled := true;
  frmFlickrMain.btnBanGroups.Enabled := true;
  frmFlickrMain.btnUnBanGroups.Enabled := true;
  showmessage('Congratulations, application authenticated with token ' + oauth_token);
  authenticated := true;
  Self.Close;
end;

procedure TfrmAuthenticate.Button3Click(Sender: TObject);
begin
  WebBrowser1.Navigate(Edit1.text);
end;

procedure TfrmAuthenticate.Button4Click(Sender: TObject);
begin
  WebBrowser1.Navigate(NavigationUrl);
end;

procedure TfrmAuthenticate.Button5Click(Sender: TObject);
var
	flags: OleVariant;
begin
	if not WebBrowser1.Busy then
	begin
		flags := REFRESH_COMPLETELY;
		WebBrowser1.Refresh2(flags);
	end;
end;

procedure TfrmAuthenticate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not authenticated then
    btnCloseClick(sender);
end;

procedure TfrmAuthenticate.FormCreate(Sender: TObject);
begin
  authenticated := false;
end;

end.
