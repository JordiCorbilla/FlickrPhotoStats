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

unit frmSetup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList;

type
  TfrmSetupApp = class(TForm)
    lblText: TLabel;
    Label1: TLabel;
    apikey: TEdit;
    Label8: TLabel;
    secret: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    ImageList1: TImageList;
    btnConfirm: TButton;
    btnDeny: TButton;
    Label3: TLabel;
    edtUserId: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    edtWorkspace: TEdit;
    Label10: TLabel;
    btnLoadDirectory: TButton;
    Label11: TLabel;
    edtAppId: TEdit;
    procedure Label4Click(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure btnDenyClick(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure btnLoadDirectoryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSetupApp: TfrmSetupApp;

implementation

uses
  ShellApi, frmFlickrStatsMain, flickr.lib.folder;

{$R *.dfm}

procedure TfrmSetupApp.btnConfirmClick(Sender: TObject);
begin
  if (apikey.Text = '') or (secret.Text = '') or (edtuserid.Text = '') or (edtWorkspace.Text = '') then
  begin
    showMessage('Parameters can''t be empty');
    exit;
  end;
  frmFlickrMain.apikey.Text := apikey.Text;
  frmFlickrMain.secret.Text := secret.Text;
  frmFlickrMain.edtuserid.Text := edtuserid.Text;
  frmFlickrMain.edtWorkspace.text := edtWorkspace.Text;
  frmFlickrMain.edtAppId.Text := edtAppId.Text;
  //Save the file
  frmFlickrMain.btnSaveOptionsClick(sender);
  //Create the new folders
  if ForceDirectories(edtWorkspace.Text + '\Albums') then
    if ForceDirectories(edtWorkspace.Text + '\Groups') then
      if ForceDirectories(edtWorkspace.Text + '\History') then
      Showmessage('Application has been correctly configured!');
  Self.Close;
end;

procedure TfrmSetupApp.btnDenyClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TfrmSetupApp.btnLoadDirectoryClick(Sender: TObject);
var
  myFolder : string;
begin
  myFolder := TFolder.BrowseForFolder;
  edtWorkspace.Text := myFolder;
end;

procedure TfrmSetupApp.Label4Click(Sender: TObject);
var
  MyLink: string;
begin
  MyLink := 'https://www.flickr.com/services/api/keys/';
  ShellExecute(self.WindowHandle,'open','chrome.exe', PChar(MyLink), nil, SW_SHOW);
end;

procedure TfrmSetupApp.Label6Click(Sender: TObject);
var
  MyLink: string;
begin
  MyLink := 'https://www.flickr.com/services/api/explore/flickr.people.getInfo';
  ShellExecute(self.WindowHandle,'open','chrome.exe', PChar(MyLink), nil, SW_SHOW);
end;

end.
