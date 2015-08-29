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
    procedure Label4Click(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure btnDenyClick(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSetupApp: TfrmSetupApp;

implementation

uses
  ShellApi, frmFlickrStats;

{$R *.dfm}

procedure TfrmSetupApp.btnConfirmClick(Sender: TObject);
begin
  if (apikey.Text = '') or (secret.Text = '') or (edtuserid.Text = '') then
  begin
    showMessage('Parameters can''t be empty');
    exit;
  end;
  frmFlickr.apikey.Text := apikey.Text;
  frmFlickr.secret.Text := secret.Text;
  frmFlickr.edtuserid.Text := edtuserid.Text;
  //Save the file
  frmFlickr.btnSaveOptionsClick(sender);
  //Create the new folders
  if ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Albums') then
    if ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Groups') then
      Showmessage('Application has been correctly configured!');
  Self.Close;
end;

procedure TfrmSetupApp.btnDenyClick(Sender: TObject);
begin
  application.Terminate;
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
