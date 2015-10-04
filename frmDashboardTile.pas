unit frmDashboardTile;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, System.Actions, Vcl.ActnList, Vcl.Touch.GestureMgr;

type
  TfrmDashboard = class(TForm)
    AppBar: TPanel;
    CloseButton: TImage;
    ActionList1: TActionList;
    Action1: TAction;
    GestureManager1: TGestureManager;
    UpperPanel: TPanel;
    TitleLabel: TLabel;
    Image1: TImage;
    lowerPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure BackToMainForm(Sender: TObject);
  private
    { Private declarations }
    procedure AppBarResize;
    procedure AppBarShow(mode: integer);
  public
    { Public declarations }
  end;

var
  frmDashboard: TfrmDashboard;

implementation

uses frmFlickrMainTiles;

{$R *.dfm}

const
  AppBarHeight = 75;

procedure TfrmDashboard.AppBarResize;
begin
  AppBar.SetBounds(0, AppBar.Parent.Height - AppBarHeight,
    AppBar.Parent.Width, AppBarHeight);
end;

procedure TfrmDashboard.AppBarShow(mode: integer);
begin
  if mode = -1 then // Toggle
    mode := integer(not AppBar.Visible );

  if mode = 0 then
    AppBar.Visible := False
  else
  begin
    AppBar.Visible := True;
    AppBar.BringToFront;
  end;
end;

procedure TfrmDashboard.BackToMainForm(Sender: TObject);
begin
  Hide;
  frmMainTiles.BringToFront;
end;

procedure TfrmDashboard.Action1Execute(Sender: TObject);
begin
  AppBarShow(-1);
end;

procedure TfrmDashboard.CloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmDashboard.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  AppBarShow(0);
end;

procedure TfrmDashboard.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    AppBarShow(-1)
  else
    AppBarShow(0);
end;

procedure TfrmDashboard.FormResize(Sender: TObject);
begin
  AppBarResize;
end;

end.
