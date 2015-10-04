unit frmFlickrMainTilesGrid;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.ListBox,
  FMX.Menus, FMX.Media, FMX.Ani, FMX.Gestures, FMX.StdCtrls;

type
  TfrmMainGridTile = class(TForm)
    StyleBook2: TStyleBook;
    ToolbarHolder: TLayout;
    ToolbarPopup: TPopup;
    ToolBar1: TToolBar;
    ToolbarApplyButton: TButton;
    ToolbarCloseButton: TButton;
    ToolbarAddButton: TButton;
    ToolbarPopupAnimation: TFloatAnimation;
    MainLayout: TLayout;
    HeaderLayout: TLayout;
    TitleLabel1: TLabel;
    HorzScrollBox1: THorzScrollBox;
    GroupLayout1: TLayout;
    ListBox1: TListBox;
    MetroListBoxItem6: TMetropolisUIListBoxItem;
    MetroListBoxItem1: TMetropolisUIListBoxItem;
    MetroListBoxItem2: TMetropolisUIListBoxItem;
    GroupTitle1: TLabel;
    GroupLayout2: TLayout;
    ListBox2: TListBox;
    MetroListBoxItem3: TMetropolisUIListBoxItem;
    MetroListBoxItem4: TMetropolisUIListBoxItem;
    GroupTitle2: TLabel;
    GroupLayout3: TLayout;
    ListBox3: TListBox;
    MetroListBoxItem7: TMetropolisUIListBoxItem;
    MetroListBoxItem8: TMetropolisUIListBoxItem;
    MetroListBoxItem9: TMetropolisUIListBoxItem;
    GroupTitle3: TLabel;

    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ToolbarCloseButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ItemClick(Sender: TObject);
  private
    FGestureOrigin: TPointF;
    FGestureInProgress: Boolean;

    { Private declarations }
    procedure ShowToolbar(AShow: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainGridTile: TfrmMainGridTile;

implementation

uses Math;

{$R *.fmx}

procedure TfrmMainGridTile.ItemClick(Sender: TObject);
var
  Form: TCommonCustomForm;
begin
  Form := Application.GetDeviceForm('DetailView');
  if Assigned(Form) then
    Form.Show;
end;

procedure TfrmMainGridTile.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  DX, DY: Single;
begin
  if EventInfo.GestureID = igiPan then
  begin
    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) and ((Sender = ToolbarPopup) or (EventInfo.Location.Y > (ClientHeight - 70))) then
    begin
      FGestureOrigin := EventInfo.Location;
      FGestureInProgress := True;
    end;

    if FGestureInProgress and (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
    begin
      FGestureInProgress := False;
      DX := EventInfo.Location.X - FGestureOrigin.X;
      DY := EventInfo.Location.Y - FGestureOrigin.Y;
      if (Abs(DY) > Abs(DX)) then
        ShowToolbar(DY < 0);
    end;
  end
end;

procedure TfrmMainGridTile.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
    ShowToolbar(not ToolbarPopup.IsOpen);
end;

procedure TfrmMainGridTile.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
    ShowToolbar(True)
  else
    ShowToolbar(False);
end;

procedure TfrmMainGridTile.ShowToolbar(AShow: Boolean);
begin
  ToolbarPopup.Width := ClientWidth;
  ToolbarPopup.PlacementRectangle.Rect := TRectF.Create(0, ClientHeight - ToolbarPopup.Height, ClientWidth - 1, ClientHeight - 1);
  ToolbarPopupAnimation.StartValue := ToolbarPopup.Height;
  ToolbarPopupAnimation.StopValue := 0;

  ToolbarPopup.IsOpen := AShow;
end;

procedure TfrmMainGridTile.ToolbarCloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
