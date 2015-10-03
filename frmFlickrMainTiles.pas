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

unit frmFlickrMainTiles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, System.Actions, Vcl.ActnList,
  Vcl.Touch.GestureMgr;

type
  TfrmMainTiles = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    GroupPanel1: TPanel;
    FlowPanel1: TFlowPanel;
    GroupPanel1_1: TPanel;
    GroupPanel1_2: TPanel;
    Image1: TImage;
    Panel5: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Image2: TImage;
    Panel6: TPanel;
    Label5: TLabel;
    GroupPanel1_3: TPanel;
    Image3: TImage;
    Panel4: TPanel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ScrollBox2: TScrollBox;
    GroupPanel1_4: TPanel;
    Image4: TImage;
    Panel3: TPanel;
    Label2: TLabel;
    Label7: TLabel;
    GroupPanel1_5: TPanel;
    Image5: TImage;
    Panel7: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    Action1: TAction;
    AppBar: TPanel;
    CloseButton: TImage;
    procedure ScrollBox2Resize(Sender: TObject);
    procedure GroupPanel1_1Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
  private
    { Private declarations }
    procedure AppBarResize;
    procedure AppBarShow(mode: integer);
  public
    { Public declarations }
    SelectedGroup: String; // group string from
    procedure PickImageColor(img: TImage; AColor: TColor);
  end;

const
  GenericText = 'Sed ut perspiciatis unde omnis iste natus error ' + 'sit voluptatem accusantium doloremque laudantium, totam rem aperiam, ' +
    'eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae ' + 'vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas ' +
    'sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores ' + 'eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, ' +
    'qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, ' + 'sed quia non numquam eius modi tempora incidunt ut labore et dolore ' +
    'magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis ' + 'nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut ' +
    'aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit ' + 'qui in ea voluptate velit esse quam nihil molestiae consequatur, vel ' +
    'illum qui dolorem eum fugiat quo voluptas nulla pariatur?';

var
  frmMainTiles: TfrmMainTiles;

implementation

{$R *.dfm}

uses SplitItemDetail1;

const
  AppBarHeight = 75;

procedure TfrmMainTiles.AppBarResize;
begin
  AppBar.SetBounds(0, AppBar.Parent.Height - AppBarHeight, AppBar.Parent.Width, AppBarHeight);
end;

procedure TfrmMainTiles.AppBarShow(mode: integer);
begin
  if mode = -1 then // Toggle
    mode := integer(not AppBar.Visible);

  if mode = 0 then
    AppBar.Visible := False
  else
  begin
    AppBar.Visible := True;
    AppBar.BringToFront;
  end;
end;

procedure TfrmMainTiles.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  AppBarShow(0);
end;

procedure TfrmMainTiles.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    AppBarShow(-1)
  else
    AppBarShow(0);
end;

procedure TfrmMainTiles.FormResize(Sender: TObject);
begin
  AppBarResize;
end;

procedure TfrmMainTiles.Action1Execute(Sender: TObject);
begin
  AppBarShow(-1);
end;

procedure TfrmMainTiles.CloseButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMainTiles.GroupPanel1_1Click(Sender: TObject);
begin
  // Assuming here the image will be clicked
  SelectedGroup := TPanel(TControl(Sender).Parent).Name;
  if not Assigned(DetailForm) then
    DetailForm := TDetailForm.Create(Self);
  DetailForm.Show;
  DetailForm.BringToFront;
end;

procedure TfrmMainTiles.PickImageColor(img: TImage; AColor: TColor);
var
  ARect: TRect;
begin
  ARect := img.ClientRect;
  img.Canvas.Brush.Color := AColor;
  img.Canvas.Brush.Style := bsSolid;
  img.Canvas.FillRect(ARect);
  img.Canvas.Refresh;
end;

procedure TfrmMainTiles.ScrollBox2Resize(Sender: TObject);
begin
  // Init panels
  PickImageColor(Image1, clBtnShadow);
  PickImageColor(Image2, clInactiveCaption);
  PickImageColor(Image3, cl3DLight);
  PickImageColor(Image4, clBtnShadow);
  PickImageColor(Image5, clBtnShadow);
end;

end.
