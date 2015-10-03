unit SplitItemDetail1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, Vcl.Styles, Vcl.Themes, System.Actions, Vcl.ActnList,
  Vcl.Touch.GestureMgr;

type
  TDetailForm = class(TForm)
    UpperPanel: TPanel;
    TitleLabel: TLabel;
    Image1: TImage;
    ScrollBox1: TScrollBox;
    GridPanel1: TGridPanel;
    GridPanel2: TGridPanel;
    ItemTitle1: TLabel;
    ItemSubTitle1: TLabel;
    ItemDescription1: TLabel;
    GridPanel3: TGridPanel;
    ItemTitle2: TLabel;
    ItemSubTitle2: TLabel;
    ItemDescription2: TLabel;
    ItemImage3: TImage;
    GridPanel4: TGridPanel;
    ItemTitle3: TLabel;
    ItemSubTitle3: TLabel;
    ItemDescription3: TLabel;
    ItemImage4: TImage;
    GridPanel5: TGridPanel;
    ItemTitle4: TLabel;
    ItemSubTitle4: TLabel;
    ItemDescription4: TLabel;
    ItemImage5: TImage;
    GridPanel6: TGridPanel;
    ItemTitle5: TLabel;
    ItemSubTitle5: TLabel;
    ItemDescription5: TLabel;
    ItemImage6: TImage;
    GridPanel7: TGridPanel;
    ItemTitle6: TLabel;
    ItemSubTitle6: TLabel;
    ItemDescription6: TLabel;
    ItemImage1: TImage;
    ItemImage2: TImage;
    ScrollBox2: TScrollBox;
    ItemImage: TImage;
    TextPanel: TPanel;
    ItemTitle: TLabel;
    ItemSubtitle: TLabel;
    ItemImage7: TImage;
    GridPanel9: TGridPanel;
    ItemTitle7: TLabel;
    ItemSubTitle7: TLabel;
    ItemDescription7: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    Action1: TAction;
    AppBar: TPanel;
    CloseButton: TImage;
    LowerPanel: TPanel;
    procedure BackToMainForm(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridPanel1Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private declarations }
    procedure AppBarResize;
    procedure AppBarShow(mode: integer);

    procedure HandleClick();
  public
    { Public declarations }
  end;

var
  DetailForm: TDetailForm = nil;

implementation

{$R *.dfm}

uses GroupedItems1;

const
  AppBarHeight = 75;

procedure TDetailForm.AppBarResize;
begin
  AppBar.SetBounds(0, AppBar.Parent.Height - AppBarHeight,
    AppBar.Parent.Width, AppBarHeight);
end;

procedure TDetailForm.AppBarShow(mode: integer);
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

procedure TDetailForm.HandleClick;
var
  i, j: Integer;
begin
  for I := 0 to GridPanel1.ControlCount -1 do begin
      if GridPanel1.Controls[i].ClassName = 'TImage' then begin
        TImage(GridPanel1.Controls[i]).OnClick := GridPanel1Click;
      end else if GridPanel1.Controls[i].ClassName = 'TGridPanel' then begin
        for j := 0 to TGridPanel(GridPanel1.Controls[i]).ControlCount -1 do begin
          TLabel(TGridPanel(GridPanel1.Controls[i]).Controls[j]).OnClick := GridPanel1Click;
        end;
      end;
  end;
end;

procedure TDetailForm.Action1Execute(Sender: TObject);
begin
  AppBarShow(-1);
end;

procedure TDetailForm.FormCreate(Sender: TObject);
var
  LStyle: TCustomStyleServices;
begin
  //Set background color for memos to the color of the form, from the active style.
  LStyle := TStyleManager.ActiveStyle;
  Memo1.Color := LStyle.GetStyleColor(scGenericBackground);
  Memo1.Font.Color := LStyle.GetStyleFontColor(sfButtonTextNormal);
  Memo2.Color := LStyle.GetStyleColor(scGenericBackground);
  Memo2.Font.Color := LStyle.GetStyleFontColor(sfButtonTextNormal);
  Memo3.Color := LStyle.GetStyleColor(scGenericBackground);
  Memo3.Font.Color := LStyle.GetStyleFontColor(sfButtonTextNormal);
end;

procedure TDetailForm.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  AppBarShow(0);
end;

procedure TDetailForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    AppBarShow(-1)
  else
    AppBarShow(0);
end;

procedure TDetailForm.FormResize(Sender: TObject);
begin
  AppBarResize;
end;

procedure TDetailForm.FormShow(Sender: TObject);
var
  GroupElements: TStringList;
  memoStr: String;
begin
  AppBarShow(0);
  //ReAssign OnClick handlers for all grid child components to the grid
  HandleClick();
  splitForm.PickImageColor(ItemImage, clBtnShadow);
  splitForm.PickImageColor(ItemImage1, clBtnShadow);
  splitForm.PickImageColor(ItemImage2, clBtnShadow);
  splitForm.PickImageColor(ItemImage3, clBtnShadow);
  splitForm.PickImageColor(ItemImage4, clBtnShadow);
  splitForm.PickImageColor(ItemImage5, clBtnShadow);
  splitForm.PickImageColor(ItemImage6, clBtnShadow);
  splitForm.PickImageColor(ItemImage7, clBtnShadow);
    // Show the originating badge's title
  GroupElements:= TStringList.Create;
  try
    GroupElements.Delimiter := '_';
    GroupElements.DelimitedText := SplitForm.SelectedGroup;
    TitleLabel.Caption := 'Title: ' + GroupElements[0];
    ItemTitle.Caption :=  'Item Title: ' + GroupElements[1];
  finally
    GroupElements.Free;
  end;

  memoStr := GenericText + sLineBreak + sLineBreak +
                  GenericText + sLineBreak + sLineBreak  +
                  GenericText + sLineBreak + sLineBreak  +
                  GenericText + sLineBreak + sLineBreak  +
                  GenericText + sLineBreak + sLineBreak  +
                  GenericText + sLineBreak;

  memo1.lines.add(memoStr);
  memo2.lines.add(memoStr);
  memo3.lines.add(memoStr);
end;

procedure TDetailForm.GridPanel1Click(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  //set title and subtitle
  if Pos('Item', TControl(Sender).Name) > 0 then begin
    if Sender.ClassName = 'TImage' then begin
      i := Length(TControl(Sender).Name) - Length('ItemImage') - 1;
      s := Copy(TControl(Sender).Name, Length(TControl(Sender).Name) - i, Length(TControl(Sender).Name));
      ItemTitle.Caption := TLabel(DetailForm.FindComponent('ItemTitle' + s)).Caption;
      ItemSubtitle.Caption := TLabel(DetailForm.FindComponent('ItemSubTitle' + s)).Caption;
    end else if Pos('ItemSubTitle', TControl(Sender).Name) > 0 then begin
      ItemSubtitle.Caption := TLabel(Sender).Caption;
      i := Length(TControl(Sender).Name) - Length('ItemSubTitle') - 1;
      s := Copy(TControl(Sender).Name, Length(TControl(Sender).Name) - i, Length(TControl(Sender).Name));
      ItemTitle.Caption := TLabel(DetailForm.FindComponent('ItemTitle' + s)).Caption;
    end else if Pos('ItemDescription', TControl(Sender).Name) > 0 then begin
      i := Length(TControl(Sender).Name) - Length('ItemDescription') - 1;
      s := Copy(TControl(Sender).Name, Length(TControl(Sender).Name) - i, Length(TControl(Sender).Name));
      ItemTitle.Caption := TLabel(DetailForm.FindComponent('ItemTitle' + s)).Caption;
      ItemSubtitle.Caption := TLabel(DetailForm.FindComponent('ItemSubTitle' + s)).Caption;
    end else begin
      ItemTitle.Caption := TLabel(Sender).Caption;
      i := Length(TControl(Sender).Name) - Length('ItemTitle') - 1;
      s := Copy(TControl(Sender).Name, Length(TControl(Sender).Name) - i, Length(TControl(Sender).Name));
      ItemSubTitle.Caption := TLabel(DetailForm.FindComponent('ItemSubTitle' + s)).Caption;
    end;
  end;
end;


procedure TDetailForm.BackToMainForm(Sender: TObject);
begin
  Hide;
  SplitForm.BringToFront;
end;

end.
