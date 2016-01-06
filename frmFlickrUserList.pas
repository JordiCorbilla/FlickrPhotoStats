unit frmFlickrUserList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus;

type
  TfrmUserList = class(TForm)
    Panel1: TPanel;
    Splitter2: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    ListView1: TListView;
    Panel4: TPanel;
    ListView2: TListView;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    ShowUser1: TMenuItem;
    ShowUser2: TMenuItem;
    procedure ShowUser1Click(Sender: TObject);
    procedure ShowUser2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure PopulateList(folder, id : string);
  end;

var
  frmUserList: TfrmUserList;

implementation

{$R *.dfm}

uses
  Contnrs, Generics.Collections, flickr.user.faves, XMLDoc, xmldom, XMLIntf, msxmldom, frmFlickrStatsMain, Shellapi;

{ TfrmUserList }

procedure TfrmUserList.PopulateList(folder, id: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode2: IXMLNode;
  userFave : IUserFave;
  listItem : TListItem;
begin
  if fileExists(folder + '\Users\'+ id + '.xml') then
  begin
    Document := TXMLDocument.Create(nil);
    try
      Document.LoadFromFile(folder + '\Users\'+ id + '.xml');
      iXMLRootNode := Document.ChildNodes.first;
      iNode2 := iXMLRootNode.ChildNodes.first;
      while iNode2 <> nil do
      begin
        if iNode2.NodeName = 'Added' then
        begin
          userFave := TUserFave.Create;
          try
            userFave.Load(iNode2);
            listItem := Listview1.Items.Add;
            listItem.Caption := userFave.Id;
            listItem.SubItems.Add(userFave.username);
          finally
            userFave := nil;
          end;
        end;
        if iNode2.NodeName = 'Removed' then
        begin
          userFave := TUserFave.Create;
          try
            userFave.Load(iNode2);
            listItem := Listview2.Items.Add;
            listItem.Caption := userFave.Id;
            listItem.SubItems.Add(userFave.username);
          finally
            userFave := nil;
          end;
        end;
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TfrmUserList.ShowUser1Click(Sender: TObject);
var
  id : string;
begin
  //Show item in the URL view
  //https://www.flickr.com/photos/jordicorbillaphotography/
  if Listview1.ItemIndex <> -1 then
  begin
    id := Listview1.Items[Listview1.ItemIndex].Caption;
    ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('https://m.flickr.com/'+id), nil, SW_SHOW);
  end;
end;

procedure TfrmUserList.ShowUser2Click(Sender: TObject);
var
  id : string;
begin
  //Show item in the URL view
  //https://www.flickr.com/photos/jordicorbillaphotography/
  if Listview2.ItemIndex <> -1 then
  begin
    id := Listview2.Items[Listview2.ItemIndex].Caption;
    ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('https://m.flickr.com/'+id), nil, SW_SHOW);
  end;
end;

end.
