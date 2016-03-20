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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmUserList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

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
            listItem.SubItems.Add(userFave.Location);
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
            listItem.SubItems.Add(userFave.Location);
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
    ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('https://www.flickr.com/people/'+id + '/'), nil, SW_SHOW);
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
    ShellExecute(self.WindowHandle,'open','chrome.exe', PChar('https://www.flickr.com/people/'+id + '/'), nil, SW_SHOW);
  end;
end;

end.
