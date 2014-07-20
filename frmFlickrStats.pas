// Copyright (c) 2014, Jordi Corbilla
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

unit frmFlickrStats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, IdIOHandler, IdIOHandlerStream, IdIOHandlerSocket, IdIOHandlerStack,
  IdSSL, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, ComCtrls, flickr.repository,
  ExtCtrls, TeEngine, TeeProcs, Chart, Series, VclTee.TeeGDIPlus, System.UITypes;

type
  TfrmFlickr = class(TForm)
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    XMLDocument1: TXMLDocument;
    Panel1: TPanel;
    btnLoad: TButton;
    btnSave: TButton;
    Label1: TLabel;
    apikey: TEdit;
    Label3: TLabel;
    Edit1: TEdit;
    Panel2: TPanel;
    listPhotos: TListView;
    Splitter1: TSplitter;
    Panel3: TPanel;
    ProgressBar1: TProgressBar;
    rbViews: TRadioButton;
    rbLikes: TRadioButton;
    rbComments: TRadioButton;
    Panel4: TPanel;
    Chart2: TChart;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TBarSeries;
    Panel5: TPanel;
    Label2: TLabel;
    photoId: TEdit;
    btnAdd: TButton;
    batchUpdate: TButton;
    procedure batchUpdateClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure listPhotosItemChecked(Sender: TObject; Item: TListItem);
    procedure FormDestroy(Sender: TObject);
    function isInSeries(id : string) : Boolean;
    procedure listPhotosCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    procedure LoadForms(repository: IFlickrRepository);
    function ExistPhotoInList(id: string; var Item: TListItem): Boolean;
    procedure RequestInformation_REST_Flickr(id: string);
    procedure UpdateCounts;
    procedure UpdateTotals;
    procedure UpdateChart(totalViews, totalLikes, totalComments, totalPhotos: integer);
    { Private declarations }
  public
    repository: IFlickrRepository;
    CheckedSeries : TStringList;
  end;

var
  frmFlickr: TfrmFlickr;

implementation

uses
  flickr.photos, flickr.stats, flickr.rest;

{$R *.dfm}

procedure TfrmFlickr.batchUpdateClick(Sender: TObject);
var
  i: integer;
begin
  ProgressBar1.Visible := true;
  ProgressBar1.Min := 0;
  ProgressBar1.Max := listPhotos.Items.Count;
  for i := 0 to listPhotos.Items.Count - 1 do
  begin
    ProgressBar1.position := i;
    Application.ProcessMessages;
    RequestInformation_REST_Flickr(listPhotos.Items[i].Caption);
  end;
  ProgressBar1.Visible := false;
  UpdateTotals();
end;

procedure TfrmFlickr.UpdateTotals();
var
  i: integer;
  Item: TListItem;
  totalViews, totalViewsacc : integer;
  totalLikes, totalLikesacc : Integer;
  totalComments, totalCommentsacc : Integer;
begin
  totalViewsacc := 0;
  totalLikesacc := 0;
  totalCommentsacc := 0;
  for i := 0 to listPhotos.Items.Count - 1 do
  begin
    Item := listPhotos.Items[i];
    totalViews := StrToInt(Item.SubItems.Strings[1]);
    totalViewsacc := totalViewsacc + totalViews;

    totalLikes := StrToInt(Item.SubItems.Strings[2]);;
    totalLikesacc := totalLikesacc + totalLikes;

    totalComments := StrToInt(Item.SubItems.Strings[3]);;
    totalCommentsacc := totalCommentsacc + totalComments;
  end;

  UpdateChart(totalViewsacc, totalLikesacc, totalCommentsacc, repository.photos.Count);
end;

function TfrmFlickr.ExistPhotoInList(id: string; var Item: TListItem): Boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < listPhotos.Items.Count) do
  begin
    found := listPhotos.Items[i].Caption = id;
    inc(i);
  end;
  if found then
    Item := listPhotos.Items[i - 1];
  Result := found;
end;

procedure TfrmFlickr.RequestInformation_REST_Flickr(id: string);
var
  Item, itemExisting: TListItem;
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  views, title, likes, comments: string;
  stat: IStat;
  photo, existing: IPhoto;
begin
  response := IdHTTP1.Get(TFlickrRest.new().getInfo(apikey.text, id));
  XMLDocument1.LoadFromXML(response);
  iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
  iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
  iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photo>
  views := iXMLRootNode3.attributes['views'];
  iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <owner>
  while iXMLRootNode4 <> nil do
  begin
    if iXMLRootNode4.NodeName = 'title' then
      title := iXMLRootNode4.NodeValue;
    if iXMLRootNode4.NodeName = 'comments' then
      comments := iXMLRootNode4.NodeValue;
    iXMLRootNode4 := iXMLRootNode4.NextSibling;
  end;

  response := IdHTTP1.Get(TFlickrRest.new().getFavorites(apikey.text, id));
  XMLDocument1.LoadFromXML(response);
  iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
  iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
  iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photo>
  likes := iXMLRootNode3.attributes['total'];

  photo := TPhoto.Create(id, title);
  stat := TStat.Create(Date, StrToInt(views), StrToInt(likes), StrToInt(comments));

  if repository.ExistPhoto(photo, existing) then
  begin
    photo := existing;
    photo.AddStats(stat);
    photo.LastUpdate := Date;
  end
  else
  begin
    photo.AddStats(stat);
    photo.LastUpdate := Date;
    repository.AddPhoto(photo);
  end;

  if not ExistPhotoInList(id, itemExisting) then
  begin
    Item := listPhotos.Items.Add;
    Item.Caption := photoId.text;
    Item.SubItems.Add(title);
    Item.SubItems.Add(views);
    Item.SubItems.Add(likes);
    Item.SubItems.Add(comments);
    Item.SubItems.Add(DateToStr(Date));
    Item.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger/views.ToInteger)*100.0));
  end
  else
  begin
    itemExisting.Caption := id;
    itemExisting.SubItems.Clear;
    itemExisting.SubItems.Add(title);
    itemExisting.SubItems.Add(views);
    itemExisting.SubItems.Add(likes);
    itemExisting.SubItems.Add(comments);
    itemExisting.SubItems.Add(DateToStr(Date));
    itemExisting.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger/views.ToInteger)*100.0));
  end;
end;

procedure TfrmFlickr.btnAddClick(Sender: TObject);
begin
  RequestInformation_REST_Flickr(photoId.text);
  photoId.text := '';
  UpdateTotals();
end;

procedure TfrmFlickr.btnLoadClick(Sender: TObject);
begin
  if Assigned(repository) then
  begin
    repository := nil;
    repository := TFlickrRepository.Create();
  end;
  repository.load('flickrRepository.xml');

  LoadForms(repository);
end;

procedure TfrmFlickr.LoadForms(repository: IFlickrRepository);
begin
  apikey.text := repository.apikey;
  listPhotos.Clear;
  UpdateCounts();
end;

procedure TfrmFlickr.UpdateCounts();
var
  i: integer;
  Item: TListItem;
  totalViews, totalViewsacc : integer;
  totalLikes, totalLikesacc : Integer;
  totalComments, totalCommentsacc : Integer;
begin
  totalViewsacc := 0;
  totalLikesacc := 0;
  totalCommentsacc := 0;
  for i := 0 to repository.photos.Count - 1 do
  begin
    Item := listPhotos.Items.Add;
    Item.Caption := repository.photos[i].id;
    Item.SubItems.Add(repository.photos[i].title);
    totalViews := repository.photos[i].getTotalViews;
    totalViewsacc := totalViewsacc + totalViews;
    Item.SubItems.Add(IntToStr(totalViews));
    totalLikes := repository.photos[i].getTotalLikes;
    totalLikesacc := totalLikesacc + totalLikes;
    Item.SubItems.Add(IntToStr(totalLikes));
    totalComments := repository.photos[i].getTotalComments;
    totalCommentsacc := totalCommentsacc + totalComments;
    Item.SubItems.Add(IntToStr(totalComments));
    Item.SubItems.Add(DateToStr(repository.photos[i].LastUpdate));
    Item.SubItems.Add(FormatFloat('0.##%', (totalLikes/totalViews)*100.0));
  end;

  UpdateChart(totalViewsacc, totalLikesacc, totalCommentsacc, repository.photos.Count);
end;

procedure TfrmFlickr.UpdateChart(totalViews, totalLikes, totalComments, totalPhotos : integer);
var
  Series : TBarSeries;
  color : TColor;
begin
  if chart2.SeriesList.Count = 1 then
    chart2.RemoveAllSeries;

  Series := TBarSeries.Create(Chart2);
  Series.Marks.Arrow.Visible := True;
  Series.Marks.Callout.Brush.Color := clBlack;
  Series.Marks.Callout.Arrow.Visible := True;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.Color := 8487297;
  //Series.Marks.Visible := true;
  Series.SeriesColor := 10708548;
  //Series.Stairs := true;
  Series.XValues.DateTime := True;
  Series.XValues.Name := 'X';
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Y';
  Series.YValues.Order := loNone;
  Series.ParentChart := Chart2;
  color := RGB(Random(255), Random(255), Random(255));

  Series.AddBar(totalViews, 'Views', color);
  Series.AddBar(totalLikes, 'Likes', color);
  Series.AddBar(totalComments, 'Comments', color);
  Series.AddBar(totalPhotos, 'Photos', color);
  chart2.AddSeries(Series);
end;

procedure TfrmFlickr.btnSaveClick(Sender: TObject);
begin
  repository.save(apikey.text, 'flickrRepository.xml');
end;

procedure TfrmFlickr.FormCreate(Sender: TObject);
begin
  repository := TFlickrRepository.Create();
  CheckedSeries := TStringList.Create;
end;

procedure TfrmFlickr.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CheckedSeries);
  repository := nil;
end;

function TfrmFlickr.isInSeries(id: string): Boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < CheckedSeries.count) do
  begin
    found := CheckedSeries[i] = id;
    inc(i);
  end;
  Result := found;
end;

procedure TfrmFlickr.listPhotosCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  Color, Color2: TColor;
begin
  Color := Sender.Canvas.Font.Color;
  Color2 := Sender.Canvas.Brush.Color;
  if SubItem = 1 then
  begin
    if ((Item.SubItems.Strings[1].ToInteger >= 1000) and (Item.SubItems.Strings[1].ToInteger < 2000)) then
    begin
      Sender.Canvas.Font.Color := clBlue;
      Sender.Canvas.Brush.Color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 2000) and (Item.SubItems.Strings[1].ToInteger < 4000)) then
    begin
      Sender.Canvas.Font.Color := clGreen;
      Sender.Canvas.Brush.Color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 4000)) then
    begin
      Sender.Canvas.Font.Color := clRed;
      Sender.Canvas.Brush.Color := Color2;
    end;
  end
  else
  begin
    Sender.Canvas.Font.Color := Color;
    Sender.Canvas.Brush.Color := Color2;
  end;
end;

procedure TfrmFlickr.listPhotosItemChecked(Sender: TObject; Item: TListItem);
var
  id, title, views, likes, comments, LastUpdate: string;
  photo: IPhoto;
  stat: IStat;
  i: integer;
  Series : TLineSeries;
  colour : TColor;
begin
  if (Item.Checked) then
  begin
    id := Item.Caption;
    title := Item.SubItems.Strings[0];
    views := Item.SubItems.Strings[1];
    likes := Item.SubItems.Strings[2];
    comments := Item.SubItems.Strings[3];
    LastUpdate := Item.SubItems.Strings[4];

    photo := repository.GetPhoto(id);
    if photo <> nil then
    begin
      Series :=  TLineSeries.Create(Chart1);
      Series.Marks.Arrow.Visible := True;
      Series.Marks.Callout.Brush.Color := clBlack;
      Series.Marks.Callout.Arrow.Visible := True;
      Series.Marks.DrawEvery := 10;
      Series.Marks.Shadow.Color := 8487297;
      Series.Marks.Visible := true;
      Series.SeriesColor := 10708548;
      Series.Title := id ;
      //Series.Stairs := true;
      Series.LinePen.Width := 1;
      Series.LinePen.Color := 10708548;
      Series.Pointer.InflateMargins := True;
      Series.Pointer.Style := psRectangle;
      Series.Pointer.Brush.Gradient.EndColor := 10708548;
      Series.Pointer.Gradient.EndColor := 10708548;
      Series.Pointer.InflateMargins := True;
      Series.Pointer.Visible := False;
      Series.XValues.DateTime := True;
      Series.XValues.Name := 'X';
      Series.XValues.Order := loAscending;
      Series.YValues.Name := 'Y';
      Series.YValues.Order := loNone;
      Series.ParentChart := Chart1;
      CheckedSeries.Add(id);
      colour := RGB(Random(255), Random(255), Random(255));
      for i := 0 to photo.stats.Count - 1 do
      begin
        stat := photo.stats[i];
        if rbViews.Checked then
          Series.AddXY(stat.Date, stat.views, '', colour);
        if rbLikes.Checked then
          Series.AddXY(stat.Date, stat.Likes, '', colour);
        if rbComments.Checked then
          Series.AddXY(stat.Date, stat.numComments, '', colour);
      end;
      Chart1.AddSeries(Series);
    end;
  end
  else
  begin
    id := Item.Caption;
    if isInSeries(id) then
    begin
      Series := nil;
      for i := 0 to Chart1.SeriesList.Count-1 do
      begin
        if Chart1.SeriesList[i].Title = id then
        begin
          Series := TLineSeries(Chart1.SeriesList[i]);
          Break;
        end;
      end;
      if Series <> nil then
        Chart1.RemoveSeries(Series);
    end;
  end;
end;

end.
