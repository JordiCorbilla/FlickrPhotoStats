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
  ExtCtrls, TeEngine, TeeProcs, Chart, Series, VclTee.TeeGDIPlus, System.UITypes, flickr.globals,
  Vcl.ImgList, Vcl.Buttons, System.Win.TaskbarCore, Vcl.Taskbar, System.Actions,
  Vcl.ActnList, IdHashMessageDigest, idHash, IdGlobal;

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
    Panel5: TPanel;
    Label2: TLabel;
    photoId: TEdit;
    btnAdd: TButton;
    batchUpdate: TButton;
    PageControl1: TPageControl;
    Statistics: TTabSheet;
    Panel4: TPanel;
    Chart2: TChart;
    Series4: TBarSeries;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Globals: TTabSheet;
    Process: TLabel;
    ChartComments: TChart;
    LineSeries1: TLineSeries;
    ChartViews: TChart;
    LineSeries4: TLineSeries;
    ChartLikes: TChart;
    LineSeries7: TLineSeries;
    Memo1: TMemo;
    ImageList1: TImageList;
    TabSheet1: TTabSheet;
    Panel6: TPanel;
    btnGetList: TButton;
    Label4: TLabel;
    edtUserId: TEdit;
    listPhotosUser: TListView;
    Panel7: TPanel;
    btnAddItems: TButton;
    lblfetching: TLabel;
    progressfetching: TProgressBar;
    Taskbar1: TTaskbar;
    ActionList1: TActionList;
    TabSheet2: TTabSheet;
    Panel8: TPanel;
    lblfetchinggroup: TLabel;
    btnGetGroups: TButton;
    progressfetchinggroups: TProgressBar;
    listGroups: TListView;
    Label5: TLabel;
    edtAuthToken: TEdit;
    Label6: TLabel;
    edtApiSig: TEdit;
    Label7: TLabel;
    Edit2: TEdit;
    Label8: TLabel;
    secret: TEdit;
    Authenticate: TButton;
    Splitter2: TSplitter;
    TabSheet3: TTabSheet;
    edtMax: TEdit;
    Label9: TLabel;
    btnExcel: TButton;
    TabSheet4: TTabSheet;
    dailyViews: TChart;
    LineSeries2: TBarSeries;
    dailyLikes: TChart;
    BarSeries1: TBarSeries;
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
    procedure btnGetListClick(Sender: TObject);
    procedure apikeyChange(Sender: TObject);
    procedure btnAddItemsClick(Sender: TObject);
    procedure btnGetGroupsClick(Sender: TObject);
    procedure AuthenticateClick(Sender: TObject);
    procedure btnExcelClick(Sender: TObject);
  private
    procedure LoadForms(repository: IFlickrRepository);
    function ExistPhotoInList(id: string; var Item: TListItem): Boolean;
    procedure RequestInformation_REST_Flickr(id: string);
    procedure UpdateCounts;
    procedure UpdateTotals;
    procedure UpdateChart(totalViews, totalLikes, totalComments, totalPhotos: integer);
    procedure UpdateGlobals();
    procedure UpdateAnalytics();
    procedure LoadHallOfFame(repository: IFlickrRepository);
    function MD5(apikey, secret: string): string;
    function SaveToExcel(AView: TListView; ASheetName, AFileName: string): Boolean;
    function getTotalGroupCounts: integer;
    { Private declarations }
  public
    repository: IFlickrRepository;
    globalsRepository : IFlickrGlobals;
    CheckedSeries : TStringList;
  end;

var
  frmFlickr: TfrmFlickr;

implementation

uses
  flickr.photos, flickr.stats, flickr.rest, flickr.top.stats, ComObj, flickr.oauth;

{$R *.dfm}

procedure TfrmFlickr.apikeyChange(Sender: TObject);
begin
  btnSave.Enabled := true;
end;

procedure TfrmFlickr.AuthenticateClick(Sender: TObject);
var
  OAuthUrl : string;
  response : string;
begin
//oauthr authentication
  OAuthUrl := TOAuth.New(apikey.text, secret.text).GenerateRequestTokenQuery();

  //Example successful response
  //oauth_callback_confirmed=true&
  //oauth_token=72157650113637896-43aba062d96def83&
  //oauth_token_secret=153e53c592649722
  response := IdHTTP1.Get(OAuthUrl);

end;

procedure TfrmFlickr.batchUpdateClick(Sender: TObject);
var
  i: integer;
begin
  if apikey.Text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  batchUpdate.enabled := false;
  btnGetList.enabled := false;
  ProgressBar1.Visible := true;
  photoId.Enabled := false;
  btnAdd.Enabled := false;
  Process.Visible := true;
  taskbar1.ProgressState := TTaskBarProgressState.Normal;
  taskbar1.ProgressMaxValue := listPhotos.Items.Count;

  ProgressBar1.Min := 0;
  ProgressBar1.Max := listPhotos.Items.Count;
  for i := 0 to listPhotos.Items.Count - 1 do
  begin
    Process.Caption := 'Processing image: ' + listPhotos.Items[i].Caption + ' ' + i.ToString + ' out of ' +  listPhotos.Items.Count.ToString;
    ProgressBar1.position := i;
    taskbar1.ProgressValue := i;
    Application.ProcessMessages;
    RequestInformation_REST_Flickr(listPhotos.Items[i].Caption);
  end;
  ProgressBar1.Visible := false;
  Process.Visible := false;
  UpdateTotals();
  LoadHallOfFame(repository);
  btnSave.Enabled := true;
  photoId.Enabled := true;
  btnAdd.Enabled := true;
  taskbar1.ProgressValue := 0;
  btnGetList.Enabled := true;
  batchUpdate.enabled := false;
end;

procedure TfrmFlickr.UpdateTotals();
var
  i: integer;
  Item: TListItem;
  totalViews, totalViewsacc : integer;
  totalLikes, totalLikesacc : Integer;
  totalComments, totalCommentsacc : Integer;
  stat : IStat;
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

  totalViewsacc := totalViewsacc + getTotalGroupCounts();

  stat := TStat.Create(Date, totalViewsacc, totalLikesacc, totalCommentsacc);
  globalsRepository.AddGlobals(stat);

  UpdateChart(totalViewsacc, totalLikesacc, totalCommentsacc, repository.photos.Count);
  UpdateGlobals();
  UpdateAnalytics();
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
    if views = '0' then
      views := '1';
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
    if views = '0' then
      views := '1';
    itemExisting.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger/views.ToInteger)*100.0));
  end;
end;

procedure TfrmFlickr.btnAddClick(Sender: TObject);
begin
  if apikey.Text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;

  RequestInformation_REST_Flickr(photoId.text);
  photoId.text := '';
  UpdateTotals();
  btnSave.Enabled := true;
end;

procedure TfrmFlickr.btnLoadClick(Sender: TObject);
begin
  if Assigned(repository) then
  begin
    repository := nil;
    repository := TFlickrRepository.Create();
  end;
  repository.load('flickrRepository.xml');

  if Assigned(globalsRepository) then
  begin
    globalsRepository := nil;
    globalsRepository := TFlickrGlobals.Create();
  end;
  globalsRepository.load('flickrRepositoryGlobal.xml');

  LoadForms(repository);

  LoadHallOfFame(repository);
end;

procedure TfrmFlickr.LoadHallOfFame(repository : IFlickrRepository);
var
  topStats : TTopStats;
  maxValues : string;
begin
  topStats := TTopStats.Create(repository);
  memo1.Lines.Clear;
  memo1.Lines.Add('************************************');
  memo1.Lines.Add('************ HALL OF FAME **********');
  memo1.Lines.Add('************************************');

  maxValues := edtMax.Text;
  memo1.Lines.Add(topStats.GetTopXNumberOfViews(maxValues.ToInteger()));
  memo1.Lines.Add(topStats.GetTopXNumberOfLikes(maxValues.ToInteger()));
  memo1.Lines.Add(topStats.GetTopXNumberOfComments(maxValues.ToInteger()));
  topStats.Free;
end;

procedure TfrmFlickr.LoadForms(repository: IFlickrRepository);
begin
  apikey.text := repository.apikey;
  edtUserId.Text := repository.UserId;
  edtAuthToken.text := repository.Auth_token;
  edtApiSig.text := repository.Api_Sig;
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
    if totalViews = 0 then
      totalViews := 1;
    Item.SubItems.Add(FormatFloat('0.##%', (totalLikes/totalViews)*100.0));
  end;

  UpdateChart(totalViewsacc, totalLikesacc, totalCommentsacc, repository.photos.Count);
  UpdateGlobals();
  UpdateAnalytics();
end;

procedure TfrmFlickr.UpdateGlobals;
var
  Series : TLineSeries;
  color : TColor;
  i : integer;
begin
  if chartViews.SeriesList.Count = 1 then
    chartViews.RemoveAllSeries;

  Series := TLineSeries.Create(chartViews);
  Series.Marks.Arrow.Visible := True;
  Series.Marks.Callout.Brush.Color := clBlack;
  Series.Marks.Callout.Arrow.Visible := True;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.Color := 8487297;
  Series.Marks.Visible := true;
  Series.SeriesColor := 10708548;
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
  Series.ParentChart := chartViews;
  color := RGB(Random(255), Random(255), Random(255));

  for i := 0 to globalsRepository.Globals.Count-1 do
  begin
    Series.AddXY(globalsRepository.Globals[i].Date, globalsRepository.Globals[i].views, '', color);
  end;
  chartViews.AddSeries(Series);

  if chartLikes.SeriesList.Count = 1 then
    chartLikes.RemoveAllSeries;

  Series := TLineSeries.Create(chartLikes);
  Series.Marks.Arrow.Visible := True;
  Series.Marks.Callout.Brush.Color := clBlack;
  Series.Marks.Callout.Arrow.Visible := True;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.Color := 8487297;
  Series.Marks.Visible := true;
  Series.SeriesColor := 10708548;
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
  Series.ParentChart := chartLikes;
  color := RGB(Random(255), Random(255), Random(255));

  for i := 0 to globalsRepository.Globals.Count-1 do
  begin
    Series.AddXY(globalsRepository.Globals[i].Date, globalsRepository.Globals[i].likes, '', color);
  end;
  chartLikes.AddSeries(Series);

  if chartComments.SeriesList.Count = 1 then
    chartComments.RemoveAllSeries;

  Series := TLineSeries.Create(chartComments);
  Series.Marks.Arrow.Visible := True;
  Series.Marks.Callout.Brush.Color := clBlack;
  Series.Marks.Callout.Arrow.Visible := True;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.Color := 8487297;
  Series.Marks.Visible := true;
  Series.SeriesColor := 10708548;
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
  Series.ParentChart := chartComments;
  color := RGB(Random(255), Random(255), Random(255));

  for i := 0 to globalsRepository.Globals.Count-1 do
  begin
    Series.AddXY(globalsRepository.Globals[i].Date, globalsRepository.Globals[i].numComments, '', color);
  end;
  chartComments.AddSeries(Series);
end;

procedure TfrmFlickr.UpdateAnalytics;
var
  Series : TBarSeries;
  color : TColor;
  i : integer;
  theDate : TDateTime;
  views : integer;
begin
  if dailyViews.SeriesList.Count = 1 then
    dailyViews.RemoveAllSeries;

  Series := TBarSeries.Create(dailyViews);
  Series.Marks.Arrow.Visible := True;
  Series.Marks.Callout.Brush.Color := clBlack;
  Series.Marks.Callout.Arrow.Visible := True;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.Color := 8487297;
  Series.SeriesColor := 10708548;
  Series.XValues.DateTime := True;
  Series.XValues.Name := 'X';
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Y';
  Series.YValues.Order := loNone;
  Series.ParentChart := Chart2;
  color := RGB(Random(255), Random(255), Random(255));

  for i := 1 to globalsRepository.Globals.Count-1 do
  begin
    theDate :=  globalsRepository.Globals[i].Date;
    views :=  globalsRepository.Globals[i].views - globalsRepository.Globals[i-1].views;
    Series.AddXY(theDate, views, '', color);
  end;

  dailyViews.AddSeries(Series);

  if dailyLikes.SeriesList.Count = 1 then
    dailyLikes.RemoveAllSeries;

  Series := TBarSeries.Create(dailyLikes);
  Series.Marks.Arrow.Visible := True;
  Series.Marks.Callout.Brush.Color := clBlack;
  Series.Marks.Callout.Arrow.Visible := True;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.Color := 8487297;
  Series.SeriesColor := 10708548;
  Series.XValues.DateTime := True;
  Series.XValues.Name := 'X';
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Y';
  Series.YValues.Order := loNone;
  Series.ParentChart := Chart2;
  color := RGB(Random(255), Random(255), Random(255));

  for i := 1 to globalsRepository.Globals.Count-1 do
  begin
    theDate :=  globalsRepository.Globals[i].Date;
    views :=  globalsRepository.Globals[i].likes - globalsRepository.Globals[i-1].likes;
    Series.AddXY(theDate, views, '', color);
  end;

  dailyLikes.AddSeries(Series);
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
  repository.save(apikey.text, edtUserId.text, edtAuthtoken.text, edtApisig.text, 'flickrRepository.xml');
  globalsRepository.save('flickrRepositoryGlobal.xml');
  btnSave.Enabled := false;
end;


//returns MD5 has for a file
function TfrmFlickr.MD5(apikey : string; secret : string): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := idmd5.HashStringAsHex(secret +'api_key'+apikey+'permswrite',IndyTextEncoding_OSDefault());
  finally
    idmd5.Free;
  end;
end;

procedure TfrmFlickr.btnGetGroupsClick(Sender: TObject);
var
  Item: TListItem;
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  pages, title, id, ismember, total: string;
  numPages, numTotal : integer;
  i: Integer;
begin
  edtapisig.Text := MD5(apikey.Text, secret.text);
  exit;


  if apikey.Text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  if edtUserId.Text = '' then
  begin
    showmessage('User ID key can''t be empty');
    exit;
  end;
  if edtAuthToken.Text = '' then
  begin
    showmessage('Authorisation token key can''t be empty');
    exit;
  end;
  if edtApiSig.Text = '' then
  begin
    showmessage('Api signature key can''t be empty');
    exit;
  end;
  btnLoad.Enabled := false;
  btnAdd.Enabled := false;
  btnAddItems.Enabled := false;
  batchUpdate.Enabled := false;
  listGroups.Visible := false;
  lblfetchingGroup.visible := true;
  progressfetchinggroups.visible := true;
  Application.ProcessMessages;
  response := IdHTTP1.Get(TFlickrRest.new().getGroups(apikey.text, '1', '500', edtAuthToken.Text, edtApiSig.text));
  XMLDocument1.LoadFromXML(response);
  iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
  iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
  iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <groups>
  pages := iXMLRootNode3.attributes['page'];
  total := iXMLRootNode3.attributes['pages'];
  iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <group>
  listGroups.Clear;
  numTotal := total.ToInteger();
  progressfetchinggroups.Max := numTotal;
  taskbar1.ProgressState := TTaskBarProgressState.Normal;
  taskbar1.ProgressMaxValue := numTotal;
  progressfetchinggroups.position := 0;
  while iXMLRootNode4 <> nil do
  begin
    if iXMLRootNode4.NodeName = 'group' then
    begin
      id := iXMLRootNode4.attributes['id'];
      ismember := iXMLRootNode4.attributes['member'];
      title := iXMLRootNode4.attributes['name'];
      if ismember = '1' then
      begin
        Item := listGroups.Items.Add;
        Item.Caption := id;
        Item.SubItems.Add(title);
      end;
    end;
    progressfetchinggroups.position := progressfetchinggroups.position + 1;
    taskbar1.ProgressValue := progressfetchinggroups.position;
    Application.ProcessMessages;
    iXMLRootNode4 := iXMLRootNode4.NextSibling;
  end;

  //Load the remaining pages
  numPages := total.ToInteger;
  for i := 2 to numpages do
  begin
    response := IdHTTP1.Get(TFlickrRest.new().getGroups(apikey.text, i.ToString, '500', edtAuthToken.Text, edtApiSig.text));
    XMLDocument1.LoadFromXML(response);
    iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
    iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
    iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <groups>
    pages := iXMLRootNode3.attributes['page'];
    iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <group>
    while iXMLRootNode4 <> nil do
    begin
      if iXMLRootNode4.NodeName = 'photo' then
      begin
        id := iXMLRootNode4.attributes['id'];
        ismember := iXMLRootNode4.attributes['member'];
        title := iXMLRootNode4.attributes['name'];
        if ismember = '1' then
        begin
          Item := listGroups.Items.Add;
          Item.Caption := id;
          Item.SubItems.Add(title);
        end;
      end;
      progressfetchinggroups.position := progressfetchinggroups.position + 1;
      taskbar1.ProgressValue := progressfetchinggroups.position;
      Application.ProcessMessages;
      iXMLRootNode4 := iXMLRootNode4.NextSibling;
    end;
  end;
  btnLoad.Enabled := true;
  btnAdd.Enabled := true;
  batchUpdate.Enabled := true;
  btnAddItems.Enabled := true;
  lblfetchingGroup.visible := false;
  progressfetchinggroups.visible := false;
  taskbar1.ProgressValue := 0;
  listGroups.Visible := true;
end;

function TfrmFlickr.getTotalGroupCounts() : integer;
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  pages, total: string;
  numPages, numTotal : integer;
  i: Integer;
  totalViews : integer;
begin
  btnLoad.Enabled := false;
  btnAdd.Enabled := false;
  btnAddItems.Enabled := false;
  batchUpdate.Enabled := false;
  listPhotosUser.Visible := false;
  lblfetching.visible := true;
  progressfetching.visible := true;
  Application.ProcessMessages;
  response := IdHTTP1.Get(TFlickrRest.new().getPhotoSets(apikey.text, edtUserId.Text, '1', '500'));
  XMLDocument1.LoadFromXML(response);
  iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
  iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
  iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photosets>
  pages := iXMLRootNode3.attributes['pages'];
  total := iXMLRootNode3.attributes['total'];
  iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photoset>
  listPhotosUser.Clear;
  numTotal := total.ToInteger();
  progressfetching.Max := numTotal;
  taskbar1.ProgressState := TTaskBarProgressState.Normal;
  taskbar1.ProgressMaxValue := numTotal;
  progressfetching.position := 0;
  totalViews := 0;
  while iXMLRootNode4 <> nil do
  begin
    if iXMLRootNode4.NodeName = 'photoset' then
    begin
      totalViews := totalViews + iXMLRootNode4.attributes['count_views'];
    end;
    progressfetching.position := progressfetching.position + 1;
    taskbar1.ProgressValue := progressfetching.position;
    Application.ProcessMessages;
    iXMLRootNode4 := iXMLRootNode4.NextSibling;
  end;

  //Load the remaining pages
  numPages := pages.ToInteger;
  for i := 2 to numpages do
  begin
    response := IdHTTP1.Get(TFlickrRest.new().getPhotoSets(apikey.text, edtUserId.Text, i.ToString, '500'));
    XMLDocument1.LoadFromXML(response);
    iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
    iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
    iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photosets>
    pages := iXMLRootNode3.attributes['pages'];
    iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photoset>
    while iXMLRootNode4 <> nil do
    begin
      if iXMLRootNode4.NodeName = 'photoset' then
      begin
        totalViews := totalViews + iXMLRootNode4.attributes['count_views'];
      end;
      progressfetching.position := progressfetching.position + 1;
      taskbar1.ProgressValue := progressfetching.position;
      Application.ProcessMessages;
      iXMLRootNode4 := iXMLRootNode4.NextSibling;
    end;
  end;
  btnLoad.Enabled := true;
  btnAdd.Enabled := true;
  batchUpdate.Enabled := true;
  btnAddItems.Enabled := true;
  lblfetching.visible := false;
  progressfetching.visible := false;
  taskbar1.ProgressValue := 0;
  listPhotosUser.Visible := true;
  result := totalViews;
end;

procedure TfrmFlickr.btnGetListClick(Sender: TObject);
var
  Item: TListItem;
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  pages, title, id, ispublic, total: string;
  numPages, numTotal : integer;
  i: Integer;
begin
  if apikey.Text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  if edtUserId.Text = '' then
  begin
    showmessage('Api key can''t be empty');
    exit;
  end;
  btnLoad.Enabled := false;
  btnAdd.Enabled := false;
  btnAddItems.Enabled := false;
  batchUpdate.Enabled := false;
  listPhotosUser.Visible := false;
  lblfetching.visible := true;
  progressfetching.visible := true;
  Application.ProcessMessages;
  response := IdHTTP1.Get(TFlickrRest.new().getPhotos(apikey.text, edtUserId.Text, '1', '500'));
  XMLDocument1.LoadFromXML(response);
  iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
  iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
  iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photos>
  pages := iXMLRootNode3.attributes['pages'];
  total := iXMLRootNode3.attributes['total'];
  iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photo>
  listPhotosUser.Clear;
  numTotal := total.ToInteger();
  progressfetching.Max := numTotal;
  taskbar1.ProgressState := TTaskBarProgressState.Normal;
  taskbar1.ProgressMaxValue := numTotal;
  progressfetching.position := 0;
  while iXMLRootNode4 <> nil do
  begin
    if iXMLRootNode4.NodeName = 'photo' then
    begin
      id := iXMLRootNode4.attributes['id'];
      ispublic := iXMLRootNode4.attributes['ispublic'];
      title := iXMLRootNode4.attributes['title'];
      if ispublic = '1' then
      begin
        Item := listPhotosUser.Items.Add;
        Item.Caption := id;
        Item.SubItems.Add(title);
      end;
    end;
    progressfetching.position := progressfetching.position + 1;
    taskbar1.ProgressValue := progressfetching.position;
    Application.ProcessMessages;
    iXMLRootNode4 := iXMLRootNode4.NextSibling;
  end;

  //Load the remaining pages
  numPages := pages.ToInteger;
  for i := 2 to numpages do
  begin
    response := IdHTTP1.Get(TFlickrRest.new().getPhotos(apikey.text, edtUserId.Text, i.ToString, '500'));
    XMLDocument1.LoadFromXML(response);
    iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
    iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
    iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photos>
    pages := iXMLRootNode3.attributes['pages'];
    iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photo>
    while iXMLRootNode4 <> nil do
    begin
      if iXMLRootNode4.NodeName = 'photo' then
      begin
        id := iXMLRootNode4.attributes['id'];
        ispublic := iXMLRootNode4.attributes['ispublic'];
        title := iXMLRootNode4.attributes['title'];
        if ispublic = '1' then
        begin
          Item := listPhotosUser.Items.Add;
          Item.Caption := id;
          Item.SubItems.Add(title);
        end;
      end;
      progressfetching.position := progressfetching.position + 1;
      taskbar1.ProgressValue := progressfetching.position;
      Application.ProcessMessages;
      iXMLRootNode4 := iXMLRootNode4.NextSibling;
    end;
  end;
  btnLoad.Enabled := true;
  btnAdd.Enabled := true;
  batchUpdate.Enabled := true;
  btnAddItems.Enabled := true;
  lblfetching.visible := false;
  progressfetching.visible := false;
  taskbar1.ProgressValue := 0;
  listPhotosUser.Visible := true;
end;

procedure TfrmFlickr.btnAddItemsClick(Sender: TObject);
var
  i : integer;
  item : TListItem;
begin
  btnAddItems.enabled := false;
  ProgressBar1.Visible := true;
  photoId.Enabled := false;
  btnAdd.Enabled := false;
  batchUpdate.Enabled := false;
  Process.Visible := true;
  ProgressBar1.Min := 0;
  ProgressBar1.Max := listPhotos.Items.Count;
  for i := 0 to listPhotosuser.Items.Count - 1 do
  begin
    Process.Caption := 'Processing image: ' + listPhotosuser.Items[i].Caption + ' ' + i.ToString + ' out of ' +  listPhotosuser.Items.Count.ToString;
    ProgressBar1.position := i;
    Application.ProcessMessages;

    if not ExistPhotoInList(listPhotosuser.Items[i].Caption, item)  then
    begin
      photoId.Text := listPhotosuser.Items[i].Caption;
      btnAddClick(sender);
    end;
  end;
  photoId.Text := '';
  ProgressBar1.Visible := false;
  Process.Visible := false;
  UpdateTotals();
  LoadHallOfFame(repository);
  btnSave.Enabled := true;
  batchUpdate.Enabled := true;
  photoId.Enabled := true;
  btnAdd.Enabled := true;
  btnAddItems.enabled := true;
end;

function TfrmFlickr.SaveToExcel(AView: TListView; ASheetName, AFileName: string): Boolean;
const
  xlWBATWorksheet = -4167;
var
  Row: Integer;
  ExcelOLE, Sheet: OLEVariant;
  i: Integer;
begin
  // Create Excel-OLE Object
  Result := False;
  ExcelOLE := CreateOleObject('Excel.Application');
  try
    // Hide Excel
    ExcelOLE.Visible := False;

    ExcelOLE.Workbooks.Add(xlWBatWorkSheet);
    Sheet := ExcelOLE.Workbooks[1].WorkSheets[1];
    Sheet.Name := ASheetName;

    Sheet.Cells[1, 1] := 'Id';
    Sheet.Cells[1, 2] := 'Title';
    Sheet.Cells[1, 3] := 'Views';
    Sheet.Cells[1, 4] := 'Likes';
    Sheet.Cells[1, 5] := 'Comments';
    Sheet.Cells[1, 6] := 'Last Update';
    Sheet.Cells[1, 7] := 'Affection';

    Row := 2;
    for i := 0 to AView.Items.Count - 1 do
    begin
      Sheet.Cells[Row, 1] := AView.Items.Item[i].Caption;
      Sheet.Cells[Row, 2] := AView.Items.Item[i].SubItems[0];
      Sheet.Cells[Row, 3] := AView.Items.Item[i].SubItems[1];
      Sheet.Cells[Row, 4] := AView.Items.Item[i].SubItems[2];
      Sheet.Cells[Row, 5] := AView.Items.Item[i].SubItems[3];
      Sheet.Cells[Row, 6] := AView.Items.Item[i].SubItems[4];
      Sheet.Cells[Row, 7] := AView.Items.Item[i].SubItems[5];
      inc(Row);
    end;

    try
      ExcelOLE.Workbooks[1].SaveAs(AFileName);
      Result := True;
    except

    end;
  finally
    if not VarIsEmpty(ExcelOLE) then
    begin
      ExcelOLE.DisplayAlerts := False;
      ExcelOLE.Quit;
      ExcelOLE := Unassigned;
      Sheet := Unassigned;
    end;
  end;
end;

procedure TfrmFlickr.btnExcelClick(Sender: TObject);
begin
if SaveToExcel(listPhotos, 'Flickr Analytics', ExtractFilePath(ParamStr(0)) + 'FlickrAnalytics.xls') then
    ShowMessage('Data saved successfully!');
end;

procedure TfrmFlickr.FormCreate(Sender: TObject);
begin
  repository := TFlickrRepository.Create();
  globalsRepository := TFlickrGlobals.Create();
  CheckedSeries := TStringList.Create;
  Process.Visible := false;
end;

procedure TfrmFlickr.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CheckedSeries);
  repository := nil;
  globalsRepository := nil;
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
    if ((Item.SubItems.Strings[1].ToInteger >= 1000) and (Item.SubItems.Strings[1].ToInteger < 3000)) then
    begin
      Sender.Canvas.Font.Color := clBlue;
      Sender.Canvas.Brush.Color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 3000) and (Item.SubItems.Strings[1].ToInteger < 5000)) then
    begin
      Sender.Canvas.Font.Color := clGreen;
      Sender.Canvas.Brush.Color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 5000) and (Item.SubItems.Strings[1].ToInteger < 8000)) then
    begin
      Sender.Canvas.Font.Color := clOlive;
      Sender.Canvas.Brush.Color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 8000) and (Item.SubItems.Strings[1].ToInteger < 10000)) then
    begin
      Sender.Canvas.Font.Color := clFuchsia;
      Sender.Canvas.Brush.Color := Color2;
    end;
    if ((Item.SubItems.Strings[1].ToInteger >= 10000)) then
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
