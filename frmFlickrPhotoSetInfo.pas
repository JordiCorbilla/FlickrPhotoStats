unit frmFlickrPhotoSetInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, Vcl.ExtCtrls,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, IdHTTP, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, ActiveX, IdGlobal;

type
  TfrmFlickrPhotoSet = class(TForm)
    Panel1: TPanel;
    chartItemCommentsH: TChart;
    BarSeries2: TBarSeries;
    Splitter1: TSplitter;
    Panel2: TPanel;
    chartitemLikesH: TChart;
    BarSeries8: TBarSeries;
    Splitter2: TSplitter;
    Panel3: TPanel;
    ChartItemViewsH: TChart;
    BarSeries7: TBarSeries;
    Splitter3: TSplitter;
    Panel4: TPanel;
    Panel24: TPanel;
    Panel27: TPanel;
    Image1: TImage;
    Label14: TLabel;
    LabelTodayViews: TLabel;
    Panel26: TPanel;
    Panel29: TPanel;
    Image3: TImage;
    Label42: TLabel;
    LabelTodayComments: TLabel;
    Panel25: TPanel;
    Panel28: TPanel;
    Image2: TImage;
    Label15: TLabel;
    LabelTodayLikes: TLabel;
  private
    { Private declarations }
  public
    procedure LoadPhotos(photosetId : string);
  end;

var
  frmFlickrPhotoSet: TfrmFlickrPhotoSet;

implementation

uses
  Flickr.rest;

{$R *.dfm}

{ TfrmFlickrPhotoSet }

procedure TfrmFlickrPhotoSet.LoadPhotos(photosetId: string);
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4, iXMLRootNode5: IXMLNode;
  pages, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  totalViews: Integer;
  title: string;
  countViews: Integer;
  numPhotos: Integer;
  Series : TPieSeries;
  color : TColor;
  IdHTTP: TIdHTTP;
  xmlDocument: IXMLDocument;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  timedout : boolean;
begin
  if ChartItemViewsH.SeriesList.Count > 0 then
    ChartItemViewsH.RemoveAllSeries;
  if chartitemLikesH.SeriesList.Count > 0 then
    chartitemLikesH.RemoveAllSeries;
  if chartItemCommentsH.SeriesList.Count > 0 then
    chartItemCommentsH.RemoveAllSeries;

  CoInitialize(nil);
  try
    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    xmlDocument := TXMLDocument.Create(nil);
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      timedout := false;
      while (not timedout) do
      begin
        try
          response := IdHTTP.Get(TFlickrRest.New().getPhotosPhotoSet(apikey.text, id));
          timedout := true;
        except
          on e: exception do
          begin
            sleep(timeout);
            timedout := false;
          end;
        end;

      end;

      xmlDocument.LoadFromXML(response);
      iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
      iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
      iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photo>
      views := iXMLRootNode3.attributes['views'];
      iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <owner>
      tags := '';
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'title' then
          title := iXMLRootNode4.NodeValue;
        if iXMLRootNode4.NodeName = 'dates' then
          taken := iXMLRootNode4.attributes['taken'];
        if iXMLRootNode4.NodeName = 'comments' then
          comments := iXMLRootNode4.NodeValue;
        if iXMLRootNode4.NodeName = 'tags' then
        begin
          iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
          while iXMLRootNode5 <> nil do
          begin
            if (iXMLRootNode5.NodeName = 'tag') and (iXMLRootNode5.NodeValue <> 'jordicorbilla') and (iXMLRootNode5.NodeValue <> 'jordicorbillaphotography') then
              tags := tags + iXMLRootNode5.NodeValue + ',';
            iXMLRootNode5 := iXMLRootNode5.NextSibling;
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
      xmlDocument := nil;
    end;

    IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdIOHandler.ReadTimeout := IdTimeoutInfinite;
    IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
    xmlDocument := TXMLDocument.Create(nil);
    IdHTTP := TIdHTTP.Create(nil);
    try
      IdHTTP.IOHandler := IdIOHandler;
      timedout := false;
      while (not timedout) do
      begin
        try
          response := IdHTTP.Get(TFlickrRest.New().getFavorites(apikey.text, id));
          timedout := true;
        except
          on e: exception do
          begin
            sleep(timeout);
            timedout := false;
          end;
        end;
      end;

      xmlDocument.LoadFromXML(response);
      iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
      iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
      iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photo>
      likes := iXMLRootNode3.attributes['total'];
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
      xmlDocument := nil;
    end;

    stat := TStat.Create(Date, StrToInt(views), StrToInt(likes), StrToInt(comments));

    photoGroups := repository.GetPhoto(id);
    if photoGroups <> nil then
    begin
      Groups := photoGroups.Groups;
      Albums := photoGroups.Albums;
    end
    else
    begin
      photo := TPhoto.Create(id, title, taken, tags);
      Groups := photo.Groups;
      Albums := photo.Albums;
    end;

    if chkUpdateCollections.checked then
    begin
      IdIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      IdIOHandler.ReadTimeout := IdTimeoutInfinite;
      IdIOHandler.ConnectTimeout := IdTimeoutInfinite;
      xmlDocument := TXMLDocument.Create(nil);
      IdHTTP := TIdHTTP.Create(nil);
      try
        IdHTTP.IOHandler := IdIOHandler;
        timedout := false;
        while (not timedout) do
        begin
          try
            response := IdHTTP.Get(TFlickrRest.New().getAllContexts(apikey.text, id));
            timedout := true;
          except
            on e: exception do
            begin
              sleep(timeout);
              timedout := false;
            end;
          end;
        end;

        xmlDocument.LoadFromXML(response);
        iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
        iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
        iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <set or pool>
        while iXMLRootNode3 <> nil do
        begin
          if iXMLRootNode3.NodeName = 'set' then
            Albums.AddItem(TAlbum.create(iXMLRootNode3.attributes['id'], iXMLRootNode3.attributes['title']));
          if iXMLRootNode3.NodeName = 'pool' then
            Groups.AddItem(TPool.create(iXMLRootNode3.attributes['id'], iXMLRootNode3.attributes['title'], Date));
          iXMLRootNode3 := iXMLRootNode3.NextSibling;
        end;
      finally
        IdIOHandler.Free;
        IdHTTP.Free;
        xmlDocument := nil;
      end;
    end;

    if repository.ExistPhoto(id, existing) then
    begin
      photo := existing;

      if organicStat <> nil then
      begin
        if photo.getTotalViewsDay() >= views.ToInteger() then
          organicStat.negativeViews := organicStat.negativeViews + 1
        else
          organicStat.positiveViews := organicStat.positiveViews + 1;

        if photo.getTotalLikesDay() > likes.ToInteger() then
          organicStat.lostLikes := organicStat.lostLikes + 1
        else if photo.getTotalLikesDay() = likes.ToInteger() then
          organicStat.negativeLikes := organicStat.negativeLikes + 1
        else
          organicStat.positiveLikes := organicStat.positiveLikes + 1;

        if photo.getTotalCommentsDay() > comments.ToInteger() then
          organicStat.lostComments := organicStat.lostComments + 1
        else if photo.getTotalCommentsDay() = comments.ToInteger() then
          organicStat.negativeComments := organicStat.negativeComments + 1
        else
          organicStat.positiveComments := organicStat.positiveComments + 1;

        organicStat.TotalGroups := repository.getTotalSpreadGroups();
      end;

      photo.Title := title; //replace the title as it changes
      photo.Taken := taken;
      photo.tags := tags;
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
      Item := frmFlickrMain.listPhotos.Items.Add;
      Item.Caption := frmFlickrMain.photoId.text;
      Item.SubItems.Add(title);
      Item.SubItems.Add(views);
      Item.SubItems.Add(likes);
      Item.SubItems.Add(comments);
      Item.SubItems.Add(DateToStr(Date));
      if views = '0' then
        views := '1';
      Item.SubItems.Add(taken);
      Item.SubItems.Add(photo.Albums.Count.ToString());
      Item.SubItems.Add(photo.Groups.Count.ToString());
      Item.SubItems.Add(tags);
      Item.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger / views.ToInteger) * 100.0));
      Item.SubItems.Add(photo.banned.ToString());
      Item.SubItems.Add(photo.getTrend.ToString());
      Item.SubItems.Add(photo.OmitGroups);
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
      itemExisting.SubItems.Add(taken);
      itemExisting.SubItems.Add(photo.Albums.Count.ToString());
      itemExisting.SubItems.Add(photo.Groups.Count.ToString());
      itemExisting.SubItems.Add(tags);
      itemExisting.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger / views.ToInteger) * 100.0));
      itemExisting.SubItems.Add(photo.banned.ToString());
      itemExisting.SubItems.Add(photo.getTrend.ToString());
      itemExisting.SubItems.Add(photo.OmitGroups);
    end;
    if chkRealTime.Checked then
      UpdateTotals(true);
  finally
    CoUninitialize;
  end;



  Series := flickrChart.GetNewPieSeries(chartAlbum, true);


  Application.ProcessMessages;
  response := IdHTTP1.Get(TFlickrRest.New().getPhotoSets(apikey.text, edtUserId.text, '1', '500'));
  XMLDocument1.LoadFromXML(response);
  iXMLRootNode := XMLDocument1.ChildNodes.first; // <xml>
  iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
  iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photosets>
  pages := iXMLRootNode3.attributes['pages'];
  total := iXMLRootNode3.attributes['total'];
  iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photoset>
  numTotal := total.ToInteger();
  progressbar1.Max := numTotal;
  Taskbar1.ProgressState := TTaskBarProgressState.Normal;
  Taskbar1.ProgressMaxValue := numTotal;
  progressbar1.position := 0;
  totalViews := 0;
  while iXMLRootNode4 <> nil do
  begin
    if iXMLRootNode4.NodeName = 'photoset' then
    begin
      photosetId := iXMLRootNode4.attributes['id'];
      numPhotos := iXMLRootNode4.attributes['photos'];
      countViews := iXMLRootNode4.attributes['count_views'];
      iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
      title := iXMLRootNode5.text;
      totalViews := totalViews + countViews;
    end;
    progressbar1.position := progressbar1.position + 1;
    listAlbums.Lines.Add('Id: ' + photosetId + ' title: ' + title + ' Photos: ' + numPhotos.ToString() + ' Views: ' + countViews.ToString());
    color := RGB(Random(255), Random(255), Random(255));
    Series.Add(countViews.ToDouble, photosetId + '/' + numPhotos.ToString() + '/' + countViews.ToString(), color);
    Taskbar1.ProgressValue := progressbar1.position;
    Application.ProcessMessages;
    iXMLRootNode4 := iXMLRootNode4.NextSibling;
  end;

  // Load the remaining pages
  numPages := pages.ToInteger;
  for i := 2 to numPages do
  begin
    response := IdHTTP1.Get(TFlickrRest.New().getPhotoSets(apikey.text, edtUserId.text, i.ToString, '500'));
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
        photosetId := iXMLRootNode4.attributes['id'];
        numPhotos := iXMLRootNode4.attributes['photos'];
        countViews := iXMLRootNode4.attributes['count_views'];
        iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
        title := iXMLRootNode5.text;
        totalViews := totalViews + countViews;
      end;
      progressbar1.position := progressbar1.position + 1;
      listAlbums.Lines.Add('Id: ' + photosetId + ' title: ' + title + ' Photos: ' + numPhotos.ToString() + ' Views: ' + countViews.ToString());
      color := RGB(Random(255), Random(255), Random(255));
      Series.Add(countViews.ToDouble, photosetId + '/' + numPhotos.ToString() + '/' + countViews.ToString(), color);
      Taskbar1.ProgressValue := progressbar1.position;
      Application.ProcessMessages;
      iXMLRootNode4 := iXMLRootNode4.NextSibling;
    end;
  end;

  chartAlbum.AddSeries(Series);

  progressbar1.Visible := false;
  Taskbar1.ProgressValue := 0;
  listPhotosUser.Visible := true;
  listAlbums.Lines.Add('******************************************');
  listAlbums.Lines.Add('Total views: ' + totalViews.ToString());
  Result := totalViews;
end;

end.
