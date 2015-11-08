unit frmFlickrPhotoSetInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, Vcl.ExtCtrls,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, IdHTTP, IdSSLOpenSSL, XMLDoc, xmldom, XMLIntf, msxmldom, ActiveX, IdGlobal,
  Generics.collections, flickr.repository, flickr.photos;

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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    repository: IFlickrRepository;
    total : string;
    procedure LoadPhotos(api_key: string; user_id: string; photosetId : string; auth_token : string; secret : string; token_secret : string);
  end;

var
  frmFlickrPhotoSet: TfrmFlickrPhotoSet;

implementation

uses
  Flickr.rest, flickr.charts;

{$R *.dfm}

{ TfrmFlickrPhotoSet }

procedure TfrmFlickrPhotoSet.FormResize(Sender: TObject);
begin
  panel4.Height := 75;
  ChartItemViewsH.Height := (frmFlickrPhotoSet.Height - panel4.Height) div 3;
  chartitemLikesH.Height := (frmFlickrPhotoSet.Height - panel4.Height) div 3;
  chartItemCommentsH.Height := (frmFlickrPhotoSet.Height - panel4.Height) div 3;

  panel24.Width := round(panel4.Width / 3);
  panel25.Width := round(panel4.Width / 3);

  panel27.Left := (panel24.Width-panel27.Width) div 2;
  panel28.Left := (panel25.Width-panel28.Width) div 2;
  panel29.Left := (panel26.Width-panel29.Width) div 2;
end;

procedure TfrmFlickrPhotoSet.LoadPhotos(api_key: string; user_id: string; photosetId : string; auth_token : string; secret : string; token_secret : string);
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  pages, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  totalViews: Integer;
  IdHTTP: TIdHTTP;
  xmlDocument: IXMLDocument;
  IdIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  timedout : boolean;
  ListPhotos : TList<string>;
  photoId : string;
  flickrChart : IFlickrChart;
  SeriesViews, SeriesLikes, SeriesComments : TBarSeries;
  photo : IPhoto;
  totalViewsMark, totalLikesMark, totalCommentsMark : integer;
  FColorComments, FColorViews, FColorLikes : TColor;
  valueTotal : integer;
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
    ListPhotos := TList<string>.create();
    try
      IdHTTP.IOHandler := IdIOHandler;
      timedout := false;
      while (not timedout) do
      begin
        try
          response := IdHTTP.Get(TFlickrRest.New().getPhotosPhotoSet(api_key, user_id, photosetId, '1', '500', auth_token, secret, token_secret));
          timedout := true;
        except
          on e: exception do
          begin
            sleep(200);
            timedout := false;
          end;
        end;
      end;

      xmlDocument.LoadFromXML(response);
      iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
      iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
      iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photosets>
      pages := iXMLRootNode3.attributes['pages'];
      total := iXMLRootNode3.attributes['total'];
      iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photoset>
      numTotal := total.ToInteger();
      totalViews := 0;
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photo' then
        begin
          photoId := iXMLRootNode4.attributes['id'];
          ListPhotos.Add(photoId);
        end;
        Application.ProcessMessages;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    finally
      IdIOHandler.Free;
      IdHTTP.Free;
      xmlDocument := nil;
    end;

    numPages := pages.ToInteger;
    for i := 2 to numPages do
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
            response := IdHTTP.Get(TFlickrRest.New().getPhotosPhotoSet(api_key, user_id, photosetId, i.ToString(), '500', auth_token, secret, token_secret));
            timedout := true;
          except
            on e: exception do
            begin
              sleep(200);
              timedout := false;
            end;
          end;
        end;

        xmlDocument.LoadFromXML(response);
        iXMLRootNode := xmlDocument.ChildNodes.first; // <xml>
        iXMLRootNode2 := iXMLRootNode.NextSibling; // <rsp>
        iXMLRootNode3 := iXMLRootNode2.ChildNodes.first; // <photosets>
        iXMLRootNode4 := iXMLRootNode3.ChildNodes.first; // <photoset>
        while iXMLRootNode4 <> nil do
        begin
          if iXMLRootNode4.NodeName = 'photo' then
          begin
            photoId := iXMLRootNode4.attributes['id'];
            ListPhotos.Add(photoId);
          end;
          Application.ProcessMessages;
          iXMLRootNode4 := iXMLRootNode4.NextSibling;
        end;
      finally
        IdIOHandler.Free;
        IdHTTP.Free;
        xmlDocument := nil;
      end;
    end;

    flickrChart := TFlickrChart.create;
    SeriesViews := flickrChart.GetNewBarSeries(ChartItemViewsH, false);
    SeriesLikes := flickrChart.GetNewBarSeries(chartitemLikesH, false);
    SeriesComments := flickrChart.GetNewBarSeries(chartItemCommentsH, false);

    totalViewsMark := 0;
    totalLikesMark := 0;
    totalCommentsMark := 0;
    FColorViews := RGB(107, 60, 82);
    FColorLikes := RGB(57, 65, 173);
    FColorComments := RGB(148, 101, 24);
    for i := 0 to ListPhotos.Count-1 do
    begin
      if repository.ExistPhoto(ListPhotos[i], photo) then
      begin
        SeriesViews.AddBar(photo.getTotalViewsDay(), photo.Id, FColorViews);
        SeriesLikes.AddBar(photo.getTotalLikesDay(), photo.Id, FColorLikes);
        SeriesComments.AddBar(photo.getTotalCommentsDay(), photo.Id, FColorComments);
        totalViewsMark := totalViewsMark + photo.getTotalViewsDay();
        totalLikesMark := totalLikesMark + photo.getTotalLikesDay();
        totalCommentsMark := totalCommentsMark + photo.getTotalCommentsDay();
      end;
    end;

    ChartItemViewsH.AddSeries(SeriesViews);
    chartitemLikesH.AddSeries(SeriesLikes);
    chartItemCommentsH.AddSeries(SeriesComments);

    LabelTodayViews.Caption :=  Format('%n',[totalViewsMark.ToDouble]).Replace('.00','');
    LabelTodayLikes.Caption :=  Format('%n',[totalLikesMark.ToDouble]).Replace('.00','');
    LabelTodayComments.Caption :=  Format('%n',[totalCommentsMark.ToDouble]).Replace('.00','');
    valueTotal := total.toInteger();

    Label2.Caption := Format('%n',[valueTotal.ToDouble]).Replace('.00','');
    valueTotal := valueTotal + totalViewsMark;
    Label4.Caption := Format('%n',[valueTotal.ToDouble]).Replace('.00','');
  finally
    ListPhotos.Free;
  end;
end;

end.
