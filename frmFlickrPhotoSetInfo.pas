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

unit frmFlickrPhotoSetInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, Vcl.ExtCtrls,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, XMLIntf, Generics.collections, flickr.repository, flickr.photos, flickr.lib.options.agent;

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
    Label14: TLabel;
    LabelTodayViews: TLabel;
    Panel26: TPanel;
    Panel29: TPanel;
    Label42: TLabel;
    LabelTodayComments: TLabel;
    Panel25: TPanel;
    Panel28: TPanel;
    Label15: TLabel;
    LabelTodayLikes: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Shape1: TShape;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    repository: IFlickrRepository;
    optionsAgent : IOptionsAgent;
    totalFromAlbum : string;
    procedure LoadPhotos(photosetId : string);
  end;

var
  frmFlickrPhotoSet: TfrmFlickrPhotoSet;

implementation

uses
  Flickr.rest, flickr.charts, flickr.http.lib;

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

procedure TfrmFlickrPhotoSet.LoadPhotos(photosetId : string);
var
  iXMLRootNode4: IXMLNode;
  pages, total: string;
  numPages, numTotal: Integer;
  i: Integer;
  totalViews: Integer;
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

  ListPhotos := TList<string>.create();
  try
    THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotosPhotoSet(photosetId, '1', '500'), procedure (iXMLRootNode : IXMLNode)
      begin
        pages := iXMLRootNode.attributes['pages'];
        total := iXMLRootNode.attributes['total'];
        iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photoset>
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
      end);

    numPages := pages.ToInteger;
    for i := 2 to numPages do
    begin
      THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotosPhotoSet(photosetId, i.ToString(), '500'), procedure (iXMLRootNode : IXMLNode)
      begin
        iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photoset>
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
      end);
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
    valueTotal := totalFromAlbum.toInteger();

    Label2.Caption := Format('%n',[valueTotal.ToDouble]).Replace('.00','');
    valueTotal := valueTotal + totalViewsMark;
    Label4.Caption := Format('%n',[valueTotal.ToDouble]).Replace('.00','');
  finally
    ListPhotos.Free;
  end;
end;

end.
