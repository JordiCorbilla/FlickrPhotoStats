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

unit flickr.top.stats;

interface

uses
  Flickr.repository, Contnrs, Generics.Collections, Generics.defaults, flickr.photos, vcltee.series, flickr.photo.trend.info;

type
  TTopStats = class(TObject)
  private
    FRepository : IFlickrRepository;
  public
    constructor Create(repository : IFlickrRepository);
    destructor Destroy(); override;
    function GetTopXNumberOfLikes(num : integer; series : TPieSeries) : string;
    function GetTopXNumberOfViews(num : integer; series : TPieSeries) : string;
    function GetTopXNumberOfComments(num : integer) : string;
    function GetTopXNumberofMostViewed() : TList<IPhoto>;
    function GetTopXNumberofMostLiked() : TList<IPhoto>;
    function GetListNumberOfViews(num : integer) : TList<IPhoto>;
    function GetListNumberOfLikes(num : integer) : TList<IPhoto>;
    function GetListTrendingActivityViews(num : integer) : TList<IPhotoTrend>;
    function GetListTrendingActivityLikes(num : integer) : TList<IPhotoTrend>;
    function GetListTrendingActivityComments(num : integer) : TList<IPhotoTrend>;
  end;


implementation

uses
  Sysutils, vcl.Dialogs, System.UITypes, windows, flickr.list.comparer;

{ TTopStats }

constructor TTopStats.Create(repository: IFlickrRepository);
begin
  FRepository := repository;
end;

destructor TTopStats.Destroy;
begin
  FRepository := nil;
  inherited;
end;

function TTopStats.GetListNumberOfLikes(num: integer): TList<IPhoto>;
var
  PhotosSorted: TList<IPhoto>;
  PhotosResult: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerLikes;
  i : integer;
begin
  IPhotoComparer := TIPhotoComparerLikes.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);
  PhotosResult := TList<IPhoto>.Create();

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  for i := 0 to num-1 do
  begin
    if i < photosSorted.count then
    begin
      PhotosResult.Add(PhotosSorted[i]);
    end;
  end;
  PhotosSorted.Free;
  result := PhotosResult;
end;

function TTopStats.GetListNumberOfViews(num: integer): TList<IPhoto>;
var
  PhotosSorted: TList<IPhoto>;
  PhotosResult: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerViews;
  i : integer;
begin
  IPhotoComparer := TIPhotoComparerViews.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);
  PhotosResult := TList<IPhoto>.Create();

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  for i := 0 to num-1 do
  begin
    if i < photosSorted.count then
    begin
      PhotosResult.Add(PhotosSorted[i]);
    end;
  end;
  PhotosSorted.Free;
  result := PhotosResult;
end;

function TTopStats.GetListTrendingActivityComments(num: integer): TList<IPhotoTrend>;
var
  PhotosResult, PhotosTransfer: TList<IPhotoTrend>;
  IPhotoComparer : TIPhotoTrendComparer;
  i : integer;
  iTrend : IPhotoTrend;
begin
  IPhotoComparer := TIPhotoTrendComparer.Create;
  PhotosResult := TList<IPhotoTrend>.Create(IPhotoComparer);
  PhotosTransfer := TList<IPhotoTrend>.Create();

  for I := 0 to FRepository.photos.Count-1 do
  begin
    iTrend := TPhotoTrend.Create;
    iTrend.Id := FRepository.photos[i].Id;
    iTrend.Title := FRepository.photos[i].Title;
    iTrend.Value := FRepository.photos[i].getTotalCommentsDay() - FRepository.photos[i].getTotalCommentsDay(-1);
    PhotosResult.Add(iTrend);
  end;

  PhotosResult.Sort;

  for i := 0 to num-1 do
  begin
    if i < PhotosResult.count then
    begin
      PhotosTransfer.Add(PhotosResult[i]);
    end;
  end;
  PhotosResult.Free;
  result := PhotosTransfer;
end;

function TTopStats.GetListTrendingActivityLikes(num: integer): TList<IPhotoTrend>;
var
  PhotosResult, PhotosTransfer: TList<IPhotoTrend>;
  IPhotoComparer : TIPhotoTrendComparer;
  i : integer;
  iTrend : IPhotoTrend;
begin
  IPhotoComparer := TIPhotoTrendComparer.Create;
  PhotosResult := TList<IPhotoTrend>.Create(IPhotoComparer);
  PhotosTransfer := TList<IPhotoTrend>.Create();

  for I := 0 to FRepository.photos.Count-1 do
  begin
    iTrend := TPhotoTrend.Create;
    iTrend.Id := FRepository.photos[i].Id;
    iTrend.Title := FRepository.photos[i].Title;
    iTrend.Value := FRepository.photos[i].getTotalLikesDay() - FRepository.photos[i].getTotalLikesDay(-1);
    PhotosResult.Add(iTrend);
  end;

  PhotosResult.Sort;

  for i := 0 to num-1 do
  begin
    if i < PhotosResult.count then
    begin
      PhotosTransfer.Add(PhotosResult[i]);
    end;
  end;
  PhotosResult.Free;
  result := PhotosTransfer;
end;

function TTopStats.GetListTrendingActivityViews(num: integer): TList<IPhotoTrend>;
var
  PhotosResult, PhotosTransfer: TList<IPhotoTrend>;
  IPhotoComparer : TIPhotoTrendComparer;
  i : integer;
  iTrend : IPhotoTrend;
begin
  IPhotoComparer := TIPhotoTrendComparer.Create;
  PhotosResult := TList<IPhotoTrend>.Create(IPhotoComparer);
  PhotosTransfer := TList<IPhotoTrend>.Create();

  for I := 0 to FRepository.photos.Count-1 do
  begin
    iTrend := TPhotoTrend.Create;
    iTrend.Id := FRepository.photos[i].Id;
    iTrend.Title := FRepository.photos[i].Title;
    iTrend.Value := FRepository.photos[i].getTotalViewsDay() - FRepository.photos[i].getTotalViewsDay(-1);
    PhotosResult.Add(iTrend);
  end;

  PhotosResult.Sort;

  for i := 0 to num-1 do
  begin
    if i < PhotosResult.count then
    begin
      PhotosTransfer.Add(PhotosResult[i]);
    end;
  end;
  PhotosResult.Free;
  result := PhotosTransfer;
end;

function TTopStats.GetTopXNumberOfComments(num: integer): string;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerComments;
  i : integer;
  description : string;
begin
  IPhotoComparer := TIPhotoComparerComments.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  description := 'Top ' + num.ToString + ' images with most Comments: ' + sLineBreak;
  for i := 0 to num-1 do
  begin
    if i < photosSorted.count then
    begin
      description := description + PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Comments: ' + PhotosSorted[i].getTotalCommentsDay.ToString + sLineBreak;
    end;
  end;
  PhotosSorted.Free;
  //IPhotoComparer := nil;
  result := description;
end;

function TTopStats.GetTopXNumberOfLikes(num: integer; series : TPieSeries): string;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerLikes;
  i : integer;
  description : string;
  color : TColor;
begin
  IPhotoComparer := TIPhotoComparerLikes.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  description := 'Top ' + num.ToString + ' images with most likes: ' + sLineBreak;
  for i := 0 to num-1 do
  begin
    if i < photosSorted.count then
    begin
      description := description + PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Likes: ' + PhotosSorted[i].getTotalLikesDay.ToString + sLineBreak;
      color := RGB(Random(255), Random(255), Random(255));
      series.Add(PhotosSorted[i].getTotalLikesDay.ToDouble, PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Likes: ' + PhotosSorted[i].getTotalLikesDay.ToString, color);
    end;
  end;
  PhotosSorted.Free;
  //IPhotoComparer := nil;
  result := description;
end;

function TTopStats.GetTopXNumberofMostLiked(): TList<IPhoto>;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerMostLikes;
  i : integer;
begin
  IPhotoComparer := TIPhotoComparerMostLikes.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  result := PhotosSorted;
end;

function TTopStats.GetTopXNumberofMostViewed(): TList<IPhoto>;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerMostViews;
  i : integer;
begin
  IPhotoComparer := TIPhotoComparerMostViews.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  result := PhotosSorted;
end;

function TTopStats.GetTopXNumberOfViews(num: integer; series : TPieSeries): string;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerViews;
  i : integer;
  description : string;
  color : TColor;
begin
  IPhotoComparer := TIPhotoComparerViews.Create;
  PhotosSorted := TList<IPhoto>.Create(IPhotoComparer);

  for I := 0 to FRepository.photos.Count-1 do
  begin
    PhotosSorted.Add(FRepository.photos[i]);
  end;

  PhotosSorted.Sort;

  description := 'Top ' + num.ToString + ' images with most Views: ' + sLineBreak;
  for i := 0 to num-1 do
  begin
    if i < photosSorted.count then
    begin
      description := description + PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Views: ' + PhotosSorted[i].getTotalViewsDay.ToString + sLineBreak;
      color := RGB(Random(255), Random(255), Random(255));
      series.Add(PhotosSorted[i].getTotalViewsDay.ToDouble, PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Views: ' + PhotosSorted[i].getTotalViewsDay.ToString, color);
    end;
  end;
  PhotosSorted.Free;
  //IPhotoComparer := nil;
  result := description;
end;

end.
