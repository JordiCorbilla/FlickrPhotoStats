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
  Flickr.repository, Contnrs, Generics.Collections, Generics.defaults, flickr.photos;

type
  TIPhotoComparerLikes = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPhotoComparerViews = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPhotoComparerComments = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TTopStats = class(TObject)
  private
    FRepository : IFlickrRepository;
  public
    constructor Create(repository : IFlickrRepository);
    destructor Destroy(); override;
    function GetTopXNumberOfLikes(num : integer) : string;
    function GetTopXNumberOfViews(num : integer) : string;
    function GetTopXNumberOfComments(num : integer) : string;
  end;

implementation

uses
  Sysutils, Dialogs;

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
    description := description + PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Comments: ' + PhotosSorted[i].getTotalComments.ToString + sLineBreak;
  end;
  PhotosSorted.Free;
  //IPhotoComparer := nil;
  result := description;
end;

function TTopStats.GetTopXNumberOfLikes(num: integer): string;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerLikes;
  i : integer;
  description : string;
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
    description := description + PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Likes: ' + PhotosSorted[i].getTotalLikes.ToString + sLineBreak;
  end;
  PhotosSorted.Free;
  //IPhotoComparer := nil;
  result := description;
end;

function TTopStats.GetTopXNumberOfViews(num: integer): string;
var
  PhotosSorted: TList<IPhoto>;
  IPhotoComparer : TIPhotoComparerViews;
  i : integer;
  description : string;
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
    description := description + PhotosSorted[i].Id + ' (' + PhotosSorted[i].Title + ')' + ' Number of Views: ' + PhotosSorted[i].getTotalViews.ToString + sLineBreak;
  end;
  PhotosSorted.Free;
  //IPhotoComparer := nil;
  result := description;
end;

{ TIPhotoComparer }

function TIPhotoComparerLikes.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.getTotalLikes;
  RightTerm := Right.getTotalLikes;
  Result := RightTerm - LeftTerm;
end;

{ TIPhotoComparerViews }

function TIPhotoComparerViews.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.getTotalViews;
  RightTerm := Right.getTotalViews;
  Result := RightTerm - LeftTerm;
end;

{ TIPhotoComparerComments }

function TIPhotoComparerComments.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.getTotalComments;
  RightTerm := Right.getTotalComments;
  Result := RightTerm - LeftTerm;
end;

end.
