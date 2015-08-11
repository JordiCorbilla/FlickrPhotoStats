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

unit flickr.list.comparer;

interface

uses
  Contnrs, Generics.Collections, Generics.defaults, flickr.photos, vcltee.series, flickr.base;

type
  TCompareType = (tCompareId, tCompareLikes, tCompareViews, tCompareComments, tCompareTaken, tCompareAlbums, tCompareGroups, tCompareMembers, tComparePoolSize);

  TIPhotoComparerID = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

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

  TIPhotoComparerTaken = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPhotoComparerAlbums = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPhotoComparerGroups = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPhotoComparerMostLikes = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPhotoComparerMostViews = class(TComparer<IPhoto>)
  public
    function Compare(const Left, Right: IPhoto): Integer; override;
  end;

  TIPoolComparerMembers = class(TComparer<IBase>)
  public
    function Compare(const Left, Right: IBase): Integer; override;
  end;

  TIPoolComparerPoolSize = class(TComparer<IBase>)
  public
    function Compare(const Left, Right: IBase): Integer; override;
  end;

implementation

uses
  Sysutils, vcl.Dialogs, System.UITypes, windows, DateUtils;

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

{ TIPhotoComparerMostLikes }

function TIPhotoComparerMostLikes.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.getHighestLikes;
  RightTerm := Right.getHighestLikes;
  Result := RightTerm - LeftTerm;
end;

{ TIPhotoComparerMostViews }

function TIPhotoComparerMostViews.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.getHighestViews;
  RightTerm := Right.getHighestViews;
  Result := RightTerm - LeftTerm;
end;

{ TIPhotoComparerID }

function TIPhotoComparerID.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: Extended;
begin
  LeftTerm := Left.Id.ToExtended;
  RightTerm := Right.Id.ToExtended;
  Result := Round(RightTerm - LeftTerm);
end;


{ TIPhotoComparerTaken }

function TIPhotoComparerTaken.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: TDateTime;
begin
  LeftTerm := StrToDateTime(Left.Taken);
  RightTerm := StrToDateTime(Right.Taken);
  Result := DaysBetween(LeftTerm, RightTerm);
end;

{ TIPhotoComparerAlbums }

function TIPhotoComparerAlbums.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: integer;
begin
  LeftTerm := Left.Albums.count;
  RightTerm := Right.Albums.count;
  Result := RightTerm - LeftTerm;
end;

{ TIPhotoComparerGroups }

function TIPhotoComparerGroups.Compare(const Left, Right: IPhoto): Integer;
var
  LeftTerm, RightTerm: integer;
begin
  LeftTerm := Left.Groups.Count;
  RightTerm := Right.Groups.Count;
  Result := RightTerm - LeftTerm;
end;

{ TIPoolComparerMembers }

function TIPoolComparerMembers.Compare(const Left, Right: IBase): Integer;
var
  LeftTerm, RightTerm: integer;
begin
  LeftTerm := Left.Members;
  RightTerm := Right.Members;
  Result := RightTerm - LeftTerm;
end;

{ TIPoolComparerPoolSize }

function TIPoolComparerPoolSize.Compare(const Left, Right: IBase): Integer;
var
  LeftTerm, RightTerm: integer;
begin
  LeftTerm := Left.Photos;
  RightTerm := Right.Photos;
  Result := RightTerm - LeftTerm;
end;

end.
