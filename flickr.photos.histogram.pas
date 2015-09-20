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

unit flickr.photos.histogram;

interface

uses
  flickr.photos, Contnrs, Generics.Collections, Generics.defaults, flickr.lib.item;

type
  TPhotoHistogram = class(TObject)
  private
    FPhotos : TList<IPhoto>;
  public
    constructor Create(photos : TList<IPhoto>);
    function Histogram() : TList<IItem>;
  end;

implementation

uses
  flickr.lib.item.list, DateUtils, Sysutils, System.StrUtils;

{ TPhotoHistogram }

constructor TPhotoHistogram.Create(photos: TList<IPhoto>);
begin
  FPhotos := photos;
end;

function TPhotoHistogram.Histogram: TList<IItem>;
var
  i : integer;
  item : IItem;
  FHistogram : TItemList;
  accumulated : integer;
  dateTaken : TDateTime;
  year, month, day : string;
  parseDate : string;
begin
  FHistogram := TItemList.create;

  accumulated := 1;
  for i := 0 to FPhotos.Count-1 do
  begin
    parseDate := FPhotos[i].Taken;
    year := AnsiLeftStr(parseDate, AnsiPos('-', parseDate));
    parseDate := AnsiRightStr(parseDate, length(parseDate) - length(year));
    month := AnsiLeftStr(parseDate, AnsiPos('-', parseDate));
    parseDate := AnsiRightStr(parseDate, length(parseDate) - length(month));
    day := AnsiLeftStr(parseDate, AnsiPos(' ', parseDate));
    parseDate := AnsiRightStr(parseDate, length(parseDate) - length(day));
    year := year.Replace('-', '');
    month := month.Replace('-', '');
    day := day.Replace(' ', '');
    dateTaken := StrToDate(day + '/' + month + '/' + year);
    if FHistogram.Exists(dateTaken, item) then
    begin
      item.count := item.count + 1;
      accumulated := accumulated + 1;
    end
    else
    begin
      item := TItem.Create;
      item.date := dateTaken;
      item.count := item.count + 1 + accumulated;
      FHistogram.Add(item);
    end;
  end;

  result := FHistogram;
end;

end.
