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


unit flickr.pools.histogram;

interface

uses
  flickr.pools.list, Contnrs, Generics.Collections, Generics.defaults, flickr.lib.item, flickr.pools;

type
  TPoolHistogram = class(TObject)
  private
    FGroupPool : TPoolList;
  public
    constructor Create(poolList : TPoolList);
    function Histogram() : TList<IItem>;
  end;

implementation

uses
  DateUtils, Sysutils, flickr.lib.item.list;

{ TPoolHistogram }

constructor TPoolHistogram.Create(poolList: TPoolList);
begin
  FGroupPool := poolList;
end;

function TPoolHistogram.Histogram: TList<IItem>;
var
  item : IItem;
  FHistogram : TItemList;
  pool: TPair<string, IPool>;
  IPoolComparer : TComparer<IItem>;
begin
  IPoolComparer := TITemComparerDate.Create;

  FHistogram := TItemList.create(IPoolComparer);

  for pool in FGroupPool do
  begin
    if FHistogram.Exists(pool.Value.Added, item) then
    begin
      item.count := item.count + 1;
    end
    else
    begin
      item := TItem.Create;
      item.date := pool.Value.Added;
      item.count := item.count + 1;
      FHistogram.Add(item);
    end;
  end;

  FHistogram.Sort;

  result := FHistogram;
end;

end.
