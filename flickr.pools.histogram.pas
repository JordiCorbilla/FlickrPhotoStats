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
  flickr.pools.list, Contnrs, Generics.Collections, Generics.defaults;

type
  IItem = interface
    function GetDate() : TDateTime;
    function GetCount() : integer;
    procedure Setcount(const Value: integer);
    procedure Setdate(const Value: TDateTime);
    property date : TDateTime read Getdate write Setdate;
    property count : integer read Getcount write Setcount;
  end;

  TITem = class(TInterfacedObject, IItem)
  private
    Fdate: TDateTime;
    Fcount: integer;
    function GetDate() : TDateTime;
    function GetCount() : integer;
    procedure Setcount(const Value: integer);
    procedure Setdate(const Value: TDateTime);
  public
    property date : TDateTime read Getdate write Setdate;
    property count : integer read Getcount write Setcount;
    Constructor Create();
  end;

  TPoolHistogram = class(TObject)
  private
    FGroupPool : TPoolList;
  public
    constructor Create(poolList : TPoolList);
    function Histogram() : TList<IItem>;
  end;

  TITemComparerDate = class(TComparer<IItem>)
  public
    function Compare(const Left, Right: IItem): Integer; override;
  end;

  TItemList = class(TList<IItem>)
  public
    function AddItem(const item : IItem) : integer;
    function Exists(const item : TdateTime; var existing: IItem) : boolean;
  end;

implementation

uses
  DateUtils, Sysutils;

{ TITem }

constructor TITem.Create;
begin
  SetDate(Date);
  SetCount(0);
end;

function TITem.GetCount: integer;
begin
  result := FCount;
end;

function TITem.GetDate: TDateTime;
begin
  result := FDate;
end;

procedure TITem.Setcount(const Value: integer);
begin
  Fcount := Value;
end;

procedure TITem.Setdate(const Value: TDateTime);
begin
  Fdate := Value;
end;

{ TPoolHistogram }

constructor TPoolHistogram.Create(poolList: TPoolList);
begin
  FGroupPool := poolList;
end;

function TPoolHistogram.Histogram: TList<IItem>;
var
  i : integer;
  item : IItem;
  FHistogram : TItemList;
  accumulated : integer;
begin
  FHistogram := TItemList.create;

  accumulated := 1;
  for i := 0 to FGroupPool.Count-1 do
  begin
    if FHistogram.Exists(FGroupPool[i].Added, item) then
    begin
      item.count := item.count + 1;
      accumulated := accumulated + 1;
    end
    else
    begin
      item := TItem.Create;
      item.date := FGroupPool[i].Added;
      item.count := item.count + 1 + accumulated;
      FHistogram.Add(item);
    end;
  end;

  result := FHistogram;
end;

{ TITemComparerDate }

function TITemComparerDate.Compare(const Left, Right: IItem): Integer;
var
  LeftTerm, RightTerm: TDateTime;
begin
  LeftTerm := Left.Date;
  RightTerm := Right.Date;
  Result := DaysBetween(LeftTerm, RightTerm);
end;

{ TItemList }

function TItemList.AddItem(const item: IItem): integer;
var
  poolRet : IItem;
begin
  result := 0;
  if not Exists(item.date, poolRet) then
    result := Add(item);
end;

function TItemList.Exists(const item: TDateTime; var existing: IItem): boolean;
var
  i: Integer;
  found: boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < Self.count) do
  begin
    found := Self[i].date = item;
    inc(i);
  end;
  if found then
    existing := Self[i - 1];
  result := found;
end;

end.
