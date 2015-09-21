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

unit flickr.lib.item.list;

interface

uses
  Contnrs, Generics.Collections, Generics.defaults, flickr.lib.item;

type
  TITemComparerDate = class(TComparer<IItem>)
  public
    function Compare(const Left, Right: IItem): integer; override;
  end;

  TItemList = class(TList<IItem>)
  public
    function AddItem(const item : IItem) : integer;
    function Exists(const item : TdateTime; var existing: IItem) : boolean;
  end;

implementation

uses
  DateUtils, Sysutils;

{ TITemComparerDate }

function TITemComparerDate.Compare(const Left, Right: IItem): integer;
var
  LeftTerm, RightTerm: double;
begin
  LeftTerm := Left.Date;
  RightTerm := Right.Date;
  Result := Round(LeftTerm - RightTerm);
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
