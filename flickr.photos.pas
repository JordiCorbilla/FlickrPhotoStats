// Copyright (c) 2014, Jordi Corbilla
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

unit flickr.photos;

interface

uses
  Contnrs, Generics.Collections, flickr.stats, XMLDoc, xmldom, XMLIntf;

type
  IPhoto = interface
    function AddStats(stat: IStat): boolean;
    function getLastUpdate(): TDatetime;
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    procedure SetStats(value: TList<IStat>);
    procedure SetLastUpdate(value: TDatetime);
    function getId(): string;
    function getTitle(): string;
    function GetStats(): TList<IStat>;
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property LastUpdate: TDatetime read getLastUpdate write SetLastUpdate;
    property stats: TList<IStat>read GetStats write SetStats;
    procedure Load(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
    function getTotalLikes(): Integer;
    function getTotalComments(): Integer;
    function getTotalViews(): Integer;
  end;

  TPhoto = class(TInterfacedObject, IPhoto)
  private
    FStats: TList<IStat>;
    FId: string;
    FTitle: string;
    FLastUpdate: TDatetime;
    procedure SetStats(value: TList<IStat>);
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    function getId(): string;
    function getLastUpdate(): TDatetime;
    procedure SetLastUpdate(value: TDatetime);
    function getTitle(): string;
    function GetStats(): TList<IStat>;
    function ExistStat(stat: IStat; var existing: IStat): boolean;
  public
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property LastUpdate: TDatetime read getLastUpdate write SetLastUpdate;
    property stats: TList<IStat>read GetStats write SetStats;
    function AddStats(stat: IStat): boolean;
    constructor Create(); overload;
    constructor Create(Id: string; Title: string); overload;
    destructor Destroy(); override;
    procedure Load(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
    function getTotalLikes(): Integer;
    function getTotalComments(): Integer;
    function getTotalViews(): Integer;
  end;

implementation

{ TPhoto }

uses
  SysUtils;

function TPhoto.AddStats(stat: IStat): boolean;
var
  existing: IStat;
begin
  existing := nil;
  if not ExistStat(stat, existing) then
    FStats.Add(stat)
  else
    existing.Copy(stat);
  result := (existing = nil);
end;

constructor TPhoto.Create(Id: string; Title: string);
begin
  FStats := TList<IStat>.Create;
  SetId(Id);
  SetTitle(Title);
end;

constructor TPhoto.Create;
begin
  FStats := TList<IStat>.Create;
end;

destructor TPhoto.Destroy;
begin
  FreeAndNil(FStats);
  inherited;
end;

function TPhoto.ExistStat(stat: IStat; var existing: IStat): boolean;
var
  i: Integer;
  found: boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FStats.count) do
  begin
    found := FStats[i].date = stat.date;
    inc(i);
  end;
  if found then
    existing := FStats[i - 1];
  result := found;
end;

function TPhoto.getId: string;
begin
  result := FId;
end;

function TPhoto.getLastUpdate: TDatetime;
begin
  result := FLastUpdate;
end;

function TPhoto.GetStats: TList<IStat>;
begin
  result := FStats;
end;

function TPhoto.getTitle: string;
begin
  result := FTitle;
end;

function TPhoto.getTotalComments: Integer;
var
  i, index: Integer;
  date: TDatetime;
begin
  date := 0;
  index := 0;
  for i := 0 to FStats.count - 1 do
  begin
    if date < FStats[i].date then
    begin
      date := FStats[i].date;
      index := i;
    end;
  end;
  result := FStats[index].numComments;
end;

function TPhoto.getTotalLikes: Integer;
var
  i, index: Integer;
  date: TDatetime;
begin
  date := 0;
  index := 0;
  for i := 0 to FStats.count - 1 do
  begin
    if date < FStats[i].date then
    begin
      date := FStats[i].date;
      index := i;
    end;
  end;
  result := FStats[index].likes;
end;

function TPhoto.getTotalViews: Integer;
var
  i, index: Integer;
  date: TDatetime;
begin
  date := 0;
  index := 0;
  for i := 0 to FStats.count - 1 do
  begin
    if date < FStats[i].date then
    begin
      date := FStats[i].date;
      index := i;
    end;
  end;
  result := FStats[index].views;
end;

procedure TPhoto.Load(iNode: IXMLNode);
var
  iNode2: IXMLNode;
  stat: IStat;
begin
  FId := iNode.Attributes['id'];
  FTitle := iNode.Attributes['title'];
  FLastUpdate := StrToDate(iNode.Attributes['LastUpdate']);

  iNode2 := iNode.ChildNodes.First;
  while iNode2 <> nil do
  begin
    stat := TStat.Create();
    stat.Load(iNode2);
    FStats.Add(stat);
    iNode2 := iNode2.NextSibling;
  end;
end;

procedure TPhoto.Save(iNode: IXMLNode);
var
  i: Integer;
  iNode2: IXMLNode;
begin
  iNode2 := iNode.AddChild('Photo');
  iNode2.Attributes['id'] := FId;
  iNode2.Attributes['title'] := FTitle;
  iNode2.Attributes['LastUpdate'] := DateToStr(FLastUpdate);
  for i := 0 to FStats.count - 1 do
  begin
    FStats[i].Save(iNode2);
  end;
end;

procedure TPhoto.SetId(value: string);
begin
  FId := value;
end;

procedure TPhoto.SetLastUpdate(value: TDatetime);
begin
  FLastUpdate := value;
end;

procedure TPhoto.SetStats(value: TList<IStat>);
begin
  FStats := value;
end;

procedure TPhoto.SetTitle(value: string);
begin
  FTitle := value;
end;

end.