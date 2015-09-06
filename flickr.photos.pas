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

unit flickr.photos;

interface

uses
  Contnrs, Generics.Collections, flickr.stats, XMLDoc, xmldom, XMLIntf, flickr.pools,
  flickr.albums, flickr.pools.list, winapi.msxml, flickr.albums.list;

type
  IPhoto = interface
    function AddStats(stat: IStat): boolean;
    procedure AddCollections(albums: TAlbumList; groups : TPoolList);
    function getLastUpdate(): TDatetime;
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    procedure SetStats(value: TList<IStat>);
    procedure SetLastUpdate(value: TDatetime);
    procedure SetAlbums(value: TAlbumList);
    procedure SetGroups(value: TPoolList);
    function getId(): string;
    function getTitle(): string;
    function GetStats(): TList<IStat>;
    function GetAlbums(): TAlbumList;
    function GetGroups(): TPoolList;
    function getTaken: string;
    function GetTags: string;
    function GetTodayTrend : integer;
    function InGroup(groupId : string) : boolean;
    function InAlbum(albumId : string) : boolean;
    function getBanned: boolean;
    function GetOmitGroups : string;
    procedure SetOmitGroups(const Value : string);
    procedure SetBanned(const Value: boolean);
    procedure SetTaken(const Value: string);
    procedure SetTags(const Value: string);
    procedure SetTodayTrend(const Value : integer);
    procedure SetFolder(const Value: string);
    function GetFolder() : string;
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property LastUpdate: TDatetime read getLastUpdate write SetLastUpdate;
    property stats: TList<IStat>read GetStats write SetStats;
    property Taken : string read getTaken write SetTaken;
    property Albums : TAlbumList read GetAlbums write SetAlbums;
    property Groups : TPoolList read GetGroups write SetGroups;
    property Tags : string read GetTags write SetTags;
    property banned : boolean read getBanned write SetBanned;
    property TodayTrend : integer read GetTodayTrend write SetTodayTrend;
    property OmitGroups : string read GetOmitGroups write SetOmitGroups;
    property Folder : string read GetFolder write SetFolder;
    procedure Load(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
    function getTotalLikes(incday : integer = 0): Integer;
    function getTotalComments(incday : integer = 0): Integer;
    function getTotalViews(incday : integer = 0): Integer;
    function getTrend() : integer;
    function getHighestViews() : Integer;
    function getHighestLikes() : Integer;
  end;

  TPhoto = class(TInterfacedObject, IPhoto)
  private
    FStats: TList<IStat>;
    FAlbums: TAlbumList;
    FGroups: TPoolList;
    FId: string;
    FTitle: string;
    FLastUpdate: TDatetime;
    FTaken : string;
    FTags: string;
    FBanned : boolean;
    FTodayTrend : integer;
    FOmitGroups : string;
    FFolder: string;
    procedure SetStats(value: TList<IStat>);
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    function getId(): string;
    function getLastUpdate(): TDatetime;
    procedure SetLastUpdate(value: TDatetime);
    function getTitle(): string;
    function GetStats(): TList<IStat>;
    function GetTodayTrend : integer;
    function ExistStat(stat: IStat; var existing: IStat): boolean;
    function getTaken: string;
    function GetTags: string;
    procedure SetTaken(const Value: string);
    function GetAlbums: TAlbumList;
    function GetGroups: TPoolList;
    procedure SetAlbums(Value: TAlbumList);
    procedure SetGroups(Value: TPoolList);
    procedure SetTags(const Value: string);
    function getBanned: boolean;
    procedure SetBanned(const Value: boolean);
    procedure SetTodayTrend(const Value : integer);
    function GetOmitGroups : string;
    procedure SetOmitGroups(const Value : string);
    procedure SetFolder(const Value: string);
    function GetFolder() : string;
  public
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property LastUpdate: TDatetime read getLastUpdate write SetLastUpdate;
    property stats: TList<IStat>read GetStats write SetStats;
    property Taken : string read getTaken write SetTaken;
    property banned : boolean read getBanned write SetBanned;
    property Albums : TAlbumList read GetAlbums write SetAlbums;
    property Groups : TPoolList read GetGroups write SetGroups;
    property Tags : string read GetTags write SetTags;
    property TodayTrend : integer read GetTodayTrend write SetTodayTrend;
    property OmitGroups : string read GetOmitGroups write SetOmitGroups;
    property Folder : string read GetFolder write SetFolder;
    function AddStats(stat: IStat): boolean;
    procedure AddCollections(albums: TAlbumList; groups : TPoolList);
    function InGroup(groupId : string) : boolean;
    function InAlbum(albumId : string) : boolean;
    constructor Create(); overload;
    constructor Create(Id: string; Title: string; taken : string; tags : string); overload;
    destructor Destroy(); override;
    procedure Load(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
    function getTotalLikes(incday : integer = 0): Integer;
    function getTotalComments(incday : integer = 0): Integer;
    function getTotalViews(incday : integer = 0): Integer;
    function getTrend() : integer;
    function getHighestViews() : Integer;
    function getHighestLikes() : Integer;
  end;

implementation

{ TPhoto }

uses
  SysUtils, System.Variants, DateUtils;

procedure TPhoto.AddCollections(albums: TAlbumList; groups: TPoolList);
begin
  if Assigned(FAlbums) then
    FAlbums.Free;
  FAlbums := albums;
//  if Assigned(FGroups) then
//    FGroups.Free;
  //FGroups := groups;
end;

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

constructor TPhoto.Create(Id: string; Title: string; taken : string; tags : string);
begin
  FStats := TList<IStat>.Create;
  FAlbums := TAlbumList.Create;
  FGroups := TPoolList.Create;
  SetId(Id);
  SetTitle(Title);
  SetTaken(taken);
  SetTags(tags);
  SetBanned(false);
end;

constructor TPhoto.Create;
begin
  FStats := TList<IStat>.Create;
  FAlbums := TAlbumList.Create;
  FGroups := TPoolList.Create;
end;

destructor TPhoto.Destroy;
begin
  FreeAndNil(FStats);
  FreeAndNil(FAlbums);
  FreeAndNil(FGroups);
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

function TPhoto.GetAlbums: TAlbumList;
begin
  result := FAlbums;
end;

function TPhoto.getBanned: boolean;
begin
  result := FBanned;
end;

function TPhoto.GetFolder: string;
begin
  result := FFolder;
end;

function TPhoto.GetGroups: TPoolList;
begin
  result := FGroups;
end;

function TPhoto.getHighestLikes: Integer;
var
  i: Integer;
  max : integer;
begin
  max := 0;
  for i := 0 to FStats.count - 1 do
  begin
    if FStats[i].likes >= max then
      max := FStats[i].likes;
  end;
  result := max;
end;

function TPhoto.getHighestViews: Integer;
var
  i: Integer;
  max : integer;
begin
  max := 0;
  for i := 0 to FStats.count - 1 do
  begin
    if FStats[i].views >= max then
      max := FStats[i].views;
  end;
  result := max;
end;

function TPhoto.getId: string;
begin
  result := FId;
end;

function TPhoto.getLastUpdate: TDatetime;
begin
  result := FLastUpdate;
end;

function TPhoto.GetOmitGroups: string;
begin
  result := FOmitGroups;
end;

function TPhoto.GetStats: TList<IStat>;
begin
  result := FStats;
end;

function TPhoto.GetTags: string;
begin
  result := FTags;
end;

function TPhoto.getTaken: string;
begin
  result := FTaken;
end;

function TPhoto.getTitle: string;
begin
  result := FTitle;
end;

function TPhoto.GetTodayTrend: integer;
begin
  result := FTodayTrend;
end;

function TPhoto.getTotalComments(incday : integer = 0): Integer;
begin
  if (FStats.count - 1 + incday) >= 0 then
    result := FStats[FStats.count - 1 + incday].numComments
  else
    result := 0;
end;

function TPhoto.getTotalLikes(incday : integer = 0): Integer;
begin
  if (FStats.count - 1 + incday) >= 0 then
    result := FStats[FStats.count - 1 + incday].likes
  else
    result := 0;
end;

function TPhoto.getTotalViews(incday : integer = 0): Integer;
begin
  if (FStats.count - 1 + incday) >= 0 then
    result := FStats[FStats.count - 1 + incday].views
  else
    result := 0;
end;

function TPhoto.getTrend: integer;
var
  numViews, numLikes, numComments : integer;
begin
  numViews := getTotalViews(0) - getTotalViews(-1);
  numLikes := getTotalLikes(0) - getTotalLikes(-1);
  numComments := getTotalComments(0) - getTotalComments(-1);
  SetTodayTrend(numViews + numLikes + numComments);
  result := FTodayTrend;
end;

function TPhoto.InAlbum(albumId: string): boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FAlbums.count) do
  begin
    found := FAlbums[i].id = albumId;
    inc(i);
  end;
  Result := found;
end;

function TPhoto.InGroup(groupId: string): boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FGroups.count) do
  begin
    found := FGroups[i].id = groupid;
    inc(i);
  end;
  Result := found;
end;

procedure TPhoto.Load(iNode: IXMLNode);
var
  iNode2: IXMLNode;
  stat: IStat;
  Document: IXMLDocument;
  iXMLRootNode: IXMLNode;
  id : string;
  title : string;
  added : tdatetime;
begin
  FId := iNode.Attributes['id'];
  FTitle := iNode.Attributes['title'];
  FLastUpdate := StrToDate(iNode.Attributes['LastUpdate']);
  FTaken := iNode.Attributes['Taken'];

  try
    if iNode.Attributes['Tags'] <> null then
      FTags := iNode.Attributes['Tags']
    else
      FTags := '';
  except
    FTags := '';
  end;

  try
    if iNode.Attributes['OmitGroups'] <> null then
      FOmitGroups := iNode.Attributes['OmitGroups']
    else
      FOmitGroups := '';
  except
    FOmitGroups := '';
  end;

  try
    if iNode.Attributes['Banned'] <> null then
      FBanned := iNode.Attributes['Banned']
    else
      FBanned := false;
  except
    FBanned := false;
  end;

  iNode2 := iNode.ChildNodes.First;
  while iNode2 <> nil do
  begin
    stat := TStat.Create();
    stat.Load(iNode2);
    FStats.Add(stat);
    iNode2 := iNode2.NextSibling;
  end;

  FAlbums.Clear;
  if fileExists(FFolder + 'Albums\'+ FId + '.xml') then
  begin
    Document := TXMLDocument.Create(nil);
    try
      Document.LoadFromFile(FFolder + 'Albums\'+ FId + '.xml');
      iXMLRootNode := Document.ChildNodes.first;
      iNode2 := iXMLRootNode.ChildNodes.first;
      while iNode2 <> nil do
      begin
        FAlbums.Add(TAlbum.create(iNode2.attributes['id'], iNode2.attributes['title']));
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;

  FGroups.Clear;
  if fileExists(FFolder + 'Groups\'+ FId + '.xml') then
  begin
    Document := TXMLDocument.Create(nil);
    try
      Document.LoadFromFile(FFolder + 'Groups\'+ FId + '.xml');
      iXMLRootNode := Document.ChildNodes.first;
      iNode2 := iXMLRootNode.ChildNodes.first;
      while iNode2 <> nil do
      begin
        id := iNode2.attributes['id'];
        title := iNode2.attributes['title'];
        if iNode2.attributes['added'] <> null then
          added := StrToDate(iNode2.attributes['added'])
        else
          added := Yesterday;
        FGroups.Add(TPool.create(id, title, added));
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TPhoto.Save(iNode: IXMLNode);
var
  i: Integer;
  iNode2: IXMLNode;
  XMLDoc: TXMLDocument;
begin
  iNode2 := iNode.AddChild('Photo');
  iNode2.Attributes['id'] := FId;
  iNode2.Attributes['title'] := FTitle;
  iNode2.Attributes['LastUpdate'] := DateToStr(FLastUpdate);
  iNode2.Attributes['Taken'] := FTaken;
  iNode2.Attributes['Banned'] := FBanned;
  iNode2.Attributes['Tags'] := FTags;
  iNode2.Attributes['OmitGroups'] := FOmitGroups;
  for i := 0 to FStats.count - 1 do
  begin
    FStats[i].Save(iNode2);
  end;

  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Albums');
  for i := 0 to FAlbums.count - 1 do
  begin
    iNode2 := iNode.AddChild('Set');
    iNode2.Attributes['id'] := FAlbums[i].Id;
    iNode2.Attributes['title'] := FAlbums[i].Title;
  end;
  XMLDoc.SaveToFile(FFolder + 'Albums\'+ FId + '.xml');

  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Groups');
  for i := 0 to FGroups.count - 1 do
  begin
    iNode2 := iNode.AddChild('Pool');
    iNode2.Attributes['id'] := FGroups[i].Id;
    iNode2.Attributes['title'] := FGroups[i].Title;
    iNode2.Attributes['added'] := FGroups[i].Added;
  end;
  XMLDoc.SaveToFile(FFolder + 'Groups\'+ FId + '.xml');
end;

procedure TPhoto.SetAlbums(Value: TAlbumList);
begin
  FAlbums := Value;
end;

procedure TPhoto.SetBanned(const Value: boolean);
begin
  FBanned := Value;
end;

procedure TPhoto.SetFolder(const Value: string);
begin
  FFolder := Value;
end;

procedure TPhoto.SetGroups(Value: TPoolList);
begin
  FGroups := Value;
end;

procedure TPhoto.SetId(value: string);
begin
  FId := value;
end;

procedure TPhoto.SetLastUpdate(value: TDatetime);
begin
  FLastUpdate := value;
end;

procedure TPhoto.SetOmitGroups(const Value: string);
begin
  FOmitGroups := value;
end;

procedure TPhoto.SetStats(value: TList<IStat>);
begin
  FStats := value;
end;

procedure TPhoto.SetTags(const Value: string);
begin
  FTags := Value;
end;

procedure TPhoto.SetTaken(const Value: string);
begin
  FTaken := value;
end;

procedure TPhoto.SetTitle(value: string);
begin
  FTitle := value;
end;

procedure TPhoto.SetTodayTrend(const Value: integer);
begin
  FTodayTrend := value;
end;

end.
