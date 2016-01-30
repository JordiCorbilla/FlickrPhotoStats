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
  flickr.albums, flickr.pools.list, winapi.msxml, flickr.albums.list, flickr.user.faves,
  flickr.user.tracking;

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
    procedure SetTotalAlbums(const Value: integer);
    procedure SetTotalComments(const Value: integer);
    procedure SetTotalGroups(const Value: integer);
    procedure SetTotalLikes(const Value: integer);
    procedure SetTotalViews(const Value: integer);
    function GetTotalAlbums() : integer;
    function GetUserTracking() : IUserTracking;
    procedure SetUserTracking(const Value : IUserTracking);
    function GetTotalComments() : integer;
    function GetTotalGroups() : integer;
    function GetTotalLikes() : integer;
    function GetTotalViews() : integer;
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
    property TotalViews : integer read GetTotalViews write SetTotalViews;
    property TotalLikes : integer read GetTotalLikes write SetTotalLikes;
    property TotalComments : integer read GetTotalComments write SetTotalComments;
    property TotalAlbums : integer read GetTotalAlbums write SetTotalAlbums;
    property TotalGroups : integer read GetTotalGroups write SetTotalGroups;
    property UserTracking : IUserTracking read GetUserTracking write SetUserTracking;
    procedure Load(iNode: IXMLNode);
    procedure LoadNew(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
    procedure SaveNew(iNode: IXMLNode);
    function getTotalLikesDay(incday : integer = 0): Integer;
    function getTotalCommentsDay(incday : integer = 0): Integer;
    function getTotalViewsDay(incday : integer = 0): Integer;
    function getTrend() : integer;
    function getHighestViews() : Integer;
    function getHighestLikes() : Integer;
    procedure LoadStats();
    procedure LoadGroups();
    procedure LoadAlbums();
    procedure SaveStats();
    procedure SaveGroups();
    procedure SaveAlbums();
    procedure SaveNewFinal(iNode: IXMLNode);
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
    FTotalGroups: integer;
    FTotalComments: integer;
    FTotalViews: integer;
    FTotalLikes: integer;
    FTotalAlbums: integer;
    FUserTracking: IUserTracking;
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
    procedure SetTotalAlbums(const Value: integer);
    procedure SetTotalComments(const Value: integer);
    procedure SetTotalGroups(const Value: integer);
    procedure SetTotalLikes(const Value: integer);
    procedure SetTotalViews(const Value: integer);
    function GetTotalAlbums() : integer;
    function GetTotalComments() : integer;
    function GetTotalGroups() : integer;
    function GetUserTracking() : IUserTracking;
    procedure SetUserTracking(const Value : IUserTracking);
    function GetTotalLikes() : integer;
    function GetTotalViews() : integer;
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
    property TotalViews : integer read GetTotalViews write SetTotalViews;
    property TotalLikes : integer read GetTotalLikes write SetTotalLikes;
    property TotalComments : integer read GetTotalComments write SetTotalComments;
    property TotalAlbums : integer read GetTotalAlbums write SetTotalAlbums;
    property TotalGroups : integer read GetTotalGroups write SetTotalGroups;
    property UserTracking : IUserTracking read GetUserTracking write SetUserTracking;
    function AddStats(stat: IStat): boolean;
    procedure AddCollections(albums: TAlbumList; groups : TPoolList);
    function InGroup(groupId : string) : boolean;
    function InAlbum(albumId : string) : boolean;
    constructor Create(); overload;
    constructor Create(Id: string; Title: string; taken : string; tags : string); overload;
    destructor Destroy(); override;
    procedure Load(iNode: IXMLNode);
    procedure LoadStats();
    procedure LoadGroups();
    procedure LoadAlbums();
    procedure SaveGroups();
    procedure SaveAlbums();
    procedure LoadNew(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
    procedure SaveNew(iNode: IXMLNode);
    procedure SaveNewFinal(iNode: IXMLNode);
    procedure SaveStats();
    function getTotalLikesDay(incday : integer = 0): Integer;
    function getTotalCommentsDay(incday : integer = 0): Integer;
    function getTotalViewsDay(incday : integer = 0): Integer;
    function getTrend() : integer;
    function getHighestViews() : Integer;
    function getHighestLikes() : Integer;
  end;

implementation

{ TPhoto }

uses
  SysUtils, System.Variants, DateUtils, flickr.xml.helper;

procedure TPhoto.AddCollections(albums: TAlbumList; groups: TPoolList);
begin
  if Assigned(FAlbums) then
    FAlbums.Free;
  FAlbums := albums;
end;

function TPhoto.AddStats(stat: IStat): boolean;
var
  existing: IStat;
begin
  LoadStats;
  LoadAlbums;
  LoadGroups;
  existing := nil;
  if not ExistStat(stat, existing) then
    FStats.Add(stat)
  else
    existing.Copy(stat);
  SaveStats();
  result := (existing = nil);
end;

constructor TPhoto.Create(Id: string; Title: string; taken : string; tags : string);
begin
  FStats := TList<IStat>.Create;
  FAlbums := TAlbumList.Create;
  FGroups := TPoolList.Create;
  FUserTracking := TUserTracking.Create;
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
  FUserTracking := TUserTracking.Create;
end;

destructor TPhoto.Destroy;
begin
  FreeAndNil(FStats);
  FreeAndNil(FAlbums);
  FreeAndNil(FGroups);
  FUserTracking := nil;
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
begin
  result := FTotalLikes;
end;

function TPhoto.getHighestViews: Integer;
begin
  result := FTotalViews;
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

function TPhoto.GetTotalAlbums: integer;
begin
  result := FTotalAlbums;
end;

function TPhoto.GetTotalComments: integer;
begin
  result := FTotalComments;
end;

function TPhoto.getTotalCommentsDay(incday : integer = 0): Integer;
begin
  if (FStats.count - 1 + incday) >= 0 then
    result := FStats[FStats.count - 1 + incday].Comments
  else
    result := 0;
end;

function TPhoto.GetTotalGroups: integer;
begin
  result := FTotalGroups;
end;

function TPhoto.GetTotalLikes: integer;
begin
  result := FTotalLikes;
end;

function TPhoto.getTotalLikesDay(incday : integer = 0): Integer;
begin
  if (FStats.count - 1 + incday) >= 0 then
    result := FStats[FStats.count - 1 + incday].likes
  else
    result := 0;
end;

function TPhoto.GetTotalViews: integer;
begin
  result := FTotalViews;
end;

function TPhoto.getTotalViewsDay(incday : integer = 0): Integer;
begin
  if (FStats.count - 1 + incday) >= 0 then
    result := FStats[FStats.count - 1 + incday].views
  else
    result := 0;
end;

function TPhoto.getTrend: integer;
//var
//  numViews, numLikes, Comments : integer;
begin
  //TODO: jc
  //numViews := getTotalViewsDay(0) - getTotalViewsDay(-1);
 // numLikes := getTotalLikesDay(0) - getTotalLikesDay(-1);
  //Comments := getTotalCommentsDay(0) - getTotalCommentsDay(-1);
  SetTodayTrend(0); //numViews + numLikes + Comments
  result := FTodayTrend;
end;

function TPhoto.GetUserTracking: IUserTracking;
begin
  result := FUserTracking;
end;

function TPhoto.InAlbum(albumId: string): boolean;
var
  album : IAlbum;
begin
  Result := FAlbums.Exists(albumId, album);
end;

function TPhoto.InGroup(groupId: string): boolean;
var
  pool : Ipool;
begin
  result := FGroups.TryGetValue(groupId, pool);
end;

procedure TPhoto.Load(iNode: IXMLNode);
var
  iNode2: IXMLNode;
  stat: IStat;
begin
  FId := iNode.Attributes['id'];
  FTitle := iNode.Attributes['title'];
  FLastUpdate := StrToDate(iNode.Attributes['LastUpdate']);
  FTaken := iNode.Attributes['Taken'];
  FTags := TXMLHelper.new(iNode.attributes['Tags']).getString;
  FOmitGroups := TXMLHelper.new(iNode.Attributes['OmitGroups']).getString;
  FBanned := TXMLHelper.new(iNode.Attributes['Banned']).getBool;

  iNode2 := iNode.ChildNodes.First;
  while iNode2 <> nil do
  begin
    stat := TStat.Create();
    stat.Load(iNode2);
    FStats.Add(stat);
    iNode2 := iNode2.NextSibling;
  end;

  LoadAlbums;
  LoadGroups;
end;

procedure TPhoto.LoadAlbums;
var
  iNode2: IXMLNode;
  Document: IXMLDocument;
  iXMLRootNode: IXMLNode;
begin
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
        FAlbums.AddItem(TAlbum.create(iNode2.attributes['id'], iNode2.attributes['title']));
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TPhoto.LoadGroups;
var
  iNode2: IXMLNode;
  Document: IXMLDocument;
  iXMLRootNode: IXMLNode;
  added : tdatetime;
  groupid : string;
  groupTitle : string;
begin
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
        groupid := iNode2.attributes['id'];
        groupTitle := iNode2.attributes['title'];
        if iNode2.attributes['added'] <> null then
          added := StrToDate(iNode2.attributes['added'])
        else
          added := Yesterday;
        FGroups.AddItem(TPool.create(groupid, groupTitle, added));
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TPhoto.LoadNew(iNode: IXMLNode);
begin
  FId := iNode.Attributes['id'];
  FTitle := iNode.Attributes['title'];
  FLastUpdate := StrToDate(iNode.Attributes['LastUpdate']);
  FTaken := iNode.Attributes['Taken'];
  FTags := TXMLHelper.new(iNode.attributes['Tags']).getString;
  FOmitGroups := TXMLHelper.new(iNode.Attributes['OmitGroups']).getString;
  FBanned := TXMLHelper.new(iNode.Attributes['Banned']).getBool;

  FTotalViews := TXMLHelper.new(iNode.Attributes['TotalViews']).getInt;
  FTotalLikes := TXMLHelper.new(iNode.Attributes['TotalLikes']).getInt;
  FTotalComments := TXMLHelper.new(iNode.Attributes['TotalComments']).getInt;
  FTotalAlbums := TXMLHelper.new(iNode.Attributes['TotalAlbums']).getInt;
  FTotalGroups := TXMLHelper.new(iNode.Attributes['TotalGroups']).getInt;
end;

procedure TPhoto.LoadStats;
var
  iNode2: IXMLNode;
  stat: IStat;
  Document: IXMLDocument;
  iXMLRootNode: IXMLNode;
begin
  FStats.Clear;
  if fileExists(FFolder + 'History\'+ FId + '.xml') then
  begin
    Document := TXMLDocument.Create(nil);
    try
      Document.LoadFromFile(FFolder + 'History\'+ FId + '.xml');
      iXMLRootNode := Document.ChildNodes.first;
      iNode2 := iXMLRootNode.ChildNodes.first;
      while iNode2 <> nil do
      begin
        stat := TStat.Create();
        stat.Load(iNode2);
        FStats.Add(stat);
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TPhoto.SaveNew(iNode: IXMLNode);
var
  i: Integer;
  iNode2: IXMLNode;
  XMLDoc: TXMLDocument;
  item : TPair<string, IPool>;
  album : TPair<string, IAlbum>;
begin
  iNode2 := iNode.AddChild('Photo');
  iNode2.Attributes['id'] := FId;
  iNode2.Attributes['title'] := FTitle;
  iNode2.Attributes['LastUpdate'] := DateToStr(FLastUpdate);
  iNode2.Attributes['Taken'] := FTaken;
  iNode2.Attributes['Banned'] := FBanned;
  iNode2.Attributes['Tags'] := FTags;
  iNode2.Attributes['OmitGroups'] := FOmitGroups;

  //New nodes for version 4.8.0.2
  iNode2.Attributes['TotalViews'] := FStats[FStats.Count-1].views;
  iNode2.Attributes['TotalLikes'] := FStats[FStats.Count-1].likes;
  iNode2.Attributes['TotalComments'] := FStats[FStats.Count-1].Comments;
  iNode2.Attributes['TotalAlbums'] := FAlbums.Count;
  iNode2.Attributes['TotalGroups'] := FGroups.Count;

  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('History');
  for i := 0 to FStats.count - 1 do
  begin
    FStats[i].Save(iNode);
  end;
  if DirectoryExists(FFolder + 'History') then
    XMLDoc.SaveToFile(FFolder + 'History\'+ FId + '.xml')
  else
    raise Exception.Create('Directory can''t be found: ' + FFolder + 'History');

  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Albums');
  for album in FAlbums do
  begin
    iNode2 := iNode.AddChild('Set');
    iNode2.Attributes['id'] := album.value.Id;
    iNode2.Attributes['title'] := album.value.Title;
  end;
  if DirectoryExists(FFolder + 'Albums') then
    XMLDoc.SaveToFile(FFolder + 'Albums\'+ FId + '.xml')
  else
    raise Exception.Create('Directory can''t be found: ' + FFolder + 'Albums');

  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Groups');
  for Item in FGroups do
  begin
    iNode2 := iNode.AddChild('Pool');
    iNode2.Attributes['id'] := Item.Value.Id;
    iNode2.Attributes['title'] := Item.Value.Title;
    iNode2.Attributes['added'] := Item.Value.Added;
  end;
  if DirectoryExists(FFolder + 'Groups') then
    XMLDoc.SaveToFile(FFolder + 'Groups\'+ FId + '.xml')
  else
    raise Exception.Create('Directory can''t be found: ' + FFolder + 'Groups');
end;

procedure TPhoto.SaveNewFinal(iNode: IXMLNode);
var
  iNode2: IXMLNode;
begin
  iNode2 := iNode.AddChild('Photo');
  iNode2.Attributes['id'] := FId;
  iNode2.Attributes['title'] := FTitle;
  iNode2.Attributes['LastUpdate'] := DateToStr(FLastUpdate);
  iNode2.Attributes['Taken'] := FTaken;
  iNode2.Attributes['Banned'] := FBanned;
  iNode2.Attributes['Tags'] := FTags;
  iNode2.Attributes['OmitGroups'] := FOmitGroups;

  //New nodes for version 4.8.0.2
  iNode2.Attributes['TotalViews'] := FTotalViews;
  iNode2.Attributes['TotalLikes'] := FTotalLikes;
  iNode2.Attributes['TotalComments'] := FTotalComments;
  iNode2.Attributes['TotalAlbums'] := FTotalAlbums;
  iNode2.Attributes['TotalGroups'] := FTotalGroups;
end;

procedure TPhoto.SaveStats;
var
  i: Integer;
  iNode: IXMLNode;
  XMLDoc: TXMLDocument;
begin
  FTotalViews := FStats[FStats.Count-1].views;
  FTotalLikes := FStats[FStats.Count-1].likes;
  FTotalComments := FStats[FStats.Count-1].Comments;
  FTotalAlbums := FAlbums.Count;
  FTotalGroups := FGroups.Count;

  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('History');
  for i := 0 to FStats.count - 1 do
  begin
    FStats[i].Save(iNode);
  end;
  if DirectoryExists(FFolder + 'History') then
    XMLDoc.SaveToFile(FFolder + 'History\'+ FId + '.xml')
  else
    raise Exception.Create('Directory can''t be found: ' + FFolder + 'History');
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
  iNode2.Attributes['Taken'] := FTaken;
  iNode2.Attributes['Banned'] := FBanned;
  iNode2.Attributes['Tags'] := FTags;
  iNode2.Attributes['OmitGroups'] := FOmitGroups;

  for i := 0 to FStats.count - 1 do
  begin
    FStats[i].Save(iNode2);
  end;

  SaveAlbums;
  SaveGroups;
end;

procedure TPhoto.SaveAlbums;
var
  iNode, iNode2: IXMLNode;
  XMLDoc: TXMLDocument;
  album : TPair<string, IAlbum>;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Albums');
  for album in FAlbums do
  begin
    iNode2 := iNode.AddChild('Set');
    iNode2.Attributes['id'] := album.value.Id;
    iNode2.Attributes['title'] := album.value.Title;
  end;
  if DirectoryExists(FFolder + 'Albums') then
    XMLDoc.SaveToFile(FFolder + 'Albums\'+ FId + '.xml')
  else
    raise Exception.Create('Directory can''t be found: ' + FFolder + 'Albums');
end;

procedure TPhoto.SaveGroups;
var
  iNode, iNode2: IXMLNode;
  XMLDoc: TXMLDocument;
  item : TPair<string, IPool>;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Groups');
  for Item in FGroups do
  begin
    iNode2 := iNode.AddChild('Pool');
    iNode2.Attributes['id'] := Item.Value.Id;
    iNode2.Attributes['title'] := Item.Value.Title;
    iNode2.Attributes['added'] := Item.Value.Added;
  end;
  if DirectoryExists(FFolder + 'Groups') then
    XMLDoc.SaveToFile(FFolder + 'Groups\'+ FId + '.xml')
  else
    raise Exception.Create('Directory can''t be found: ' + FFolder + 'Groups');
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

procedure TPhoto.SetTotalAlbums(const Value: integer);
begin
  FTotalAlbums := Value;
end;

procedure TPhoto.SetTotalComments(const Value: integer);
begin
  FTotalComments := Value;
end;

procedure TPhoto.SetTotalGroups(const Value: integer);
begin
  FTotalGroups := Value;
end;

procedure TPhoto.SetTotalLikes(const Value: integer);
begin
  FTotalLikes := Value;
end;

procedure TPhoto.SetTotalViews(const Value: integer);
begin
  FTotalViews := Value;
end;

procedure TPhoto.SetUserTracking(const Value: IUserTracking);
begin
  FUserTracking := Value;
end;

end.
