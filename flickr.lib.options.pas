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

unit flickr.lib.options;

interface

uses
  System.Classes;

type
  IOptions = Interface
    procedure SetConsiderPendingQueueItems(const Value: boolean);
    procedure SeteMailAddress(const Value: string);
    procedure SetMaxItems(const Value: integer);
    procedure SetMaxItemsListGlobals(const Value: string);
    procedure SetMaxNumberOfLinesLog(const Value: string);
    procedure SetShowMarksInGraphs(const Value: boolean);
    procedure SetUpdateCountsRealTime(const Value: boolean);
    function GetMaxItemsListGlobals() : string;
    function GetShowMarksInGraphs() : boolean;
    function GetConsiderPendingQueueItems() : boolean;
    function GetUpdateCountsRealTime() : boolean;
    function GetMaxNumberOfLinesLog() : string;
    function GeteMailAddress() : string;
    procedure SeturlName(const Value: string);
    function GeturlName() : string;
    function GetMaxItems(): integer;
    function GetsortingEnabled() : boolean;
    procedure SetsortingEnabled(const Value: boolean);
    function GetSortedBy(): integer;
    procedure SetSortedBy(const Value: integer);
    function GetAlbumViews(): TStrings;
    procedure SetAlbumViews(const Value: TStrings);
    function GetAlbumLikes(): TStrings;
    procedure SetAlbumLikes(const Value: TStrings);
    function GetSortedByGropus(): integer;
    procedure SetSortedByGropus(const Value: integer);
    procedure SetDisableTrendDisplay(const Value: boolean);
    procedure SetDisplaySuccessfulResponses(const Value: boolean);
    procedure SetKeepRejectedListAlive(const Value: boolean);
    procedure SetUpdateCollections(const Value: boolean);
    function GetKeepRejectedListAlive() : boolean;
    function GetDisplaySuccessfulResponses() : boolean;
    function GetUpdateCollections() : boolean;
    function GetDisableTrendDisplay() : boolean;
    function GetAlbumViewsID(): TStrings;
    procedure SetAlbumViewsID(const Value: TStrings);
    function GetAlbumLikesID(): TStrings;
    procedure SetAlbumLikesID(const Value: TStrings);
    property MaxItemsListGlobals : string read GetMaxItemsListGlobals write SetMaxItemsListGlobals;
    property ShowMarksInGraphs : boolean read GetShowMarksInGraphs write SetShowMarksInGraphs;
    property ConsiderPendingQueueItems : boolean read GetConsiderPendingQueueItems write SetConsiderPendingQueueItems;
    property UpdateCountsRealTime : boolean read GetUpdateCountsRealTime write SetUpdateCountsRealTime;
    property MaxNumberOfLinesLog : string read GetMaxNumberOfLinesLog write SetMaxNumberOfLinesLog;
    property eMailAddress : string read GeteMailAddress write SeteMailAddress;
    property MaxItems : integer read GetMaxItems write SetMaxItems;
    property urlName : string read GeturlName write SeturlName;
    property sortingEnabled : boolean read GetsortingEnabled write SetsortingEnabled;
    property SortedBy : integer read GetSortedBy write SetSortedBy;
    property AlbumViews : TStrings read GetAlbumViews write SetAlbumViews;
    property AlbumLikes : TStrings read GetAlbumLikes write SetAlbumLikes;
    property SortedByGropus : integer read GetSortedByGropus write SetSortedByGropus;
    property KeepRejectedListAlive: boolean read GetKeepRejectedListAlive write SetKeepRejectedListAlive;
    property DisplaySuccessfulResponses: boolean read GetDisplaySuccessfulResponses write SetDisplaySuccessfulResponses;
    property UpdateCollections: boolean read GetUpdateCollections write SetUpdateCollections;
    property DisableTrendDisplay: boolean read GetDisableTrendDisplay write SetDisableTrendDisplay;
    property AlbumViewsID : TStrings read GetAlbumViewsID write SetAlbumViewsID;
    property AlbumLikesID : TStrings read GetAlbumLikesID write SetAlbumLikesID;
    function Load() : IOptions;
    procedure Save();
  End;

  TOptions = class(TInterfacedObject, IOptions)
  private
    FConsiderPendingQueueItems: boolean;
    FUpdateCountsRealTime: boolean;
    FMaxNumberOfLinesLog: string;
    FMaxItemsListGlobals: string;
    FMaxItems: integer;
    FShowMarksInGraphs: boolean;
    FeMailAddress: string;
    FurlName: string;
    FSortingEnabled : boolean;
    FSortedBy : integer;
    FAlbumViews : TStrings;
    FAlbumLikes : TStrings;
    FAlbumViewsID : TStrings;
    FAlbumLikesID : TStrings;
    FSortedByGroups : integer;
    FKeepRejectedListAlive: boolean;
    FDisplaySuccessfulResponses: boolean;
    FUpdateCollections: boolean;
    FDisableTrendDisplay: boolean;
    procedure SetConsiderPendingQueueItems(const Value: boolean);
    procedure SeteMailAddress(const Value: string);
    procedure SetMaxItems(const Value: integer);
    procedure SetMaxItemsListGlobals(const Value: string);
    procedure SetMaxNumberOfLinesLog(const Value: string);
    procedure SetShowMarksInGraphs(const Value: boolean);
    procedure SetUpdateCountsRealTime(const Value: boolean);
    function GetMaxItemsListGlobals() : string;
    function GetShowMarksInGraphs() : boolean;
    function GetConsiderPendingQueueItems() : boolean;
    function GetUpdateCountsRealTime() : boolean;
    function GetMaxNumberOfLinesLog() : string;
    function GeteMailAddress() : string;
    function GetMaxItems(): integer;
    procedure SeturlName(const Value: string);
    function GeturlName() : string;
    function GetsortingEnabled() : boolean;
    procedure SetsortingEnabled(const Value: boolean);
    function GetSortedBy(): integer;
    procedure SetSortedBy(const Value: integer);
    function GetSortedByGropus(): integer;
    procedure SetSortedByGropus(const Value: integer);
    function GetAlbumViews(): TStrings;
    procedure SetAlbumViews(const Value: TStrings);
    function GetAlbumLikes(): TStrings;
    procedure SetAlbumLikes(const Value: TStrings);
    function GetAlbumViewsID(): TStrings;
    procedure SetAlbumViewsID(const Value: TStrings);
    function GetAlbumLikesID(): TStrings;
    procedure SetAlbumLikesID(const Value: TStrings);
    procedure SetDisableTrendDisplay(const Value: boolean);
    procedure SetDisplaySuccessfulResponses(const Value: boolean);
    procedure SetKeepRejectedListAlive(const Value: boolean);
    procedure SetUpdateCollections(const Value: boolean);
    function GetKeepRejectedListAlive() : boolean;
    function GetDisplaySuccessfulResponses() : boolean;
    function GetUpdateCollections() : boolean;
    function GetDisableTrendDisplay() : boolean;
  public
    property MaxItemsListGlobals : string read GetMaxItemsListGlobals write SetMaxItemsListGlobals;
    property ShowMarksInGraphs : boolean read GetShowMarksInGraphs write SetShowMarksInGraphs;
    property ConsiderPendingQueueItems : boolean read GetConsiderPendingQueueItems write SetConsiderPendingQueueItems;
    property UpdateCountsRealTime : boolean read GetUpdateCountsRealTime write SetUpdateCountsRealTime;
    property MaxNumberOfLinesLog : string read GetMaxNumberOfLinesLog write SetMaxNumberOfLinesLog;
    property eMailAddress : string read GeteMailAddress write SeteMailAddress;
    property MaxItems : integer read GetMaxItems write SetMaxItems;
    property urlName : string read GeturlName write SeturlName;
    property sortingEnabled : boolean read GetsortingEnabled write SetsortingEnabled;
    property SortedBy : integer read GetSortedBy write SetSortedBy;
    property SortedByGropus : integer read GetSortedByGropus write SetSortedByGropus;
    property AlbumViews : TStrings read GetAlbumViews write SetAlbumViews;
    property AlbumLikes : TStrings read GetAlbumLikes write SetAlbumLikes;
    property AlbumViewsID : TStrings read GetAlbumViewsID write SetAlbumViewsID;
    property AlbumLikesID : TStrings read GetAlbumLikesID write SetAlbumLikesID;
    property KeepRejectedListAlive: boolean read GetKeepRejectedListAlive write SetKeepRejectedListAlive;
    property DisplaySuccessfulResponses: boolean read GetDisplaySuccessfulResponses write SetDisplaySuccessfulResponses;
    property UpdateCollections: boolean read GetUpdateCollections write SetUpdateCollections;
    property DisableTrendDisplay: boolean read GetDisableTrendDisplay write SetDisableTrendDisplay;
    class function New(): IOptions;
    function Load() : IOptions;
    procedure Save();
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  System.inifiles, SysUtils;

{ TOptions }

constructor TOptions.Create;
begin
  FAlbumViews := TStrings.Create;
  FAlbumLikes := TStrings.Create;
  FAlbumViewsID := TStrings.Create;
  FAlbumLikesID := TStrings.Create;
end;

destructor TOptions.Destroy;
begin
//  if assigned(FAlbumViews) then
//    FAlbumViews.Free;
//  if assigned(FAlbumLikes) then
//    FAlbumLikes.Free;
//  if assigned(FAlbumViewsID) then
//    FAlbumViewsID.Free;
//  if assigned(FAlbumLikesID) then
//    FAlbumLikesID.Free;
  inherited;
end;

function TOptions.GetAlbumLikes: TStrings;
begin
  result := FAlbumLikes;
end;

function TOptions.GetAlbumLikesID: TStrings;
begin
  result := FAlbumLikesID;
end;

function TOptions.GetAlbumViews: TStrings;
begin
  result := FAlbumViews;
end;

function TOptions.GetAlbumViewsID: TStrings;
begin
  result := FAlbumViewsID;
end;

function TOptions.GetConsiderPendingQueueItems: boolean;
begin
  result := FConsiderPendingQueueItems;
end;

function TOptions.GetDisableTrendDisplay: boolean;
begin
  result := FDisableTrendDisplay;
end;

function TOptions.GetDisplaySuccessfulResponses: boolean;
begin
  result := FDisplaySuccessfulResponses;
end;

function TOptions.GeteMailAddress: string;
begin
  result := FeMailAddress;
end;

function TOptions.GetKeepRejectedListAlive: boolean;
begin
  result := FKeepRejectedListAlive;
end;

function TOptions.GetMaxItems: integer;
begin
  result := FMaxItems;
end;

function TOptions.GetMaxItemsListGlobals: string;
begin
  result := FMaxItemsListGlobals;
end;

function TOptions.GetMaxNumberOfLinesLog: string;
begin
  result := FMaxNumberOfLinesLog;
end;

function TOptions.GetShowMarksInGraphs: boolean;
begin
  result := FShowMarksInGraphs;
end;

function TOptions.GetSortedBy: integer;
begin
  result := FSortedBy;
end;

function TOptions.GetSortedByGropus: integer;
begin
  result := FSortedByGroups;
end;

function TOptions.GetsortingEnabled: boolean;
begin
  result := FSortingEnabled;
end;

function TOptions.GetUpdateCollections: boolean;
begin
  result := FUpdateCollections;
end;

function TOptions.GetUpdateCountsRealTime: boolean;
begin
  result := FUpdateCountsRealTime;
end;

function TOptions.GeturlName: string;
begin
  result := FUrlName;
end;

function TOptions.Load : IOptions;
var
  inifile : Tinifile;
begin
  inifile := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'FlickrAnalytics.ini');
  try
    FMaxItemsListGlobals := inifile.ReadString('System', 'MaxItemsListGlobals', '80');
    FShowMarksInGraphs := inifile.ReadBool('System', 'ShowMarksInGraphs', false);
    FConsiderPendingQueueItems := inifile.ReadBool('System', 'ConsiderPendingQueueItems', true);
    FUpdateCountsRealTime := inifile.ReadBool('System', 'UpdateCountsRealTime', false);
    FMaxNumberOfLinesLog := inifile.ReadString('System', 'MaxNumberOfLinesLog', '10000');
    FeMailAddress := inifile.ReadString('System', 'eMailAddress', '');

    FMaxItems := inifile.ReadInteger('AlbumViews', 'MaxItems', 0);
  finally
    inifile.Free;
  end;
  result := self;
end;

class function TOptions.New: IOptions;
begin
  result := Create;
end;

procedure TOptions.Save;
var
  iniFile : TInifile;
  i : integer;
begin
  inifile := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'FlickrAnalytics.ini');
  try
    inifile.WriteString('System', 'MaxItemsListGlobals', FMaxItemsListGlobals);
    inifile.WriteString('System', 'UrlName', FUrlName);
    inifile.WriteBool('System', 'ShowMarksInGraphs', FShowMarksInGraphs);
    inifile.WriteBool('System', 'ConsiderPendingQueueItems', FConsiderPendingQueueItems);
    inifile.WriteBool('System', 'UpdateCountsRealTime', FUpdateCountsRealTime);
    inifile.WriteString('System', 'MaxNumberOfLinesLog', FMaxNumberOfLinesLog);
    inifile.WriteString('System', 'eMailAddress', FeMailAddress);
    inifile.WriteBool('System', 'SortingEnabled', FSortingEnabled);

    inifile.WriteBool('System', 'KeepRejectedListAlive', FKeepRejectedListAlive);
    inifile.WriteBool('System', 'DisplaySuccessfulResponses', FDisplaySuccessfulResponses);
    inifile.WriteBool('System', 'UpdateCollections', FUpdateCollections);
    inifile.WriteBool('System', 'DisableTrendDisplay', FDisableTrendDisplay);

    inifile.WriteInteger('System', 'SortedBy', FSortedBy);
    inifile.WriteInteger('AlbumViews', 'MaxItems', FAlbumViews.count);

    for i := 0 to FAlbumViews.Count-1 do
    begin
      inifile.WriteString('AlbumViews', 'Value' + i.ToString(), FAlbumViews[i]);
      inifile.WriteString('AlbumViews', 'ValueID' + i.ToString(), FAlbumViewsID[i]);
    end;

    inifile.WriteInteger('AlbumLikes', 'MaxItems', FAlbumLikes.count);

    for i := 0 to FAlbumLikes.Count-1 do
    begin
      inifile.WriteString('AlbumLikes', 'Value' + i.ToString(), FAlbumLikes[i]);
      inifile.WriteString('AlbumLikes', 'ValueID' + i.ToString(), FAlbumLikesID[i]);
    end;
  finally
    inifile.Free;
  end;
end;

procedure TOptions.SetAlbumLikes(const Value: TStrings);
begin
  if Assigned(FAlbumLikes) then
    FAlbumLikes.Free;
  FAlbumLikes := value;
end;

procedure TOptions.SetAlbumLikesID(const Value: TStrings);
begin
  if Assigned(FAlbumLikesID) then
    FAlbumLikesID.Free;
  FAlbumLikesID := value;
end;

procedure TOptions.SetAlbumViews(const Value: TStrings);
begin
  if Assigned(FAlbumViews) then
    FAlbumViews.Free;
  FAlbumViews := Value;
end;

procedure TOptions.SetAlbumViewsID(const Value: TStrings);
begin
  if Assigned(FAlbumViewsID) then
    FAlbumViewsID.Free;
  FAlbumViewsID := Value;
end;

procedure TOptions.SetConsiderPendingQueueItems(const Value: boolean);
begin
  FConsiderPendingQueueItems := Value;
end;

procedure TOptions.SetDisableTrendDisplay(const Value: boolean);
begin
  FDisableTrendDisplay := Value;
end;

procedure TOptions.SetDisplaySuccessfulResponses(const Value: boolean);
begin
  FDisplaySuccessfulResponses := Value;
end;

procedure TOptions.SeteMailAddress(const Value: string);
begin
  FeMailAddress := Value;
end;

procedure TOptions.SetKeepRejectedListAlive(const Value: boolean);
begin
  FKeepRejectedListAlive := Value;
end;

procedure TOptions.SetMaxItems(const Value: integer);
begin
  FMaxItems := Value;
end;

procedure TOptions.SetMaxItemsListGlobals(const Value: string);
begin
  FMaxItemsListGlobals := Value;
end;

procedure TOptions.SetMaxNumberOfLinesLog(const Value: string);
begin
  FMaxNumberOfLinesLog := Value;
end;

procedure TOptions.SetShowMarksInGraphs(const Value: boolean);
begin
  FShowMarksInGraphs := Value;
end;

procedure TOptions.SetSortedBy(const Value: integer);
begin
  FSortedBy := Value;
end;

procedure TOptions.SetSortedByGropus(const Value: integer);
begin
  FSortedByGroups := Value;
end;

procedure TOptions.SetsortingEnabled(const Value: boolean);
begin
  FSortingEnabled := value;
end;

procedure TOptions.SetUpdateCollections(const Value: boolean);
begin
  FUpdateCollections := Value;
end;

procedure TOptions.SetUpdateCountsRealTime(const Value: boolean);
begin
  FUpdateCountsRealTime := Value;
end;

procedure TOptions.SeturlName(const Value: string);
begin
  FurlName := Value;
end;

end.
