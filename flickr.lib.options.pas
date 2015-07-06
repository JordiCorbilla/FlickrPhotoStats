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
    function GetMaxItems(): integer;
    property MaxItemsListGlobals : string read GetMaxItemsListGlobals write SetMaxItemsListGlobals;
    property  ShowMarksInGraphs : boolean read GetShowMarksInGraphs write SetShowMarksInGraphs;
    property ConsiderPendingQueueItems : boolean read GetConsiderPendingQueueItems write SetConsiderPendingQueueItems;
    property UpdateCountsRealTime : boolean read GetUpdateCountsRealTime write SetUpdateCountsRealTime;
    property MaxNumberOfLinesLog : string read GetMaxNumberOfLinesLog write SetMaxNumberOfLinesLog;
    property eMailAddress : string read GeteMailAddress write SeteMailAddress;
    property MaxItems : integer read GetMaxItems write SetMaxItems;
    function Load() : IOptions;
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
  public
    property MaxItemsListGlobals : string read GetMaxItemsListGlobals write SetMaxItemsListGlobals;
    property  ShowMarksInGraphs : boolean read GetShowMarksInGraphs write SetShowMarksInGraphs;
    property ConsiderPendingQueueItems : boolean read GetConsiderPendingQueueItems write SetConsiderPendingQueueItems;
    property UpdateCountsRealTime : boolean read GetUpdateCountsRealTime write SetUpdateCountsRealTime;
    property MaxNumberOfLinesLog : string read GetMaxNumberOfLinesLog write SetMaxNumberOfLinesLog;
    property eMailAddress : string read GeteMailAddress write SeteMailAddress;
    property MaxItems : integer read GetMaxItems write SetMaxItems;
    class function New(): IOptions;
    function Load() : IOptions;
  end;

implementation

uses
  System.inifiles, SysUtils;

{ TOptions }

function TOptions.GetConsiderPendingQueueItems: boolean;
begin
  result := FConsiderPendingQueueItems;
end;

function TOptions.GeteMailAddress: string;
begin
  result := FeMailAddress;
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

function TOptions.GetUpdateCountsRealTime: boolean;
begin
  result := FUpdateCountsRealTime;
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

procedure TOptions.SetConsiderPendingQueueItems(const Value: boolean);
begin
  FConsiderPendingQueueItems := Value;
end;

procedure TOptions.SeteMailAddress(const Value: string);
begin
  FeMailAddress := Value;
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

procedure TOptions.SetUpdateCountsRealTime(const Value: boolean);
begin
  FUpdateCountsRealTime := Value;
end;

end.
