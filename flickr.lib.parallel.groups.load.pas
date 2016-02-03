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

unit flickr.lib.parallel.groups.load;

interface

uses
  Classes, SysUtils, System.SyncObjs, Vcl.ComCtrls, vcl.taskbar, VCLtee.series, System.Win.taskbarcore, graphics, generics.collections,
  flickr.base, flickr.lib.options.agent, flickr.xml.helper, System.AnsiStrings, flickr.filtered.list;

type
  TParallelGroupLoad = class(TThread)
  private
    FRestURL : String;
    FProgressbar : TProgressBar;
    FTaskBar : TTaskbar;
    FSeries : TPieSeries;
    FTotal : integer;
    FlvGroups : TListView;
    FphotosetId: string;
    FnumPhotos: Integer;
    FcountViews: Integer;
    Ftitle: string;
    FtotalViews: Integer;
    FPages: string;
    FInitialize: boolean;
    FOptionsAgent : IOptionsAgent;
    FFilteredGroupList : IFilteredList;
    procedure DoInitialize();
    procedure DoUpdate();
    procedure AddAdditionalGroupDetails(base: IBase);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy(); override;
    property restUrl : string read FRestUrl write FRestUrl;
    property progressBar : TProgressBar read FProgressBar write FProgressBar;
    property taskBar : TTaskBar read FTaskBar write FTaskBar;
    property series : TPieSeries read FSeries write FSeries;
    property lvGroups: TListView read FlvGroups write FlvGroups;
    property pages : string read FPages write FPages;
    property Initialize : boolean read FInitialize write FInitialize;
    property TotalViews : integer read FTotalViews write FTotalViews;
    property OptionsAgent : IOptionsAgent read FOptionsAgent write FOptionsAgent;
    property FilteredGroupList: IFilteredList read FFilteredGroupList write FFilteredGroupList;
  end;

implementation

uses
  Windows, flickr.http.lib, xmlintf, flickr.rest;

{ TParallelGroupLoad }

constructor TParallelGroupLoad.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TParallelGroupLoad.Destroy;
begin

  inherited;
end;

procedure TParallelGroupLoad.DoInitialize;
begin
  FlvGroups.Clear;
  FProgressbar.Max := FTotal;
  FTaskbar.ProgressState := TTaskBarProgressState.Normal;
  FTaskbar.ProgressMaxValue := FTotal;
  Fprogressbar.position := 0;
end;

procedure TParallelGroupLoad.DoUpdate;
begin
  Fprogressbar.position := Fprogressbar.position + 1;
  FTaskbar.ProgressValue := Fprogressbar.position;
end;

procedure TParallelGroupLoad.Execute;
begin
  THttpRest.Post(FRestURL, procedure (iXMLRootNode : IXMLNode)
    var
      iXMLRootNode4, iXMLRootNode5: IXMLNode;
      total: string;
      numTotal, totalitems: Integer;
      Item: TListItem;
      pages, title, id, ismember: string;
      numPages: Integer;
      urlGroups: string;
      i: Integer;
      photos : string;
      members : string;
      base : IBase;
    begin
      FPages := iXMLRootNode.attributes['page'];
      if FInitialize then
      begin
        total := iXMLRootNode.attributes['pages'];
        totalitems := iXMLRootNode.attributes['total'];
      end;

      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <group>

      if FInitialize then
      begin
        FTotal := totalitems.ToInteger();
        Synchronize(DoInitialize);
      end;

      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'group' then
        begin
          id := iXMLRootNode4.attributes['id'];
          ismember := iXMLRootNode4.attributes['member'];
          title := iXMLRootNode4.attributes['name'];
          photos := iXMLRootNode4.attributes['photos'];
          members := iXMLRootNode4.attributes['member_count'];
          if ismember = '1' then
          begin
            base := TBase.New(id, title, StrToInt(photos), StrToInt(members));
            AddAdditionalGroupDetails(base);
            FFilteredGroupList.Add(base);
          end;
        end;
        Synchronize(DoUpdate);
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);
end;

procedure TParallelGroupLoad.AddAdditionalGroupDetails(base : IBase);
var
  response: string;
  iXMLRootNode, iXMLRootNode2, iXMLRootNode3, iXMLRootNode4: IXMLNode;
  urlGroups: string;
  timedout: Boolean;
  xmlDocument: IXMLDocument;
  description : string;
  IsModerated : boolean;
  ThrottleCount : integer;
  ThrottleMode : string;
  ThrottleRemaining : integer;
  photos_ok : boolean;
  videos_ok : boolean;
  images_ok : boolean;
  screens_ok : boolean;
  art_ok : boolean;
  safe_ok : boolean;
  moderate_ok : boolean;
  restricted_ok : boolean;
  has_geo : boolean;
begin
  THttpRest.Post(TFlickrRest.New(FoptionsAgent).getGroupInfo(base.Id), procedure (iXMLRootNode : IXMLNode)
    begin
      description := '';
      ThrottleCount := 0;
      ThrottleMode := '';
      ThrottleRemaining := 0;
      photos_ok := false;
      videos_ok := false;
      images_ok := false;
      screens_ok := false;
      art_ok := false;
      safe_ok := false;
      moderate_ok := false;
      restricted_ok := false;
      has_geo := false;

      try
        IsModerated := TXMLHelper.new(iXMLRootNode.attributes['ispoolmoderated']).getBool;
        iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <group>

        while iXMLRootNode4 <> nil do
        begin
          if iXMLRootNode4.NodeName = 'description' then
          begin
            try
              description := TXMLHelper.new(iXMLRootNode4.NodeValue).getString;
              description := AnsiLeftStr(description, 200);
            except
//              log('');
//              log(base.Id);
//              log(description);
              description := 'ERROR INVALID DESCRIPTION';
            end;
          end;

          if iXMLRootNode4.NodeName = 'throttle' then
          begin
            ThrottleCount := TXMLHelper.new(iXMLRootNode4.attributes['count']).getInt;
            ThrottleMode := TXMLHelper.new(iXMLRootNode4.attributes['mode']).getString;
            ThrottleRemaining := TXMLHelper.new(iXMLRootNode4.attributes['remaining']).getInt;
          end;

          if iXMLRootNode4.NodeName = 'restrictions' then
          begin
            photos_ok := TXMLHelper.new(iXMLRootNode4.attributes['photos_ok']).getBool;
            videos_ok := TXMLHelper.new(iXMLRootNode4.attributes['videos_ok']).getBool;
            images_ok := TXMLHelper.new(iXMLRootNode4.attributes['images_ok']).getBool;
            screens_ok := TXMLHelper.new(iXMLRootNode4.attributes['screens_ok']).getBool;
            art_ok := TXMLHelper.new(iXMLRootNode4.attributes['art_ok']).getBool;
            safe_ok := TXMLHelper.new(iXMLRootNode4.attributes['safe_ok']).getBool;
            moderate_ok := TXMLHelper.new(iXMLRootNode4.attributes['moderate_ok']).getBool;
            restricted_ok := TXMLHelper.new(iXMLRootNode4.attributes['restricted_ok']).getBool;
            has_geo := TXMLHelper.new(iXMLRootNode4.attributes['has_geo']).getBool;
          end;
          iXMLRootNode4 := iXMLRootNode4.NextSibling;
        end;
      except
//          log('');
//          log(base.Id);
//          log(response);
        description := 'ERROR INVALID CHARACTERS';
      end;

      base.Description := description;
      base.ThrottleCount := ThrottleCount;
      base.ThrottleMode := ThrottleMode;
      base.ThrottleRemaining := ThrottleRemaining;
      base.photos_ok := photos_ok;
      base.videos_ok := videos_ok;
      base.images_ok := images_ok;
      base.screens_ok := screens_ok;
      base.art_ok := art_ok;
      base.safe_ok := safe_ok;
      base.moderate_ok := moderate_ok;
      base.restricted_ok := restricted_ok;
      base.has_geo := has_geo;
    end);
end;


end.
