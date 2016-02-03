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
  Classes, SysUtils, System.SyncObjs, Vcl.ComCtrls, vcl.taskbar, VCLtee.series, System.Win.taskbarcore, graphics, generics.collections;

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
    procedure DoInitialize();
    procedure DoUpdate();
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
  end;

implementation

uses
  Windows, flickr.http.lib, xmlintf;

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
            FilteredGroupList.Add(base);
          end;
        end;
        Synchronize(DoUpdate);
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

end;

end.
