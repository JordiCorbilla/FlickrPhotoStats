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

unit flickr.lib.parallel.albums;

interface

uses
  Classes, SysUtils, System.SyncObjs, Vcl.ComCtrls, vcl.taskbar, VCLtee.series, System.Win.taskbarcore, graphics, generics.collections;

type
  TParallelAlbum = class(TThread)
  private
    FRestURL : String;
    FProgressbar : TProgressBar;
    FTaskBar : TTaskbar;
    FSeries : TPieSeries;
    FTotal : integer;
    FlvAlbums : TListView;
    FphotosetId: string;
    FnumPhotos: Integer;
    FcountViews: Integer;
    Ftitle: string;
    FtotalViews: Integer;
    FPages: string;
    FInitialize: boolean;
    FBag : TDictionary<string, TComponent>;
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
    property lvAlbums: TListView read FlvAlbums write FlvAlbums;
    property pages : string read FPages write FPages;
    property Initialize : boolean read FInitialize write FInitialize;
    property TotalViews : integer read FTotalViews write FTotalViews;
    property Bag : TDictionary<string, TComponent> read FBag write FBag;
  end;

implementation

uses
  Windows, flickr.http.lib, xmlintf;

{ TThreadExec }

constructor TParallelAlbum.Create;
begin
  inherited Create(True);
  FBag := TDictionary<string, TComponent>.create();
  FreeOnTerminate := False;
end;

destructor TParallelAlbum.Destroy;
begin
  FBag.Free;
  inherited;
end;

procedure TParallelAlbum.DoInitialize;
begin
  Fprogressbar.Max := FTotal;
  FTaskbar.ProgressState := TTaskBarProgressState.Normal;
  FTaskbar.ProgressMaxValue := FTotal;
  Fprogressbar.position := 0;
end;

procedure TParallelAlbum.DoUpdate;
var
  Item : TListItem;
  color : TColor;
begin
  Fprogressbar.position := Fprogressbar.position + 1;
  Item := FlvAlbums.Items.Add;
  Item.Caption := FphotosetId;
  Item.SubItems.Add(Ftitle);
  Item.SubItems.Add(FnumPhotos.ToString());
  Item.SubItems.Add(FcountViews.ToString());

  color := RGB(Random(255), Random(255), Random(255));
  FSeries.Add(FcountViews.ToDouble, FphotosetId + '/' + Ftitle + '/' + FnumPhotos.ToString() + '/' + FcountViews.ToString(), color);
  FTaskbar.ProgressValue := Fprogressbar.position;
end;

procedure TParallelAlbum.Execute;
begin
  THttpRest.Post(FRestURL, procedure (iXMLRootNode : IXMLNode)
    var
      iXMLRootNode4, iXMLRootNode5: IXMLNode;
      total: string;
      numTotal: Integer;
    begin
      FPages := iXMLRootNode.attributes['pages'];
      if FInitialize then
        total := iXMLRootNode.attributes['total'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photoset>

      if FInitialize then
      begin
        numTotal := total.ToInteger();
        FTotal := numTotal;
        Synchronize(DoInitialize);
      end;

      FtotalViews := 0;
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photoset' then
        begin
          FphotosetId := iXMLRootNode4.attributes['id'];
          FnumPhotos := iXMLRootNode4.attributes['photos'];
          FcountViews := iXMLRootNode4.attributes['count_views'];
          iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
          Ftitle := iXMLRootNode5.text;
          FtotalViews := FtotalViews + FcountViews;
        end;
        Synchronize(DoUpdate);
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);
end;

end.
