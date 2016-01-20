// Copyright (c) 2015-2016, Jordi Corbilla
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

unit flickr.stats.global;

interface

uses
  XMLDoc, xmldom, XMLIntf;

type
  IStatGlobal = interface
    function GetDate(): TDateTime;
    function GetViews(): Integer;
    function GetComments(): Integer;
    function GetLikes(): Integer;
    function GetstreamViews(): Integer;
    function GetalbumViews(): Integer;
    procedure SetDate(value: TDateTime);
    procedure SetViews(value: Integer);
    procedure SetComments(value: Integer);
    procedure SetLikes(value: Integer);
    procedure SetalbumViews(const Value: Integer);
    procedure SetstreamViews(const Value: Integer);
    property date: TDateTime read GetDate write SetDate;
    property views: Integer read GetViews write SetViews;
    property Comments: Integer read GetComments write SetComments;
    property likes: Integer read GetLikes write SetLikes;
    property streamViews : Integer read GetstreamViews write SetstreamViews;
    property albumViews : Integer read GetalbumViews write SetalbumViews;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    procedure Copy(stat: IStatGlobal);
  end;

  TStatGlobal = class(TInterfacedObject, IStatGlobal)
  private
    FDate: TDateTime;
    FViews: Integer;
    FComments: Integer;
    FLikes: Integer;
    FstreamViews: Integer;
    FalbumViews: Integer;
    function GetDate(): TDateTime;
    function GetViews(): Integer;
    function GetLikes(): Integer;
    function GetComments(): Integer;
    function GetstreamViews(): Integer;
    function GetalbumViews(): Integer;
    procedure SetDate(value: TDateTime);
    procedure SetViews(value: Integer);
    procedure SetLikes(value: Integer);
    procedure SetComments(value: Integer);
    procedure SetalbumViews(const Value: Integer);
    procedure SetstreamViews(const Value: Integer);
  public
    property date: TDateTime read GetDate write SetDate;
    property views: Integer read GetViews write SetViews;
    property comments: Integer read GetComments write SetComments;
    property likes: Integer read GetLikes write SetLikes;
    property streamViews : Integer read GetstreamViews write SetstreamViews;
    property albumViews : Integer read GetalbumViews write SetalbumViews;
    Constructor Create(); overload;
    Constructor Create(date: TDateTime; views: Integer; likes: Integer; Comments: Integer; albumViews : integer; streamViews : integer); overload;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    procedure Copy(stat: IStatGlobal);
  end;

implementation

{ TViews }

uses
  System.SysUtils, flickr.xml.helper;

procedure TStatGlobal.Copy(stat: IStatGlobal);
begin
  SetComments(stat.Comments);
  SetLikes(stat.likes);
  SetViews(stat.views);
  SetAlbumViews(stat.albumViews);
  SetStreamViews(stat.streamViews);
end;

constructor TStatGlobal.Create(date: TDateTime; views: Integer; likes: Integer; Comments: Integer; albumViews : integer; streamViews : integer);
begin
  SetDate(date);
  SetViews(views);
  SetComments(Comments);
  SetLikes(likes);
  SetAlbumViews(albumViews);
  SetStreamViews(streamViews);
end;

constructor TStatGlobal.Create;
begin

end;

function TStatGlobal.GetDate: TDateTime;
begin
  result := FDate;
end;

function TStatGlobal.GetLikes: Integer;
begin
  result := FLikes;
end;

function TStatGlobal.GetalbumViews: Integer;
begin
  result := FAlbumViews;
end;

function TStatGlobal.GetstreamViews: Integer;
begin
  result := FStreamViews;
end;

function TStatGlobal.GetComments: Integer;
begin
  result := FComments;
end;

function TStatGlobal.GetViews: Integer;
begin
  result := FViews;
end;

procedure TStatGlobal.Load(iNode: IXMLNode);
begin
  FDate := TXMLHelper.new(iNode.Attributes['Date']).getDate();
  FViews := TXMLHelper.new(iNode.Attributes['Views']).getInt();
  FComments := TXMLHelper.new(iNode.Attributes['Comments']).getInt();
  FLikes := TXMLHelper.new(iNode.Attributes['Likes']).getInt();
  FAlbumViews := TXMLHelper.new(iNode.Attributes['AlbumViews']).getInt();
  FStreamViews := TXMLHelper.new(iNode.Attributes['StreamViews']).getInt();
end;

procedure TStatGlobal.Save(iNode: IXMLNode);
var
  iNode2: IXMLNode;
begin
  iNode2 := iNode.AddChild('Stats');
  iNode2.Attributes['Date'] := DateToStr(FDate);
  iNode2.Attributes['Views'] := IntToStr(FViews);
  iNode2.Attributes['Comments'] := IntToStr(FComments);
  iNode2.Attributes['Likes'] := IntToStr(FLikes);
  iNode2.Attributes['AlbumViews'] := IntToStr(FAlbumViews);
  iNode2.Attributes['StreamViews'] := IntToStr(FStreamViews);
end;

procedure TStatGlobal.SetDate(value: TDateTime);
begin
  FDate := value;
end;

procedure TStatGlobal.SetLikes(value: Integer);
begin
  FLikes := value;
end;

procedure TStatGlobal.SetalbumViews(const Value: Integer);
begin
  FalbumViews := Value;
end;

procedure TStatGlobal.SetstreamViews(const Value: Integer);
begin
  FstreamViews := Value;
end;

procedure TStatGlobal.SetComments(value: Integer);
begin
  FComments := value;
end;

procedure TStatGlobal.SetViews(value: Integer);
begin
  FViews := value;
end;

end.
