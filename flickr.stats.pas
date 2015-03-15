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

unit flickr.stats;

interface

uses
  XMLDoc, xmldom, XMLIntf;

type
  IStat = interface
    function GetDate(): TDateTime;
    function GetViews(): Integer;
    function GetNumComments(): Integer;
    function GetLikes(): Integer;
    procedure SetDate(value: TDateTime);
    procedure SetViews(value: Integer);
    procedure SetNumComments(value: Integer);
    procedure SetLikes(value: Integer);
    property date: TDateTime read GetDate write SetDate;
    property views: Integer read GetViews write SetViews;
    property numComments: Integer read GetNumComments write SetNumComments;
    property likes: Integer read GetLikes write SetLikes;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    procedure Copy(stat: IStat);
  end;

  TStat = class(TInterfacedObject, IStat)
  private
    FDate: TDateTime;
    FViews: Integer;
    FNumComments: Integer;
    FLikes: Integer;
    function GetDate(): TDateTime;
    function GetViews(): Integer;
    function GetLikes(): Integer;
    function GetNumComments(): Integer;
    procedure SetDate(value: TDateTime);
    procedure SetViews(value: Integer);
    procedure SetLikes(value: Integer);
    procedure SetNumComments(value: Integer);
  public
    property date: TDateTime read GetDate write SetDate;
    property views: Integer read GetViews write SetViews;
    property numComments: Integer read GetNumComments write SetNumComments;
    property likes: Integer read GetLikes write SetLikes;
    Constructor Create(); overload;
    Constructor Create(date: TDateTime; views: Integer; likes: Integer; numComments: Integer); overload;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    procedure Copy(stat: IStat);
  end;

implementation

{ TViews }

uses
  SysUtils;

procedure TStat.Copy(stat: IStat);
begin
  SetNumComments(stat.numComments);
  SetLikes(stat.likes);
  SetViews(stat.views);
end;

constructor TStat.Create(date: TDateTime; views: Integer; likes: Integer; numComments: Integer);
begin
  SetDate(date);
  SetViews(views);
  SetNumComments(numComments);
  SetLikes(likes);
end;

constructor TStat.Create;
begin

end;

function TStat.GetDate: TDateTime;
begin
  result := FDate;
end;

function TStat.GetLikes: Integer;
begin
  result := FLikes;
end;

function TStat.GetNumComments: Integer;
begin
  result := FNumComments;
end;

function TStat.GetViews: Integer;
begin
  result := FViews;
end;

procedure TStat.Load(iNode: IXMLNode);
begin
  FDate := StrToDate(iNode.Attributes['Date']);
  FViews := StrToInt(iNode.Attributes['Views']);
  FNumComments := StrToInt(iNode.Attributes['Comments']);
  FLikes := StrToInt(iNode.Attributes['Likes']);
end;

procedure TStat.Save(iNode: IXMLNode);
var
  iNode2: IXMLNode;
begin
  iNode2 := iNode.AddChild('Stats');
  iNode2.Attributes['Date'] := DateToStr(FDate);
  iNode2.Attributes['Views'] := IntToStr(FViews);
  iNode2.Attributes['Comments'] := IntToStr(FNumComments);
  iNode2.Attributes['Likes'] := IntToStr(FLikes);
end;

procedure TStat.SetDate(value: TDateTime);
begin
  FDate := value;
end;

procedure TStat.SetLikes(value: Integer);
begin
  FLikes := value;
end;

procedure TStat.SetNumComments(value: Integer);
begin
  FNumComments := value;
end;

procedure TStat.SetViews(value: Integer);
begin
  FViews := value;
end;

end.
