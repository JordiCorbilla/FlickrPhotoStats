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

unit flickr.base;

interface

uses
  XMLDoc, xmldom, XMLIntf;

type
  IBase = interface
    function GetId: string;
    function GetTitle: string;
    function GetPhotos: Int64;
    function GetMembers: Int64;
    procedure SetId(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetMembers(const Value: Int64);
    procedure SetPhotos(const Value: Int64);
    property Id : string read GetId write SetId;
    property Title : string read GetTitle write SetTitle;
    property Photos : Int64 read GetPhotos write SetPhotos;
    property Members : Int64 read GetMembers write SetMembers;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
  end;

  //Base class as Id-title pair.
  TBase = class(TInterfacedObject, IBase)
  private
    FId : string;
    FTitle : string;
    FPhotos: Int64;
    FMembers: Int64;
    function GetId: string;
    function GetTitle: string;
    function GetPhotos: Int64;
    function GetMembers: Int64;
    procedure SetId(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetMembers(const Value: Int64);
    procedure SetPhotos(const Value: Int64);
  public
    property Id : string read GetId write SetId;
    property Title : string read GetTitle write SetTitle;
    property Photos : Int64 read GetPhotos write SetPhotos;
    property Members : Int64 read GetMembers write SetMembers;
    constructor Create(id, title : string; photos, members : Int64); overload;
    constructor Create(); overload;
    destructor Destroy; override;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    class function New(id, title : string; photos, members : Int64) : IBase;
  end;

implementation

uses
  Sysutils;

{ TGroup }

constructor TBase.Create(id, title: string; photos, members : Int64);
begin
  SetId(id);
  SetTitle(title);
  SetPhotos(photos);
  SetMembers(members);
end;

constructor TBase.Create;
begin

end;

destructor TBase.Destroy;
begin

  inherited;
end;

function TBase.GetId: string;
begin
  result := FId;
end;

function TBase.GetMembers: Int64;
begin
  result := FMembers;
end;

function TBase.GetPhotos: Int64;
begin
  result := FPhotos;
end;

function TBase.GetTitle: string;
begin
  result := FTitle;
end;

procedure TBase.Load(iNode: IXMLNode);
begin
  Fid := iNode.Attributes['id'];
  Ftitle := iNode.Attributes['title'];
  Fphotos := StrToInt(iNode.Attributes['photos']);
  Fmembers := StrToInt(iNode.Attributes['members']);
end;

class function TBase.New(id, title : string; photos, members : Int64): IBase;
begin
  result := Create(id, title, photos, members);
end;

procedure TBase.Save(iNode: IXMLNode);
var
  iNode2: IXMLNode;
begin
  iNode2 := iNode.AddChild('Group');
  iNode2.Attributes['id'] := Fid;
  iNode2.Attributes['title'] := Ftitle;
  iNode2.Attributes['photos'] := Fphotos.toString;
  iNode2.Attributes['members'] := Fmembers.ToString;
end;

procedure TBase.SetId(const Value: string);
begin
  FId := value;
end;

procedure TBase.SetMembers(const Value: Int64);
begin
  FMembers := Value;
end;

procedure TBase.SetPhotos(const Value: Int64);
begin
  FPhotos := Value;
end;

procedure TBase.SetTitle(const Value: string);
begin
  FTitle := value;
end;

end.
