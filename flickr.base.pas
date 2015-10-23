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
    procedure Setart_ok(const Value: boolean);
    procedure SetDescription(const Value: string);
    procedure Sethas_geo(const Value: boolean);
    procedure Setimages_ok(const Value: boolean);
    procedure SetIsModerated(const Value: boolean);
    procedure Setmoderate_ok(const Value: boolean);
    procedure Setphotos_ok(const Value: boolean);
    procedure Setrestricted_ok(const Value: boolean);
    procedure Setsafe_ok(const Value: boolean);
    procedure Setscreens_ok(const Value: boolean);
    procedure SetThrottleCount(const Value: integer);
    procedure SetThrottleMode(const Value: string);
    procedure SetThrottleRemaining(const Value: integer);
    procedure Setvideos_ok(const Value: boolean);
    function GetIsModerated() : boolean;
    function GetDescription() : string;
    function GetThrottleCount() : integer;
    function GetThrottleMode() : string;
    function GetThrottleRemaining() : integer;
    function Getphotos_ok() : boolean;
    function Getvideos_ok() : boolean;
    function Getimages_ok() : boolean;
    function Getscreens_ok() : boolean;
    function Getart_ok() : boolean;
    function Getsafe_ok() : boolean;
    function Getmoderate_ok() : boolean;
    function Getrestricted_ok() : boolean;
    function Gethas_geo() : boolean;
    property Id : string read GetId write SetId;
    property Title : string read GetTitle write SetTitle;
    property Photos : Int64 read GetPhotos write SetPhotos;
    property Members : Int64 read GetMembers write SetMembers;
    property IsModerated : boolean read GetIsModerated write SetIsModerated;
    property Description : string read GetDescription write SetDescription;
    property ThrottleCount : integer read GetThrottleCount write SetThrottleCount;
    property ThrottleMode : string read GetThrottleMode write SetThrottleMode;
    property ThrottleRemaining : integer read GetThrottleRemaining write SetThrottleRemaining;
    property photos_ok : boolean read Getphotos_ok write Setphotos_ok;
    property videos_ok : boolean read Getvideos_ok write Setvideos_ok;
    property images_ok : boolean read Getimages_ok write Setimages_ok;
    property screens_ok : boolean read Getscreens_ok write Setscreens_ok;
    property art_ok : boolean read Getart_ok write Setart_ok;
    property safe_ok : boolean read Getsafe_ok write Setsafe_ok;
    property moderate_ok : boolean read Getmoderate_ok write Setmoderate_ok;
    property restricted_ok : boolean read Getrestricted_ok write Setrestricted_ok;
    property has_geo : boolean read Gethas_geo write Sethas_geo;
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
    Fscreens_ok: boolean;
    Fmoderate_ok: boolean;
    Frestricted_ok: boolean;
    FThrottleMode: string;
    Fvideos_ok: boolean;
    Fphotos_ok: boolean;
    FThrottleCount: integer;
    FThrottleRemaining: integer;
    Fsafe_ok: boolean;
    FDescription: string;
    Fhas_geo: boolean;
    Fart_ok: boolean;
    FIsModerated: boolean;
    Fimages_ok: boolean;
    function GetId: string;
    function GetTitle: string;
    function GetPhotos: Int64;
    function GetMembers: Int64;
    procedure SetId(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetMembers(const Value: Int64);
    procedure SetPhotos(const Value: Int64);
    procedure Setart_ok(const Value: boolean);
    procedure SetDescription(const Value: string);
    procedure Sethas_geo(const Value: boolean);
    procedure Setimages_ok(const Value: boolean);
    procedure SetIsModerated(const Value: boolean);
    procedure Setmoderate_ok(const Value: boolean);
    procedure Setphotos_ok(const Value: boolean);
    procedure Setrestricted_ok(const Value: boolean);
    procedure Setsafe_ok(const Value: boolean);
    procedure Setscreens_ok(const Value: boolean);
    procedure SetThrottleCount(const Value: integer);
    procedure SetThrottleMode(const Value: string);
    procedure SetThrottleRemaining(const Value: integer);
    procedure Setvideos_ok(const Value: boolean);
    function GetIsModerated() : boolean;
    function GetDescription() : string;
    function GetThrottleCount() : integer;
    function GetThrottleMode() : string;
    function GetThrottleRemaining() : integer;
    function Getphotos_ok() : boolean;
    function Getvideos_ok() : boolean;
    function Getimages_ok() : boolean;
    function Getscreens_ok() : boolean;
    function Getart_ok() : boolean;
    function Getsafe_ok() : boolean;
    function Getmoderate_ok() : boolean;
    function Getrestricted_ok() : boolean;
    function Gethas_geo() : boolean;
  public
    property Id : string read GetId write SetId;
    property Title : string read GetTitle write SetTitle;
    property Photos : Int64 read GetPhotos write SetPhotos;
    property Members : Int64 read GetMembers write SetMembers;
    property IsModerated : boolean read GetIsModerated write SetIsModerated;
    property Description : string read GetDescription write SetDescription;
    property ThrottleCount : integer read GetThrottleCount write SetThrottleCount;
    property ThrottleMode : string read GetThrottleMode write SetThrottleMode;
    property ThrottleRemaining : integer read GetThrottleRemaining write SetThrottleRemaining;
    property photos_ok : boolean read Getphotos_ok write Setphotos_ok;
    property videos_ok : boolean read Getvideos_ok write Setvideos_ok;
    property images_ok : boolean read Getimages_ok write Setimages_ok;
    property screens_ok : boolean read Getscreens_ok write Setscreens_ok;
    property art_ok : boolean read Getart_ok write Setart_ok;
    property safe_ok : boolean read Getsafe_ok write Setsafe_ok;
    property moderate_ok : boolean read Getmoderate_ok write Setmoderate_ok;
    property restricted_ok : boolean read Getrestricted_ok write Setrestricted_ok;
    property has_geo : boolean read Gethas_geo write Sethas_geo;
    constructor Create(id, title : string; photos, members : Int64); overload;
    constructor Create(); overload;
    destructor Destroy; override;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    class function New(id, title : string; photos, members : Int64) : IBase;
  end;

implementation

uses
  Sysutils, flickr.xml.helper;

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

function TBase.Getart_ok: boolean;
begin
  result := Fart_ok;
end;

function TBase.GetDescription: string;
begin
  result := FDescription;
end;

function TBase.Gethas_geo: boolean;
begin
  result := Fhas_geo;
end;

function TBase.GetId: string;
begin
  result := FId;
end;

function TBase.Getimages_ok: boolean;
begin
  result := Fimages_ok;
end;

function TBase.GetIsModerated: boolean;
begin
  result := FIsModerated;
end;

function TBase.GetMembers: Int64;
begin
  result := FMembers;
end;

function TBase.Getmoderate_ok: boolean;
begin
  result := Fmoderate_ok;
end;

function TBase.GetPhotos: Int64;
begin
  result := FPhotos;
end;

function TBase.Getphotos_ok: boolean;
begin
  result := Fphotos_ok;
end;

function TBase.Getrestricted_ok: boolean;
begin
  result := Frestricted_ok;
end;

function TBase.Getsafe_ok: boolean;
begin
  result := Fsafe_ok;
end;

function TBase.Getscreens_ok: boolean;
begin
  result := Fscreens_ok;
end;

function TBase.GetThrottleCount: integer;
begin
  result := FThrottleCount;
end;

function TBase.GetThrottleMode: string;
begin
  result := FThrottleMode;
end;

function TBase.GetThrottleRemaining: integer;
begin
  result := FThrottleRemaining;
end;

function TBase.GetTitle: string;
begin
  result := FTitle;
end;

function TBase.Getvideos_ok: boolean;
begin
  result := Fvideos_ok;
end;

procedure TBase.Load(iNode: IXMLNode);
begin
  Fid := iNode.Attributes['id'];
  Ftitle := iNode.Attributes['title'];
  Fphotos := StrToInt(iNode.Attributes['photos']);
  Fmembers := StrToInt(iNode.Attributes['members']);

  Fdescription := TXMLHelper.new(iNode.Attributes['Description']).getString;
  FThrottleCount := TXMLHelper.new(iNode.attributes['count']).getInt;
  FThrottleMode := TXMLHelper.new(iNode.attributes['mode']).getString;
  FThrottleRemaining := TXMLHelper.new(iNode.attributes['remaining']).getInt;
  Fphotos_ok := TXMLHelper.new(iNode.attributes['photos_ok']).getBool;
  Fvideos_ok := TXMLHelper.new(iNode.attributes['videos_ok']).getBool;
  Fimages_ok := TXMLHelper.new(iNode.attributes['images_ok']).getBool;
  Fscreens_ok := TXMLHelper.new(iNode.attributes['screens_ok']).getBool;
  Fart_ok := TXMLHelper.new(iNode.attributes['art_ok']).getBool;
  Fsafe_ok := TXMLHelper.new(iNode.attributes['safe_ok']).getBool;
  Fmoderate_ok := TXMLHelper.new(iNode.attributes['moderate_ok']).getBool;
  Frestricted_ok := TXMLHelper.new(iNode.attributes['restricted_ok']).getBool;
  Fhas_geo := TXMLHelper.new(iNode.attributes['has_geo']).getBool;

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

  iNode2.Attributes['IsModerated'] := IsModerated.ToString;
  iNode2.Attributes['Description'] := Description;
  iNode2.Attributes['ThrottleCount'] := ThrottleCount.ToString;
  iNode2.Attributes['ThrottleMode'] := ThrottleMode;
  iNode2.Attributes['ThrottleRemaining'] := ThrottleRemaining.ToString;
  iNode2.Attributes['photos_ok'] := photos_ok.ToString;
  iNode2.Attributes['videos_ok'] := videos_ok.ToString;
  iNode2.Attributes['images_ok'] := images_ok.ToString;
  iNode2.Attributes['screens_ok'] := screens_ok.ToString;
  iNode2.Attributes['art_ok'] := art_ok.ToString;
  iNode2.Attributes['safe_ok'] := safe_ok.ToString;
  iNode2.Attributes['moderate_ok'] := moderate_ok.ToString;
  iNode2.Attributes['restricted_ok'] := restricted_ok.ToString;
  iNode2.Attributes['has_geo'] := has_geo.ToString;
end;

procedure TBase.Setart_ok(const Value: boolean);
begin
  Fart_ok := Value;
end;

procedure TBase.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TBase.Sethas_geo(const Value: boolean);
begin
  Fhas_geo := Value;
end;

procedure TBase.SetId(const Value: string);
begin
  FId := value;
end;

procedure TBase.Setimages_ok(const Value: boolean);
begin
  Fimages_ok := Value;
end;

procedure TBase.SetIsModerated(const Value: boolean);
begin
  FIsModerated := Value;
end;

procedure TBase.SetMembers(const Value: Int64);
begin
  FMembers := Value;
end;

procedure TBase.Setmoderate_ok(const Value: boolean);
begin
  Fmoderate_ok := Value;
end;

procedure TBase.SetPhotos(const Value: Int64);
begin
  FPhotos := Value;
end;

procedure TBase.Setphotos_ok(const Value: boolean);
begin
  Fphotos_ok := Value;
end;

procedure TBase.Setrestricted_ok(const Value: boolean);
begin
  Frestricted_ok := Value;
end;

procedure TBase.Setsafe_ok(const Value: boolean);
begin
  Fsafe_ok := Value;
end;

procedure TBase.Setscreens_ok(const Value: boolean);
begin
  Fscreens_ok := Value;
end;

procedure TBase.SetThrottleCount(const Value: integer);
begin
  FThrottleCount := Value;
end;

procedure TBase.SetThrottleMode(const Value: string);
begin
  FThrottleMode := Value;
end;

procedure TBase.SetThrottleRemaining(const Value: integer);
begin
  FThrottleRemaining := Value;
end;

procedure TBase.SetTitle(const Value: string);
begin
  FTitle := value;
end;

procedure TBase.Setvideos_ok(const Value: boolean);
begin
  Fvideos_ok := Value;
end;

end.
