// Copyright (c) 2016, Jordi Corbilla
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

unit flickr.user.faves;

interface

uses
  XMLDoc, xmldom, XMLIntf, msxmldom;

type
  IUserFave = interface
    procedure SetId(const Value: string);
    procedure SetisContact(const Value: boolean);
    procedure SetisFamily(const Value: boolean);
    procedure SetisFriend(const Value: boolean);
    procedure SetMarked(const Value : boolean);
    procedure Setusername(const Value: string);
    function GetId() : string;
    function GetIsContact() : boolean;
    function GetIsFamily() : boolean;
    function GetIsFriend() : boolean;
    function GetUsername() : string;
    function GetMarked() : boolean;
    function GetLocation() : string;
    procedure SetLocation(const Value: string);
    property Id : string read GetId write SetId;
    property username : string read GetUsername write Setusername;
    property isContact : boolean read GetIsContact write SetisContact;
    property isFamily : boolean read GetIsFamily write SetisFamily;
    property isFriend : boolean read GetIsFriend write SetisFriend;
    property Marked : boolean read GetMarked write SetMarked;
    property Location : string read GetLocation write SetLocation;
    procedure Load(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
  end;

  TUserFave = Class(TInterfacedObject, IUserFave)
  private
    FisFamily: boolean;
    FId: string;
    FisContact: boolean;
    FisFriend: boolean;
    Fusername: string;
    Fmarked : boolean;
    FLocation: string;
    procedure SetId(const Value: string);
    procedure SetisContact(const Value: boolean);
    procedure SetisFamily(const Value: boolean);
    procedure SetisFriend(const Value: boolean);
    procedure Setusername(const Value: string);
    procedure SetMarked(const Value : boolean);
    function GetId() : string;
    function GetIsContact() : boolean;
    function GetIsFamily() : boolean;
    function GetIsFriend() : boolean;
    function GetUsername() : string;
    function GetMarked() : boolean;
    function GetLocation() : string;
    procedure SetLocation(const Value: string);
  public
    property Id : string read GetId write SetId;
    property username : string read GetUsername write Setusername;
    property isContact : boolean read GetIsContact write SetisContact;
    property isFamily : boolean read GetIsFamily write SetisFamily;
    property isFriend : boolean read GetIsFriend write SetisFriend;
    property Marked : boolean read GetMarked write SetMarked;
    property Location : string read GetLocation write SetLocation;
    procedure Load(iNode: IXMLNode);
    procedure Save(iNode: IXMLNode);
  End;

implementation

uses
  SysUtils, flickr.xml.helper;

{ TUserFave }

function TUserFave.GetId: string;
begin
  result := FId;
end;

function TUserFave.GetIsContact: boolean;
begin
  result := FIsContact;
end;

function TUserFave.GetIsFamily: boolean;
begin
  result := FIsFamily;
end;

function TUserFave.GetIsFriend: boolean;
begin
  result := FIsFriend;
end;

function TUserFave.GetLocation: string;
begin
  result := FLocation;
end;

function TUserFave.GetMarked: boolean;
begin
  result := FMarked;
end;

function TUserFave.GetUsername: string;
begin
  result := FUsername;
end;

procedure TUserFave.Load(iNode: IXMLNode);
begin
  FId := iNode.Attributes['id'];
  Fusername := iNode.Attributes['username'];
  FIsContact := TXMLHelper.new(iNode.Attributes['Contact']).getBool;
  FIsFriend := TXMLHelper.new(iNode.Attributes['Friend']).getBool;
  FIsFamily := TXMLHelper.new(iNode.Attributes['Family']).getBool;
  FLocation := TXMLHelper.new(iNode.Attributes['Location']).getString;
end;

procedure TUserFave.Save(iNode: IXMLNode);
begin
  iNode.Attributes['id'] := FId;
  iNode.Attributes['username'] := Fusername;
  iNode.Attributes['Contact'] := FIsContact.ToString;
  iNode.Attributes['Friend'] := FIsFriend.ToString;
  iNode.Attributes['Family'] := FIsFamily.ToString;
  iNode.Attributes['Location'] := FLocation;
end;

procedure TUserFave.SetId(const Value: string);
begin
  FId := Value;
end;

procedure TUserFave.SetisContact(const Value: boolean);
begin
  FisContact := Value;
end;

procedure TUserFave.SetisFamily(const Value: boolean);
begin
  FisFamily := Value;
end;

procedure TUserFave.SetisFriend(const Value: boolean);
begin
  FisFriend := Value;
end;

procedure TUserFave.SetLocation(const Value: string);
begin
  FLocation := Value;
end;

procedure TUserFave.SetMarked(const Value: boolean);
begin
  FMarked := Value;
end;

procedure TUserFave.Setusername(const Value: string);
begin
  Fusername := Value;
end;

end.
