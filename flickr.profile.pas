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

unit flickr.profile;

interface

uses
  XMLDoc, xmldom, XMLIntf, SysUtils, Dialogs, Generics.Collections;

type
  IProfile = interface
    function GetGroupId: TList<String>;
    function GetName: string;
    procedure SetGroupId(const Value: TList<String>);
    procedure SetName(const Value: string);
    property Name: string read GetName write SetName;
    property GroupId: TList<String> read GetGroupId write SetGroupId;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
  end;

  TProfile = class(TinterfacedObject, IProfile)
  private
    FName: string;
    FGroupId: TList<String>;
    function GetGroupId: TList<String>;
    function GetName: string;
    procedure SetGroupId(const Value: TList<String>);
    procedure SetName(const Value: string);
  public
    property Name: string read GetName write SetName;
    property GroupId: TList<String> read GetGroupId write SetGroupId;
    procedure Save(iNode: IXMLNode);
    procedure Load(iNode: IXMLNode);
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

{ TProfile }

constructor TProfile.Create;
begin
  FGroupId := TList<String>.Create();
end;

destructor TProfile.Destroy;
begin
  FreeAndNil(FGroupId);
  inherited;
end;

function TProfile.GetGroupId: TList<String>;
begin
  result := FGroupId;
end;

function TProfile.GetName: string;
begin
  result := FName;
end;

procedure TProfile.Load(iNode: IXMLNode);
var
  iNode2, iNode3: IXMLNode;
begin
  FName := iNode.Attributes['Name'];
  iNode2 := iNode.ChildNodes.First;
  while iNode2 <> nil do
  begin
    FGroupId.Add(iNode2.Attributes['Id']);
    iNode2 := iNode2.NextSibling;
  end;
end;

procedure TProfile.Save(iNode: IXMLNode);
var
  i: Integer;
  iNode2, iNode3: IXMLNode;
begin
  iNode2 := iNode.AddChild('Profile');
  iNode2.Attributes['Name'] := FName;
  for i := 0 to FGroupId.Count - 1 do
  begin
    iNode3 := iNode2.AddChild('Group');
    iNode3.Attributes['Id'] := FGroupId[i];
  end;
end;

procedure TProfile.SetGroupId(const Value: TList<String>);
begin
  FGroupId := Value;
end;

procedure TProfile.SetName(const Value: string);
begin
  FName := Value;
end;

end.
