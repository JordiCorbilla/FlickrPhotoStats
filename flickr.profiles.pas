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

unit flickr.profiles;

interface

uses
  generics.collections, flickr.profile;

type
  IProfiles = interface
    function getList: TList<IProfile>;
    procedure setList(const Value: TList<IProfile>);
    property list: TList<IProfile> read getList write setList;
    procedure Save(FileName: string);
    procedure Load(FileName: string);
    procedure Add(profile : IProfile);
    function getProfile(name : string) : IProfile;
  end;

  TProfiles = class(TInterfacedObject, IProfiles)
  private
    FList: TList<IProfile>;
    function getList: TList<IProfile>;
    procedure setList(const Value: TList<IProfile>);
  public
    property list: TList<IProfile> read getList write setList;
    procedure Save(FileName: string);
    procedure Load(FileName: string);
    procedure Add(profile : IProfile);
    function getProfile(name : string) : IProfile;
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  XMLDoc, xmldom, XMLIntf, SysUtils, Dialogs;

{ TProfiles }

procedure TProfiles.Add(profile: IProfile);
begin
  FList.Add(profile);
end;

constructor TProfiles.Create;
begin
  FList := TList<IProfile>.Create;
end;

destructor TProfiles.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TProfiles.getList: TList<IProfile>;
begin
  result := FList;
end;

function TProfiles.getProfile(name: string): IProfile;
var
  found : boolean;
  i : integer;
  profile : IProfile;
begin
  profile := nil;
  i := 0;
  found := false;
  while not( found) and (i<FList.Count) do
  begin
    found := FList[i].Name = name;
    if found then
      profile := FList[i];
    inc(i);
  end;
  result := profile;
end;

procedure TProfiles.Load(FileName: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode: IXMLNode;
  profile: IProfile;
begin
  if fileExists(FileName) then
  begin
    Document := TXMLDocument.Create(nil);
    try
      Document.LoadFromFile(FileName);
      iXMLRootNode := Document.ChildNodes.first;
      iNode := iXMLRootNode.ChildNodes.first;
      while iNode <> nil do
      begin
        profile := TProfile.Create();
        profile.Load(iNode);
        FList.Add(profile);
        iNode := iNode.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end
  else
    ShowMessage('File does not exists in location: ' + FileName);
end;

procedure TProfiles.Save(FileName: string);
var
  XMLDoc: TXMLDocument;
  iNode: IXMLNode;
  i: integer;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Profiles');

  for i := 0 to FList.count - 1 do
  begin
    FList[i].Save(iNode);
  end;
  XMLDoc.SaveToFile(FileName);
end;

procedure TProfiles.setList(const Value: TList<IProfile>);
begin
  FList := Value;
end;

end.
