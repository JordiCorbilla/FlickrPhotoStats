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

unit flickr.user.tracking;

interface

uses
  Contnrs, Generics.Collections, flickr.user.faves, XMLDoc, xmldom, XMLIntf, msxmldom;

type
  IUserTracking = interface
    function GetAdded(): TList<IUserFave>;
    procedure SetAdded(value: TList<IUserFave>);
    function GetRemoved(): TList<IUserFave>;
    procedure SetRemoved(value: TList<IUserFave>);
    property Added: TList<IUserFave> read GetAdded write SetAdded;
    property Removed: TList<IUserFave> read GetRemoved write SetRemoved;
    function ExistsAdded(value : IUSerFave; var user : IUSerFave) : boolean;
    function ExistsRemoved(value : IUSerFave; var user : IUSerFave) : boolean;
    procedure Load(path : string);
    procedure Save(path : string);
  end;

  TUserTracking = class (TInterfacedObject, IUserTracking)
  private
    FAdded: TList<IUserFave>;
    FRemoved: TList<IUserFave>;
    function GetAdded(): TList<IUserFave>;
    procedure SetAdded(value: TList<IUserFave>);
    function GetRemoved(): TList<IUserFave>;
    procedure SetRemoved(value: TList<IUserFave>);
    function ExistsAdded(value : IUSerFave; var user : IUSerFave) : boolean;
    function ExistsRemoved(value : IUSerFave; var user : IUSerFave) : boolean;
  public
    property Added: TList<IUserFave> read GetAdded write SetAdded;
    property Removed: TList<IUserFave> read GetRemoved write SetRemoved;
    constructor Create();
    destructor Destroy(); override;
    procedure Load(path : string);
    procedure Save(path : string);
  end;

implementation

uses
  Sysutils;

{ TUserTracking }

constructor TUserTracking.Create;
begin
  FAdded := TList<IUserFave>.create;
  FRemoved := TList<IUserFave>.create;
end;

destructor TUserTracking.Destroy;
begin
  FAdded.Free;
  FRemoved.Free;
  inherited;
end;

function TUserTracking.ExistsAdded(value: IUSerFave; var user: IUSerFave): boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FAdded.count) do
  begin
    found := FAdded[i].id = value.id;
    inc(i);
  end;
  if found then
    user := FAdded[i - 1];
  Result := found;
end;

function TUserTracking.ExistsRemoved(value: IUSerFave; var user: IUSerFave): boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FRemoved.count) do
  begin
    found := FRemoved[i].id = value.id;
    inc(i);
  end;
  if found then
    user := FRemoved[i - 1];
  Result := found;
end;

function TUserTracking.GetAdded: TList<IUserFave>;
begin
  result := FAdded;
end;

function TUserTracking.GetRemoved: TList<IUserFave>;
begin
  result := FRemoved;
end;

procedure TUserTracking.Load(path: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode2: IXMLNode;
  userFave : IUserFave;
begin
  if fileExists(path) then
  begin
    Document := TXMLDocument.Create(nil);
    try
      Document.LoadFromFile(path);
      iXMLRootNode := Document.ChildNodes.first;
      iNode2 := iXMLRootNode.ChildNodes.first;
      while iNode2 <> nil do
      begin
        if iNode2.NodeName = 'Added' then
        begin
          userFave := TUserFave.Create;
          userFave.Load(iNode2);
          FAdded.Add(userFave);
        end;
        if iNode2.NodeName = 'Removed' then
        begin
          userFave := TUserFave.Create;
          userFave.Load(iNode2);
          FRemoved.Add(userFave);
        end;
        iNode2 := iNode2.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TUserTracking.Save(path: string);
var
  i: Integer;
  iNode, iNode2: IXMLNode;
  XMLDoc: TXMLDocument;
  existing : IUserFave;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Users');
  for i := 0 to FAdded.count - 1 do
  begin
    if FAdded[i].Marked then
    begin
      iNode2 := iNode.AddChild('Added');
      FAdded[i].Save(iNode2);
    end
    else
    begin
      if not ExistsRemoved(Fadded[i], existing) then
      begin
        iNode2 := iNode.AddChild('Removed');
        FAdded[i].Save(iNode2);
      end;
    end;
  end;
  for i := 0 to FRemoved.count - 1 do
  begin
    if not ExistsAdded(FRemoved[i], existing) then
    begin
      iNode2 := iNode.AddChild('Removed');
      FRemoved[i].Save(iNode2);
    end;
  end;
  XMLDoc.SaveToFile(path)
end;

procedure TUserTracking.SetAdded(value: TList<IUserFave>);
begin
  FAdded := value;
end;

procedure TUserTracking.SetRemoved(value: TList<IUserFave>);
begin
  FRemoved := value;
end;

end.
