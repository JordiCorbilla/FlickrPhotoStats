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
    function GetAdded(): TDictionary<string, IUserFave>;
    procedure SetAdded(value: TDictionary<string, IUserFave>);
    function GetRemoved(): TDictionary<string, IUserFave>;
    procedure SetRemoved(value: TDictionary<string, IUserFave>);
    property Added: TDictionary<string, IUserFave> read GetAdded write SetAdded;
    property Removed: TDictionary<string, IUserFave> read GetRemoved write SetRemoved;
    function ExistsAdded(value : IUSerFave; var user : IUSerFave) : boolean;
    function ExistsRemoved(value : IUSerFave; var user : IUSerFave) : boolean;
    function Exists(value : IUserFave; var user : IUSerFave) : boolean;
    procedure Load(path : string);
    procedure Save(path : string);
  end;

  TUserTracking = class (TInterfacedObject, IUserTracking)
  private
    FAdded: TDictionary<string, IUserFave>;
    FRemoved: TDictionary<string, IUserFave>;
    function GetAdded(): TDictionary<string, IUserFave>;
    procedure SetAdded(value: TDictionary<string, IUserFave>);
    function GetRemoved(): TDictionary<string, IUserFave>;
    procedure SetRemoved(value: TDictionary<string, IUserFave>);
    function ExistsAdded(value : IUSerFave; var user : IUSerFave) : boolean;
    function ExistsRemoved(value : IUSerFave; var user : IUSerFave) : boolean;
    function Exists(value : IUserFave; var user : IUSerFave) : boolean;
  public
    property Added: TDictionary<string, IUserFave> read GetAdded write SetAdded;
    property Removed: TDictionary<string, IUserFave> read GetRemoved write SetRemoved;
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
  FAdded := TDictionary<string, IUserFave>.create;
  FRemoved := TDictionary<string, IUserFave>.create;
end;

destructor TUserTracking.Destroy;
begin
  FAdded.Free;
  FRemoved.Free;
  inherited;
end;

function TUserTracking.Exists(value: IUserFave; var user : IUSerFave): boolean;
var
  return : boolean;
begin
  return := false;
  if not FAdded.TryGetValue(value.Id, user) then
    return := FRemoved.TryGetValue(value.Id, user)
  else
    return := true;
  result := return;
end;

function TUserTracking.ExistsAdded(value: IUSerFave; var user: IUSerFave): boolean;
begin
  result := FAdded.TryGetValue(value.Id, user);
end;

function TUserTracking.ExistsRemoved(value: IUSerFave; var user: IUSerFave): boolean;
begin
  result := FRemoved.TryGetValue(value.Id, user);
end;

function TUserTracking.GetAdded: TDictionary<string, IUserFave>;
begin
  result := FAdded;
end;

function TUserTracking.GetRemoved: TDictionary<string, IUserFave>;
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
          FAdded.Add(userFave.Id, userFave);
        end;
        if iNode2.NodeName = 'Removed' then
        begin
          userFave := TUserFave.Create;
          userFave.Load(iNode2);
          FRemoved.Add(userFave.Id, userFave);
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
  iNode, iNode2: IXMLNode;
  XMLDoc: TXMLDocument;
  existing : IUserFave;
  Item: TPair<string, IUserFave>;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Users');

  for Item in FAdded do
  begin
    if Item.Value.Marked then
    begin
      iNode2 := iNode.AddChild('Added');
      Item.Value.Save(iNode2);
    end
    else
    begin
      if not ExistsRemoved(Item.Value, existing) then
      begin
        iNode2 := iNode.AddChild('Removed');
        Item.Value.Save(iNode2);
      end;
    end;
  end;

  for Item in FRemoved do
  begin
    if not ExistsAdded(Item.Value, existing) then
    begin
      iNode2 := iNode.AddChild('Removed');
      Item.Value.Save(iNode2);
    end;
  end;
  XMLDoc.SaveToFile(path)
end;

procedure TUserTracking.SetAdded(value: TDictionary<string, IUserFave>);
begin
  FAdded := value;
end;

procedure TUserTracking.SetRemoved(value: TDictionary<string, IUserFave>);
begin
  FRemoved := value;
end;

end.
