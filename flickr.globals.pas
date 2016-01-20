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

unit flickr.globals;

interface

uses
  Contnrs, Generics.Collections, flickr.stats.global;

type
  IFlickrGlobals = interface
    procedure SetGlobals(value: TList<IStatGlobal>);
    function GetGlobals(): TList<IStatGlobal>;
    function ExistGlobal(stat: IStatGlobal; var existing: IStatGlobal): boolean;
    procedure Save(FileName: string);
    procedure Load(FileName: string);
    property Globals: TList<IStatGlobal>read GetGlobals write SetGlobals;
    function AddGlobals(stat: IStatGlobal): boolean;
  end;

  TFlickrGlobals = class(TInterfacedObject, IFlickrGlobals)
  private
    FGlobal: TList<IStatGlobal>;
    procedure SetGlobals(value: TList<IStatGlobal>);
    function GetGlobals(): TList<IStatGlobal>;
    function ExistGlobal(stat: IStatGlobal; var existing: IStatGlobal): boolean;
  public
    procedure Save(FileName: string);
    procedure Load(FileName: string);
    property Globals: TList<IStatGlobal>read GetGlobals write SetGlobals;
    function AddGlobals(stat: IStatGlobal): boolean;
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  XMLDoc, xmldom, XMLIntf, SysUtils, Vcl.Dialogs;

{ TFlickrGlobals }

function TFlickrGlobals.AddGlobals(stat: IStatGlobal): boolean;
var
  existing: IStatGlobal;
begin
  existing := nil;
  if not ExistGlobal(stat, existing) then
    FGlobal.Add(stat)
  else
    existing.Copy(stat);
  result := (existing = nil)
end;

constructor TFlickrGlobals.Create;
begin
  FGlobal := TList<IStatGlobal>.Create;
end;

destructor TFlickrGlobals.Destroy;
begin
  FreeAndNil(FGlobal);
  inherited;
end;

function TFlickrGlobals.ExistGlobal(stat: IStatGlobal; var existing: IStatGlobal): boolean;
var
  i: Integer;
  found: boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FGlobal.count) do
  begin
    found := FGlobal[i].date = stat.date;
    inc(i);
  end;
  if found then
    existing := FGlobal[i - 1];
  result := found;
end;

function TFlickrGlobals.GetGlobals: TList<IStatGlobal>;
begin
  result := FGlobal;
end;

procedure TFlickrGlobals.Load(FileName: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode: IXMLNode;
  Stats: IStatGlobal;
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
      Stats := TStatGlobal.Create();
      Stats.Load(iNode);
      FGlobal.Add(Stats);
      iNode := iNode.NextSibling;
    end;
  finally
    Document := nil;
  end;
  end
  else
    ShowMessage('File does not exists in location: ' + FileName);
end;

procedure TFlickrGlobals.Save(FileName: string);
var
  XMLDoc: TXMLDocument;
  iNode: IXMLNode;
  i: integer;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('FlickrRepositoryStats');

  for i := 0 to FGlobal.count - 1 do
  begin
    FGlobal[i].Save(iNode);
  end;
  XMLDoc.SaveToFile(FileName);
end;

procedure TFlickrGlobals.SetGlobals(value: TList<IStatGlobal>);
begin
  FGlobal := value;
end;

end.
