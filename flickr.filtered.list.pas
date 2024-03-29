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

unit flickr.filtered.list;

interface

uses
  generics.collections, flickr.base, flickr.list.comparer;

type
  IFilteredList = interface
    function GetList: TList<IBase>;
    procedure SetList(const Value: TList<IBase>);
    procedure Add(item : IBase);
    property List : TList<IBase> read GetList write SetList;
    procedure Sort;
    procedure Save(FileName: string);
    procedure Load(FileName: string);
  end;

  TFilteredList = Class(TInterfacedObject, IFilteredList)
  private
    FList : TList<IBase>;
    function GetList: TList<IBase>;
    procedure SetList(const Value: TList<IBase>);
  public
    procedure Add(item : IBase);
    property List : TList<IBase> read GetList write SetList;
    constructor Create(comparer : TCompareType);
    procedure Sort;
    procedure Save(FileName: string);
    procedure Load(FileName: string);
    destructor Destroy(); override;
  End;

implementation

uses
  SysUtils, Generics.defaults, XMLDoc, xmldom, XMLIntf, Dialogs;

{ TFilteredList }

procedure TFilteredList.Add(item: IBase);
begin
  FList.add(item);
end;

constructor TFilteredList.Create(comparer : TCompareType);
var
  IPhotoComparer : TComparer<IBase>;
begin
  case comparer of
    tCompareMembers: IPhotoComparer := TIPoolComparerMembers.Create;
    tComparePoolSize: IPhotoComparer := TIPoolComparerPoolSize.Create;
    tCompareRemaining : IPhotoComparer := TIPoolComparerRemaining.Create;
  else
    IPhotoComparer := TIPoolComparerMembers.Create;
  end;
  Flist := TList<IBase>.create(IPhotoComparer);
end;

destructor TFilteredList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TFilteredList.GetList: TList<IBase>;
begin
  result := FList;
end;

procedure TFilteredList.Load(FileName: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode: IXMLNode;
  base: IBase;
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
        base := TBase.Create();
        base.Load(iNode);
        FList.Add(base);
        iNode := iNode.NextSibling;
      end;
    finally
      Document := nil;
    end;
  end;
end;

procedure TFilteredList.Save(FileName: string);
var
  XMLDoc: TXMLDocument;
  iNode: IXMLNode;
  i: integer;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('Groups');

  for i := 0 to FList.count - 1 do
  begin
    FList[i].Save(iNode);
  end;
  XMLDoc.SaveToFile(FileName);
end;

procedure TFilteredList.SetList(const Value: TList<IBase>);
begin
  Flist := value;
end;

procedure TFilteredList.Sort;
begin
  FList.Sort;
end;

end.
