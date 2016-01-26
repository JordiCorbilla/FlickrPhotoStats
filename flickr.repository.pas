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

unit flickr.repository;

interface

uses
  Contnrs, Generics.Collections, flickr.photos, flickr.list.comparer;

type
  IFlickrRepository = interface
    procedure SetApiKey(value: string);
    function GetApiKey(): string;
    procedure SetUserId(value: string);
    function GetUserId(): string;
    procedure AddPhoto(photo: IPhoto);
    function GetPhotos(): TList<IPhoto>;
    procedure SetPhotos(value: TList<IPhoto>);
    function GetPhoto(id: string): IPhoto;
    procedure SetSecret(value: string);
    function GetSecret(): string;
    procedure Save(ApiKey: string; Secret: string; UserId: string; FileName: string);
    procedure Load(FileName: string);
    procedure DeletePhoto(id : string);
    function Getsorted(): boolean;
    function Getversion() : string;
    function GetDateSaved() : TDateTime;
    procedure Setsorted(const Value: boolean);
    procedure SetDateSaved(const Value: TDateTime);
    procedure Setversion(const Value: string);
    procedure SetPreviousVersion(const Value: string);
    function GetPreviousVersion() : string;
    function ExistPhoto(photo: IPhoto; var existing: IPhoto): Boolean; overload;
    function ExistPhoto(id: string; var existing: IPhoto): Boolean; overload;
    function isPhotoInGroup(photo : string; groupId : string; var resPhoto : IPhoto) : boolean;
    property ApiKey: string read GetApiKey write SetApiKey;
    property UserId: string read GetUserId write SetUserId;
    property photos: TList<IPhoto>read GetPhotos write SetPhotos;
    property Secret: string read GetSecret write SetSecret;
    property sorted : boolean read Getsorted write Setsorted;
    property version : string read Getversion write Setversion;
    property DateSaved : TDateTime read GetDateSaved write SetDateSaved;
    property PreviousVersion : string read GetPreviousVersion write SetPreviousVersion;
    function getTotalSpreadGroups() : integer;
  end;

  TFlickrRepository = class(TInterfacedObject, IFlickrRepository)
  private
    FApiKey: String;
    FUserId : String;
    FPhotos: TList<IPhoto>;
    FSecret: String;
    Fsorted: boolean;
    Fversion: string;
    FDateSaved: TDateTime;
    FPreviousVersion: string;
    procedure SetApiKey(value: string);
    function GetApiKey(): string;
    function GetPhotos(): TList<IPhoto>;
    procedure SetPhotos(value: TList<IPhoto>);
    procedure SetUserId(value: string);
    function GetUserId(): string;
    procedure SetSecret(value: string);
    function GetSecret(): string;
    function Getsorted(): boolean;
    function Getversion() : string;
    function GetDateSaved() : TDateTime;
    function GetPreviousVersion() : string;
    procedure Setsorted(const Value: boolean);
    procedure SetDateSaved(const Value: TDateTime);
    procedure Setversion(const Value: string);
    procedure SetPreviousVersion(const Value: string);
  public
    procedure AddPhoto(photo: IPhoto);
    procedure Load(FileName: string);
    function ExistPhoto(photo: IPhoto; var existing: IPhoto): Boolean; overload;
    function ExistPhoto(id: string; var existing: IPhoto): Boolean; overload;
    function isPhotoInGroup(photo : string; groupId : string; var resPhoto : IPhoto) : boolean;
    constructor Create(); overload;
    constructor Create(sorting : boolean; comparer : TCompareType); overload;
    function GetPhoto(id: string): IPhoto;
    destructor Destroy(); override;
    function getTotalSpreadGroups() : integer;
    procedure Save(ApiKey: string; Secret: string; UserId: string; FileName: string);
    property ApiKey: string read GetApiKey write SetApiKey;
    property UserId: string read GetUserId write SetUserId;
    property photos: TList<IPhoto>read GetPhotos write SetPhotos;
    property sorted : boolean read Getsorted write Setsorted;
    property version : string read Getversion write Setversion;
    property DateSaved : TDateTime read GetDateSaved write SetDateSaved;
    property PreviousVersion : string read GetPreviousVersion write SetPreviousVersion;
    procedure DeletePhoto(id : string);
  end;

implementation

{ TFlickrRepository }

uses
  XMLDoc, xmldom, XMLIntf, SysUtils, Vcl.Dialogs, flickr.top.stats, variants, Generics.defaults;

procedure TFlickrRepository.AddPhoto(photo: IPhoto);
begin
  FPhotos.Add(photo);
end;

constructor TFlickrRepository.Create;
begin
  FPhotos := TList<IPhoto>.Create();
end;

constructor TFlickrRepository.Create(sorting: boolean; comparer : TCompareType);
var
  IPhotoComparer : TComparer<IPhoto>;
begin
  case comparer of
    tCompareId: IPhotoComparer := TIPhotoComparerId.Create;
    tCompareLikes: IPhotoComparer := TIPhotoComparerLikes.Create;
    tCompareViews: IPhotoComparer := TIPhotoComparerViews.Create;
    tCompareComments: IPhotoComparer := TIPhotoComparerComments.Create;
    tCompareTaken: IPhotoComparer := TIPhotoComparerTaken.Create;
    tCompareAlbums: IPhotoComparer := TIPhotoComparerAlbums.Create;
    tCompareGroups: IPhotoComparer := TIPhotoComparerGroups.Create;
    tCompareTrend: IPhotoComparer := TIPhotoComparerTrend.Create;
  else
    IPhotoComparer := TIPhotoComparerViews.Create;
  end;
  FPhotos := TList<IPhoto>.Create(IPhotoComparer);
end;

procedure TFlickrRepository.DeletePhoto(id: string);
var
  photo : IPhoto;
begin
  photo := GetPhoto(id);
  FPhotos.Remove(photo);
end;

destructor TFlickrRepository.Destroy;
begin
  FreeAndNil(FPhotos);
  inherited;
end;

function TFlickrRepository.ExistPhoto(photo: IPhoto; var existing: IPhoto): Boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FPhotos.count) do
  begin
    found := FPhotos[i].id = photo.id;
    inc(i);
  end;
  if found then
    existing := FPhotos[i - 1];
  Result := found;
end;

function TFlickrRepository.ExistPhoto(id: string; var existing: IPhoto): Boolean;
var
  i: integer;
  found: Boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FPhotos.count) do
  begin
    found := FPhotos[i].id = id;
    inc(i);
  end;
  if found then
    existing := FPhotos[i - 1];
  Result := found;
end;

function TFlickrRepository.GetApiKey: string;
begin
  Result := FApiKey;
end;

function TFlickrRepository.GetDateSaved: TDateTime;
begin
  result := FDateSaved;
end;

function TFlickrRepository.GetPhoto(id: string): IPhoto;
var
  i: integer;
  found: Boolean;
  existing: IPhoto;
begin
  i := 0;
  found := false;
  existing := nil;
  while (not found) and (i < FPhotos.count) do
  begin
    found := FPhotos[i].id = id;
    inc(i);
  end;
  if found then
    existing := FPhotos[i - 1];
  Result := existing;
end;

function TFlickrRepository.GetPhotos: TList<IPhoto>;
begin
  Result := FPhotos;
end;

function TFlickrRepository.GetPreviousVersion: string;
begin
  result := FPreviousVersion;
end;

function TFlickrRepository.GetSecret: string;
begin
  Result := FSecret;
end;

function TFlickrRepository.Getsorted: boolean;
begin
  result := FSorted;
end;

function TFlickrRepository.getTotalSpreadGroups: integer;
var
  total : integer;
  i: Integer;
begin
  total := 0;
  for i := 0 to FPhotos.Count-1 do
  begin
    total := total + FPhotos[i].Groups.Count;
  end;
  result := total;
end;

function TFlickrRepository.GetUserId: string;
begin
  Result := FUserId;
end;

function TFlickrRepository.Getversion: string;
begin
  result := FVersion;
end;

function TFlickrRepository.isPhotoInGroup(photo, groupId: string; var resPhoto : IPhoto): boolean;
var
  p : IPhoto;
begin
  p := GetPhoto(photo);
  resPhoto := p;
  result := p.InGroup(groupid);
end;

procedure TFlickrRepository.Load(FileName: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode: IXMLNode;
  photo: IPhoto;
begin
  if fileExists(FileName) then
  begin
  Document := TXMLDocument.Create(nil);
  try
    Document.LoadFromFile(FileName);
    iXMLRootNode := Document.ChildNodes.first;
    Self.FApiKey := iXMLRootNode.attributes['ApiKey'];

    try
      if (iXMLRootNode.attributes['UserId'] <>  null) then
        Self.FUserId := iXMLRootNode.attributes['UserId'];
    except
      Self.FUserId := '';
    end;

    try
      if (iXMLRootNode.attributes['Secret'] <> null) then
        Self.FSecret := iXMLRootNode.attributes['Secret'];
    except
      Self.FSecret := '';
    end;

    try
      if (iXMLRootNode.attributes['Version'] <> null) then
        Self.FPreviousversion := iXMLRootNode.attributes['Version'];
    except
      Self.Fversion := '';
    end;

    try
      if (iXMLRootNode.attributes['DateSaved'] <> null) then
        Self.FDateSaved := StrToDateTime(iXMLRootNode.attributes['DateSaved']);
    except
      Self.FDateSaved := Now;
    end;

    iNode := iXMLRootNode.ChildNodes.first;
    while iNode <> nil do
    begin
      photo := TPhoto.Create();
      photo.folder := ExtractFilePath(FileName);
      if (FPreviousVersion = '4.8.0.2') and (FVersion = '4.8.0.2') then
        photo.LoadNew(iNode)
      else
        photo.Load(iNode);
      FPhotos.Add(photo);
      iNode := iNode.NextSibling;
    end;

    if sorted then
      FPhotos.Sort;
  finally
    Document := nil;
  end;
  end
  else
    ShowMessage('File does not exists in location: ' + FileName);
end;

procedure TFlickrRepository.Save(ApiKey: string; Secret: string; UserId: string; FileName: string);
var
  XMLDoc: TXMLDocument;
  iNode: IXMLNode;
  i: integer;
begin
  // Create the XML file
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Active := true;
  iNode := XMLDoc.AddChild('FlickrRepository');
  iNode.attributes['ApiKey'] := ApiKey;
  iNode.attributes['UserId'] := UserId;
  iNode.attributes['Secret'] := Secret;
  iNode.Attributes['Version'] := FVersion;
  iNode.Attributes['DateSaved'] := DateTimeToStr(FDateSaved);

  for i := 0 to FPhotos.count - 1 do
  begin
    FPhotos[i].folder := ExtractFilePath(FileName);
    if (FPreviousVersion = '4.8.0.1') and (Fversion = '4.8.0.2') then
      FPhotos[i].SaveNew(iNode)
    else
      FPhotos[i].Save(iNode);
  end;
  XMLDoc.SaveToFile(FileName);
end;

procedure TFlickrRepository.SetApiKey(value: string);
begin
  FApiKey := value;
end;

procedure TFlickrRepository.SetDateSaved(const Value: TDateTime);
begin
  FDateSaved := Value;
end;

procedure TFlickrRepository.SetPhotos(value: TList<IPhoto>);
begin
  FPhotos := value;
end;

procedure TFlickrRepository.SetPreviousVersion(const Value: string);
begin
  FPreviousVersion := Value;
end;

procedure TFlickrRepository.SetSecret(value: string);
begin
  FSecret := value;
end;

procedure TFlickrRepository.Setsorted(const Value: boolean);
begin
  Fsorted := Value;
end;

procedure TFlickrRepository.SetUserId(value: string);
begin
  FUserId := value;
end;

procedure TFlickrRepository.Setversion(const Value: string);
begin
  Fversion := Value;
end;

end.
