// Copyright (c) 2014, Jordi Corbilla
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
  Contnrs, Generics.Collections, flickr.photos;

type
  IFlickrRepository = interface
    procedure SetApiKey(value: string);
    function GetApiKey(): string;
    procedure SetAuth_token(value: string);
    function GetAuth_token(): string;
    procedure SetApi_sig(value: string);
    function GetApi_sig(): string;
    procedure SetUserId(value: string);
    function GetUserId(): string;
    procedure AddPhoto(photo: IPhoto);
    function GetPhotos(): TList<IPhoto>;
    procedure SetPhotos(value: TList<IPhoto>);
    function GetPhoto(id: string): IPhoto;
    procedure SetSecret(value: string);
    function GetSecret(): string;
    procedure Save(ApiKey: string; Secret: string; UserId: string; auth_token: string; api_sig : string; FileName: string);
    procedure Load(FileName: string);
    function ExistPhoto(photo: IPhoto; var existing: IPhoto): Boolean;
    property ApiKey: string read GetApiKey write SetApiKey;
    property Auth_token: string read GetAuth_token write SetAuth_token;
    property Api_sig: string read GetApi_sig write SetApi_sig;
    property UserId: string read GetUserId write SetUserId;
    property photos: TList<IPhoto>read GetPhotos write SetPhotos;
    property Secret: string read GetSecret write SetSecret;
  end;

  TFlickrRepository = class(TInterfacedObject, IFlickrRepository)
  private
    FApiKey: String;
    FUserId : String;
    FAuth_token : String;
    FApi_sig : String;
    FPhotos: TList<IPhoto>;
    FSecret: String;
    procedure SetApiKey(value: string);
    function GetApiKey(): string;
    function GetPhotos(): TList<IPhoto>;
    procedure SetPhotos(value: TList<IPhoto>);
    procedure SetUserId(value: string);
    function GetUserId(): string;
    procedure SetAuth_token(value: string);
    function GetAuth_token(): string;
    procedure SetApi_sig(value: string);
    function GetApi_sig(): string;
    procedure SetSecret(value: string);
    function GetSecret(): string;
  public
    procedure AddPhoto(photo: IPhoto);
    procedure Load(FileName: string);
    function ExistPhoto(photo: IPhoto; var existing: IPhoto): Boolean;
    constructor Create();
    function GetPhoto(id: string): IPhoto;
    destructor Destroy(); override;
    procedure Save(ApiKey: string; Secret: string; UserId: string; auth_token: string; api_sig : string; FileName: string);
    property ApiKey: string read GetApiKey write SetApiKey;
    property UserId: string read GetUserId write SetUserId;
    property Auth_token: string read GetAuth_token write SetAuth_token;
    property Api_sig: string read GetApi_sig write SetApi_sig;
    property photos: TList<IPhoto>read GetPhotos write SetPhotos;
  end;

implementation

{ TFlickrRepository }

uses
  XMLDoc, xmldom, XMLIntf, SysUtils, Dialogs;

procedure TFlickrRepository.AddPhoto(photo: IPhoto);
begin
  FPhotos.Add(photo);
end;

constructor TFlickrRepository.Create;
begin
  FPhotos := TList<IPhoto>.Create;
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

function TFlickrRepository.GetApiKey: string;
begin
  Result := FApiKey;
end;

function TFlickrRepository.GetApi_sig: string;
begin
  Result := FApi_sig;
end;

function TFlickrRepository.GetAuth_token: string;
begin
  Result := FAuth_token;
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

function TFlickrRepository.GetSecret: string;
begin
  Result := FSecret;
end;

function TFlickrRepository.GetUserId: string;
begin
  Result := FUserId;
end;

procedure TFlickrRepository.Load(FileName: string);
var
  Document: IXMLDocument;
  iXMLRootNode, iNode: IXMLNode;
  photo: IPhoto;
begin
  if fileExists(ExtractFilePath(ParamStr(0)) + FileName) then
  begin
  Document := TXMLDocument.Create(nil);
  try
    Document.LoadFromFile(ExtractFilePath(ParamStr(0)) + FileName);
    iXMLRootNode := Document.ChildNodes.first;
    Self.FApiKey := iXMLRootNode.attributes['ApiKey'];

    try
      Self.FUserId := iXMLRootNode.attributes['UserId'];
    except
      Self.FUserId := '';
    end;

    try
      Self.FAuth_token := iXMLRootNode.attributes['Auth_token'];
    except
      Self.FAuth_token := '';
    end;

    try
      Self.FApi_sig := iXMLRootNode.attributes['Api_sig'];
    except
      Self.FApi_sig := '';
    end;

    try
      Self.FSecret := iXMLRootNode.attributes['Secret'];
    except
      Self.FSecret := '';
    end;

    iNode := iXMLRootNode.ChildNodes.first;
    while iNode <> nil do
    begin
      photo := TPhoto.Create();
      photo.Load(iNode);
      FPhotos.Add(photo);
      iNode := iNode.NextSibling;
    end;
  finally
    Document := nil;
  end;
  end
  else
    ShowMessage('File does not exists in location: ' + ExtractFilePath(ParamStr(0)) + FileName);
end;

procedure TFlickrRepository.Save(ApiKey: string; Secret: string; UserId: string; auth_token: string; api_sig : string; FileName: string);
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
  iNode.attributes['auth_token'] := auth_token;
  iNode.attributes['api_sig'] := api_sig;
  iNode.attributes['Secret'] := Secret;

  for i := 0 to FPhotos.count - 1 do
  begin
    FPhotos[i].Save(iNode);
  end;
  XMLDoc.SaveToFile(ExtractFilePath(ParamStr(0)) + FileName);
end;

procedure TFlickrRepository.SetApiKey(value: string);
begin
  FApiKey := value;
end;

procedure TFlickrRepository.SetApi_sig(value: string);
begin
  FApi_sig := value;
end;

procedure TFlickrRepository.SetAuth_token(value: string);
begin
  FAuth_token := value;
end;

procedure TFlickrRepository.SetPhotos(value: TList<IPhoto>);
begin
  FPhotos := value;
end;

procedure TFlickrRepository.SetSecret(value: string);
begin
  FSecret := value;
end;

procedure TFlickrRepository.SetUserId(value: string);
begin
  FUserId := value;
end;

end.
