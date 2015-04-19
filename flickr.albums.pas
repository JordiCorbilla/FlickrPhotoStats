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

unit flickr.albums;

interface

uses
  XMLDoc, xmldom, XMLIntf;

type
  IAlbum = interface
    function GetId: string;
    function GetTitle: string;
    procedure SetId(const Value: string);
    procedure SetTitle(const Value: string);
    property Id: string read GetId write SetId;
    property Title: string read GetTitle write SetTitle;
  end;

  TAlbum = class(TInterfacedObject, IAlbum)
  private
    FId : string;
    FTitle : string;
    function GetId: string;
    function GetTitle: string;
    procedure SetId(const Value: string);
    procedure SetTitle(const Value: string);
  public
    property Id: string read GetId write SetId;
    property Title: string read GetTitle write SetTitle;
    constructor Create (id, title : string);
  end;

implementation

{ TPool }

constructor TAlbum.Create(id, title: string);
begin
  SetId(id);
  SetTitle(title);
end;

function TAlbum.GetId: string;
begin
  result := FId;
end;

function TAlbum.GetTitle: string;
begin
  result := FTitle;
end;

procedure TAlbum.SetId(const Value: string);
begin
  FId := value;
end;

procedure TAlbum.SetTitle(const Value: string);
begin
  FTitle := value;
end;

end.
