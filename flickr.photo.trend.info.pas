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

unit flickr.photo.trend.info;

interface

uses
  Generics.Collections, Generics.defaults;

type
  IPhotoTrend = interface
    function getId(): string;
    function getTitle(): string;
    function getValue(): integer;
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    procedure SetValue(value: integer);
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property Value: integer read getValue write SetValue;
  end;

  TPhotoTrend = class(TInterfacedObject, IPhotoTrend)
  private
    FId : string;
    FTitle : string;
    FValue : integer;
    function getId(): string;
    function getTitle(): string;
    function getValue(): integer;
    procedure SetId(value: string);
    procedure SetTitle(value: string);
    procedure SetValue(value: integer);
  public
    property Id: string read getId write SetId;
    property Title: string read getTitle write SetTitle;
    property Value: integer read getValue write SetValue;
  end;

  TIPhotoTrendComparer = class(TComparer<IPhotoTrend>)
  public
    function Compare(const Left, Right: IPhotoTrend): Integer; override;
  end;

implementation

{ TPhotoTrend }

function TPhotoTrend.getId: string;
begin
  result := FId;
end;

function TPhotoTrend.getTitle: string;
begin
  result := FTitle;
end;

function TPhotoTrend.getValue: integer;
begin
  result := FValue;
end;

procedure TPhotoTrend.SetId(value: string);
begin
  FId := value;
end;

procedure TPhotoTrend.SetTitle(value: string);
begin
  FTitle := value;
end;

procedure TPhotoTrend.SetValue(value: integer);
begin
  FValue := value;
end;

{ TIPhotoTrendComparerViews }

function TIPhotoTrendComparer.Compare(const Left, Right: IPhotoTrend): Integer;
var
  LeftTerm, RightTerm: Integer;
begin
  LeftTerm := Left.Value;
  RightTerm := Right.Value;
  Result := RightTerm - LeftTerm;
end;

end.
