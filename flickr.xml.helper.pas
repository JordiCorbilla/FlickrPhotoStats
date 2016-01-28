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

unit flickr.xml.helper;

interface

uses
  Variants;

type
  IXMLHelper = interface
    function getInt() : integer;
    function getBool() : boolean;
    function getString() : string;
    function getDate() : TDatetime;
    function getDateTime() : TDatetime;
  end;

  TXMLHelper = class(TInterfacedObject, IXMLHelper)
  private
    FAttribute : OleVariant;
  public
    function getInt() : integer;
    function getBool() : boolean;
    function getString() : string;
    function getDate() : TDatetime;
    function getDateTime() : TDatetime;
    constructor Create(attribute : Variant);
    class function New(attribute : Variant) : IXMLHelper;
  end;

implementation

uses
  StrUtils, Sysutils;

{ TXMLHelper }

constructor TXMLHelper.Create(attribute: Variant);
begin
  FAttribute := attribute;
end;

function TXMLHelper.getBool: boolean;
var
  value : boolean;
begin
  value := false;
  if FAttribute <> null then
  begin
    try
      value := StrToBool(FAttribute);
    except
      value := false;
    end;
  end;
  result := value;
end;

function TXMLHelper.getDate: TDatetime;
var
  value : TDateTime;
begin
  value := 0;
  if FAttribute <> null then
  begin
    try
      value := StrToDate(FAttribute);
    except
      value := 0;
    end;
  end;
  result := value;
end;

function TXMLHelper.getDateTime: TDatetime;
var
  value : TDateTime;
begin
  value := 0;
  if FAttribute <> null then
  begin
    try
      value := StrToDateTime(FAttribute);
    except
      value := 0;
    end;
  end;
  result := value;
end;

function TXMLHelper.getInt: integer;
var
  value : integer;
begin
  value := 0;
  if FAttribute <> null then
  begin
    try
      value := StrToInt(FAttribute);
    except
      value := 0;
    end;
  end;
  result := value;
end;

function TXMLHelper.getString: string;
var
  value : string;
begin
  if FAttribute <> null then
  begin
    try
      value := FAttribute;
    except
      value := '';
    end;
  end;
  result := value;
end;

class function TXMLHelper.New(attribute: Variant): IXMLHelper;
begin
  result := Create(attribute);
end;

end.
