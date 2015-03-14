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

unit flickr.rejected;

interface

uses
  generics.collections;

type
  IRejected = interface
    procedure Add(id : string);
    function Exists(id : string) : boolean;
  end;

  TRejected = class(TInterfacedObject, IRejected)
  private
    FList : TList<string>;
  public
    procedure Add(id : string);
    function Exists(id : string) : boolean;
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

{ TRejected }

procedure TRejected.Add(id: string);
begin
  FList.Add(id);
end;

constructor TRejected.Create;
begin
  FList := TList<string>.create;
end;

destructor TRejected.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRejected.Exists(id: string): boolean;
var
  i: Integer;
  found: boolean;
begin
  i := 0;
  found := false;
  while (not found) and (i < FList.count) do
  begin
    found := FList[i] = id;
    inc(i);
  end;
  result := found;
end;

end.
