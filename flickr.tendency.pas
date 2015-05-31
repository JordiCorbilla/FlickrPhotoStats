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

unit flickr.tendency;

interface

uses
  Contnrs, Generics.Collections, flickr.lib.integerlist;

type
  ITendency = interface
    procedure AddX(item : integer);
    procedure AddY(item : integer);
    procedure AddXY(itemX, itemY : integer);
    procedure Calculate();
    function tendencyResult( a , b : integer) : integer;
  end;

  TTendency = Class(TInterfacedObject, ITendency)
  private
    Fa : integer;
    Fb : integer;
    Fc : integer;
    Fd : integer;
    Fe : integer;
    Ff : Double;
    Fm : Double;
    Fyb : double;
    x : TIntegerList;
    y : TIntegerList;
    function slope() : double;
    function interception() : double;
  public
    procedure AddX(item : integer);
    procedure AddY(item : integer);
    procedure AddXY(itemX, itemY : integer);
    procedure Calculate();
    function tendencyResult( a , b : integer) : integer;
    constructor Create();
    Destructor Destroy(); override;
  End;

implementation

uses
  Sysutils;

{ TTendency }

procedure TTendency.AddX(item: integer);
begin
  x.Add(item);
end;

procedure TTendency.AddXY(itemX, itemY: integer);
begin
  AddX(itemX);
  AddY(itemY);
end;

procedure TTendency.AddY(item: integer);
begin
  y.Add(item);
end;

procedure TTendency.Calculate;
begin
  slope();
  interception();
end;

constructor TTendency.Create;
begin
    x := TIntegerList.create();
    y := TIntegerList.create();
end;

destructor TTendency.Destroy;
begin
  FreeAndNil(x);
  FreeAndNil(y);
  inherited;
end;

function TTendency.interception: double;
begin
  Fe := y.Total();
  ff := Fm * x.Total().ToDouble;
  Fyb := (Fe.ToDouble - Ff) / x.Count.ToDouble;
  result := Fyb;
end;

function TTendency.slope: double;
var
  i: Integer;
  acc : integer;
begin
  acc := 0;
  for i := 0 to x.Count-1 do
  begin
    acc := acc + (x[i] * y[i]);
  end;

  Fa := x.Count * acc;
  Fb := x.Total() * y.Total();
  Fc := x.count * x.Squared();
  Fd := x.TotalSquared();

  Fm := (Fa - Fb) / (Fc - Fd);
  result := Fm;
end;

function TTendency.tendencyResult(a, b: integer): integer;
begin
  result := round((a.ToDouble * Fm) + Fyb);
end;

end.
