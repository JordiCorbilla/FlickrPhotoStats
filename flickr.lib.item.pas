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

unit flickr.lib.item;

interface

type
  IItem = interface
    function GetDate() : TDateTime;
    function GetCount() : integer;
    procedure Setcount(const Value: integer);
    procedure Setdate(const Value: TDateTime);
    property date : TDateTime read Getdate write Setdate;
    property count : integer read Getcount write Setcount;
  end;

  TITem = class(TInterfacedObject, IItem)
  private
    Fdate: TDateTime;
    Fcount: integer;
    function GetDate() : TDateTime;
    function GetCount() : integer;
    procedure Setcount(const Value: integer);
    procedure Setdate(const Value: TDateTime);
  public
    property date : TDateTime read Getdate write Setdate;
    property count : integer read Getcount write Setcount;
    Constructor Create();
  end;

implementation

{ TITem }

constructor TITem.Create;
begin
  SetDate(Date);
  SetCount(0);
end;

function TITem.GetCount: integer;
begin
  result := FCount;
end;

function TITem.GetDate: TDateTime;
begin
  result := FDate;
end;

procedure TITem.Setcount(const Value: integer);
begin
  Fcount := Value;
end;

procedure TITem.Setdate(const Value: TDateTime);
begin
  Fdate := Value;
end;

end.
