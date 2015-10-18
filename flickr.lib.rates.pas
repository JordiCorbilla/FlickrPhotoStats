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

unit flickr.lib.rates;

interface

type
  TRate = class(TObject)
    class procedure UpdateRate(valueBeforeYesterday, valueYesterday, valueToday: integer; out rateYesterday, rateToday: double); overload;
    class procedure UpdateRate(viewsYesterday, viewsToday, likesYesterday, likesToday : integer; out rateYesterday : double; out rateToday : double); overload;
  end;

implementation

{ TRate }

class procedure TRate.UpdateRate(valueBeforeYesterday, valueYesterday, valueToday: integer; out rateYesterday, rateToday: double);
begin
  rateYesterday := 0.0;
  if valueBeforeYesterday > 0 then
  begin
    if valueBeforeYesterday < valueYesterday then //green
      rateYesterday := (1 - (valueYesterday / valueBeforeYesterday)) * 100.0;
    if valueBeforeYesterday = valueYesterday then //red
      rateYesterday := 0.0;
    if valueBeforeYesterday > valueYesterday then //red
      rateYesterday := -(1 - (valueYesterday / valueBeforeYesterday)) * 100.0;
  end;
  rateToday := 0.0;
  if valueYesterday > 0 then
  begin
    if valueYesterday < valueToday then //green
      rateToday := (1 - (valueToday / valueYesterday)) * 100.0;
    if valueYesterday = valueToday then //red
      rateToday := 0.0;
    if valueYesterday > valueToday then //red
      rateToday := -(1 - (valueToday / valueYesterday)) * 100.0;
  end;
end;

class procedure TRate.UpdateRate(viewsYesterday, viewsToday, likesYesterday, likesToday: integer; out rateYesterday, rateToday: double);
begin
  rateYesterday := 0.0;
  if viewsYesterday > 0 then
  begin
    rateYesterday := (1-(likesYesterday / viewsYesterday))*100.0;
  end;
  rateToday := 0.0;
  if viewsToday > 0 then
  begin
    rateToday := (1-(likesToday / viewsToday))*100.0;
  end;
end;

end.
