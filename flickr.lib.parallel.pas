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

unit flickr.lib.parallel;

interface

uses
  Classes, SysUtils, System.SyncObjs;

type
  TParallelProc = reference to procedure(index: Integer; ThreadID: Integer);
  TSyncProc = reference to procedure();

  TParallel = class(TThread)
  private
    FProc: TParallelProc;
    FSync : TSyncProc;
    FThreadID: Integer;
    procedure DoVisualChange();
  protected
    procedure Execute; override;
    function GetNextValue: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Proc: TParallelProc read FProc write FProc;
    property Sync: TSyncProc read FSync write FSync;
    class var
      FCriticalSection: TCriticalSection;
      FThreadCount: Integer;
      FCurrentIndex: Integer;
      FMaxIndex: Integer;
    class procedure ForEach(aMin, aMax, aThreads: Integer; aProc: TParallelProc; aSync: TSyncProc); overload;
    class procedure ForEach(aMin, aMax: Integer; aProc: TParallelProc; aSync: TSyncProc); overload;
  end;

  TParallelSync = class(TThread)

  end;

implementation

uses
  Windows;

{ TParallel }

constructor TParallel.Create;
begin
  inherited Create(True);
  InterlockedIncrement(FThreadCount);
  FreeOnTerminate := False;
  FThreadID := 0;
end;

destructor TParallel.Destroy;
begin
  InterlockedDecrement(FThreadCount);
  inherited;
end;

procedure TParallel.DoVisualChange;
begin
  FSync();
end;

procedure TParallel.Execute;
var
  nCurrent: Integer;
begin
  nCurrent := GetNextValue;
  while nCurrent <= FMaxIndex do
  begin
    Proc(nCurrent, FThreadID);
    Synchronize(DoVisualChange);
    nCurrent := GetNextValue;
  end;
end;

class procedure TParallel.ForEach(aMin, aMax, aThreads: Integer; aProc: TParallelProc; aSync: TSyncProc);
var
  threads: array of TParallel;
  index: Integer;
begin
  if aMin > aMax then
    Exit;
  // initialize TParallel class data
  TParallel.FCurrentIndex := aMin;
  TParallel.FMaxIndex := aMax;
  TParallel.FCriticalSection := TCriticalSection.Create;
  TParallel.FThreadCount := 0;

  // create the threads
  SetLength(threads, aThreads);
  for index := 0 to aThreads - 1 do
  begin
    threads[index] := TParallel.Create;
    threads[index].FThreadID := index;
    threads[index].Proc := aProc;
    threads[index].FSync := aSync;
    threads[index].Start;
  end;

  for index := 0 to aThreads - 1 do
  begin
    threads[index].WaitFor;
  end;

  for index := 0 to aThreads - 1 do
  begin
    threads[index].Free;
  end;

  TParallel.FCriticalSection.Free;
end;

class procedure TParallel.ForEach(aMin, aMax: Integer; aProc: TParallelProc; aSync: TSyncProc);
var
  CPUMaxOut : integer;
begin
  CPUMaxOut := CPUCount * 8;
  //{$IF CONSOLE}
  //Writeln('Number of threads: ' + CPUMaxOut.ToString());
  ForEach(aMin, aMax, CPUMaxOut, aProc, aSync);
end;

function TParallel.GetNextValue: Integer;
begin
  FCriticalSection.Acquire;
  try
    Result := FCurrentIndex;
    Inc(FCurrentIndex);
  finally
    FCriticalSection.Release;
  end;
end;

end.
