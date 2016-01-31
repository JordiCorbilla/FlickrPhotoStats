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

unit frmThreadMonitor;

interface

uses
  Classes, System.SysUtils, System.Variants, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TfrmThreadMonitorPool = class(TForm)
    Test: TButton;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    ProgressBar4: TProgressBar;
    ProgressBar5: TProgressBar;
    ProgressBar6: TProgressBar;
    ProgressBar7: TProgressBar;
    ProgressBar8: TProgressBar;
    procedure TestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmThreadMonitorPool: TfrmThreadMonitorPool;

implementation

uses
  Contnrs, Generics.Collections, flickr.lib.parallel;

{$R *.dfm}

procedure TfrmThreadMonitorPool.TestClick(Sender: TObject);
var
  items : Tlist<integer>;
  i: Integer;
begin
  items := TList<integer>.create();
  for i := 0 to 7 do
    items.Add(i);

  TParallel.ForEach(0, items.count - 1,
      procedure(index: Integer; threadId: Integer)
      var
        j : integer;
        k: Integer;
        m : integer;
        l: Integer;
      begin
        //Do the task here
        for j := 0 to 1000 do
        begin
          for k := 0 to 1000 do
          begin
            for l := 0 to 1000 do
            begin
              m := j + k;
            end;
          end
        end;
      end,
      procedure (index : integer)
      begin
        //Synchronize when finished
        case index of
          0 : frmThreadMonitorPool.ProgressBar1.position := frmThreadMonitorPool.ProgressBar1.position + 1;
          1 : frmThreadMonitorPool.ProgressBar2.position := frmThreadMonitorPool.ProgressBar2.position + 1;
          2 : frmThreadMonitorPool.ProgressBar3.position := frmThreadMonitorPool.ProgressBar3.position + 1;
          3 : frmThreadMonitorPool.ProgressBar4.position := frmThreadMonitorPool.ProgressBar4.position + 1;
          4 : frmThreadMonitorPool.ProgressBar5.position := frmThreadMonitorPool.ProgressBar5.position + 1;
          5 : frmThreadMonitorPool.ProgressBar6.position := frmThreadMonitorPool.ProgressBar6.position + 1;
          6 : frmThreadMonitorPool.ProgressBar7.position := frmThreadMonitorPool.ProgressBar7.position + 1;
          7 : frmThreadMonitorPool.ProgressBar8.position := frmThreadMonitorPool.ProgressBar8.position + 1;
        end;

      end);
end;

end.
