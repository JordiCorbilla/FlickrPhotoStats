// Copyright (c) 2015-2016, Jordi Corbilla
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

unit frmChart;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart;

type
  TfrmChartViewer = class(TForm)
    ChartViewer: TChart;
    LineSeries4: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    procedure CloneChart(chartSender : TChart);
  end;

var
  frmChartViewer: TfrmChartViewer;

implementation

{$R *.dfm}

uses
  flickr.charts;

{ TfrmChartViewer }

procedure TfrmChartViewer.CloneChart(chartSender : TChart);
var
  flickrChart : IFlickrChart;
  i,j : integer;
  Series : TChartSeries;
begin
  chartViewer.Title := chartSender.Title;
  for i := 0 to chartSender.SeriesList.Count-1 do
  begin
    flickrChart := TFlickrChart.create;
    Series := flickrChart.Get(chartSender.SeriesList[i].ClassName, ChartViewer, false, chartSender.SeriesList[i].SeriesColor);
    for j := 0 to chartSender.SeriesList[i].XValues.count -1 do
    begin
      Series.AddXY(chartSender.SeriesList[i].XValue[j], chartSender.SeriesList[i].YValue[j], '', chartSender.SeriesList[i].ValueColor[j]);
    end;
    ChartViewer.AddSeries(Series);
  end;
end;

procedure TfrmChartViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmChartViewer.FormCreate(Sender: TObject);
begin
  ChartViewer.SeriesList.Clear;
end;

procedure TfrmChartViewer.FormDestroy(Sender: TObject);
begin
  ChartViewer.SeriesList.Clear;
  ChartViewer.FreeAllSeries();
  ChartViewer.Free;
end;

end.
