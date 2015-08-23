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

unit flickr.charts;

interface

uses
  VclTee.Series, VCLTee.Chart, graphics, VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.TeeProcs;

type
  IFlickrChart = interface
    function GetNewLineSeries(parent : TChart; marks : boolean = false) : TLineSeries;
    function GetNewBarSeries(parent : TChart; marks : boolean = false) : TBarSeries;
    function GetNewPieSeries(parent : TChart; marks : boolean = false) : TPieSeries;
    function GetNewAreaSeries(parent : TChart; marks : boolean = false) : TAreaSeries;
    procedure VisibleMarks(mainChart : TChart; option : boolean);
    function Get(series : string; parent : TChart; marks : boolean = false) : TChartSeries;
  end;

  TFlickrChart = Class(TInterfacedObject, IFlickrChart)
    function GetNewLineSeries(parent : TChart; marks : boolean = false) : TLineSeries;
    function GetNewBarSeries(parent : TChart; marks : boolean = false) : TBarSeries;
    function GetNewPieSeries(parent : TChart; marks : boolean = false) : TPieSeries;
    function GetNewAreaSeries(parent : TChart; marks : boolean = false) : TAreaSeries;
    procedure VisibleMarks(mainChart : TChart; option : boolean);
    function Get(series : string; parent : TChart; marks : boolean = false) : TChartSeries;
  End;

implementation

{ TFlickrChart }

function TFlickrChart.Get(series: string; parent : TChart; marks : boolean = false): TChartSeries;
begin
  result := nil;
  if series = 'TLineSeries' then
    result := GetNewLineSeries(parent, marks);
  if series = 'TBarSeries' then
    result := GetNewBarSeries(parent, marks);
  if series = 'TPieSeries' then
    result := GetNewPieSeries(parent, marks);
  if series = 'TAreaSeries' then
    result := GetNewAreaSeries(parent, marks);
end;

function TFlickrChart.GetNewAreaSeries(parent: TChart;
  marks: boolean): TAreaSeries;
var
  Series : TAreaSeries;
begin
  Series := TAreaSeries.Create(parent);
  Series.Marks.Arrow.Visible := true;
  Series.Marks.Callout.Brush.color := clBlack;
  Series.Marks.Callout.Arrow.Visible := true;
  Series.Marks.DrawEvery := 10;
  Series.DrawArea := true;
  //Series.DrawStyle := dsCurve;
  Series.Marks.Shadow.color := 8487297;
  Series.Marks.Visible := marks;
  Series.SeriesColor := 10708548;
  Series.LinePen.Width := 1;
  Series.LinePen.color := 10708548;
  Series.Pointer.InflateMargins := true;
  Series.Pointer.Style := psRectangle;
  Series.Pointer.Brush.Gradient.EndColor := 10708548;
  Series.Pointer.Gradient.EndColor := 10708548;
  Series.Pointer.InflateMargins := true;
  Series.Pointer.Visible := false;
  Series.XValues.DateTime := true;
  Series.XValues.Name := 'X';
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Y';
  Series.YValues.Order := loNone;
  Series.ParentChart := parent;
  Series.Transparency := 32;
  result := Series;
end;

function TFlickrChart.GetNewBarSeries(parent: TChart; marks : boolean = false): TBarSeries;
var
  Series : TBarSeries;
begin
  Series := TBarSeries.Create(parent);
//    series.MultiBar := mbStacked100;
  Series.Marks.Arrow.Visible := true;
  Series.Marks.Callout.Brush.color := clBlack;
  Series.Marks.Callout.Arrow.Visible := true;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Visible := marks;
  Series.Marks.Shadow.color := 8487297;
  Series.SeriesColor := 10708548;
  Series.XValues.DateTime := true;
  Series.XValues.Name := 'X';
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Y';
  Series.YValues.Order := loNone;
  Series.ParentChart := parent;
  Series.Transparency := 32;
  result := Series;
end;

function TFlickrChart.GetNewLineSeries(parent : TChart; marks : boolean = false): TLineSeries;
var
  Series : TLineSeries;
begin
  Series := TLineSeries.Create(parent);
  Series.Marks.Arrow.Visible := true;
  Series.Marks.Callout.Brush.color := clBlack;
  Series.Marks.Callout.Arrow.Visible := true;
  Series.Marks.DrawEvery := 10;
  Series.Marks.Shadow.color := 8487297;
  Series.Marks.Visible := marks;
  Series.SeriesColor := 10708548;
  Series.LinePen.Width := 2;
  Series.LinePen.color := 10708548;
  Series.Pointer.InflateMargins := true;
  Series.Pointer.Style := psRectangle;
  Series.Pointer.Brush.Gradient.EndColor := 10708548;
  Series.Pointer.Gradient.EndColor := 10708548;
  Series.Pointer.InflateMargins := true;
  Series.Pointer.Visible := false;
  Series.XValues.DateTime := true;
  Series.XValues.Name := 'X';
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Y';
  Series.YValues.Order := loNone;
  Series.Transparency := 32;
  Series.ParentChart := parent;
  result := Series;
end;

function TFlickrChart.GetNewPieSeries(parent: TChart; marks: boolean): TPieSeries;
var
  Series : TPieSeries;
begin
  Series := TPieSeries.Create(parent);
  Series.Marks.Visible := false;
  Series.XValues.Order := loAscending;
  Series.YValues.Name := 'Pie';
  Series.YValues.Order := loNone;
  Series.Frame.InnerBrush.BackColor := clRed;
  Series.Frame.InnerBrush.Gradient.EndColor := clGray;
  Series.Frame.InnerBrush.Gradient.MidColor := clWhite;
  Series.Frame.InnerBrush.Gradient.StartColor := 4210752;
  Series.Frame.InnerBrush.Gradient.Visible := True;
  Series.Frame.MiddleBrush.BackColor := clYellow;
  Series.Frame.MiddleBrush.Gradient.EndColor := 8553090;
  Series.Frame.MiddleBrush.Gradient.MidColor := clWhite;
  Series.Frame.MiddleBrush.Gradient.StartColor := clGray;
  Series.Frame.MiddleBrush.Gradient.Visible := True;
  Series.Frame.OuterBrush.BackColor := clGreen;
  Series.Frame.OuterBrush.Gradient.EndColor := 4210752;
  Series.Frame.OuterBrush.Gradient.MidColor := clWhite;
  Series.Frame.OuterBrush.Gradient.StartColor := clSilver;
  Series.Frame.OuterBrush.Gradient.Visible := True;
  Series.Frame.Width := 4;
  Series.Transparency := 32;
  Series.OtherSlice.Legend.Visible := False;
  result := Series;
end;

procedure TFlickrChart.VisibleMarks(mainChart: TChart; option: boolean);
var
  i: Integer;
begin
  for i := 0 to mainChart.SeriesList.Count-1 do
    mainChart.seriesList[i].Marks.Visible := option;
end;

end.
