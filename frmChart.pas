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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChartViewer: TfrmChartViewer;

implementation

{$R *.dfm}

end.
