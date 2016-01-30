program FlickrPhotoThreadMonitor;

uses
  Vcl.Forms,
  frmThreadMonitor in 'frmThreadMonitor.pas' {frmThreadMonitorPool},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmThreadMonitorPool, frmThreadMonitorPool);
  Application.Run;
end.
