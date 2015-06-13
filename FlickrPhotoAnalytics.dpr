program FlickrPhotoAnalytics;

uses
  Forms,
  frmFlickrStats in 'frmFlickrStats.pas' {frmFlickr},
  Vcl.Themes,
  Vcl.Styles,
  frmFlickrContextList in 'frmFlickrContextList.pas' {frmFlickrContext};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmFlickr, frmFlickr);
  Application.Run;
end.
