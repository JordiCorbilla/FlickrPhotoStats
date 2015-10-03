program FlickrPhotoAnalyticsUI;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  frmFlickrMainTiles in 'frmFlickrMainTiles.pas' {frmMainTiles},
  SplitItemDetail1 in 'SplitItemDetail1.pas' {DetailForm};

{$R *.res}

begin
  Application.Initialize;
  Application.UseMetropolisUI;
  TStyleManager.TrySetStyle('Metropolis UI Dark');
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Metropolis UI Application';
  Application.CreateForm(TfrmMainTiles, frmMainTiles);
  Application.CreateForm(TDetailForm, DetailForm);
  Application.Run;
end.
