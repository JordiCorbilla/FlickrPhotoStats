program FlickrPhotoMigrationTool;

uses
  Vcl.Forms,
  frmMigrationMain in 'frmMigrationMain.pas' {frmMigration},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmMigration, frmMigration);
  Application.Run;
end.
