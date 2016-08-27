program FlickrPhotoAnalyticsMetropolisUI;

uses
  FMX.Forms,
  frmFlickrMainTilesGrid in 'frmFlickrMainTilesGrid.pas' {frmMainGridTile},
  DetailView1 in 'DetailView1.pas' {DetailViewForm},
  frmDashboardTileUI in 'frmDashboardTileUI.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainGridTile, frmMainGridTile);
  Application.CreateForm(TDetailViewForm, DetailViewForm);
  Application.CreateForm(TForm1, Form1);
  Application.RegisterFormFamily('Main', [TfrmMainGridTile]);
  Application.RegisterFormFamily('DetailView', [TDetailViewForm]);
  Application.Run;
end.
