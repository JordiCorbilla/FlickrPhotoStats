program FlickrPhotoAnalyticsMetropolisUI;

uses
  FMX.Forms,
  GridView1 in 'GridView1.pas' {GridViewForm},
  DetailView1 in 'DetailView1.pas' {DetailViewForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGridViewForm, GridViewForm);
  Application.CreateForm(TDetailViewForm, DetailViewForm);
  Application.RegisterFormFamily('Main', [TGridViewForm]);
  Application.RegisterFormFamily('DetailView', [TDetailViewForm]);
  Application.Run;
end.
