program FlickrPhotoAnalyticsUI;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  GroupedItems1 in 'GroupedItems1.pas' {SplitForm},
  SplitItemDetail1 in 'SplitItemDetail1.pas' {DetailForm};

{$R *.res}

begin
  Application.Initialize;
  Application.UseMetropolisUI;
  TStyleManager.TrySetStyle('Metropolis UI Dark');
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Metropolis UI Application';
  Application.CreateForm(TSplitForm, SplitForm);
  Application.CreateForm(TDetailForm, DetailForm);
  Application.Run;
end.
