program FlickrPhotoStats;

uses
  Forms,
  frmFlickrStats in 'frmFlickrStats.pas' {frmFlickr},
  flickr.repository in 'flickr.repository.pas',
  flickr.photos in 'flickr.photos.pas',
  flickr.stats in 'flickr.stats.pas',
  flickr.rest in 'flickr.rest.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmFlickr, frmFlickr);
  Application.Run;
end.
