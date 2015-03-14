program FlickrPhotoAnalytics;

uses
  Forms,
  frmFlickrStats in 'frmFlickrStats.pas' {frmFlickr},
  flickr.repository in 'flickr.repository.pas',
  flickr.photos in 'flickr.photos.pas',
  flickr.stats in 'flickr.stats.pas',
  flickr.rest in 'flickr.rest.pas',
  Vcl.Themes,
  Vcl.Styles,
  flickr.globals in 'flickr.globals.pas',
  flickr.top.stats in 'flickr.top.stats.pas',
  flickr.oauth in 'flickr.oauth.pas',
  flickr.signature in 'flickr.signature.pas',
  flickr.access.token in 'flickr.access.token.pas',
  flickr.lib.parallel in 'flickr.lib.parallel.pas',
  flickr.call.methods in 'flickr.call.methods.pas',
  flickr.profiles in 'flickr.profiles.pas',
  flickr.profile in 'flickr.profile.pas',
  flickr.rejected in 'flickr.rejected.pas',
  flickr.filtered.list in 'flickr.filtered.list.pas',
  flickr.base in 'flickr.base.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TfrmFlickr, frmFlickr);
  Application.Run;
end.
