program FlickrPhotoAnalyticsBatchUpdate;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  flickr.repository,
  System.Diagnostics,
  flickr.lib.parallel,
  flickr.repository.rest in 'flickr.repository.rest.pas';

var
  repository: IFlickrRepository;
  st : TStopWatch;
begin
  try
    WriteLn('###################################################');
    WriteLn('# Welcome to Flickr Photo Analytics Batch Update  #');
    WriteLn('# version 4.2 @author: Jordi Corbilla             #');
    WriteLn('###################################################');
    sleep(2000);
    //Load repository
    WriteLn('Loading Repository');
    repository := TFlickrRepository.Create();
    try
      st := TStopWatch.Create;
      st.Start;
      repository.load('flickrRepository.xml');
      st.Stop;
      WriteLn('Loaded repository flickrRepository: ' + st.ElapsedMilliseconds.ToString() + 'ms');

      //Use parallel looping
      TParallel.ForEach(0, repository.photos.count - 1,
        procedure(index: Integer; threadId: Integer)
        begin
          RequestInformation_REST_Flickr(listPhotos.Items[index].Caption);
        end);

    finally

    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
