// Copyright (c) 2015, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

program FlickrPhotoAnalyticsBatchUpdate;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  flickr.repository,
  System.Diagnostics,
  flickr.lib.parallel,
  flickr.globals,
  flickr.stats,
  flickr.repository.rest in 'flickr.repository.rest.pas';

var
  repository: IFlickrRepository;
  globalsRepository: IFlickrGlobals;
  st : TStopWatch;
  apikey : string;
  secret : string;
  userId : string;
  i: Integer;
  totalViews, totalViewsacc: Integer;
  totalLikes, totalLikesacc: Integer;
  totalComments, totalCommentsacc: Integer;
  stat: IStat;
begin
  try
    WriteLn('###################################################');
    WriteLn('# Welcome to Flickr Photo Analytics Batch Update  #');
    WriteLn('# version 4.2 @author: Jordi Corbilla             #');
    WriteLn('###################################################');
    if ParamStr(1) = '' then
    begin
      WriteLn('Apikey must be entered as a parameter.');
      exit;
    end;
    if ParamStr(2) = '' then
    begin
      WriteLn('Secret must be entered as a parameter.');
      exit;
    end;
    if ParamStr(3) = '' then
    begin
      WriteLn('UserId must be entered as a parameter.');
      exit;
    end;
    apikey := ParamStr(1);
    secret := ParamStr(2);
    userid := ParamStr(3);
    sleep(2000);
    //Load repository
    WriteLn('Loading Repository with ' + apikey + ' ' + secret + ' ' + userid);
    repository := TFlickrRepository.Create();
    try
      st := TStopWatch.Create;
      st.Start;
      repository.load('flickrRepository.xml');
      st.Stop;
      WriteLn('Loaded repository flickrRepository: ' + st.ElapsedMilliseconds.ToString() + 'ms');

      //Use parallel looping
      st := TStopWatch.Create;
      st.Start;

      TParallel.ForEach(0, repository.photos.count - 1,
        procedure(index: Integer; threadId: Integer)
        begin
          TRepositoryRest.updatePhoto(repository, apikey, repository.photos[index].id);
        end);

      st.Stop;
      WriteLn('Update repository: ' + st.ElapsedMilliseconds.ToString() + 'ms');

      st := TStopWatch.Create;
      st.Start;
      repository.save(apikey, secret, userid, 'flickrRepository.xml');
      st.Stop;
      WriteLn('Saving repository: ' + st.ElapsedMilliseconds.ToString() + 'ms');
    finally

    end;

    globalsRepository := TFlickrGlobals.Create();
    try
      st := TStopWatch.Create;
      st.Start;
      globalsRepository.load('flickrRepositoryGlobal.xml');
      st.Stop;
      WriteLn('Loaded repository flickrRepositoryGlobal: ' + st.ElapsedMilliseconds.ToString() + 'ms');

      totalViewsacc := 0;
      totalLikesacc := 0;
      totalCommentsacc := 0;
      st := TStopWatch.Create;
      st.Start;
      for i := 0 to repository.photos.Count - 1 do
      begin
        totalViews := repository.photos[i].getTotalViews();
        totalViewsacc := totalViewsacc + totalViews;

        totalLikes := repository.photos[i].getTotalLikes();
        totalLikesacc := totalLikesacc + totalLikes;

        totalComments := repository.photos[i].getTotalComments();
        totalCommentsacc := totalCommentsacc + totalComments;
      end;

      totalViewsacc := totalViewsacc + TRepositoryRest.getTotalAlbumsCounts(apikey, userid);

      stat := TStat.Create(Date, totalViewsacc, totalLikesacc, totalCommentsacc);
      globalsRepository.AddGlobals(stat);
      st.Stop;
      WriteLn('Update flickrRepositoryGlobal: ' + st.ElapsedMilliseconds.ToString() + 'ms');
      st := TStopWatch.Create;
      st.Start;
      globalsRepository.save('flickrRepositoryGlobal.xml');
      st.Stop;
      WriteLn('Save flickrRepositoryGlobal: ' + st.ElapsedMilliseconds.ToString() + 'ms');
    finally
      repository := nil;
      globalsRepository := nil;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
