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
  flickr.time,
  flickr.lib.logging,
  flickr.organic,
  flickr.organic.stats,
  flickr.lib.options,
  flickr.lib.email,
  flickr.lib.email.html,
  flickr.lib.utils,
  System.Classes,
  flickr.repository.rest in 'flickr.repository.rest.pas', Winapi.Windows;

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
  verbosity, loadrepository, loadglobals : boolean;
  organic : IFlickrOrganic;
  organicStat : IFlickrOrganicStats;
  options : IOptions;
  description : TStrings;
  totalContacts : integer;
begin
  try
    TLogger.LogFile('Starting Batch Update');
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
    WriteLn('###################################################');
    WriteLn('# Welcome to Flickr Photo Analytics Batch Update  #');
    WriteLn('# version '+TUtils.GetVersion+' @author: Jordi Corbilla         #');
    WriteLn('###################################################');
    verbosity := false;
    if paramstr(1) = '-v' then
      verbosity := true;

    loadrepository := false;
    if paramstr(2) = '-r' then
      loadrepository := true;

    loadglobals := false;
    if paramstr(3) = '-g' then
      loadglobals := true;

    //Load repository
    WriteLn('Loading Repository');
    repository := TFlickrRepository.Create();
    try
      st := TStopWatch.Create;
      st.Start;
      repository.load('flickrRepository.xml');
      st.Stop;
      WriteLn('Loaded repository flickrRepository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      TLogger.LogFile('Loaded repository flickrRepository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      if loadrepository then
      begin
        //Use parallel looping
        st := TStopWatch.Create;
        st.Start;
        apikey := repository.ApiKey;
        secret := repository.Secret;
        userId := repository.UserId;

        //Organic Growth checks
        organic := TFlickrOrganic.Create;
        try
          st := TStopWatch.Create;
          st.Start;
          organic.Load('flickrOrganic.xml');

          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
          WriteLn('Loaded organic repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
          TLogger.LogFile('Loaded organic repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
          st.Stop;

          organicStat := TFlickrOrganicStats.create();
          st := TStopWatch.Create;
          try
            st.Start;
            TParallel.ForEach(0, repository.photos.count - 1,
              procedure(index: Integer; threadId: Integer)
              begin
                TRepositoryRest.updatePhoto(repository, organicstat, apikey, repository.photos[index].id, verbosity);
              end);
            st.Stop;
          finally
            organicStat.executionTime := st.ElapsedMilliseconds;
            organicStat.date := Date;
            try
              try
                totalContacts := TRepositoryRest.getNumberOfContacts;
              except on E: Exception do
                TLogger.LogFile('Exception reading contacts: ' + e.message);
              end;
              if totalContacts < 0 then
                totalContacts := organic.Globals[organic.Globals.Count-1].following;
            finally
              organicStat.Following := totalContacts;
            end;
            organic.AddGlobals(organicStat);
          end;
        finally
          st := TStopWatch.Create;
          st.Start;
          organic.save('flickrOrganic.xml');
          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
          WriteLn('Update organic repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
          TLogger.LogFile('Update organic repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
          st.Stop;
        end;

        st.Stop;
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
        WriteLn('Update repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Update repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

        st := TStopWatch.Create;
        st.Start;
        repository.save(apikey, secret, userid, 'flickrRepository.xml');
        st.Stop;
        WriteLn('Saving repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Saving repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      end;
    finally

    end;

    if loadglobals then
    begin
      globalsRepository := TFlickrGlobals.Create();
      try
        st := TStopWatch.Create;
        st.Start;
        globalsRepository.load('flickrRepositoryGlobal.xml');
        st.Stop;
        WriteLn('Loaded repository flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Loaded repository flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
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

        totalViewsacc := totalViewsacc + TRepositoryRest.getTotalAlbumsCounts(apikey, userid, verbosity);

        stat := TStat.Create(Date, totalViewsacc, totalLikesacc, totalCommentsacc);
        globalsRepository.AddGlobals(stat);
        st.Stop;
        WriteLn('Update flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        st := TStopWatch.Create;
        st.Start;
        WriteLn('Saving flickrRepositoryGlobal');
        TLogger.LogFile('Saving flickrRepositoryGlobal');
        globalsRepository.save('flickrRepositoryGlobal.xml');
        st.Stop;
        WriteLn('Save flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Save flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      finally
        //repository := nil;
      end;
    end;

    //Send eMail
    description := nil;
    try
      options := TOptions.New().Load;
      description := THtmlComposer.getMessage(options, repository, globalsRepository, organic, false);
      TFlickrEmail.SendHTML(options.eMailAddress, description);
    except
      on E: Exception do
      begin
        TLogger.LogFile('Exception Sending eMail' + E.Message);
        Writeln(E.ClassName, ': ', E.Message);
      end;
    end;

    globalsRepository := nil;
    repository := nil;
    description.Free;

    TLogger.LogFile('Finishing Batch Update');
  except
    on E: Exception do
    begin
      TLogger.LogFile('Exception Batch Update' + E.Message);
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
end.
