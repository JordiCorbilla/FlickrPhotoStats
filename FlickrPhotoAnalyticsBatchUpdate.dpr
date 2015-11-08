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
  flickr.repository.rest,
  flickr.lib.options,
  flickr.lib.email,
  flickr.lib.email.html,
  flickr.lib.utils,
  System.Classes,
  Winapi.Windows,
  Generics.collections,
  Flickr.lib.photos.load,
  flickr.photos;

var
  repository: IFlickrRepository;
  globalsRepository: IFlickrGlobals;
  st: TStopWatch;
  apikey: string;
  secret: string;
  userId: string;
  i: Integer;
  totalViews, totalViewsacc: Integer;
  totalLikes, totalLikesacc: Integer;
  totalComments, totalCommentsacc: Integer;
  stat: IStat;
  verbosity, loadrepository, loadglobals, loademail: boolean;
  organic: IFlickrOrganic;
  organicStat: IFlickrOrganicStats;
  options: IOptions;
  description: TStrings;
  totalContacts: Integer;
  PhotosList : TList<string>;
  photo, existing: IPhoto;
begin
  try
    TLogger.LogFile('');
    TLogger.LogFile('Starting Batch Update ' + TUtils.GetVersion);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
    WriteLn('###################################################');
    WriteLn('# Welcome to Flickr Photo Analytics Batch Update  #');
    WriteLn('# version ' + TUtils.GetVersion + ' @author: Jordi Corbilla         #');
    WriteLn('###################################################');
    verbosity := false;
    if (paramstr(1) = '-v') or (paramstr(2) = '-v') or (paramstr(3) = '-v') or (paramstr(4) = '-v') then
    begin
      WriteLn('verbosity argument detected');
      TLogger.LogFile('verbosity argument detected');
      verbosity := true;
    end;

    loadrepository := false;
    if (paramstr(1) = '-r') or (paramstr(2) = '-r') or (paramstr(3) = '-r') or (paramstr(4) = '-r') then
    begin
      WriteLn('loadrepository argument detected');
      TLogger.LogFile('loadrepository argument detected');
      loadrepository := true;
    end;

    loadglobals := false;
    if (paramstr(1) = '-g') or (paramstr(2) = '-g') or (paramstr(3) = '-g') or (paramstr(4) = '-g') then
    begin
      WriteLn('loadglobals argument detected');
      TLogger.LogFile('loadglobals argument detected');
      loadglobals := true;
    end;

    loademail := false;
    if (paramstr(1) = '-e') or (paramstr(2) = '-e') or (paramstr(3) = '-e') or (paramstr(4) = '-e') then
    begin
      WriteLn('loademail argument detected');
      TLogger.LogFile('loademail argument detected');
      loademail := true;
    end;

    // Load repository
    WriteLn('Loading Repository');
    repository := TFlickrRepository.Create();
    options := TOptions.New().Load;
    try
      st := TStopWatch.Create;
      st.Start;
      repository.Load(options.Workspace + '\flickrRepository.xml');
      st.Stop;
      WriteLn('Loaded repository flickrRepository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      TLogger.LogFile('Loaded repository flickrRepository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      if loadrepository then
      begin
        // Use parallel looping
        st := TStopWatch.Create;
        st.Start;
        apikey := repository.apikey;
        secret := repository.secret;
        userId := repository.userId;

        //Now I need to add the new items in the list.
        PhotosList := nil;
        TLogger.LogFile('Loading photos');
        try
          PhotosList := TPhotoLoader.load(apikey, userId);
        except
          on E: Exception do
          begin
            TLogger.LogFile('Loading photos'  + E.message);
            WriteLn(E.ClassName, ': ', E.message);
          end;
        end;

        for i := 0 to PhotosList.Count-1 do
        begin
          if not repository.ExistPhoto(PhotosList[i], existing) then
          begin
            TLogger.LogFile('New Photo Added ' + PhotosList[i]);
            stat := TStat.Create(Date, 0, 0, 0);
            photo := TPhoto.Create(PhotosList[i], 'New', '', '');
            photo.AddStats(stat);
            photo.LastUpdate := Date;
            repository.AddPhoto(photo);
          end;
        end;

        PhotosList.Free;

        // Organic Growth checks
        organic := TFlickrOrganic.Create;
        try
          st := TStopWatch.Create;
          st.Start;
          organic.Load(options.Workspace + '\flickrOrganic.xml');

          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
          WriteLn('Loaded organic repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
          TLogger.LogFile('Loaded organic repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
          st.Stop;

          organicStat := TFlickrOrganicStats.Create();
          st := TStopWatch.Create;
          try
            st.Start;
            TParallel.ForEach(0, repository.photos.count - 1,
              procedure(index: Integer; threadId: Integer)
              begin
                TRepositoryRest.updatePhoto(repository, organicStat, apikey, repository.photos[index].id, verbosity);
              end);
            st.Stop;
          finally
            organicStat.executionTime := st.ElapsedMilliseconds;
            organicStat.date := date;
            try
              try
                totalContacts := TRepositoryRest.getNumberOfContacts;
              except
                on E: Exception do
                  TLogger.LogFile('Exception reading contacts: ' + E.message);
              end;
              if (totalContacts < 0) and (organic.globals.count > 0) then
                totalContacts := organic.globals[organic.globals.count - 1].following;
            finally
              organicStat.following := totalContacts;
            end;
            organic.AddGlobals(organicStat);
          end;
        finally
          st := TStopWatch.Create;
          st.Start;
          organic.save(options.Workspace + '\flickrOrganic.xml');
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
        repository.save(apikey, secret, userId, options.Workspace + '\flickrRepository.xml');
        st.Stop;
        WriteLn('Saving repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Saving repository: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      end;
    finally

    end;

    globalsRepository := TFlickrGlobals.Create();
    try
      st := TStopWatch.Create;
      st.Start;
      globalsRepository.Load(options.Workspace + '\flickrRepositoryGlobal.xml');
      st.Stop;
      WriteLn('Loaded repository flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      TLogger.LogFile('Loaded repository flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      if loadglobals then
      begin
        totalViewsacc := 0;
        totalLikesacc := 0;
        totalCommentsacc := 0;
        st := TStopWatch.Create;
        st.Start;
        for i := 0 to repository.photos.count - 1 do
        begin
          totalViews := repository.photos[i].getTotalViewsDay();
          totalViewsacc := totalViewsacc + totalViews;

          totalLikes := repository.photos[i].getTotalLikesDay();
          totalLikesacc := totalLikesacc + totalLikes;

          totalComments := repository.photos[i].getTotalCommentsDay();
          totalCommentsacc := totalCommentsacc + totalComments;
        end;

        totalViewsacc := totalViewsacc + TRepositoryRest.getTotalAlbumsCounts(apikey, userId, verbosity);

        stat := TStat.Create(date, totalViewsacc, totalLikesacc, totalCommentsacc);
        globalsRepository.AddGlobals(stat);
        st.Stop;
        WriteLn('Update flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        st := TStopWatch.Create;
        st.Start;
        WriteLn('Saving flickrRepositoryGlobal');
        TLogger.LogFile('Saving flickrRepositoryGlobal');
        globalsRepository.save(options.Workspace + '\flickrRepositoryGlobal.xml');
        st.Stop;
        WriteLn('Save flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Save flickrRepositoryGlobal: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      end;
    finally
      // repository := nil;
    end;

    // Send eMail
    description := nil;
    if loademail then
    begin
      try
        WriteLn('Sending eMail');
        TLogger.LogFile('Sending eMail');
        st := TStopWatch.Create;
        st.Start;
        description := THtmlComposer.getMessage(options, repository, globalsRepository, organic, false);
        TFlickrEmail.SendHTML(options.eMailAddress, description);
        st.Stop;
        WriteLn('Sending eMail: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Sending eMail: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      except
        on E: Exception do
        begin
          TLogger.LogFile('Exception Sending eMail' + E.message);
          WriteLn(E.ClassName, ': ', E.message);
        end;
      end;
    end;

    globalsRepository := nil;
    repository := nil;
    description.Free;

    TLogger.LogFile('Finishing Batch Update');
  except
    on E: Exception do
    begin
      TLogger.LogFile('Exception Batch Update' + E.message);
      WriteLn(E.ClassName, ': ', E.message);
    end;
  end;

end.
