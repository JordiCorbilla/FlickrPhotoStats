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

program FlickrPhotoAnalyticsAgent;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  flickr.repository,
  System.Diagnostics,
  flickr.lib.parallel,
  flickr.globals,
  flickr.stats,
  flickr.stats.global,
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
  flickr.photos,
  flickr.lib.options.agent,
  flickr.lib.parse,
  flickr.users.info,
  flickr.album.categoriser;

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
  streamViews, albumViews : integer;
  totalComments, totalCommentsacc: Integer;
  stat: IStat;
  globalStat : IStatGlobal;
  verbosity, loadrepository, loadglobals, loademail: boolean;
  organic: IFlickrOrganic;
  organicStat: IFlickrOrganicStats;
  options: IOptions;
  optionsAgent : IOptionsAgent;
  description: TStrings;
  totalContacts: Integer;
  PhotosList : TList<string>;
  photo, existing: IPhoto;
  success : boolean;
  iteration : integer;
begin
  try
    Setpriorityclass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
    TLogger.LogFile('');
    TLogger.LogFile('Starting Batch Update ' + TUtils.GetVersion);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN or FOREGROUND_INTENSITY);
    WriteLn('    _________      __        ____  __          __        ___                __      __  _');
    WriteLn('   / ____/ (_)____/ /_______/ __ \/ /_  ____  / /_____  /   |  ____  ____ _/ /_  __/ /_(_)_________');
    WriteLn('  / /_  / / / ___/ //_/ ___/ /_/ / __ \/ __ \/ __/ __ \/ /| | / __ \/ __ `/ / / / / __/ / ___/ ___/');
    WriteLn(' / __/ / / / /__/ ,< / /  / ____/ / / / /_/ / /_/ /_/ / ___ |/ / / / /_/ / / /_/ / /_/ / /__(__  )');
    WriteLn('/_/   /_/_/\___/_/|_/_/  /_/   /_/ /_/\____/\__/\____/_/  |_/_/ /_/\__,_/_/\__, /\__/_/\___/____/');
    WriteLn('                                                                          /____/');
    WriteLn('                                                                    _    ____ _____ _   _ _____');
    WriteLn('                                                                   / \  / ___| ____| \ | |_   _|');
    WriteLn('                                                                  / _ \| |  _|  _| |  \| | | |');
    WriteLn('                                                                 / ___ \ |_| | |___| |\  | | |');
    WriteLn('                                                                /_/   \_\____|_____|_| \_| |_|');
    WriteLn('');
    WriteLn('Copyright (C) 2016 Jordi Corbilla');
    WriteLn('Version ' + TUtils.GetVersion);
    WriteLn('Free - (Non-commercial only)');
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
    WriteLn('');
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

    if ((not verbosity) and (not loadrepository) and (not loadglobals)) then
    begin
      WriteLn('Please specify the options:');
      WriteLn('-v     verbosity');
      WriteLn('-r     Load repository');
      WriteLn('-g     Load globals');
      WriteLn('');
      WriteLn('Example: ');
      WriteLn('  FlickrPhotoAnalyticsAgent -v -r -g');
      exit;
    end;

    // Load repository
    WriteLn('Loading Repository');
    repository := TFlickrRepository.Create();
    options := TOptions.New().Load;
    optionsAgent := ToptionsAgent.New().Load;
    try
      if optionsAgent.userToken = '' then
      begin
        WriteLn('Options Agent: User Token is empty');
        Exit;
      end;
      if optionsAgent.userTokenSecret = '' then
      begin
        WriteLn('Options Agent: User Token Secret is empty');
        Exit;
      end;
      st := TStopWatch.Create;
      st.Start;
      repository.version := TUtils.GetVersion;
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
          st := TStopWatch.Create;
          st.Start;
          WriteLn('Looking for new pictures added in the stream');
          PhotosList := TPhotoLoader.load(optionsAgent);
          st.Stop;
          WriteLn('stream loaded: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
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
            WriteLn('New item found: ' + PhotosList[i]);
            TLogger.LogFile('New Photo Added ' + PhotosList[i]);
            stat := TStat.Create(Date, 0, 0, 0);
            photo := TPhoto.Create(PhotosList[i], 'New', '2016-03-02 15:57:24', '');
            photo.Folder := ExtractFilePath(options.Workspace + '\flickrRepository.xml');
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
            WriteLn('The agent will now determine the number of threads to use for ' + repository.photos.count.ToString() + ' photos');
            Writeln('Number of threads: ' + CPUCount.ToString() + 'x8');
            TParallel.ForEach(0, repository.photos.count - 1,
              procedure(index: Integer; threadId: Integer)
              begin
                TRepositoryRest.updatePhoto(repository, organicStat, repository.photos[index].id, verbosity, options, optionsAgent);
              end,
              procedure (index : integer)
              begin
                //Do nothing
              end);
            st.Stop;
          finally
            organicStat.executionTime := st.ElapsedMilliseconds;
            organicStat.date := date;
            try
              try
                totalContacts := TRepositoryRest.getNumberOfContacts(optionsAgent);
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

        AlbumViews := TRepositoryRest.getTotalAlbumsCounts(optionsAgent, verbosity);
        StreamViews := TUserInfo.getStreamViews(userId, optionsAgent);
        totalViewsacc := totalViewsacc +  AlbumViews +  StreamViews;
        globalStat := TStatGlobal.Create(date, totalViewsacc, totalLikesacc, totalCommentsacc, AlbumViews, StreamViews);
        globalsRepository.AddGlobals(globalStat);
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

    //Add the items in the albums
    WriteLn('Categorising photos:');
    st := TStopWatch.Create;
    st.Start;
    TAlbumCategoriser.AutoAdd(repository, options, optionsAgent, procedure (value : string)
      begin
        WriteLn(value);
        TLogger.LogFile(value);
      end);
    st.Stop;
    WriteLn('Categorising photos finished in: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

    //Update Parse
    try
      WriteLn('Updating cloud analytics');
      TParseAnalyticsAPI.UpdateClient(optionsAgent.AppId, repository.photos.Count,
        globalsRepository.globals[globalsRepository.globals.Count-1].views,
        globalsRepository.globals[globalsRepository.globals.Count-1].likes,
        globalsRepository.globals[globalsRepository.globals.Count-1].Comments,
        TUtils.GetVersion);
    except
      on E: Exception do
      begin
        TLogger.LogFile('Exception updating cloud analytics ' + E.message);
        WriteLn(E.ClassName, ': ', E.message);
      end;
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
        success := false;
        iteration := 0;
        while not success do
        begin
          try
            TFlickrEmail.SendHTML(options.eMailAddress, description);
            success := true;
          except
            on E: Exception do
            begin
              success := false;
              TLogger.LogFile('Exception Sending eMail ' + E.message);
              WriteLn(E.ClassName, ': ', E.message);
            end;
          end;
          iteration := iteration + 1;
          if (iteration > 3) then
            success := true;
        end;
        st.Stop;
        WriteLn('Sending eMail: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
        TLogger.LogFile('Sending eMail: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      except
        on E: Exception do
        begin
          TLogger.LogFile('Exception Sending eMail ' + E.message);
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
      TLogger.LogFile('Exception Batch Update ' + E.message);
      WriteLn(E.ClassName, ': ', E.message);
    end;
  end;

end.
