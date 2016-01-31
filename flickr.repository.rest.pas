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

unit flickr.repository.rest;

interface

uses
  Windows, flickr.repository, flickr.organic.stats, flickr.lib.options, flickr.lib.options.agent;

type
  IRepositoryRest = interface

  end;

  TRepositoryRest = class(TInterfacedObject, IRepositoryRest)
    class procedure UpdatePhoto(repository: IFlickrRepository; organicStat: IFlickrOrganicStats; id: string; verbosity : boolean; options : IOptions; optionsAgent : IOptionsAgent);
    class function getTotalAlbumsCounts(userId : string; optionsAgent : IOptionsAgent; verbosity : boolean): Integer; static;
    class function getNumberOfContacts(optionsAgent : IOptionsAgent) : integer;
  end;

var
  CritSect : TRTLCriticalSection;

implementation

uses
  WinApi.ActiveX, XMLIntf, Vcl.ComCtrls, flickr.photos,
  System.SyncObjs, generics.collections, flickr.stats, flickr.Pools, flickr.Albums, IdGlobal,
  flickr.rest, System.SysUtils, flickr.pools.list, flickr.albums.list,
  flickr.tracker, Diagnostics, flickr.time, flickr.http.lib;

{ TRepositoryRest }

class procedure TRepositoryRest.UpdatePhoto(repository: IFlickrRepository; organicStat: IFlickrOrganicStats; id: string; verbosity : boolean; options : IOptions; optionsAgent : IOptionsAgent);
var
  iXMLRootNode4, iXMLRootNode5: IXMLNode;
  views, title, likes, comments, taken: string;
  stat: IStat;
  photo, existing: IPhoto;
  Albums: TAlbumList;
  Groups: TPoolList;
  difference : integer;
  tags : string;
  photoGroups : IPhoto;
  st: TStopWatch;
begin
  THttpRest.Post(TFlickrRest.New(optionsAgent).getInfo(id), procedure (iXMLRootNode : IXMLNode)
    begin
      views := iXMLRootNode.attributes['views'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <owner>
      tags := '';
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'title' then
          title := iXMLRootNode4.NodeValue;
        if iXMLRootNode4.NodeName = 'dates' then
          taken := iXMLRootNode4.attributes['taken'];
        if iXMLRootNode4.NodeName = 'comments' then
          comments := iXMLRootNode4.NodeValue;
        if iXMLRootNode4.NodeName = 'tags' then
        begin
          iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
          while iXMLRootNode5 <> nil do
          begin
            if (iXMLRootNode5.NodeName = 'tag') and (iXMLRootNode5.NodeValue <> 'jordicorbilla') and (iXMLRootNode5.NodeValue <> 'jordicorbillaphotography') then
              tags := tags + iXMLRootNode5.NodeValue + ',';
            iXMLRootNode5 := iXMLRootNode5.NextSibling;
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

  THttpRest.Post(TFlickrRest.New(optionsAgent).getFavorites(id), procedure (iXMLRootNode : IXMLNode)
    begin
      likes := iXMLRootNode.attributes['total'];
    end);

  stat := TStat.Create(Date, StrToInt(views), StrToInt(likes), StrToInt(comments));

  EnterCriticalSection(CritSect);
  photoGroups := repository.GetPhoto(id);
  if photoGroups <> nil then
  begin
    Groups := photoGroups.Groups;
    Albums := photoGroups.Albums;
  end
  else
  begin
    photo := TPhoto.Create(id, title, taken, tags);
    Groups := photo.Groups;
    Albums := photo.Albums;
  end;
  LeaveCriticalSection(CritSect);

  THttpRest.Post(TFlickrRest.New(optionsAgent).getAllContexts(id), procedure (iXMLRootNode : IXMLNode)
    begin
      while iXMLRootNode <> nil do
      begin
        if iXMLRootNode.NodeName = 'set' then
        begin
          EnterCriticalSection(CritSect);
          Albums.AddItem(TAlbum.create(iXMLRootNode.attributes['id'], iXMLRootNode.attributes['title']));
          LeaveCriticalSection(CritSect);
        end;
        if iXMLRootNode.NodeName = 'pool' then
        begin
          EnterCriticalSection(CritSect);
          Groups.AddItem(TPool.create(iXMLRootNode.attributes['id'], iXMLRootNode.attributes['title'], date));
          LeaveCriticalSection(CritSect);
        end;
        iXMLRootNode := iXMLRootNode.NextSibling;
      end;
    end);

  st := TStopWatch.Create;
  st.Start;
  TTracking.TrackPhoto(options.Workspace, id, '1', '50', optionsAgent);
  st.Stop;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_INTENSITY);
  WriteLn('   Tracking users: ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));

  EnterCriticalSection(CritSect);
  if repository.ExistPhoto(id, existing) then
  begin
    photo := existing;
    if photo.getTotalViewsDay() >= views.ToInteger() then
    begin
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_INTENSITY);
      organicStat.negativeViews := organicStat.negativeViews + 1;
    end
    else
    begin
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN);
      organicStat.positiveViews := organicStat.positiveViews + 1;
    end;
    if photo.getTotalLikesDay() > likes.ToInteger() then
    begin
      organicStat.lostLikes := organicStat.lostLikes + 1;
    end
    else if photo.getTotalLikesDay() = likes.ToInteger() then
    begin
      organicStat.negativeLikes := organicStat.negativeLikes + 1;
    end
    else
    begin
      organicStat.positiveLikes := organicStat.positiveLikes + 1;
    end;
    if photo.getTotalCommentsDay() > comments.ToInteger() then
    begin
      organicStat.lostComments := organicStat.lostComments + 1;
    end
    else if photo.getTotalCommentsDay() = comments.ToInteger() then
    begin
      organicStat.negativeComments := organicStat.negativeComments + 1;
    end
    else
    begin
      organicStat.positiveComments := organicStat.positiveComments + 1;
    end;

    organicStat.TotalGroups := repository.getTotalSpreadGroups();

    if verbosity then
    begin
      difference := views.ToInteger() - photo.getTotalViewsDay();
      WriteLn('Updating : ' + title + ' previous: '+ photo.getTotalViewsDay().ToString() + ' views: ' + views + ' difference: '+ difference.ToString());
    end;
    photo.Title := title; //replace the title as it changes
    photo.Tags := tags;
    photo.Taken := taken;
    photo.AddStats(stat);
    photo.LastUpdate := Date;
  end
  else
  begin
    photo.AddStats(stat);
    photo.LastUpdate := Date;
    repository.AddPhoto(photo);
    if verbosity then
    begin
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN);
      WriteLn('Updating : ' + title + ' views: ' + views);
    end;
  end;
  LeaveCriticalSection(CritSect);
end;


class function TRepositoryRest.getNumberOfContacts(optionsAgent : IOptionsAgent): integer;
var
  views: string;
begin
  views := '-1';
  THttpRest.Post(TFlickrRest.New(optionsAgent).getContactListTotals(), true, true, procedure (iXMLRootNode : IXMLNode)
      begin
        views := iXMLRootNode.attributes['total'];
      end);
  result := views.ToInteger;
end;

class function TRepositoryRest.getTotalAlbumsCounts(userId : string; optionsAgent : IOptionsAgent; verbosity : boolean): Integer;
var
  iXMLRootNode4, iXMLRootNode5: IXMLNode;
  pages, total: string;
  numPages: Integer;
  i: Integer;
  totalViews: Integer;
  photosetId: string;
  title: string;
  countViews: Integer;
begin
  THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotoSets(userId, '1', '500'), procedure (iXMLRootNode : IXMLNode)
    begin
      pages := iXMLRootNode.attributes['pages'];
      total := iXMLRootNode.attributes['total'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photoset>
      totalViews := 0;
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'photoset' then
        begin
          photosetId := iXMLRootNode4.attributes['id'];
          countViews := iXMLRootNode4.attributes['count_views'];
          iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
          title := iXMLRootNode5.text;
          totalViews := totalViews + countViews;
        end;
        if verbosity then
        begin
          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN);
          WriteLn('Updating : ' + photosetId + ' views: ' + countViews.ToString());
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

    numPages := pages.ToInteger;

    for i := 2 to numPages do
    begin
      THttpRest.Post(TFlickrRest.New(optionsAgent).getPhotoSets(userid, i.ToString, '500'), procedure (iXMLRootNode : IXMLNode)
        begin
          pages := iXMLRootNode.attributes['pages'];
          iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <photoset>
          while iXMLRootNode4 <> nil do
          begin
            if iXMLRootNode4.NodeName = 'photoset' then
            begin
              photosetId := iXMLRootNode4.attributes['id'];
              countViews := iXMLRootNode4.attributes['count_views'];
              iXMLRootNode5 := iXMLRootNode4.ChildNodes.first;
              title := iXMLRootNode5.text;
              totalViews := totalViews + countViews;
            end;
            if verbosity then
            begin
              SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN);
              WriteLn('Updating : ' + photosetId + ' views: ' + countViews.ToString());
            end;
            iXMLRootNode4 := iXMLRootNode4.NextSibling;
          end;
        end);
    end;

  Result := totalViews;
end;

initialization
  InitializeCriticalSection(CritSect);

finalization
  DeleteCriticalSection(CritSect);

end.
