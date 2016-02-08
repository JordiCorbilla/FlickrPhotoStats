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

unit flickr.lib.parallel.photos;

interface

uses
  Classes, SysUtils, System.SyncObjs, Vcl.ComCtrls, vcl.taskbar, VCLtee.series, System.Win.taskbarcore, graphics, generics.collections,
  flickr.lib.options.agent, flickr.photos, flickr.tracker;

type
  TParallelPhotos = class(TThread)
  private
    FRestURL : String;
    FOptionsAgent : IOptionsAgent;
    FId : string;
    FPhotoRepository : IPhoto;
    FUpdateCollections : Boolean;
    FWorkspace : string;
  protected
    procedure Execute; override;
  public
    property restUrl : string read FRestUrl write FRestUrl;
    property OptionsAgent : IOptionsAgent read FOptionsAgent write FOptionsAgent;
    property Id : string read FId write FId;
    property PhotoRepository : IPhoto read FPhotoRepository write FPhotoRepository;
    property UpdateCollections : Boolean read FUpdateCollections write FUpdateCollections;
    property Workspace : string read FWorkspace write FWorkspace;
  end;

implementation

uses
  Windows, flickr.http.lib, xmlintf, flickr.albums.list, flickr.pools.list, flickr.stats, flickr.rest;

{ TParallelPhotos }

procedure TParallelPhotos.Execute;
var
  Item, itemExisting: TListItem;
  iXMLRootNode4, iXMLRootNode5: IXMLNode;
  views, title, likes, comments, taken: string;
  stat: IStat;
  photo, existing: IPhoto;
  Albums: TAlbumList;
  Groups: TPoolList;
  tags : string;
  photoGroups : IPhoto;
begin
  THttpRest.Post(FRestURL, procedure (iXMLRootNode : IXMLNode)
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
        if iXMLRootNode4.NodeName = 'location' then
          break;

        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

    THttpRest.Post(TFlickrRest.New(optionsAgent).getFavorites(FId), procedure (iXMLRootNode : IXMLNode)
    begin
      likes := iXMLRootNode.attributes['total'];
    end);

    stat := TStat.Create(Date, StrToInt(views), StrToInt(likes), StrToInt(comments));

    photoGroups := FPhotoRepository;
    if photoGroups <> nil then
    begin
      photoGroups.LoadGroups;
      Groups := photoGroups.Groups;
      photoGroups.LoadAlbums;
      Albums := photoGroups.Albums;
    end
    else
    begin
      photo := TPhoto.Create(id, title, taken, tags);
      Groups := photo.Groups;
      Albums := photo.Albums;
    end;

    if FUpdateCollections then
    begin
      THttpRest.Post(TFlickrRest.New(optionsAgent).getAllContexts(id), procedure (iXMLRootNode : IXMLNode)
      begin
        while iXMLRootNode <> nil do
        begin
          if iXMLRootNode.NodeName = 'set' then
            Albums.AddItem(TAlbum.create(iXMLRootNode.attributes['id'], iXMLRootNode.attributes['title']));
          if iXMLRootNode.NodeName = 'pool' then
            Groups.AddItem(TPool.create(iXMLRootNode.attributes['id'], iXMLRootNode.attributes['title'], Date));
          iXMLRootNode := iXMLRootNode.NextSibling;
        end;
      end);
    end;

    TTracking.TrackPhoto(FWorkspace, id, '1', '50', FOptionsAgent);

    if repository.ExistPhoto(id, existing) then
    begin
      photo := existing;

      if organicStat <> nil then
      begin
        if photo.getTotalViewsDay() >= views.ToInteger() then
          organicStat.negativeViews := organicStat.negativeViews + 1
        else
          organicStat.positiveViews := organicStat.positiveViews + 1;

        if photo.getTotalLikesDay() > likes.ToInteger() then
          organicStat.lostLikes := organicStat.lostLikes + 1
        else if photo.getTotalLikesDay() = likes.ToInteger() then
          organicStat.negativeLikes := organicStat.negativeLikes + 1
        else
          organicStat.positiveLikes := organicStat.positiveLikes + 1;

        if photo.getTotalCommentsDay() > comments.ToInteger() then
          organicStat.lostComments := organicStat.lostComments + 1
        else if photo.getTotalCommentsDay() = comments.ToInteger() then
          organicStat.negativeComments := organicStat.negativeComments + 1
        else
          organicStat.positiveComments := organicStat.positiveComments + 1;

        organicStat.TotalGroups := repository.getTotalSpreadGroups();
      end;

      photo.Title := title; //replace the title as it changes
      photo.Taken := taken;
      photo.tags := tags;
      photo.AddStats(stat, albums, groups);
      photo.LastUpdate := Date;
    end
    else
    begin
      photo.AddStats(stat, albums, groups);
      photo.LastUpdate := Date;
      repository.AddPhoto(photo);
    end;

    if not ExistPhotoInList(id, itemExisting) then
    begin
      Item := frmFlickrMain.listPhotos.Items.Add;
      Item.Caption := frmFlickrMain.photoId.text;
      Item.SubItems.Add(title);
      Item.SubItems.Add(views);
      Item.SubItems.Add(likes);
      Item.SubItems.Add(comments);
      Item.SubItems.Add(DateToStr(Date));
      if views = '0' then
        views := '1';
      Item.SubItems.Add(taken);
      Item.SubItems.Add(photo.TotalAlbums.ToString());
      Item.SubItems.Add(photo.TotalGroups.ToString());
      Item.SubItems.Add(tags);
      Item.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger / views.ToInteger) * 100.0));
      Item.SubItems.Add(photo.banned.ToString());
      Item.SubItems.Add(photo.getTrend.ToString());
      Item.SubItems.Add(photo.OmitGroups);
    end
    else
    begin
      itemExisting.Caption := id;
      itemExisting.SubItems.Clear;
      itemExisting.SubItems.Add(title);
      itemExisting.SubItems.Add(views);
      itemExisting.SubItems.Add(likes);
      itemExisting.SubItems.Add(comments);
      itemExisting.SubItems.Add(DateToStr(Date));
      if views = '0' then
        views := '1';
      itemExisting.SubItems.Add(taken);
      itemExisting.SubItems.Add(photo.TotalAlbums.ToString());
      itemExisting.SubItems.Add(photo.TotalGroups.ToString());
      itemExisting.SubItems.Add(tags);
      itemExisting.SubItems.Add(FormatFloat('0.##%', (likes.ToInteger / views.ToInteger) * 100.0));
      itemExisting.SubItems.Add(photo.banned.ToString());
      itemExisting.SubItems.Add(photo.getTrend.ToString());
      itemExisting.SubItems.Add(photo.OmitGroups);
    end;
    if chkRealTime.Checked then
      UpdateTotals(true);
end;

end.
