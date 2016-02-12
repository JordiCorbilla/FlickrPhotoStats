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
  flickr.lib.options.agent, flickr.photos, flickr.tracker, flickr.stats;

type
  TParallelPhotos = class(TThread)
  private
    FRestURL : String;
    FOptionsAgent : IOptionsAgent;
    FId : string;
    FPhotoRepository : IPhoto;
    FUpdateCollections : Boolean;
    FWorkspace : string;
    FStat : IStat;
  protected
    procedure Execute; override;
  public
    property restUrl : string read FRestUrl write FRestUrl;
    property OptionsAgent : IOptionsAgent read FOptionsAgent write FOptionsAgent;
    property Id : string read FId write FId;
    property PhotoRepository : IPhoto read FPhotoRepository write FPhotoRepository;
    property UpdateCollections : Boolean read FUpdateCollections write FUpdateCollections;
    property Workspace : string read FWorkspace write FWorkspace;
    property Stat: IStat read FStat write FStat;
  end;

implementation

uses
  Windows, flickr.http.lib, xmlintf, flickr.albums.list, flickr.pools.list, flickr.rest, flickr.albums, flickr.pools;

{ TParallelPhotos }

procedure TParallelPhotos.Execute;
var
  iXMLRootNode4, iXMLRootNode5: IXMLNode;
  views, title, likes, comments, taken: string;

  photo: IPhoto;
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

    FStat := TStat.Create(Date, StrToInt(views), StrToInt(likes), StrToInt(comments));

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
end;

end.
