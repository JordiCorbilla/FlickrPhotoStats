// Copyright (c) 2016, Jordi Corbilla
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

unit flickr.tracker;

interface

uses
  flickr.user.tracking, SysUtils, XMLIntf, flickr.rest,
  flickr.user.faves, flickr.users.info, flickr.lib.options.agent;

type
  TTracking = Class(Tobject)
    class procedure TrackPhoto(folder, id : string; page: string; per_page: string; optionsAgent : IOptionsAgent);
  End;

implementation

uses
  flickr.http.lib;

{ TTracking }

class procedure TTracking.TrackPhoto(folder, id : string; page: string; per_page: string; optionsAgent : IOptionsAgent);
var
  userTracking : IUserTracking;
  iXMLRootNode4: IXMLNode;
  pages, total, totalitems: string;
  userFave, existing : IUserFave;
  numPages, i : integer;
begin
    if not directoryexists(folder + '\Users') then
      ForceDirectories(folder + '\Users');
    //Load first the Tracking Object with the data from the disk.
    userTracking := TUserTracking.Create;
    userTracking.Load(folder + '\Users\'+ id + '.xml');

    THttpRest.Post(TFlickrRest.New(optionsAgent).getUserFavesPhoto(id, page, per_page), procedure (iXMLRootNode : IXMLNode)
    begin
      pages := iXMLRootNode.attributes['page'];
      total := iXMLRootNode.attributes['pages'];
      totalitems := iXMLRootNode.attributes['total'];
      iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <person>
      while iXMLRootNode4 <> nil do
      begin
        if iXMLRootNode4.NodeName = 'person' then
        begin
          userFave := TUserFave.Create;
          userFave.Id := iXMLRootNode4.attributes['nsid'];
          userFave.username := iXMLRootNode4.attributes['username'];
          userFave.isContact := iXMLRootNode4.attributes['contact'];
          userFave.isFamily := iXMLRootNode4.attributes['family'];
          userFave.isFriend := iXMLRootNode4.attributes['friend'];
          userFave.Marked := true;
          if userTracking.Exists(userFave, existing) then
            userFave.Location := existing.Location
          else
            userFave.Location := TUserInfo.getLocation(userFave.Id, optionsAgent);

          if (not userTracking.existsAdded(userFave, existing)) then
            userTracking.Added.Add(userFave.Id, userFave)
          else
          begin
            existing.Marked := true;
          end;
        end;
        iXMLRootNode4 := iXMLRootNode4.NextSibling;
      end;
    end);

  // Load the remaining pages
  numPages := total.ToInteger;
  for i := 2 to numPages do
  begin
    THttpRest.Post(TFlickrRest.New(optionsAgent).getUserFavesPhoto(id, i.ToString, per_page), procedure (iXMLRootNode : IXMLNode)
      begin
        iXMLRootNode4 := iXMLRootNode.ChildNodes.first; // <person>
        while iXMLRootNode4 <> nil do
        begin
          if iXMLRootNode4.NodeName = 'person' then
          begin
            userFave := TUserFave.Create;
            userFave.Id := iXMLRootNode4.attributes['nsid'];
            userFave.username := iXMLRootNode4.attributes['username'];
            userFave.isContact := iXMLRootNode4.attributes['contact'];
            userFave.isFamily := iXMLRootNode4.attributes['family'];
            userFave.isFriend := iXMLRootNode4.attributes['friend'];
            userFave.Marked := true;
            if userTracking.Exists(userFave, existing) then
              userFave.Location := existing.Location
            else
              userFave.Location := TUserInfo.getLocation(userFave.Id, optionsAgent);
            if (not userTracking.existsAdded(userFave, existing)) then
              userTracking.Added.Add(userFave.Id, userFave)
            else
            begin
              existing.Marked := true;
            end;
          end;
          iXMLRootNode4 := iXMLRootNode4.NextSibling;
        end;
      end);
  end;

  userTracking.Save(folder + '\Users\'+ id + '.xml');
end;

end.
