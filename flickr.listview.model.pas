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

unit flickr.listview.model;

interface

uses
  Vcl.comctrls, flickr.base;

type
  TPhotoListViewModel = class(TObject)
  public
    procedure AddItemsView(list: TListView; base : IBase);
    constructor Create();
    Destructor Destroy(); override;
  end;

implementation

uses
  Sysutils;

{ TPhotoListViewModel }

procedure TPhotoListViewModel.AddItemsView(list: TListView; base: IBase);
var
  Item: TListItem;
begin
    Item := list.Items.Add;
    Item.Caption := base.id;
    Item.SubItems.Add(base.title);
    Item.SubItems.Add(base.Photos.ToString());
    Item.SubItems.Add(base.Members.ToString());
    Item.SubItems.Add(base.Description);
    Item.SubItems.Add(base.IsModerated.ToString());
    Item.SubItems.Add(base.ThrottleCount.ToString());
    Item.SubItems.Add(base.ThrottleMode);
    Item.SubItems.Add(base.ThrottleRemaining.ToString());
    Item.SubItems.Add(base.photos_ok.ToString());
    Item.SubItems.Add(base.videos_ok.ToString());
    Item.SubItems.Add(base.images_ok.ToString());
    Item.SubItems.Add(base.screens_ok.ToString());
    Item.SubItems.Add(base.art_ok.ToString());
    Item.SubItems.Add(base.safe_ok.ToString());
    Item.SubItems.Add(base.moderate_ok.ToString());
    Item.SubItems.Add(base.restricted_ok.ToString());
    Item.SubItems.Add(base.has_geo.ToString());
end;

constructor TPhotoListViewModel.Create;
begin

end;

destructor TPhotoListViewModel.Destroy;
begin

end;

end.
