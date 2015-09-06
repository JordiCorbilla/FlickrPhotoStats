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

unit flickr.lib.folder;

interface

uses
  SysUtils, ShlObj, Windows, ActiveX, Forms;

type
  TFolder = class(TObject)
    class function GetFolderDialog(const ACaption: string; out ADirectory: string): boolean;
    class function BrowseForFolder: string;
  end;

implementation

class function TFolder.BrowseForFolder: string;
begin
  GetFolderDialog('Add directory:', Result);
end;

class function TFolder.GetFolderDialog(const ACaption: string; out ADirectory: string): boolean;
const
  BIF_NEWDIALOGSTYLE = $0040;
  BIF_NONEWFOLDERBUTTON = $0200;
  BIF_USENEWUI = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
var
  pWindowList: Pointer;
  tbsBrowseInfo: TBrowseInfo;
  pBuffer: PChar;
  iOldErrorMode: cardinal;
  pIemIDList: PItemIDList;
  pShellMalloc: IMalloc;
begin
  CoInitialize(nil);
  try
    Result := false;
    ADirectory := '';
    FillChar(tbsBrowseInfo, sizeof(tbsBrowseInfo), 0);
    if (ShGetMalloc(pShellMalloc) = S_OK) and Assigned(pShellMalloc) then
    begin
      pBuffer := pShellMalloc.Alloc(MAX_PATH);
      try
        with tbsBrowseInfo do
        begin
          hwndOwner := Application.Handle;
          pidlRoot := nil;
          pszDisplayName := pBuffer;
          lpszTitle := PChar(ACaption);
          ulFlags := BIF_USENEWUI or BIF_RETURNONLYFSDIRS or BIF_NONEWFOLDERBUTTON;
          lParam := 0;
        end;
        pWindowList := DisableTaskWindows(0);
        iOldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
        try
          pIemIDList := ShBrowseForFolder(tbsBrowseInfo);
        finally
          SetErrorMode(iOldErrorMode);
          EnableTaskWindows(pWindowList);
        end;
        Result := Assigned(pIemIDList);
        if Result then
        begin
          ShGetPathFromIDList(pIemIDList, pBuffer);
          pShellMalloc.Free(pIemIDList);
          ADirectory := pBuffer;
        end;
      finally
        pShellMalloc.Free(pBuffer);
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

end.
