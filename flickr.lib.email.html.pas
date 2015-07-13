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

unit flickr.lib.email.html;

interface

uses
  Flickr.globals, System.classes, System.SysUtils;

type
  THtmlComposer = Class(Tobject)
    class function getMessage(globalsRepository : IFlickrGlobals): TStrings;
  End;

implementation

{ THtmlComposer }

class function THtmlComposer.getMessage(globalsRepository: IFlickrGlobals): TStrings;
var
  description : TStrings;
  itemToday : integer;
  itemYesterday : integer;
  difference : integer;
begin
  description := TStringList.create();
  try
    description.add('<html><head></head><body><h3>Dear User,</h3>');
    description.add('');

    description.add('Daily Stats Report: <b>' + DateToStr(Date) + '</b><br><br>');
    description.add('<b>Summary</b><br>');

    description.add('<table cellpadding="3" cellspacing="0" border="1" style="border:1px solid #000000">');
    description.add('  <tr>');
    description.add('    <td> </td>');
    description.add('    <td><b>Yesterday</b></td>');
    description.add('    <td><b>Today</b></td>');
    description.add('    <td><b>Difference</b></td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
    difference := itemToday - itemYesterday;

    description.add('  <tr>');
    description.add('    <td><b>Number of Views</b></td>');
    description.add('    <td>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
    difference := itemToday - itemYesterday;

    description.add('  <tr>');
    description.add('    <td><b>Number of Likes</b></td>');
    description.add('    <td>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
    difference := itemToday - itemYesterday;

    description.add('  <tr>');
    description.add('    <td><b>Number of Comments</b></td>');
    description.add('    <td>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    description.add('</table>');
    description.add('<br>');

    description.add('<h4>Kind regards,<br>');
    description.add('Flickr Analytics Service</h4>');
  finally

  end;
  result := description;
end;

end.
