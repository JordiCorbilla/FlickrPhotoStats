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
  Flickr.globals, System.classes, System.SysUtils, FLickr.organic;

type
  THtmlComposer = Class(Tobject)
    class function getMessage(globalsRepository : IFlickrGlobals; organic : IFlickrOrganic): TStrings;
  End;

implementation

{ THtmlComposer }

class function THtmlComposer.getMessage(globalsRepository: IFlickrGlobals; organic : IFlickrOrganic): TStrings;
var
  description : TStrings;
  itemToday,itemToday1, itemToday2, itemToday3, itemToday4, itemToday5, itemToday6 : integer;
  itemYesterday : integer;
  difference : integer;
  itemTodayx,itemToday1x, itemToday2x : double;
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

    description.add('<b>Last 7 days summary</b><br>');
    description.add('<table cellpadding="3" cellspacing="0" border="1" style="border:1px solid #000000">');
    description.add('  <tr>');
    description.add('    <td> </td>');
    description.add('    <td><b>-6</b></td>');
    description.add('    <td><b>-5</b></td>');
    description.add('    <td><b>-4</b></td>');
    description.add('    <td><b>-3</b></td>');
    description.add('    <td><b>-2</b></td>');
    description.add('    <td><b>-1</b></td>');
    description.add('    <td><b>Today</b></td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views - globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
    itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].views - globalsRepository.Globals[globalsRepository.Globals.Count-3].views;
    itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].views - globalsRepository.Globals[globalsRepository.Globals.Count-4].views;
    itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].views - globalsRepository.Globals[globalsRepository.Globals.Count-5].views;
    itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].views - globalsRepository.Globals[globalsRepository.Globals.Count-6].views;
    itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].views - globalsRepository.Globals[globalsRepository.Globals.Count-7].views;
    itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].views - globalsRepository.Globals[globalsRepository.Globals.Count-8].views;

    description.add('  <tr>');
    description.add('    <td><b>Number of Views</b></td>');
    description.add('    <td>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes - globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
    itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes - globalsRepository.Globals[globalsRepository.Globals.Count-3].likes;
    itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].likes - globalsRepository.Globals[globalsRepository.Globals.Count-4].likes;
    itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].likes - globalsRepository.Globals[globalsRepository.Globals.Count-5].likes;
    itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].likes - globalsRepository.Globals[globalsRepository.Globals.Count-6].likes;
    itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].likes - globalsRepository.Globals[globalsRepository.Globals.Count-7].likes;
    itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].likes - globalsRepository.Globals[globalsRepository.Globals.Count-8].likes;

    description.add('  <tr>');
    description.add('    <td><b>Number of Likes</b></td>');
    description.add('    <td>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
    itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-3].numComments;
    itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-4].numComments;
    itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-5].numComments;
    itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-6].numComments;
    itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-7].numComments;
    itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-8].numComments;

    description.add('  <tr>');
    description.add('    <td><b>Number of Comments</b></td>');
    description.add('    <td>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    description.add('</table>');
    description.add('<br>');

    description.add('<b>Additional Stats</b><br>');
    description.add('<table cellpadding="3" cellspacing="0" border="1" style="border:1px solid #000000">');
    description.add('  <tr>');
    description.add('    <td> </td>');
    description.add('    <td><b>Viewed</b></td>');
    description.add('    <td><b>Omitted</b></td>');
    description.add('    <td><b>Lost</b></td>');
    description.add('  </tr>');

    itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveViews * 100) / (organic.Globals[organic.Globals.Count-1].positiveViews + organic.Globals[organic.Globals.Count-1].negativeViews);
    itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeViews * 100) / (organic.Globals[organic.Globals.Count-1].positiveViews + organic.Globals[organic.Globals.Count-1].negativeViews);
    itemToday2x := 0.0;

    description.add('  <tr>');
    description.add('    <td><b>Number of Views</b></td>');
    description.add('    <td>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    description.add('    <td>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
    description.add('  </tr>');

    itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
    itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
    itemToday2x := (organic.Globals[organic.Globals.Count-1].lostLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);

    description.add('  <tr>');
    description.add('    <td><b>Number of Likes</b></td>');
    description.add('    <td>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    description.add('    <td>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
    description.add('  </tr>');

    itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
    itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
    itemToday2x := (organic.Globals[organic.Globals.Count-1].lostComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);

    description.add('  <tr>');
    description.add('    <td><b>Number of Comments</b></td>');
    description.add('    <td>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    description.add('    <td>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
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
