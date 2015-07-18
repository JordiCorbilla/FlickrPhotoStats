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
  Flickr.globals, System.classes, System.SysUtils, FLickr.organic, flickr.repository, flickr.top.stats, Generics.Collections, Generics.defaults, flickr.photos;

type
  THtmlComposer = Class(Tobject)
    class function getMessage(repository: IFlickrRepository; globalsRepository : IFlickrGlobals; organic : IFlickrOrganic): TStrings;
  End;

implementation

{ THtmlComposer }

class function THtmlComposer.getMessage(repository: IFlickrRepository; globalsRepository: IFlickrGlobals; organic : IFlickrOrganic): TStrings;
var
  description : TStrings;
  itemToday,itemToday1, itemToday2, itemToday3, itemToday4, itemToday5, itemToday6 : integer;
  itemYesterday : integer;
  difference : integer;
  itemTodayx,itemToday1x, itemToday2x : double;
  fontStyle, fontStylebig : string;
  tableStyle : string;
  trStyle : string;
  thStyle, thNoBorder : string;
  tdStyle, tdStyleText : string;
  topStats: TTopStats;
  PhotosSorted : TList<IPhoto>;
  i: integer;
begin
  fontStyle := ' style="font-family:''segoe ui'',calibri,''gill sans'',helvetica,arial;"';
  fontStylebig := ' style="font-family:''segoe ui'',calibri,''gill sans'',helvetica,arial;font-size:16px;"';
  tableStyle := ' style="border-collapse:collapse;border-spacing:0;border-color:#999;"';
  trStyle := ' style="font-family:''segoe ui'',calibri,''gill sans'',helvetica,arial;font-size:14px;padding:10px 5px;border-width:1px;overflow:hidden;word-break:normal;border-color:#999;color:#444;"';
  thStyle := ' style="font-family:''segoe ui'',calibri,''gill sans'',helvetica,arial;font-size:14px;font-weight:normal;';
  thStyle := thStyle + 'padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:#999;color:#fff;background-color:#26ADE4;text-align: center;"';
  tdStyle := ' style="font-family:''segoe ui'',calibri,''gill sans'',helvetica,arial;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:#999;color:#444;background-color:#F7FDFA;"';
  tdStyleText := ' style="font-family:''segoe ui'',calibri,''gill sans'',helvetica,arial;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:#999;color:#444;background-color:#F7FDFA;text-align: center;"';
  thNoBorder := ' style="border-style:none"';

  description := TStringList.create();
  try
    description.add('<html><head></head><body><h3 '+fontStyle+'>Dear User,</h3>');
    description.add('');

    description.add('<p '+fontStyle+'>Daily Stats Report: <b>' + DateToStr(Date) + '</b><br><br>');
    description.add('<b '+fontStylebig+'>Summary</b><br>');

    description.add('<table '+tableStyle+'>');
    description.add('  <tr '+trStyle+'>');
    description.add('    <th '+thNoBorder+'> </th>');
    description.add('    <th '+thStyle+'><b>Yesterday</b></th>');
    description.add('    <th '+thStyle+'><b>Today</b></th>');
    description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Trend</b></th>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
    difference := itemToday - itemYesterday;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Views</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
    difference := itemToday - itemYesterday;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Likes</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
    difference := itemToday - itemYesterday;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Comments</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    description.add('</table>');
    description.add('<br>');

    description.add('<b '+fontStylebig+'>Last 7 days summary</b><br><br>');
    description.add('<table '+tableStyle+'>');
    description.add('  <tr '+trStyle+'>');
    description.add('    <th '+thNoBorder+'> </td>');
    description.add('    <th '+thStyle+'><b>-6</b></th>');
    description.add('    <th '+thStyle+'><b>-5</b></th>');
    description.add('    <th '+thStyle+'><b>-4</b></th>');
    description.add('    <th '+thStyle+'><b>-3</b></th>');
    description.add('    <th '+thStyle+'><b>-2</b></th>');
    description.add('    <th '+thStyle+'><b>-1</b></th>');
    description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Today</b></th>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views - globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
    itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].views - globalsRepository.Globals[globalsRepository.Globals.Count-3].views;
    itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].views - globalsRepository.Globals[globalsRepository.Globals.Count-4].views;
    itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].views - globalsRepository.Globals[globalsRepository.Globals.Count-5].views;
    itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].views - globalsRepository.Globals[globalsRepository.Globals.Count-6].views;
    itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].views - globalsRepository.Globals[globalsRepository.Globals.Count-7].views;
    itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].views - globalsRepository.Globals[globalsRepository.Globals.Count-8].views;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Views</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes - globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
    itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes - globalsRepository.Globals[globalsRepository.Globals.Count-3].likes;
    itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].likes - globalsRepository.Globals[globalsRepository.Globals.Count-4].likes;
    itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].likes - globalsRepository.Globals[globalsRepository.Globals.Count-5].likes;
    itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].likes - globalsRepository.Globals[globalsRepository.Globals.Count-6].likes;
    itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].likes - globalsRepository.Globals[globalsRepository.Globals.Count-7].likes;
    itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].likes - globalsRepository.Globals[globalsRepository.Globals.Count-8].likes;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Likes</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
    itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-3].numComments;
    itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-4].numComments;
    itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-5].numComments;
    itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-6].numComments;
    itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-7].numComments;
    itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-8].numComments;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Comments</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    description.add('</table>');
    description.add('<br>');

    description.add('<b '+fontStylebig+'>Additional Stats</b><br><br>');
    description.add('<table '+tableStyle+'>');
    description.add('  <tr '+trStyle+'>');
    description.add('    <th '+thNoBorder+'> </td>');
    description.add('    <th '+thStyle+'><b>Viewed</b></th>');
    description.add('    <th '+thStyle+'><b>Omitted</b></th>');
    description.add('    <th '+thStyle+'><b>Lost</b></th>');
    description.add('  </tr>');

    itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveViews * 100) / (organic.Globals[organic.Globals.Count-1].positiveViews + organic.Globals[organic.Globals.Count-1].negativeViews);
    itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeViews * 100) / (organic.Globals[organic.Globals.Count-1].positiveViews + organic.Globals[organic.Globals.Count-1].negativeViews);
    itemToday2x := 0.0;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Views</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
    description.add('  </tr>');

    itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
    itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
    itemToday2x := (organic.Globals[organic.Globals.Count-1].lostLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Likes</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    if itemToday2x > 0.0 then
      description.add('    <td '+tdStyleText.Replace('F7FDFA','FDCFCF')+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>')
    else
      description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
    description.add('  </tr>');

    itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
    itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
    itemToday2x := (organic.Globals[organic.Globals.Count-1].lostComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Comments</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    if itemToday2x > 0.0 then
      description.add('    <td '+tdStyleText.Replace('F7FDFA','FDCFCF')+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>')
    else
      description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');

    description.add('  </tr>');

    description.add('</table>');
    description.add('<br>');

    topStats := TTopStats.Create(repository);
    try
      PhotosSorted := topStats.GetListNumberOfViews(10);
      description.add('<b '+fontStylebig+'>Top 10 Most Viewed Pictures</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle+'><b>Views</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosSorted.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'>'+PhotosSorted[i].Id+'</td>');
        description.add('    <td '+tdStyleText+'>'+PhotosSorted[i].Title+'</td>');
        description.add('    <td '+tdStyleText+'>'+Format('%n',[PhotosSorted[i].getTotalViews.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
    end;

    topStats := TTopStats.Create(repository);
    try
      PhotosSorted := topStats.GetListNumberOfLikes(10);
      description.add('<b '+fontStylebig+'>Top 10 Most Liked Pictures</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle+'><b>Likes</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosSorted.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'>'+PhotosSorted[i].Id+'</td>');
        description.add('    <td '+tdStyleText+'>'+PhotosSorted[i].Title+'</td>');
        description.add('    <td '+tdStyleText+'>'+Format('%n',[PhotosSorted[i].getTotalLikes.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
    end;

    description.add('<h4 '+fontStyle+'>Kind regards,<br>');
    description.add('Flickr Analytics Service</h4>');
  finally

  end;
  result := description;
end;

end.
