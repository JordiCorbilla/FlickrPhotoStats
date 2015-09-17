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
  Flickr.globals, System.classes, System.SysUtils, FLickr.organic, flickr.repository,
  flickr.top.stats, Generics.Collections, Generics.defaults, flickr.photos, System.DateUtils,
  flickr.lib.options;

type
  THtmlComposer = Class(Tobject)
    class function getMessage(options : IOptions; repository: IFlickrRepository; globalsRepository : IFlickrGlobals; organic : IFlickrOrganic; graph : boolean): TStrings;
  End;

implementation

uses
  flickr.lib.utils, flickr.photo.trend.info;

{ THtmlComposer }

class function THtmlComposer.getMessage(options : IOptions; repository: IFlickrRepository; globalsRepository: IFlickrGlobals; organic : IFlickrOrganic; graph : boolean): TStrings;
var
  description : TStrings;
  itemToday,itemToday1, itemToday2, itemToday3, itemToday4, itemToday5, itemToday6 : integer;
  itemTodayViews, itemYesterdayViews : integer;
  itemTodayLikes, itemYesterdayLikes : integer;
  itemTodayPhotos, itemYesterdayPhotos : integer;
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
  PhotosTrend : TList<IPhotoTrend>;
  average : double;
  i: integer;
  DayNames : array[1..7] of string;
  day : string;
  today, yesterday : integer;
begin
  DayNames[1] := 'Sunday';
  DayNames[2] := 'Monday';
  DayNames[3] := 'Tuesday';
  DayNames[4] := 'Wednesday';
  DayNames[5] := 'Thursday';
  DayNames[6] := 'Friday';
  DayNames[7] := 'Saturday';

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
    description.add('<html>');
    description.add('<head>');

    if graph then
    begin
      //New chart code ****************************
      description.add('<!-- amCharts javascript sources -->');
      description.add('<script type="text/javascript" src="http://www.amcharts.com/lib/3/amcharts.js"></script>');
      description.add('<script type="text/javascript" src="http://www.amcharts.com/lib/3/serial.js"></script>');
      description.add('<!-- amCharts javascript code -->');
      description.add('<script type="text/javascript">');

      description.add('      AmCharts.makeChart("chartdiv",');
      description.add('        {');
      description.add('          "type": "serial",');
      description.add('          "categoryField": "category",');
      description.add('          "startDuration": 1,');
      description.add('          "categoryAxis": {');
      description.add('            "gridPosition": "start"');
      description.add('          },');
      description.add('          "trendLines": [],');
      description.add('          "graphs": [');
      description.add('            {');
      description.add('              "balloonText": "[[title]] of [[category]]:[[value]]",');
      description.add('              "fillAlphas": 0.7,');
      description.add('              "id": "AmGraph-1",');
      description.add('              "lineAlpha": 0,');
      description.add('              "title": "graph 1",');
      description.add('              "valueField": "column-1"');
      description.add('            }');
      description.add('          ],');
      description.add('          "guides": [],');
      description.add('          "valueAxes": [');
      description.add('            {');
      description.add('              "id": "ValueAxis-1",');
      description.add('              "title": "Views"');
      description.add('            }');
      description.add('          ],');
      description.add('          "allLabels": [],');
      description.add('          "balloon": {},');
      description.add('          "legend": {},');
      description.add('          "titles": [');
      description.add('            {');
      description.add('              "id": "Title-1",');
      description.add('              "size": 15,');
      description.add('              "text": "Last 7 days summary"');
      description.add('            }');
      description.add('          ],');

      if globalsRepository.Globals.Count > 0 then
      begin
        itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views - globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
        itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].views - globalsRepository.Globals[globalsRepository.Globals.Count-3].views;
        itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].views - globalsRepository.Globals[globalsRepository.Globals.Count-4].views;
        itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].views - globalsRepository.Globals[globalsRepository.Globals.Count-5].views;
        itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].views - globalsRepository.Globals[globalsRepository.Globals.Count-6].views;
        itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].views - globalsRepository.Globals[globalsRepository.Globals.Count-7].views;
        itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].views - globalsRepository.Globals[globalsRepository.Globals.Count-8].views;
      end
      else
      begin
        itemToday := 0;
        itemToday1 := 0;
        itemToday2 := 0;
        itemToday3 := 0;
        itemToday4 := 0;
        itemTOday5 := 0;
        itemToday6 := 0;
      end;

      description.add('          "dataProvider": [');
      description.add('            {');
      description.add('              "category": "'+DayNames[DayOfWeek(IncDay(Date, -6))]+'",');
      description.add('              "column-1": "'+itemToday6.ToString+'"');
      description.add('            },');
      description.add('            {');
      description.add('              "category": "'+DayNames[DayOfWeek(IncDay(Date, -5))]+'",');
      description.add('              "column-1": "'+itemToday5.ToString+'"');
      description.add('            },');
      description.add('            {');
      description.add('              "category": "'+DayNames[DayOfWeek(IncDay(Date, -4))]+'",');
      description.add('              "column-1": "'+itemToday4.ToString+'"');
      description.add('            },');
      description.add('            {');
      description.add('              "category": "'+DayNames[DayOfWeek(IncDay(Date, -3))]+'",');
      description.add('              "column-1": "'+itemToday3.ToString+'"');
      description.add('            },');
      description.add('            {');
      description.add('              "category": "'+DayNames[DayOfWeek(IncDay(Date, -2))]+'",');
      description.add('              "column-1": "'+itemToday2.ToString+'"');
      description.add('            },');
      description.add('            {');
      description.add('              "category": "'+DayNames[DayOfWeek(IncDay(Date, -1))]+'",');
      description.add('              "column-1": "'+itemToday1.ToString+'"');
      description.add('            },');
      description.add('            {');
      description.add('              "category": "Today",');
      description.add('              "column-1": "'+itemToday.ToString+'"');
      description.add('            }');
      description.add('          ]');
      description.add('        }');
      description.add('      );');
      description.add('</script>');
      //new chart code ****************************
     end;
    description.add('</head>');
    description.add('<body><h3 '+fontStyle+'>Dear User,</h3>');
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

    itemTodayViews := 0;
    itemYesterdayViews := 0;
    difference := 0;
    if globalsRepository.Globals.Count > 0 then
    begin
      itemTodayViews := globalsRepository.Globals[globalsRepository.Globals.Count-1].views;
      itemYesterdayViews := globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
      difference := itemTodayViews - itemYesterdayViews;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Views</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterdayViews.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayViews.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if globalsRepository.Globals.Count > 0 then
    begin
      itemTodayLikes := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes;
      itemYesterdayLikes := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
      difference := itemTodayLikes - itemYesterdayLikes;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Likes</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterdayLikes.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayLikes.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if globalsRepository.Globals.Count > 0 then
    begin
      itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments;
      itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
      difference := itemToday - itemYesterday;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Comments</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if organic.Globals.Count > 0 then
    begin
      itemToday := organic.Globals[organic.Globals.Count-1].Following;
      itemYesterday := organic.Globals[organic.Globals.Count-2].Following;
      difference := itemToday - itemYesterday;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Contacts</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if organic.Globals.Count > 0 then
    begin
      itemToday := organic.Globals[organic.Globals.Count-1].TotalGroups;
      itemYesterday := organic.Globals[organic.Globals.Count-2].TotalGroups;
      difference := itemToday - itemYesterday;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Group spread</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if repository.photos.Count > 0 then
    begin
      itemTodayPhotos := repository.photos.Count;
      itemYesterdayPhotos := repository.photos.Count;
      difference := itemTodayPhotos - itemYesterdayPhotos;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Photos</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterdayPhotos.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayPhotos.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if (itemTodayPhotos >0) and (itemYesterdayPhotos >0) then
    begin
      itemToday := Round(itemTodayViews / itemTodayPhotos);
      itemYesterday := Round(itemYesterdayViews / itemYesterdayPhotos);
      difference := itemToday - itemYesterday;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Views per Photo</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if (itemTodayPhotos >0) and (itemYesterdayPhotos >0) then
    begin
      itemToday := Round(itemTodayLikes / itemTodayPhotos);
      itemYesterday := Round(itemYesterdayLikes / itemYesterdayPhotos);
      difference := itemToday - itemYesterday;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Likes per Photo</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemYesterday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    description.add('</table>');
    description.add('<br>');
    if graph then
      description.Add('<div id="chartdiv" style="width: 100%; height: 400px; background-color: #FFFFFF;" ></div>');
    description.add('<b '+fontStylebig+'>Last 7 days summary</b><br><br>');
    description.add('<table '+tableStyle+'>');
    description.add('  <tr '+trStyle+'>');
    description.add('    <th '+thNoBorder+'> </td>');
    day := DayNames[DayOfWeek(IncDay(Date, -6))];
    description.add('    <th '+thStyle+'><b>'+day+'</b></th>');
    day := DayNames[DayOfWeek(IncDay(Date, -5))];
    description.add('    <th '+thStyle+'><b>'+day+'</b></th>');
    day := DayNames[DayOfWeek(IncDay(Date, -4))];
    description.add('    <th '+thStyle+'><b>'+day+'</b></th>');
    day := DayNames[DayOfWeek(IncDay(Date, -3))];
    description.add('    <th '+thStyle+'><b>'+day+'</b></th>');
    day := DayNames[DayOfWeek(IncDay(Date, -2))];
    description.add('    <th '+thStyle+'><b>'+day+'</b></th>');
    day := DayNames[DayOfWeek(IncDay(Date, -1))];
    description.add('    <th '+thStyle+'><b>'+day+'</b></th>');
    description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Today</b></th>');
    description.add('    <th '+thStyle+'><b>Average</b></th>');
    description.add('  </tr>');

    if globalsRepository.Globals.Count > 0 then
    begin
      itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views - globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
      itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].views - globalsRepository.Globals[globalsRepository.Globals.Count-3].views;
      itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].views - globalsRepository.Globals[globalsRepository.Globals.Count-4].views;
      itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].views - globalsRepository.Globals[globalsRepository.Globals.Count-5].views;
      itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].views - globalsRepository.Globals[globalsRepository.Globals.Count-6].views;
      itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].views - globalsRepository.Globals[globalsRepository.Globals.Count-7].views;
      itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].views - globalsRepository.Globals[globalsRepository.Globals.Count-8].views;
      average := (itemToday + itemToday1 + itemToday2 + itemToday3 + itemToday4 + itemToday5 + itemToday6) / 7;
    end
    else
      average :=0;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Views</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[average]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if globalsRepository.Globals.Count > 0 then
    begin
      itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes - globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
      itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes - globalsRepository.Globals[globalsRepository.Globals.Count-3].likes;
      itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].likes - globalsRepository.Globals[globalsRepository.Globals.Count-4].likes;
      itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].likes - globalsRepository.Globals[globalsRepository.Globals.Count-5].likes;
      itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].likes - globalsRepository.Globals[globalsRepository.Globals.Count-6].likes;
      itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].likes - globalsRepository.Globals[globalsRepository.Globals.Count-7].likes;
      itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].likes - globalsRepository.Globals[globalsRepository.Globals.Count-8].likes;
      average := (itemToday + itemToday1 + itemToday2 + itemToday3 + itemToday4 + itemToday5 + itemToday6) / 7;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Likes</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[average]).Replace('.00','')+'</td>');
    description.add('  </tr>');

    if globalsRepository.Globals.Count > 0 then
    begin
      itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
      itemToday1 := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-3].numComments;
      itemToday2 := globalsRepository.Globals[globalsRepository.Globals.Count-3].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-4].numComments;
      itemToday3 := globalsRepository.Globals[globalsRepository.Globals.Count-4].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-5].numComments;
      itemToday4 := globalsRepository.Globals[globalsRepository.Globals.Count-5].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-6].numComments;
      itemToday5 := globalsRepository.Globals[globalsRepository.Globals.Count-6].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-7].numComments;
      itemToday6 := globalsRepository.Globals[globalsRepository.Globals.Count-7].numComments - globalsRepository.Globals[globalsRepository.Globals.Count-8].numComments;
      average := (itemToday + itemToday1 + itemToday2 + itemToday3 + itemToday4 + itemToday5 + itemToday6) / 7;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Comments</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday6.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday5.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday4.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday3.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday.ToDouble]).Replace('.00','')+'</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[average]).Replace('.00','')+'</td>');
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

    itemTodayx := 0;
    itemToday1x := 0;
    itemToday2x := 0;
    if organic.Globals.Count > 0 then
    begin
      itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveViews * 100) / (organic.Globals[organic.Globals.Count-1].positiveViews + organic.Globals[organic.Globals.Count-1].negativeViews);
      itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeViews * 100) / (organic.Globals[organic.Globals.Count-1].positiveViews + organic.Globals[organic.Globals.Count-1].negativeViews);
      itemToday2x := 0.0;
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Views</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
    description.add('  </tr>');

    if organic.Globals.Count > 0 then
    begin
      itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
      itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
      itemToday2x := (organic.Globals[organic.Globals.Count-1].lostLikes * 100) / (organic.Globals[organic.Globals.Count-1].positiveLikes + organic.Globals[organic.Globals.Count-1].negativeLikes + organic.Globals[organic.Globals.Count-1].lostLikes);
    end;

    description.add('  <tr>');
    description.add('    <td '+tdStyle+'><b>Number of Likes</b></td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemTodayx]).Replace('.00','')+'%</td>');
    description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday1x]).Replace('.00','')+'%</td>');
    if itemToday2x > 0.0 then
      description.add('    <td '+tdStyleText.Replace('F7FDFA','FDCFCF')+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>')
    else
      description.add('    <td '+tdStyleText+'>'+Format('%n',[itemToday2x]).Replace('.00','')+'%</td>');
    description.add('  </tr>');

    if organic.Globals.Count > 0 then
    begin
      itemTodayx := (organic.Globals[organic.Globals.Count-1].positiveComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
      itemToday1x := (organic.Globals[organic.Globals.Count-1].negativeComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
      itemToday2x := (organic.Globals[organic.Globals.Count-1].lostComments * 100) / (organic.Globals[organic.Globals.Count-1].positiveComments + organic.Globals[organic.Globals.Count-1].negativeComments + organic.Globals[organic.Globals.Count-1].lostComments);
    end;

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

    PhotosSorted := nil;
    topStats := TTopStats.Create(repository);
    try
      PhotosSorted := topStats.GetListNumberOfViews(20);
      description.add('<b '+fontStylebig+'>Top 20 Most Viewed Pictures</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle+'><b>Views Yesterday</b></th>');
      description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Views Today</b></th>');
      description.add('    <th '+thStyle+'><b>Trend</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosSorted.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'><a href="https://www.flickr.com/photos/'+options.urlName+'/' + PhotosSorted[i].Id + '/in/photostream/lightbox/"  target="_blank">'+PhotosSorted[i].Id+'</a></td>');
        description.add('    <td '+tdStyleText+'>'+PhotosSorted[i].Title+'</td>');
        yesterday := PhotosSorted[i].getTotalViews(-1);
        description.add('    <td '+tdStyleText+'>'+Format('%n',[yesterday.ToDouble]).Replace('.00','')+'</td>');
        today := PhotosSorted[i].getTotalViews;
        description.add('    <td '+tdStyleText+'>'+Format('%n',[today.ToDouble]).Replace('.00','')+'</td>');
        difference := today - yesterday;
        if difference <= 0 then
          description.add('    <td '+tdStyleText.Replace('F7FDFA','FDCFCF')+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>')
        else
          description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
      PhotosSorted.Free;
    end;

    topStats := TTopStats.Create(repository);
    try
      PhotosSorted := topStats.GetListNumberOfLikes(20);
      description.add('<b '+fontStylebig+'>Top 20 Most Liked Pictures</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle+'><b>Likes Yesterday</b></th>');
      description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Likes Today</b></th>');
      description.add('    <th '+thStyle+'><b>Trend</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosSorted.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'><a href="https://www.flickr.com/photos/'+options.urlName+'/' + PhotosSorted[i].Id + '/in/photostream/lightbox/"  target="_blank">'+PhotosSorted[i].Id+'</a></td>');
        description.add('    <td '+tdStyleText+'>'+PhotosSorted[i].Title+'</td>');
        yesterday := PhotosSorted[i].getTotalLikes(-1);
        description.add('    <td '+tdStyleText+'>'+Format('%n',[yesterday.ToDouble]).Replace('.00','')+'</td>');
        today := PhotosSorted[i].getTotalLikes;
        description.add('    <td '+tdStyleText+'>'+Format('%n',[today.ToDouble]).Replace('.00','')+'</td>');
        difference := today - yesterday;
        if difference <= 0 then
          description.add('    <td '+tdStyleText.Replace('F7FDFA','FDCFCF')+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>')
        else
          description.add('    <td '+tdStyleText+'>'+Format('%n',[difference.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
      PhotosSorted.Free;
    end;

    PhotosTrend := nil;
    topStats := TTopStats.Create(repository);
    try
      PhotosTrend := topStats.GetListTrendingActivityViews(10);
      description.add('<b '+fontStylebig+'>Top 10 Trending Pictures Today</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Views Today</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosTrend.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'><a href="https://www.flickr.com/photos/'+options.urlName+'/' + PhotosTrend[i].Id + '/in/photostream/lightbox/"  target="_blank">'+PhotosTrend[i].Id+'</a></td>');
        description.add('    <td '+tdStyleText+'>'+PhotosTrend[i].Title+'</td>');
        today := PhotosTrend[i].value;
        description.add('    <td '+tdStyleText+'>'+Format('%n',[today.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
      PhotosTrend.Free;
    end;

    topStats := TTopStats.Create(repository);
    try
      PhotosTrend := topStats.GetListTrendingActivityLikes(10);
      description.add('<b '+fontStylebig+'>Top 10 Trending Pictures Today</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Likes Today</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosTrend.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'><a href="https://www.flickr.com/photos/'+options.urlName+'/' + PhotosTrend[i].Id + '/in/photostream/lightbox/"  target="_blank">'+PhotosTrend[i].Id+'</a></td>');
        description.add('    <td '+tdStyleText+'>'+PhotosTrend[i].Title+'</td>');
        today := PhotosTrend[i].value;
        description.add('    <td '+tdStyleText+'>'+Format('%n',[today.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
      PhotosTrend.Free;
    end;

    topStats := TTopStats.Create(repository);
    try
      PhotosTrend := topStats.GetListTrendingActivityComments(10);
      description.add('<b '+fontStylebig+'>Top 10 Trending Pictures Today</b><br><br>');
      description.add('<table '+tableStyle+'>');
      description.add('  <tr '+trStyle+'>');
      description.add('    <th '+thNoBorder+'> </td>');
      description.add('    <th '+thStyle+'><b>Title</b></th>');
      description.add('    <th '+thStyle.Replace('26ADE4','AD0D98')+'><b>Comments Today</b></th>');
      description.add('  </tr>');

      for i := 0 to PhotosTrend.count-1 do
      begin
        description.add('  <tr>');
        description.add('    <td '+tdStyle+'><a href="https://www.flickr.com/photos/'+options.urlName+'/' + PhotosTrend[i].Id + '/in/photostream/lightbox/"  target="_blank">'+PhotosTrend[i].Id+'</a></td>');
        description.add('    <td '+tdStyleText+'>'+PhotosTrend[i].Title+'</td>');
        today := PhotosTrend[i].value;
        description.add('    <td '+tdStyleText+'>'+Format('%n',[today.ToDouble]).Replace('.00','')+'</td>');
        description.add('  </tr>');
      end;

      description.add('</table>');
      description.add('<br>');
    finally
      topStats.Free;
      PhotosTrend.Free;
    end;

    description.add('<h4 '+fontStyle+'>Kind regards,<br>');
    description.add('Flickr Analytics Service v'+TUtils.GetVersion+'</h4>');
    description.Add('</body></html>');
  finally

  end;
  result := description;
end;

end.
