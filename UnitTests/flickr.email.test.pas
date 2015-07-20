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

unit flickr.email.test;

interface

uses
  DUnitX.TestFramework, flickr.time, flickr.lib.email, flickr.globals, flickr.lib.options, flickr.lib.email.html, flickr.repository;

type

  [TestFixture]
  TMyTestObject = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [test]
    [TestCase('TesteMail', 'This is a test')]
    procedure Test1(const text: String);
    [test]
    procedure TestEmail();
    [test]
    procedure TestEmailHTML();
  end;

implementation

uses
  Sysutils, System.Classes, flickr.organic;

{ TMyTestObject }

procedure TMyTestObject.Setup;
begin

end;

procedure TMyTestObject.TearDown;
begin

end;

procedure TMyTestObject.Test1(const text: String);
begin
  //TFlickrEmail.Send('flickrphotoanalytics@gmail.com', text);
end;

procedure TMyTestObject.TestEmail;
var
  globalsRepository: IFlickrGlobals;
  options : IOptions;
  description : string;
  itemToday : integer;
  itemYesterday : integer;
  difference : integer;
begin
  globalsRepository := TFlickrGlobals.Create();
  try
    globalsRepository.load('flickrRepositoryGlobal.xml');
    options := TOptions.New().Load;

    description := 'Dear User,' + sLineBreak;
    description := description + '' + sLineBreak;

    description := description + 'Here are your stats for ' + DateToStr(Date) + sLineBreak;

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].views;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].views;
    difference := itemToday - itemYesterday;

    description := description + ' - Number of Total Views: ' + Format('%n',[itemToday.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '   - Number of Total Views yesterday: ' + Format('%n',[itemYesterday.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '   - Number of Views Today: ' + Format('%n',[difference.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '' + sLineBreak;

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].likes;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].likes;
    difference := itemToday - itemYesterday;

    description := description + ' - Number of Total Likes: ' + Format('%n',[itemToday.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '   - Number of Total Likes yesterday: ' + Format('%n',[itemYesterday.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '   - Number of Likes Today: ' + Format('%n',[difference.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '' + sLineBreak;

    itemToday := globalsRepository.Globals[globalsRepository.Globals.Count-1].numComments;
    itemYesterday := globalsRepository.Globals[globalsRepository.Globals.Count-2].numComments;
    difference := itemToday - itemYesterday;

    description := description + ' - Number of Total Comments: ' + Format('%n',[itemToday.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '   - Number of Total Comments yesterday: ' + Format('%n',[itemYesterday.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '   - Number of Comments Today: ' + Format('%n',[difference.ToDouble]).Replace('.00','') + sLineBreak;
    description := description + '' + sLineBreak;
    description := description + 'Regards,' + sLineBreak;
    description := description + 'Flickr Analytics Service';
    //TFlickrEmail.Send(options.eMailAddress, description);
  finally

  end;
end;

procedure TMyTestObject.TestEmailHTML;
var
  globalsRepository: IFlickrGlobals;
  options : IOptions;
  description : TStrings;
  organic : TFlickrOrganic;
  repository: IFlickrRepository;
begin
  globalsRepository := TFlickrGlobals.Create();
  repository := TFlickrRepository.create();
  organic := TFlickrOrganic.Create;
  description := nil;
  try
    repository.Load('flickrRepository.xml');
    globalsRepository.load('flickrRepositoryGlobal.xml');
    organic.Load('flickrOrganic.xml');
    options := TOptions.New().Load;
    description := THtmlComposer.getMessage(repository, globalsRepository, organic);
    TFlickrEmail.SendHTML(options.eMailAddress, description);
  finally
    description.free;
  end;
end;

end.
