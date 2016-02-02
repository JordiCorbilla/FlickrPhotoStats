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

program FlickrPhotoAnalyticsTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  flickr.Time.test in 'flickr.Time.test.pas',
  flickr.time in '..\flickr.time.pas',
  flickr.email.test in 'flickr.email.test.pas',
  flickr.lib.email in '..\flickr.lib.email.pas',
  flickr.globals in '..\flickr.globals.pas',
  flickr.stats in '..\flickr.stats.pas',
  flickr.lib.options in '..\flickr.lib.options.pas',
  flickr.lib.email.html in '..\flickr.lib.email.html.pas',
  flickr.organic in '..\flickr.organic.pas',
  flickr.organic.stats in '..\flickr.organic.stats.pas',
  flickr.encoding.test in 'flickr.encoding.test.pas',
  flickr.repository in '..\flickr.repository.pas',
  flickr.lib.options.agent in '..\flickr.lib.options.agent.pas',
  flickr.lib.encoding in '..\flickr.lib.encoding.pas',
  flickr.photos in '..\flickr.photos.pas',
  flickr.albums in '..\flickr.albums.pas',
  flickr.pools in '..\flickr.pools.pas',
  flickr.top.stats in '..\flickr.top.stats.pas',
  flickr.rest.test in 'flickr.rest.test.pas',
  flickr.rest in '..\flickr.rest.pas',
  flickr.signature in '..\flickr.signature.pas',
  flickr.call.methods in '..\flickr.call.methods.pas',
  flickr.lib.utils in '..\flickr.lib.utils.pas',
  flickr.pools.list in '..\flickr.pools.list.pas',
  flickr.list.comparer in '..\flickr.list.comparer.pas',
  flickr.base in '..\flickr.base.pas',
  flickr.photo.trend.info in '..\flickr.photo.trend.info.pas',
  flickr.albums.list in '..\flickr.albums.list.pas',
  flickr.xml.helper in '..\flickr.xml.helper.pas',
  flickr.tracker in '..\flickr.tracker.pas',
  flickr.user.faves in '..\flickr.user.faves.pas',
  flickr.user.tracking in '..\flickr.user.tracking.pas',
  flickr.users.info in '..\flickr.users.info.pas',
  flickr.stats.global in '..\flickr.stats.global.pas',
  flickr.http.lib in '..\flickr.http.lib.pas',
  flickr.lib.procedures in '..\flickr.lib.procedures.pas',
  flickr.lib.response in '..\flickr.lib.response.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    ReportMemoryLeaksOnShutdown := true;
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
