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

unit frmMigrationMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, flickr.repository, UITypes,
  flickr.lib.options, flickr.lib.options.email, flickr.lib.utils;

type
  TfrmMigration = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Log(s : string);
  end;

var
  frmMigration: TfrmMigration;

implementation

uses
  System.Diagnostics, flickr.time;

{$R *.dfm}

procedure TfrmMigration.Button1Click(Sender: TObject);
var
  buttonSelected: integer;
  repository: IFlickrRepository;
  options: IOptions;
  optionsEmail : IOptionsEmail;
  st : TStopWatch;
begin
  buttonSelected := MessageDlg('Attention, this operation cannot be undone, are you sure you want to migrate version?' +
    sLinebreak + 'Please remember to backup your repository files first!',
    mtCustom, [mbYes, mbCancel], 0);
  if buttonSelected = mrYes then
  begin
    Log('Creating repository');
    repository := TFlickrRepository.Create();
    Log('Loading options');
    options := TOptions.New().Load;
    optionsEmail := TOptionsEmail.New().Load;
    try
      repository.version := '4.8.0.2';
      Log('Loading repository. This operation might take a while....');
      st := TStopWatch.Create;
      st.Start;
      progressbar1.Position := 25;
      repository.Load(options.Workspace + '\flickrRepository.xml');
      st.Stop;
      Log('Repository loaded in ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      //Save the repository in the new format
      repository.version := '4.8.0.2';
      repository.DateSaved := Now;
      progressbar1.Position := 50;
      Log('Saving repository using new format');
      st := TStopWatch.Create;
      st.Start;
      repository.save(optionsemail.flickrApiKey, optionsemail.secret, optionsemail.user, options.Workspace + '\flickrRepository.xml');
      st.Stop;
      Log('Repository saved in ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
    finally
      repository := nil;
    end;


    //Now try loading and saving again
    Log('Now testing the new repository...');
    Log('Creating repository');
    repository := TFlickrRepository.Create();
    try
      repository.version := '4.8.0.2';
      Log('Loading repository. This operation might be really fast....');
      st := TStopWatch.Create;
      st.Start;
      progressbar1.Position := 75;
      repository.Load(options.Workspace + '\flickrRepository.xml');
      st.Stop;
      Log('Repository loaded in ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
      //Save the repository in the new format
      repository.version := '4.8.0.2';
      repository.DateSaved := Now;
      Log('Saving repository using new format');
      st := TStopWatch.Create;
      st.Start;
      progressbar1.Position := 100;
      repository.save(optionsemail.flickrApiKey, optionsemail.secret, optionsemail.user, options.Workspace + '\flickrRepository.xml');
      st.Stop;
      Log('Repository saved in ' + TTime.GetAdjustedTime(st.ElapsedMilliseconds));
    finally
      repository := nil;
    end;
  end;
end;

procedure TfrmMigration.Log(s: string);
begin
  Memo1.Lines.Add(DateTimeToStr(now) + ' ' + s);
end;

end.
