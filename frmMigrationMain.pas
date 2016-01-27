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
  flickr.lib.options, flickr.lib.options.email;

type
  TfrmMigration = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMigration: TfrmMigration;

implementation

{$R *.dfm}

procedure TfrmMigration.Button1Click(Sender: TObject);
var
  buttonSelected: integer;
  repository: IFlickrRepository;
  options: IOptions;
  optionsEmail : IOptionsEmail;
begin
  buttonSelected := MessageDlg('Attention, this operation cannot be undone, are you sure you want to migrate version?' +
    sLinebreak + 'Please remember to backup your repository files first!',
    mtCustom, [mbYes, mbCancel], 0);
  if buttonSelected = mrYes then
  begin
    repository := TFlickrRepository.Create();
    options := TOptions.New().Load;
    optionsEmail := TOptionsEmail.New().Load;
    try
      repository.Load(options.Workspace + '\flickrRepository.xml');
    finally

    end;
  end;
end;

end.
