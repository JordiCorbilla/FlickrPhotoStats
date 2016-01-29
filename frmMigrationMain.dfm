object frmMigration: TfrmMigration
  Left = 0
  Top = 0
  Caption = 'Flickr Photo Migration Tool'
  ClientHeight = 355
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 495
    Height = 13
    Caption = 
      'This utility will help you to migrate any repository from 4.7.0.' +
      '1 version to 4.8.0.2 version'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 69
    Width = 46
    Height = 13
    Caption = 'Progress:'
  end
  object Button1: TButton
    Left = 208
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Migrate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 88
    Width = 511
    Height = 17
    Smooth = True
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 8
    Top = 111
    Width = 511
    Height = 236
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
