object frmFlickrContext: TfrmFlickrContext
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Show items'
  ClientHeight = 569
  ClientWidth = 589
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
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 589
    Height = 569
    Align = alClient
    Columns = <
      item
        Caption = 'Id'
        Width = 120
      end
      item
        Caption = 'Title'
        Width = 330
      end
      item
        Caption = 'Added'
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 467
  end
end
