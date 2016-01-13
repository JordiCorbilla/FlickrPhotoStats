object frmUserList: TfrmUserList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Users that faved the picture'
  ClientHeight = 604
  ClientWidth = 925
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
  object Splitter2: TSplitter
    Left = 465
    Top = 0
    Height = 604
    ExplicitLeft = 457
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 604
    Align = alLeft
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 463
      Height = 35
      Align = alTop
      Caption = 'Added'
      TabOrder = 0
    end
    object ListView1: TListView
      Left = 1
      Top = 36
      Width = 463
      Height = 567
      Align = alClient
      Columns = <
        item
          Caption = 'Id'
          Width = 120
        end
        item
          Caption = 'Title'
          Width = 200
        end
        item
          Caption = 'Location'
          Width = 150
        end>
      RowSelect = True
      PopupMenu = PopupMenu1
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
  object Panel2: TPanel
    Left = 468
    Top = 0
    Width = 457
    Height = 604
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 455
      Height = 35
      Align = alTop
      Caption = 'Lost'
      TabOrder = 0
    end
    object ListView2: TListView
      Left = 1
      Top = 36
      Width = 455
      Height = 567
      Align = alClient
      Columns = <
        item
          Caption = 'Id'
          Width = 120
        end
        item
          Caption = 'Title'
          Width = 200
        end
        item
          Caption = 'Location'
          Width = 150
        end>
      RowSelect = True
      PopupMenu = PopupMenu2
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
  object PopupMenu1: TPopupMenu
    Images = frmFlickrMain.ImageList1
    Left = 180
    Top = 168
    object ShowUser1: TMenuItem
      Caption = 'Show User'
      SubMenuImages = frmFlickrMain.ImageList1
      ImageIndex = 73
      OnClick = ShowUser1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Images = frmFlickrMain.ImageList1
    Left = 568
    Top = 168
    object ShowUser2: TMenuItem
      Caption = 'Show User'
      SubMenuImages = frmFlickrMain.ImageList1
      ImageIndex = 73
      OnClick = ShowUser2Click
    end
  end
end
