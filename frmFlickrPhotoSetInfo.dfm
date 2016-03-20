object frmFlickrPhotoSet: TfrmFlickrPhotoSet
  Left = 0
  Top = 0
  Caption = 'PhotoSet info'
  ClientHeight = 541
  ClientWidth = 1144
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 234
    Width = 1144
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 0
    ExplicitWidth = 488
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 75
    Width = 1144
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 0
    ExplicitWidth = 364
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 385
    Width = 1144
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = 8
    ExplicitTop = 421
    ExplicitWidth = 1074
  end
  object Panel1: TPanel
    Left = 0
    Top = 388
    Width = 1144
    Height = 153
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
    object chartItemCommentsH: TChart
      Left = 1
      Top = 1
      Width = 1142
      Height = 151
      Legend.Visible = False
      Title.Font.Color = clWhite
      Title.Text.Strings = (
        'Total Comments Histogram')
      BottomAxis.DateTimeFormat = 'dd/mm/yyyy'
      BottomAxis.Grid.Width = 0
      BottomAxis.Grid.ZPosition = 1.000000000000000000
      BottomAxis.Increment = 1.000000000000000000
      BottomAxis.LabelsFormat.Font.Color = clWhite
      BottomAxis.LabelsMultiLine = True
      BottomAxis.MinimumOffset = 16
      BottomAxis.MinorTickCount = 16
      BottomAxis.EndPosition = 98.000000000000000000
      BottomAxis.PositionPercent = -1.000000000000000000
      BottomAxis.TickLength = 2
      BottomAxis.Ticks.Width = 0
      BottomAxis.Title.Font.Color = clLime
      DepthAxis.Title.Font.Color = clLime
      DepthTopAxis.Title.Font.Color = clLime
      LeftAxis.Grid.Width = 0
      LeftAxis.LabelsFormat.Font.Color = clWhite
      LeftAxis.MinorTicks.Width = 0
      LeftAxis.Ticks.Width = 0
      LeftAxis.TicksInner.Width = 0
      LeftAxis.Title.Font.Color = clLime
      RightAxis.Title.Font.Color = clLime
      TopAxis.Title.Font.Color = clLime
      View3D = False
      Align = alClient
      Color = 13135884
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      PrintMargins = (
        15
        7
        15
        7)
      ColorPaletteIndex = 13
      object BarSeries2: TBarSeries
        BarBrush.BackColor = clDefault
        BarPen.Color = 10708548
        Marks.Shadow.Color = 8487297
        Marks.Visible = False
        Marks.Callout.Length = 8
        Marks.DrawEvery = 10
        Title = 'Flickr Stats'
        XValues.DateTime = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Bar'
        YValues.Order = loNone
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 237
    Width = 1144
    Height = 148
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 1
    object chartitemLikesH: TChart
      Left = 1
      Top = 1
      Width = 1142
      Height = 146
      Legend.Visible = False
      Title.Font.Color = clWhite
      Title.Text.Strings = (
        'Total Likes Histogram')
      BottomAxis.DateTimeFormat = 'dd/mm/yyyy'
      BottomAxis.Grid.Width = 0
      BottomAxis.Grid.ZPosition = 1.000000000000000000
      BottomAxis.Increment = 1.000000000000000000
      BottomAxis.LabelsFormat.Font.Color = clWhite
      BottomAxis.LabelsMultiLine = True
      BottomAxis.MinimumOffset = 16
      BottomAxis.MinorTickCount = 16
      BottomAxis.EndPosition = 98.000000000000000000
      BottomAxis.PositionPercent = -1.000000000000000000
      BottomAxis.TickLength = 2
      BottomAxis.Ticks.Width = 0
      BottomAxis.Title.Font.Color = clLime
      DepthAxis.Title.Font.Color = clLime
      DepthTopAxis.Title.Font.Color = clLime
      LeftAxis.Grid.Width = 0
      LeftAxis.LabelsFormat.Font.Color = clWhite
      LeftAxis.MinorTicks.Width = 0
      LeftAxis.Ticks.Width = 0
      LeftAxis.TicksInner.Width = 0
      LeftAxis.Title.Font.Color = clLime
      RightAxis.Title.Font.Color = clLime
      TopAxis.Title.Font.Color = clLime
      View3D = False
      Align = alClient
      Color = 13135884
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      PrintMargins = (
        15
        7
        15
        7)
      ColorPaletteIndex = 13
      object BarSeries8: TBarSeries
        BarBrush.BackColor = clDefault
        BarPen.Color = 10708548
        Marks.Shadow.Color = 8487297
        Marks.Visible = False
        Marks.Callout.Length = 8
        Marks.DrawEvery = 10
        Title = 'Flickr Stats'
        XValues.DateTime = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Bar'
        YValues.Order = loNone
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 78
    Width = 1144
    Height = 156
    Align = alTop
    Caption = 'Panel3'
    TabOrder = 2
    object ChartItemViewsH: TChart
      Left = 1
      Top = 1
      Width = 1142
      Height = 154
      Legend.Visible = False
      Title.Font.Color = clWhite
      Title.Text.Strings = (
        'Total Views Histogram')
      BottomAxis.DateTimeFormat = 'dd/mm/yyyy'
      BottomAxis.Grid.Width = 0
      BottomAxis.Grid.ZPosition = 1.000000000000000000
      BottomAxis.Increment = 1.000000000000000000
      BottomAxis.LabelsFormat.Font.Color = clWhite
      BottomAxis.LabelsMultiLine = True
      BottomAxis.MinimumOffset = 16
      BottomAxis.MinorTickCount = 16
      BottomAxis.EndPosition = 98.000000000000000000
      BottomAxis.PositionPercent = -1.000000000000000000
      BottomAxis.TickLength = 2
      BottomAxis.Ticks.Width = 0
      BottomAxis.Title.Font.Color = clLime
      DepthAxis.Title.Font.Color = clLime
      DepthTopAxis.Title.Font.Color = clLime
      LeftAxis.Grid.Width = 0
      LeftAxis.LabelsFormat.Font.Color = clWhite
      LeftAxis.MinorTicks.Width = 0
      LeftAxis.Ticks.Width = 0
      LeftAxis.TicksInner.Width = 0
      LeftAxis.Title.Font.Color = clLime
      RightAxis.Title.Font.Color = clLime
      TopAxis.Title.Font.Color = clLime
      View3D = False
      Align = alClient
      Color = 13135884
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      PrintMargins = (
        15
        7
        15
        7)
      ColorPaletteIndex = 13
      object BarSeries7: TBarSeries
        BarBrush.BackColor = clDefault
        BarPen.Color = 10708548
        Marks.Shadow.Color = 8487297
        Marks.Visible = False
        Marks.Callout.Length = 8
        Marks.DrawEvery = 10
        Title = 'Flickr Stats'
        XValues.DateTime = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Bar'
        YValues.Order = loNone
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 1144
    Height = 75
    Align = alTop
    TabOrder = 3
    object Panel24: TPanel
      Left = 1
      Top = 1
      Width = 328
      Height = 73
      Align = alLeft
      TabOrder = 0
      object Panel27: TPanel
        Left = 9
        Top = 1
        Width = 280
        Height = 67
        BevelOuter = bvNone
        TabOrder = 0
        object Label14: TLabel
          Left = 85
          Top = 10
          Width = 120
          Height = 16
          Caption = 'Total from Photos:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelTodayViews: TLabel
          Left = 208
          Top = 10
          Width = 52
          Height = 16
          Alignment = taRightJustify
          Caption = '999,999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label1: TLabel
          Left = 90
          Top = 26
          Width = 115
          Height = 16
          Caption = 'Total from Album:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label2: TLabel
          Left = 208
          Top = 27
          Width = 52
          Height = 16
          Alignment = taRightJustify
          Caption = '999,999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 168
          Top = 47
          Width = 37
          Height = 16
          Caption = 'Total:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 208
          Top = 47
          Width = 52
          Height = 16
          Alignment = taRightJustify
          Caption = '999,999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Shape1: TShape
          Left = 159
          Top = 46
          Width = 105
          Height = 1
        end
        object Image1: TImage
          Left = 23
          Top = 12
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
            0020080300000044A48AC60000000467414D410000B18F0BFC6105000000BD50
            4C5445000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
            8AC5987E0000003D74524E53002A8ED7F8D60694FD9809BBBD952BFCDA520E8F
            D810F90CF2F366F478EF350D3650F7FB40CE5C0FB8B95BE4F56F029176F1322C
            9375CA6A1213E6E74A262E17AC00000001624B47440088051D48000000097048
            597300000B1200000B1201D2DD7EFC000001404944415478DAD592ED4BC25014
            C69FD36C41A430CB94A2A8A86451624685A3FEFA60F44209F62282985FECD526
            48484EE6E9EE36749BC3BE053D30EEF3ECFC386CE71E824F24047298FDEF4656
            253A94E68AA81B01A8D385A12FD9F63810CFFB1A5FF6C68004E5805BF10DCC7B
            40993B61607E07A8D0BB7029D681878F30A018C05D5B5A6D17309D30B09805EE
            2D69D3DB40F52D0C9C8AE72CC2FF2F20B315F8CDDA7310482BB41118549D9DD7
            11309BA435F7349372D496E186065B5D0F586D1A5EBF8A934047D1BD64AE3424
            B04ECB6E7C5AF25DF74F68F2A300363BE216F022B62D35ACB7064A3F23CE6AA2
            46E98524D0262A43D5ABDECA652B3672CC1A60B5281F073EE946560EA8241666
            9FAF652AF01CF0454733807D81281DAB408F6245A07F1E0914636E73FF58C372
            6B7F054CD2EF80A14CAC0FE884A726D42DED1B97816DB9F36014B40000000049
            454E44AE426082}
          Transparent = True
        end
      end
    end
    object Panel26: TPanel
      Left = 753
      Top = 1
      Width = 390
      Height = 73
      Align = alClient
      TabOrder = 1
      object Panel29: TPanel
        Left = 16
        Top = 1
        Width = 201
        Height = 67
        BevelOuter = bvNone
        TabOrder = 0
        object Label42: TLabel
          Left = 82
          Top = 20
          Width = 37
          Height = 16
          Caption = 'Total:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelTodayComments: TLabel
          Left = 129
          Top = 20
          Width = 52
          Height = 16
          Caption = '999,999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Image3: TImage
          Left = 13
          Top = 12
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
            0020080300000044A48AC60000000467414D410000B18F0BFC6105000000DE50
            4C5445000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
            00F22757980000004874524E53002676BCE6F93FC20898FE12C60AC8BD662406
            9BD740C00DC30F2F3881BBBE65BF23F7E422BA605D80B9822BDCD639FD1193DD
            4399C40B28B213078B097FF64CC5FAE777EA56AB169F52932700000001624B47
            440088051D48000000097048597300000B1200000B1201D2DD7EFC0000019249
            44415478DA7D920B4B024110C767402592A0CC505333533333A4A2844482E841
            1F59A908A494D24222E94124F620CB47844A6AD6367B9E79A79B7FB8BBD9DBDF
            CCCECE0C822CE4DA94CC03C6D5F9DFFEE87003D43A64CD2E3084B80EBD8A3156
            97013D8641A423569380110CF1658272284B1B06C62ADBDC88B30A07C657C94E
            6141E53DC156E89D2C11A0D3D0F9E7F8DA13DFC496298F5613C1BC48FEDA97BE
            0C2C5F14239DC7C9008163CF821CADEF14FA026D0B009947E12DEC7E804B9C9A
            07B8CA0901870FE01A355B00375921E0F402ECA396EA7F5A16028620F5056766
            018E2B0300970720CA8480DBCD8FF0B800EE6F8580D7C9939C9BA6367C08019F
            03E001FD76A9A2FF170A78219EB2B5FE521BAD52A921801632CC6961B3F4056A
            F79289D66F78A68EDFE0ED2EA6F83C18BED7F8AF52B2B319C4FC708ECFA03C30
            602A4AF313A19B1B471551E2FE843CB421AC521E9130E270773BC634BCC0F2D8
            EFD2F359B229DCEB99F60C2A803F35F0CE533D91176AA0857BBDE550013F1885
            0180C05D0588DC1580D8BD0BEC88DD49BF13967DED208B2ABC0000000049454E
            44AE426082}
        end
      end
    end
    object Panel25: TPanel
      Left = 329
      Top = 1
      Width = 424
      Height = 73
      Align = alLeft
      TabOrder = 2
      object Panel28: TPanel
        Left = 8
        Top = 1
        Width = 201
        Height = 67
        BevelOuter = bvNone
        TabOrder = 0
        object Label15: TLabel
          Left = 80
          Top = 20
          Width = 37
          Height = 16
          Caption = 'Total:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelTodayLikes: TLabel
          Left = 130
          Top = 20
          Width = 52
          Height = 16
          Caption = '999,999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Image2: TImage
          Left = 31
          Top = 12
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
            0020080300000044A48AC60000000467414D410000B18F0BFC61050000013250
            4C5445000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000002D6967480000006474524E
            530001499BDCF9F4CD7D14DD30CAEF5A5955F58FF640FC949208DAAA3F0B0526
            6EE9E7AB0765FE6417BB97F12A0F0E03E6044AE40211F8D44645D89A44F3CE88
            502EB80AD31EEEB1A65489877B817EBD9896090DC4B9C0D9AFF766B09C7AFD4E
            F24F292B9FA32778D100000001624B47440088051D4800000009704859730000
            0B1200000B1201D2DD7EFC0000020E4944415478DA7592FF4B536114C69F4388
            2C2B37105DC63233759ADE146D98ABA643FA362CD040512882FEAC8AFAA12808
            A568D50F691A9A2886B6617E09591AD1B0C029D990C8D7F3BEBBEFDD9DDD0EEC
            D939EFF3B9F73D3B3B040ED29196155C82236F9B457AFC29A05E65E0096DB216
            EEF464CAC7624B016EEA868E817578BAAC6AF06F8A8122BA866C3CA74E5BF54C
            FC242F45387B99E9A1431D0FEDC826F2C39C46F7D389366094969453FDB58FF5
            916F51555522048C90FF2C303E6FBED27D9DE569CAAC6A82C018D5B6021373E6
            515D0BCB64DCAC4E9E01DED36DCEA662BAAB53A73194D085119073A8E48BA667
            F55963CA236674E1BBA881BB024ED1D4280179C51D3887F45493B3D38E7E7303
            F0910206109F74045AEA80396AADE5F9AE3902C557814F14AC01163EA41D7C57
            931F98279CAB0696DE3900E7AB80C53142A81278101CF9C76F1BBF097C1EE57D
            68AF0062E9F81E3F4CE5C0F25BB9301D5406AC2663393E5DF0012BE28D5AB94B
            CB3CCD6FAF72802BA5BC0515AF333B89CB4758BE476D7EE430CC671460788FB2
            265F587E6709F4AD0A803750CCFA2391E9C3385EC4BAB66F1016802EF2B0A6BE
            C83FA5F9989B755D0CC006A0DE5FC81ADDDC4241E410671B0BE6EFD600BCA183
            ACBF08E2007FFF1E4E620F00A3DE65E5E9B835952C00EAA77C956C8B87D915B3
            01C00DCA63FD43F7ED23CD99DFADD57224CAEEE1BF00C20C0CE79CEC025C3993
            2E557226C40000000049454E44AE426082}
        end
      end
    end
  end
end
