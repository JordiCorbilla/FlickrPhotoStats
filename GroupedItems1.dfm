object SplitForm: TSplitForm
  Left = -183
  Top = 480
  BorderStyle = bsNone
  Caption = 'SplitForm'
  ClientHeight = 1000
  ClientWidth = 2000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  Touch.GestureManager = GestureManager1
  WindowState = wsMaximized
  OnGesture = FormGesture
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 23
  object ScrollBox2: TScrollBox
    Left = 0
    Top = 140
    Width = 2000
    Height = 860
    VertScrollBar.Visible = False
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanInertia, igoParentPassthrough]
    OnResize = ScrollBox2Resize
    object GroupPanel1: TPanel
      Left = 118
      Top = 4
      Width = 1009
      Height = 559
      BevelOuter = bvNone
      TabOrder = 0
      object FlowPanel1: TFlowPanel
        Left = 0
        Top = 56
        Width = 969
        Height = 716
        BevelEdges = []
        BevelOuter = bvNone
        BevelWidth = 5
        BorderWidth = 5
        FlowStyle = fsTopBottomLeftRight
        TabOrder = 0
        object GroupPanel1_1: TPanel
          Left = 5
          Top = 5
          Width = 250
          Height = 250
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 0
          OnClick = GroupPanel1_1Click
          object Image1: TImage
            Left = 5
            Top = 5
            Width = 240
            Height = 240
            Align = alClient
            OnClick = GroupPanel1_1Click
            ExplicitHeight = 250
          end
          object Panel5: TPanel
            Left = 5
            Top = 165
            Width = 240
            Height = 80
            BevelOuter = bvNone
            Color = clGradientActiveCaption
            ParentBackground = False
            TabOrder = 0
            object Label3: TLabel
              Left = 0
              Top = 8
              Width = 89
              Height = 20
              Caption = 'Group Title: 1'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label4: TLabel
              Left = 0
              Top = 48
              Width = 87
              Height = 15
              Caption = 'Group subtitle: 1'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
          end
        end
        object GroupPanel1_2: TPanel
          Left = 5
          Top = 256
          Width = 250
          Height = 250
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 1
          object Image2: TImage
            Left = 5
            Top = 5
            Width = 240
            Height = 240
            Align = alClient
            OnClick = GroupPanel1_1Click
            ExplicitHeight = 250
          end
          object Panel6: TPanel
            Left = 5
            Top = 165
            Width = 240
            Height = 80
            BevelOuter = bvNone
            Color = cl3DDkShadow
            ParentBackground = False
            TabOrder = 0
            object Label5: TLabel
              Left = 0
              Top = 6
              Width = 89
              Height = 20
              Caption = 'Group Title: 2'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label9: TLabel
              Left = 0
              Top = 47
              Width = 87
              Height = 15
              Caption = 'Group subtitle: 2'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
          end
        end
        object GroupPanel1_3: TPanel
          Left = 255
          Top = 5
          Width = 250
          Height = 250
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 2
          object Image3: TImage
            Left = 5
            Top = 5
            Width = 240
            Height = 240
            Align = alClient
            OnClick = GroupPanel1_1Click
            ExplicitLeft = 0
            ExplicitTop = 3
            ExplicitWidth = 250
            ExplicitHeight = 250
          end
          object Panel4: TPanel
            Left = 5
            Top = 165
            Width = 240
            Height = 80
            BevelOuter = bvNone
            Color = clBtnShadow
            ParentBackground = False
            TabOrder = 0
            object Label6: TLabel
              Left = 2
              Top = 8
              Width = 89
              Height = 20
              Caption = 'Group Title: 3'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label8: TLabel
              Left = 1
              Top = 48
              Width = 87
              Height = 15
              Caption = 'Group subtitle: 3'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
          end
        end
        object GroupPanel1_4: TPanel
          Left = 255
          Top = 256
          Width = 250
          Height = 250
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 3
          object Image4: TImage
            Left = 5
            Top = 5
            Width = 240
            Height = 240
            Align = alClient
            OnClick = GroupPanel1_1Click
            ExplicitLeft = 0
            ExplicitTop = 3
            ExplicitWidth = 250
            ExplicitHeight = 250
          end
          object Panel3: TPanel
            Left = 5
            Top = 165
            Width = 240
            Height = 80
            BevelOuter = bvNone
            Color = clBtnShadow
            ParentBackground = False
            TabOrder = 0
            object Label2: TLabel
              Left = 2
              Top = 8
              Width = 89
              Height = 20
              Caption = 'Group Title: 4'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label7: TLabel
              Left = 1
              Top = 48
              Width = 87
              Height = 15
              Caption = 'Group subtitle: 4'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
          end
        end
        object GroupPanel1_5: TPanel
          Left = 505
          Top = 5
          Width = 250
          Height = 250
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 4
          OnClick = GroupPanel1_1Click
          object Image5: TImage
            Left = 5
            Top = 5
            Width = 240
            Height = 240
            Align = alClient
            OnClick = GroupPanel1_1Click
            ExplicitHeight = 250
          end
          object Panel7: TPanel
            Left = 5
            Top = 165
            Width = 240
            Height = 80
            BevelOuter = bvNone
            Color = clGradientActiveCaption
            ParentBackground = False
            TabOrder = 0
            object Label10: TLabel
              Left = 0
              Top = 8
              Width = 89
              Height = 20
              Caption = 'Group Title: 5'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object Label11: TLabel
              Left = 0
              Top = 48
              Width = 87
              Height = 15
              Caption = 'Group subtitle: 5'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 2000
    Height = 140
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 120
      Top = 42
      Width = 96
      Height = 74
      Caption = 'Title'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -56
      Font.Name = 'Segoe UI Light'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
  end
  object AppBar: TPanel
    Left = 0
    Top = 924
    Width = 2000
    Height = 75
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object CloseButton: TImage
      Left = 1940
      Top = 0
      Width = 60
      Height = 75
      Align = alRight
      Center = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000280000
        002808060000008CFEB86D000000097048597300000B1300000B1301009A9C18
        00000A4F6943435050686F746F73686F70204943432070726F66696C65000078
        DA9D53675453E9163DF7DEF4424B8880944B6F5215082052428B801491262A21
        09104A8821A1D91551C1114545041BC8A088038E8E808C15512C0C8A0AD807E4
        21A28E83A3888ACAFBE17BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C
        9648335135800CA9421E11E083C7C4C6E1E42E40810A2470001008B3642173FD
        230100F87E3C3C2B22C007BE000178D30B0800C04D9BC0301C87FF0FEA42995C
        01808401C07491384B08801400407A8E42A600404601809D98265300A0040060
        CB6362E300502D0060277FE6D300809DF8997B01005B94211501A09100201365
        884400683B00ACCF568A450058300014664BC43900D82D00304957664800B0B7
        00C0CE100BB200080C00305188852900047B0060C8232378008499001446F257
        3CF12BAE10E72A00007899B23CB9243945815B082D710757572E1E28CE49172B
        14366102619A402EC27999193281340FE0F3CC0000A0911511E083F3FD78CE0E
        AECECE368EB60E5F2DEABF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2F
        B31A803B06806DFEA225EE04685E0BA075F78B66B20F40B500A0E9DA57F370F8
        7E3C3C45A190B9D9D9E5E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3C
        FCF7F5E0BEE22481325D814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB7
        0BFFFC1DD322C44962B9582A14E35112718E449A8CF332A52289429229C525D2
        FF64E2DF2CFB033EDF3500B06A3E017B912DA85D6303F64B27105874C0E2F700
        00F2BB6FC1D4280803806883E1CF77FFEF3FFD47A02500806649927100005E44
        242E54CAB33FC708000044A0812AB0411BF4C1182CC0061CC105DCC10BFC6036
        844224C4C24210420A64801C726029AC82422886CDB01D2A602FD4401D34C051
        688693700E2EC255B80E3D700FFA61089EC128BC81090441C808136121DA8801
        628A58238E08179985F821C14804128B2420C9881451224B91354831528A5420
        55481DF23D720239875C46BA913BC8003282FC86BC47319481B2513DD40CB543
        B9A8371A8446A20BD06474319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F
        3E43C730C0E8180733C46C302EC6C342B1382C099363CBB122AC0CABC61AB056
        AC03BB89F563CFB17704128145C0093604774220611E4148584C584ED848A820
        1C243411DA093709038451C2272293A84BB426BA11F9C4186232318758482C23
        D6128F132F107B8843C437241289433227B9900249B1A454D212D246D26E5223
        E92CA99B34481A2393C9DA646BB20739942C202BC885E49DE4C3E433E41BE421
        F25B0A9D624071A4F853E22852CA6A4A19E510E534E5066598324155A39A52DD
        A8A15411358F5A42ADA1B652AF5187A81334759A39CD8316494BA5ADA295D31A
        681768F769AFE874BA11DD951E4E97D057D2CBE947E897E803F4770C0D861583
        C7886728199B18071867197718AF984CA619D38B19C754303731EB98E7990F99
        6F55582AB62A7C1591CA0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB54
        8FA95E537DAE46553353E3A909D496AB55AA9D50EB531B5367A93BA887AA67A8
        6F543FA47E59FD890659C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B
        0DAB86758135C426B1CDD97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352
        F394663F07E39871F89C744E09E728A797F37E8ADE14EF29E2291BA6344CB931
        655C6BAA96979658AB48AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A
        275C2747678FCE059DE753D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477
        BF6EA7EE989EBE5E809E4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806
        B30C2406DB0CCE183CC535716F3C1D2FC7DBF151435DC34043A561956197E184
        91B9D13CA3D5468D460F8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE
        9A524DB9A629A63B4C3B4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDF
        B7605A785A2CB6A8B6B86549B2E45AA659EEB6BC6E855A3959A558555A5DB346
        AD9DAD25D6BBADBBA711A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806
        DBAEB66DB67D6167621767B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D
        5A1D7E73B472143A563ADE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613
        CB29C4699D539BD347671767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BD
        E44A74F5715DE17AD2F59D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E
        593373D0C3C843E051E5D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F91
        57ADD7B0B7A577AAF761EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8
        B7CB4FC36F9E5F85DF437F23FF64FF7AFFD100A78025016703898141815B02FB
        F87A7C21BF8E3F3ADB65F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC
        90AD21F7E798CE91CE690E85507EE8D6D00761E6618BC37E0C2785878557863F
        8E7088581AD131973577D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3E
        AA2E6A3CDA37BA34BA3FC62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDF
        FCEDF387E29DE20BE37B17982FC85D7079A1CEC2F485A716A92E122C3A96404C
        884E3894F041102AA8168C25F21377258E0A79C21DC267222FD136D188D8435C
        2A1E4EF2482A4D7A92EC91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A
        9E169A76206D323D3ABD31839291907142AA214D93B667EA67E66676CBAC6585
        B2FEC56E8BB72F1E9507C96BB390AC05592D0AB642A6E8545A28D72A07B26765
        5766BFCD89CA3996AB9E2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A586
        4B572D1D58E6BDAC6A39B23C7179DB0AE315052B865606AC3CB88AB62A6DD54F
        ABED5797AE7EBD267A4D6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D
        4F582F59DFB561FA869D1B3E15898AAE14DB1797157FD828DC78E51B876FCABF
        99DC94B4A9ABC4B964CF66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40D
        DF56B4EDF5F645DB2F97CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A4
        54F454FA5436EED2DDB561D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB
        5501554DD566D565FB49FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203
        FD07230EB6D7B9D4D51DD23D54528FD62BEB470EC71FBEFE9DEF772D0D360D55
        8D9CC6E223704479E4E9F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A
        429AF29A469B539AFB5B625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794A
        F354C969DAE982D39367F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F
        6FEFBA1074E1D245FF8BE73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6D
        EA74EA3CFE93D34FC7BB9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4
        BD79F116FFD6D59E393DDDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727
        EEADBC4FBC5FF440ED41D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7
        068583CFFE91F58F0F43058F998FCB860D86EB9E383E3939E23F72FDE9FCA743
        CF64CF269E17FEA2FECBAE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5
        FDEAC0EB19AFDBC6C2C61EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE4
        7C207F28FF68F9B1F553D0A7FB93199393FF040398F3FC63332DDB000003A349
        44415478DACD994B48155118C7CFADE84DD1838A164116454F082DDA25A1518B
        425133B5C7269468D3AACC8888C8B2559B8AA44D0F7B47510BC3246C176504BD
        24C98216515189A16592DD7E5FE74C0CF7CEDC7B66BCCA0CFC38F79EF9E67FFE
        77CE39DF393337A6021EF1787C34C50CC881D5B01CB2606C42E80F780B8FE13E
        B4C2C7582CD61BA4BD580063632872A1148A607CC0DFD60D37E00AB460F467C6
        0C626E0D4515147A5CD30E1DF005869BBA7E980A73605EA21CDC84D3986C1A90
        4173D70EC27698E43AF50C2EC143F8009FE03B0C33E7FFC004980E33612594C1
        529746279C8103A9EEA6AF41CCCD965F09F9AEEA36A88526443FDBDC7D97DE34
        0AE9891A58E03A754F7A07BD77D606119B4BD1002B4C55179C80434107B987B6
        4CB2FDB013269AEA475081F69BB4061198A5F460CE3155AF611717DF1D88318F
        76D6521C87F9A64A667911EDBCF735C84532336500E799AA5750CC456D9934E7
        6A4FBAFA3A2C3455CD50487BDD7E06EB28769BAF2F60E360994B307915169BAA
        63B4B927C9208172D76EC138A5C7DCA64C776B0A93D2DD97951E933D5040DBCD
        FF0D123052E9045A60AEA92560DF509873993CACF40C57E64695E2A1CF31B8CE
        548A51E9D2659CFC35C40647513C553A05F599BBD81833D3BE1EB698D8CD9C68
        B010CC26EE89455C0971D72C4D56505C305FCF4365CCA495974AAFADCF2117C1
        6F698464101F856A62EB2CE2EA89ABB2303899A2059628BD762F1283C57C707E
        610D4247D288542ABDC23887A7499739E7A823AEDAC2E45EA5572B394AC4E049
        3EEC507A81CF43A4258D80ACC9B23C65FB99F4302743219F984E0B83AB94CE87
        23E0941894FD9AAC1A92943720D26121E26B7220E68CB6EC7EEE28BD0B6A1583
        B24D9A02B7A11CA11E4B212F9332544AC29A33BA920B65A72499E5AB18947422
        E94566CF56C4E201C4BC4C8636673425F59D553AABF48941D98B49AAB9680CF6
        07141493B2D067B9AA65AB9F13D49CD1934DEF392887DE4C184C1C73CE913205
        0531E874B124C66D01BBD8CF5C68935E5DEC4C92462843B02BA4391973928C25
        47FAA6200BDDA449E2A41979F8598F587B4873FF26844D9E4CA39D94669C44FD
        5BE944FD208D40E24A92345B7D4CCA7347BD85C1A4441D74A97337EE9B4A6CE3
        3CAE4B5AEAC26C162699BB5895AA51DB3857BCE76621D4766B300ECFED963911
        DD0DAB3919ED2DBF0988EE43932B30BA8F9D2638DA0FEEE6A2E8BEFA705D1CDD
        97472EA1E8BE7E738946F7056682D168BE02F6B89BB92A8A2FD1138C0EE9DF10
        7F018E704185241CEA460000000049454E44AE426082}
      OnClick = CloseButtonClick
      ExplicitLeft = 1936
    end
  end
  object GestureManager1: TGestureManager
    Left = 296
    Top = 64
    GestureData = <
      item
        Control = Owner
        Collection = <
          item
            Action = Action1
            GestureID = sgiUp
          end>
      end>
  end
  object ActionList1: TActionList
    Left = 376
    Top = 64
    object Action1: TAction
      Caption = 'Action1'
      OnExecute = Action1Execute
    end
  end
end
