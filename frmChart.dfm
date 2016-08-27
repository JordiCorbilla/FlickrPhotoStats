object frmChartViewer: TfrmChartViewer
  Left = 0
  Top = 0
  Caption = 'Chart Viewer'
  ClientHeight = 675
  ClientWidth = 1195
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ChartViewer: TChart
    Left = 0
    Top = 0
    Width = 1195
    Height = 675
    Legend.Visible = False
    Title.Font.Color = clWhite
    Title.Text.Strings = (
      'Total Views')
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
    ColorPaletteIndex = 18
    object LineSeries4: TLineSeries
      Marks.Shadow.Color = 8487297
      Marks.DrawEvery = 10
      Title = 'Flickr Stats'
      Brush.BackColor = clDefault
      LinePen.Color = 10708548
      Pointer.Brush.Gradient.EndColor = 10708548
      Pointer.Gradient.EndColor = 10708548
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.DateTime = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
end
