object Form1: TForm1
  Left = 519
  Top = 25
  Width = 817
  Height = 608
  Caption = 'Chaos Game'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 0
    Top = 0
    Width = 129
    Height = 20
    Align = alLeft
    AutoSize = False
    Caption = 'State:'
    Constraints.MaxHeight = 20
  end
  object Chart1: TChart
    Left = 129
    Top = 0
    Width = 680
    Height = 574
    BackWall.Brush.Color = clWhite
    BackWall.Color = clWhite
    BackWall.Pen.Visible = False
    MarginBottom = 0
    MarginLeft = 0
    MarginRight = 0
    MarginTop = 0
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    AxisVisible = False
    BackColor = clWhite
    Chart3DPercent = 1
    ClipPoints = False
    Frame.Visible = False
    LeftAxis.Labels = False
    LeftAxis.Visible = False
    Legend.Visible = False
    RightAxis.Automatic = False
    RightAxis.AutomaticMaximum = False
    RightAxis.AutomaticMinimum = False
    RightAxis.Axis.Visible = False
    RightAxis.Grid.Visible = False
    View3D = False
    View3DWalls = False
    Align = alClient
    Color = clWhite
    TabOrder = 0
    object pointer: TPointSeries
      Marks.ArrowLength = 0
      Marks.Visible = False
      SeriesColor = clRed
      Title = 'initialPoint'
      Pointer.HorizSize = 1
      Pointer.InflateMargins = True
      Pointer.Pen.Visible = False
      Pointer.Style = psRectangle
      Pointer.VertSize = 1
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loNone
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object initialVertices: TPointSeries
      Marks.ArrowLength = 0
      Marks.Visible = False
      SeriesColor = clGreen
      Title = 'initialVertices'
      Pointer.HorizSize = 1
      Pointer.InflateMargins = True
      Pointer.Pen.Visible = False
      Pointer.Style = psRectangle
      Pointer.VertSize = 1
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loNone
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object algoPoints: TPointSeries
      Marks.ArrowLength = 0
      Marks.Visible = False
      SeriesColor = clBlack
      Title = 'algoPoints'
      Pointer.Brush.Color = clBlack
      Pointer.HorizSize = 1
      Pointer.InflateMargins = True
      Pointer.Pen.Visible = False
      Pointer.Style = psCircle
      Pointer.VertSize = 1
      Pointer.Visible = True
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loNone
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
  end
  object clear_btn: TButton
    Left = 16
    Top = 464
    Width = 105
    Height = 25
    Caption = 'Clear'
    TabOrder = 1
    OnClick = clear_btnClick
  end
  object iterationsCount_edt: TLabeledEdit
    Left = 0
    Top = 104
    Width = 129
    Height = 21
    EditLabel.Width = 76
    EditLabel.Height = 13
    EditLabel.Caption = 'Iterations count:'
    LabelSpacing = 0
    TabOrder = 2
    Text = '42'
    OnKeyPress = iterationsCount_edtKeyPress
  end
  object drawFast_btn: TButton
    Left = 16
    Top = 432
    Width = 105
    Height = 25
    Caption = 'Draw'
    TabOrder = 3
    OnClick = drawFast_btnClick
  end
  object setVertices_btn: TButton
    Left = 8
    Top = 192
    Width = 105
    Height = 25
    Caption = 'Set vertices...'
    TabOrder = 4
    OnClick = setVertices_btnClick
  end
  object showVertices_cb: TCheckBox
    Left = 16
    Top = 160
    Width = 97
    Height = 17
    Caption = 'Show vertices'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = showVertices_cbClick
  end
  object showPointer_cb: TCheckBox
    Left = 16
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Show pointer'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = showPointer_cbClick
  end
  object m_parameter: TLabeledEdit
    Left = 8
    Top = 376
    Width = 113
    Height = 21
    EditLabel.Width = 11
    EditLabel.Height = 13
    EditLabel.Caption = 'm:'
    TabOrder = 7
    OnKeyPress = m_parameterKeyPress
  end
  object setSP_btn: TButton
    Left = 8
    Top = 224
    Width = 105
    Height = 25
    Caption = 'Set start points...'
    TabOrder = 8
    OnClick = setSP_btnClick
  end
  object save_btn: TButton
    Left = 16
    Top = 496
    Width = 105
    Height = 25
    Caption = 'Save...'
    TabOrder = 9
    OnClick = save_btnClick
  end
  object load_btn: TButton
    Left = 16
    Top = 528
    Width = 105
    Height = 25
    Caption = 'Load...'
    TabOrder = 10
    OnClick = load_btnClick
  end
  object mode_gb: TGroupBox
    Left = 0
    Top = 272
    Width = 129
    Height = 65
    Caption = ' Mode  '
    TabOrder = 11
    object mode_poly: TRadioButton
      Left = 0
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Polygonal'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = mode_polyClick
    end
    object mode_affine: TRadioButton
      Left = 0
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Affine transformations'
      TabOrder = 1
      OnClick = mode_affineClick
    end
  end
  object logs: TMemo
    Left = 0
    Top = 16
    Width = 129
    Height = 65
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 12
  end
  object savdlg: TSaveDialog
    DefaultExt = 'cgmf'
    FileName = '*.cgmf'
    Filter = 'all files (*.*)|*.*|chaos game main files (*.cgmf)|*.cgmf'
    Options = [ofHideReadOnly, ofNoNetworkButton, ofEnableSizing, ofDontAddToRecent]
    Left = 777
  end
  object opndlg: TOpenDialog
    DefaultExt = 'cgmf'
    Filter = 'all files (*.*)|*.*|chaos game main files (*.cgmf)|*.cgmf'
    Options = [ofHideReadOnly, ofNoNetworkButton, ofEnableSizing, ofDontAddToRecent]
    Left = 737
  end
end
