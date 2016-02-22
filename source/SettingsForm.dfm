object Form2: TForm2
  Left = 285
  Top = 183
  BorderStyle = bsDialog
  Caption = 'set smt'
  ClientHeight = 344
  ClientWidth = 520
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PrintScale = poPrintToFit
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object apply: TButton
    Left = 8
    Top = 312
    Width = 225
    Height = 25
    Caption = 'Save (Ctrl + S)'
    TabOrder = 0
    OnClick = applyClick
  end
  object cancel: TButton
    Left = 304
    Top = 312
    Width = 209
    Height = 25
    Caption = 'Cancel (Ctrl + Q)'
    TabOrder = 1
    OnClick = cancelClick
  end
  object table: TStringGrid
    Left = 0
    Top = 0
    Width = 520
    Height = 273
    Align = alTop
    BorderStyle = bsNone
    ColCount = 8
    Ctl3D = False
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    ParentCtl3D = False
    TabOrder = 2
    RowHeights = (
      24
      24)
  end
  object addrow: TButton
    Left = 8
    Top = 280
    Width = 177
    Height = 25
    Caption = 'Add row (Ctrl + M)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = addrowClick
  end
  object remrow: TButton
    Left = 336
    Top = 280
    Width = 177
    Height = 25
    Caption = 'Remove last row (Ctrl + R)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = remrowClick
  end
  object regularPolygon: TButton
    Left = 200
    Top = 280
    Width = 121
    Height = 25
    Caption = 'Set regular polygon...'
    TabOrder = 5
    Visible = False
    OnClick = regularPolygonClick
  end
end
