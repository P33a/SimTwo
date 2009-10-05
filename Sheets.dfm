object FSheets: TFSheets
  Left = 247
  Top = 120
  Width = 870
  Height = 640
  Caption = 'Sheets'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelFormula: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 29
    Align = alTop
    TabOrder = 0
    OnClick = PanelFormulaClick
    object SpeedButtonOK: TSpeedButton
      Left = 176
      Top = 2
      Width = 24
      Height = 24
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777888777777
        777777770000774448877777777777770000772244887777777777770000A222
        22488777777777770000A22222248877777777770000A2222222488777777777
        0000A22222222488777777770000A22222222248877777770000A22248A22224
        887777770000A222488A2222488777770000A2224887A2224488777700007A22
        48877A222488777700007A22477777A222488777000077777777777A22244877
        0000777777777777A222488700007777777777777A2224870000777777777777
        77A224480000777777777777777A224800007777777777777777A24800007777
        7777777777777A270000}
      OnClick = SpeedButtonOKClick
    end
    object CBNames: TComboBox
      Left = 4
      Top = 4
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object EditFormula: TEdit
      Left = 204
      Top = 4
      Width = 653
      Height = 21
      TabOrder = 1
      OnExit = EditFormulaExit
      OnKeyDown = EditFormulaKeyDown
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 574
    Width = 862
    Height = 20
    Panels = <
      item
        Alignment = taCenter
        Text = 'Line:Col'
        Width = 60
      end
      item
        Alignment = taCenter
        Width = 50
      end
      item
        Alignment = taCenter
        Width = 55
      end
      item
        Alignment = taCenter
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object PageControl: TPageControl
    Left = 0
    Top = 29
    Width = 862
    Height = 545
    ActivePage = TabGlobal
    Align = alClient
    TabOrder = 2
    TabPosition = tpBottom
    object TabGlobal: TTabSheet
      Caption = 'TabGlobal'
      object SGGlobal: TStringGrid
        Left = 0
        Top = 0
        Width = 854
        Height = 519
        Align = alClient
        ColCount = 32
        Ctl3D = False
        DefaultColWidth = 50
        DefaultRowHeight = 16
        RowCount = 128
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing]
        ParentCtl3D = False
        TabOrder = 0
        OnDrawCell = SGGlobalDrawCell
        OnKeyDown = SGGlobalKeyDown
        OnKeyPress = SGGlobalKeyPress
        OnKeyUp = SGGlobalKeyUp
        OnMouseDown = SGGlobalMouseDown
        OnMouseUp = SGGlobalMouseUp
        ColWidths = (
          20
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50
          50)
      end
    end
  end
  object FormStorage: TFormStorage
    IniSection = 'Sheets'
    UseRegistry = False
    StoredProps.Strings = (
      'PageControl.ActivePage')
    StoredValues = <>
    Left = 236
    Top = 60
  end
  object MainMenu: TMainMenu
    Left = 272
    Top = 61
    object MenuFile: TMenuItem
      Caption = '&File'
      object MenuSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = MenuSaveClick
      end
      object MenuReLoad: TMenuItem
        Caption = '&ReLoad'
        ShortCut = 16466
        OnClick = MenuReLoadClick
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 304
    Top = 61
    object MenuButton: TMenuItem
      AutoCheck = True
      Caption = '&Button'
      OnClick = MenuButtonClick
    end
  end
end
