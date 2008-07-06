object FChart: TFChart
  Left = 185
  Top = 580
  Width = 715
  Height = 416
  Caption = 'Chart'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    707
    389)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 538
    Top = 368
    Width = 56
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Max Points:'
  end
  object Chart: TChart
    Left = 184
    Top = 0
    Width = 523
    Height = 355
    Legend.Alignment = laTop
    Legend.CheckBoxes = True
    Legend.DividingLines.EndStyle = esFlat
    Legend.DividingLines.Visible = True
    Legend.FontSeriesColor = True
    Legend.HorizMargin = 1
    Legend.LegendStyle = lsSeries
    Legend.Shadow.HorizSize = 0
    Legend.Shadow.VertSize = 0
    Legend.TopPos = 0
    Legend.VertMargin = 1
    MarginBottom = 3
    MarginTop = 3
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    View3D = False
    View3DWalls = False
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
    PrintMargins = (
      15
      23
      15
      23)
  end
  object CBFreeze: TCheckBox
    Left = 464
    Top = 368
    Width = 65
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Freeze'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CBFreezeClick
  end
  object EditMaxPoints: TEdit
    Left = 602
    Top = 364
    Width = 41
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Text = '200'
  end
  object BSet: TButton
    Left = 646
    Top = 364
    Width = 59
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Set'
    TabOrder = 3
    OnClick = BSetClick
  end
  object BSave: TButton
    Left = 198
    Top = 364
    Width = 59
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Save Chart'
    TabOrder = 4
    OnClick = BSaveClick
  end
  object EditFileName: TEdit
    Left = 264
    Top = 364
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    Text = 'Chartdata'
  end
  object TreeView: TTreeView
    Left = 0
    Top = 0
    Width = 185
    Height = 357
    Anchors = [akLeft, akTop, akBottom]
    HideSelection = False
    Images = ILCheckBox
    Indent = 19
    ReadOnly = True
    TabOrder = 6
    OnMouseDown = TreeViewMouseDown
  end
  object BChartRefresh: TButton
    Left = 2
    Top = 364
    Width = 50
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    TabOrder = 7
    OnClick = BChartRefreshClick
  end
  object BSaveLog: TButton
    Left = 394
    Top = 364
    Width = 59
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Save Log'
    TabOrder = 8
    OnClick = BSaveLogClick
  end
  object BClear: TButton
    Left = 132
    Top = 364
    Width = 50
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 9
    OnClick = BClearClick
  end
  object FormStorage: TFormStorage
    IniSection = 'FastChart'
    UseRegistry = False
    StoredProps.Strings = (
      'EditMaxPoints.Text'
      'EditFileName.Text')
    StoredValues = <>
    Left = 252
    Top = 72
  end
  object ILCheckBox: TImageList
    Left = 60
    Top = 76
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000009494000094
      9400009494000094940000949400009494000094940000949400009494000094
      940000949400009494000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00FFFFFF000000000000000000000000007B7B7B00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00FFFFFF000000000000000000000000000000000000949400F7F7
      F70094CEFF0094FFFF0094CEFF0094FFFF0094CEFF0094CEFF0094CEFF0094CE
      FF0063CECE00009494000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000000000000949400F7F7F70094FF
      FF0094FFFF0094CEFF0094FFFF0094CEFF0094FFFF0094CEFF0094CEFF0094CE
      FF0063CECE000000000000949400000000004242420042424200424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000000000000949400F7F7F70094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094CEFF0094FFFF0094CEFF0094CE
      FF00009494000000000000949400000000004242420042424200424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF00000000000000000000949400F7F7F70094FFFF0094FF
      FF0094FFFF0094FFFF0094CEFF0094FFFF0094FFFF0094CEFF0094FFFF0063CE
      CE000000000063CECE0063CECE00000000004242420042424200424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF00000000000000000000949400F7F7F70094FFFF0094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094CEFF0094FFFF0094CEFF0063CE
      CE000000000063CECE0063CECE00000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000094940000949400009494000094
      9400009494000094940000949400009494000094940000949400009494000094
      940063CECE0094FFFF0063CECE00000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000000000000949400F7F7F70094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF0094FF
      FF0094FFFF0094FFFF0063CECE00000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000000000000949400F7F7F70094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF00F7F7F700F7F7
      F700F7F7F700F7F7F70063CECE00000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000000000000949400F7F7F70094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF00F7F7F70000949400009494000094
      9400009494000094940000949400000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000000000000000949400F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F7000094940000000000000000000000
      0000000000000000000000000000000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00FFFFFF0000000000000000000000000000000000000000000094
      9400009494000094940000949400009494000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00000000007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00FFFFFF000000000000000000000000007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004242420042424200424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004242420042424200424242004242
      420042424200424242004242420042424200BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFE000CF7E
      80038003C000000080038003C00000009FF39FF3800000009FF39DF380000000
      9FF398F3000000009FF39073000000009FF39233000000009FF3971380000000
      9FF39F93800000009FF39FD3800100009FF39FF3C07F010080038003E0FF2600
      80038003FFFF1300FFFFFFFFFFFF870300000000000000000000000000000000
      000000000000}
  end
end
