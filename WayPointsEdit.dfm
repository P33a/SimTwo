object FWayPointsEdit: TFWayPointsEdit
  Left = 374
  Top = 356
  Width = 242
  Height = 309
  Caption = 'WayPoints Edit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    234
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 60
    Top = 8
    Width = 60
    Height = 13
    Caption = 'WayPoint ID'
  end
  object Label2: TLabel
    Left = 4
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Final Time'
  end
  object LBWayPoints: TListBox
    Left = 4
    Top = 48
    Width = 137
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBWayPointsClick
  end
  object BClose: TButton
    Left = 153
    Top = 253
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = BCloseClick
  end
  object BAdd: TButton
    Left = 152
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    Visible = False
  end
  object BDelete: TButton
    Left = 152
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    Visible = False
  end
  object BInsert: TButton
    Left = 152
    Top = 148
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 4
    Visible = False
  end
  object BInterpolate: TButton
    Left = 152
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Interpolate'
    TabOrder = 5
    Visible = False
  end
  object EditID: TEdit
    Left = 56
    Top = 24
    Width = 85
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object EditFinalT: TEdit
    Left = 4
    Top = 24
    Width = 49
    Height = 21
    TabOrder = 7
  end
  object BSet: TButton
    Left = 152
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Set'
    TabOrder = 8
    OnClick = BSetClick
  end
end
