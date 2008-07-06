object FRemoteControl: TFRemoteControl
  Left = 357
  Top = 111
  Width = 485
  Height = 272
  Caption = 'Remote Control'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label8: TLabel
    Left = 12
    Top = 38
    Width = 14
    Height = 13
    Caption = 'IP:'
  end
  object EditData: TEdit
    Left = 12
    Top = 8
    Width = 453
    Height = 21
    TabOrder = 0
    Text = 'EditData'
  end
  object Memo: TMemo
    Left = 280
    Top = 32
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo')
    TabOrder = 1
  end
  object EditRemoteIP: TEdit
    Left = 34
    Top = 34
    Width = 109
    Height = 21
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object UDPServer: TIdUDPServer
    Bindings = <>
    DefaultPort = 9801
    OnUDPRead = UDPServerUDPRead
    Left = 36
    Top = 64
  end
end
