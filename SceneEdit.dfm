object FSceneEdit: TFSceneEdit
  Left = 380
  Top = 356
  Width = 770
  Height = 606
  Caption = 'XML Scene Edit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 440
    Width = 762
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 540
    Width = 762
    Height = 20
    Panels = <
      item
        Alignment = taCenter
        Text = 'Line:Col'
        Width = 60
      end
      item
        Alignment = taCenter
        Text = 'Modified'
        Width = 50
      end
      item
        Alignment = taCenter
        Text = 'Insert'
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
  object LBErrors: TListBox
    Left = 0
    Top = 443
    Width = 762
    Height = 97
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = LBErrorsDblClick
  end
  object PageControlXML: TPageControl
    Left = 0
    Top = 0
    Width = 762
    Height = 440
    ActivePage = TabScene
    Align = alClient
    TabOrder = 2
    OnChange = PageControlXMLChange
    object TabScene: TTabSheet
      Caption = 'Scene.xml'
      object SynEditXML: TSynEdit
        Left = 0
        Top = 4
        Width = 754
        Height = 408
        Align = alBottom
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 20
        Highlighter = SynXMLSyn
        Lines.Strings = (
          '<?xml version="1.0" ?>'
          '<scene>'
          ''
          '<!--'
          '  <robot>'
          '    <ID name='#39'Classic'#39'/>'
          '    <pos x='#39'0'#39' y='#39'-1'#39' z='#39'0'#39'/>'
          '    <rot_deg x='#39'0'#39' y='#39'0'#39' z='#39'0'#39'/>'
          ''
          '    <body file='#39'Differential4.xml'#39'/>'
          '  </robot>-->'
          ''
          '   <robot>'
          '    <ID name='#39'Zeppelin'#39'/>'
          '    <pos x='#39'0'#39' y='#39'-1'#39' z='#39'0.5'#39'/>'
          '    <rot_deg x='#39'0'#39' y='#39'0'#39' z='#39'0'#39'/>'
          ''
          '    <body file='#39'zeppelin.xml'#39'/>'
          '  </robot>'
          ''
          '  <robot>'
          '    <ID name='#39'belt'#39'/>'
          '    <pos x='#39'1.5'#39' y='#39'0'#39' z='#39'0.5'#39'/>'
          '    <rot_deg x='#39'0'#39' y='#39'0'#39' z='#39'0'#39'/>'
          ''
          '    <body file='#39'Belt.xml'#39'/>'
          '  </robot>'
          ''
          '  <robot>'
          '    <ID name='#39'Omni3'#39'/>'
          '    <pos x='#39'0'#39' y='#39'0'#39' z='#39'0'#39'/>'
          '    <rot_deg x='#39'0'#39' y='#39'0'#39' z='#39'90'#39'/>'
          ''
          '    <body file='#39'Omni3.xml'#39'/>'
          '  </robot>'
          ''
          ''
          '  <things file='#39'things.xml'#39'/>'
          ''
          '  <obstacles file='#39'obstacles.xml'#39'/>'
          ''
          '  <track file='#39'track.xml'#39'/>'
          ''
          '</scene>'
          '')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollByOneLess, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces]
        WantTabs = True
        OnStatusChange = SynEditXMLStatusChange
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 164
    Top = 36
    object MenuFile: TMenuItem
      Caption = '&File'
      object MenuNew: TMenuItem
        Caption = '&New'
        ShortCut = 16462
        OnClick = MenuNewClick
      end
      object MenuOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = MenuOpenClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = MenuSaveClick
      end
      object MenuSaveAs: TMenuItem
        Caption = 'Save &As...'
        OnClick = MenuSaveAsClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuPrintSource: TMenuItem
        Caption = '&Print...'
        Enabled = False
        ShortCut = 16464
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MenuExitClick
      end
    end
    object MenuEdit: TMenuItem
      Caption = '&Edit'
      object MenuUndo: TMenuItem
        Caption = '&Undo'
        ShortCut = 16474
        OnClick = MenuUndoClick
      end
      object MenuRedo: TMenuItem
        Caption = '&Redo'
        ShortCut = 24666
        OnClick = MenuRedoClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuFind: TMenuItem
        Caption = '&Find'
        ShortCut = 16454
        OnClick = MenuFindClick
      end
      object MenuReplace: TMenuItem
        Caption = 'Replace'
        ShortCut = 16466
        OnClick = MenuReplaceClick
      end
    end
    object MenuScene: TMenuItem
      Caption = '&Scene'
      object MenuReBuild: TMenuItem
        Caption = '&Rebuild'
        ShortCut = 16450
        OnClick = MenuReBuildClick
      end
      object MenuChange: TMenuItem
        Caption = '&Change...'
        ShortCut = 16453
        OnClick = MenuChangeClick
      end
      object MenuNewScene: TMenuItem
        Caption = '&New...'
        OnClick = MenuNewSceneClick
      end
    end
    object MenuWindow: TMenuItem
      Caption = '&Window'
      object MenuSceneXML: TMenuItem
        Caption = '&Scene.XML'
      end
    end
  end
  object SynXMLSyn: TSynXMLSyn
    CommentAttri.Background = clNone
    WantBracesParsed = False
    Left = 192
    Top = 36
  end
  object ReplaceDialog: TReplaceDialog
    OnFind = ReplaceDialogAction
    OnReplace = ReplaceDialogAction
    Left = 380
    Top = 36
  end
  object FindDialog: TFindDialog
    OnFind = FindDialogFind
    Left = 352
    Top = 36
  end
  object SynEditSearch: TSynEditSearch
    Left = 352
    Top = 64
  end
  object OpenDialog: TOpenDialog
    Filter = 'Scene File|*.xml'
    Options = [ofHideReadOnly, ofNoChangeDir, ofFileMustExist, ofEnableSizing]
    Title = 'Load Scene File'
    Left = 288
    Top = 36
  end
  object SaveDialog: TSaveDialog
    Filter = 'Scene File|*.xml'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofCreatePrompt, ofEnableSizing]
    Title = 'Save Scene File'
    Left = 316
    Top = 36
  end
  object FormStorage: TFormStorage
    IniSection = 'SceneEditor'
    UseRegistry = False
    StoredProps.Strings = (
      'PageControlXML.ActivePage'
      'LBErrors.Height')
    StoredValues = <>
    Left = 476
    Top = 36
  end
end
