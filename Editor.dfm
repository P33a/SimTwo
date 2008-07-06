object FEditor: TFEditor
  Left = 228
  Top = 473
  Width = 775
  Height = 522
  Caption = 'Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    767
    476)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 769
    Height = 457
    ActivePage = TabControl
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabProject: TTabSheet
      Caption = 'Project'
      DesignSize = (
        761
        429)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Author(s):'
      end
      object Label2: TLabel
        Left = 8
        Top = 32
        Width = 54
        Height = 13
        Caption = 'Comments:'
      end
      object EditAuthors: TEdit
        Left = 68
        Top = 4
        Width = 676
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object EditComments: TEdit
        Left = 68
        Top = 28
        Width = 676
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object CBSaveOnRun: TCheckBox
        Left = 8
        Top = 92
        Width = 97
        Height = 17
        Caption = 'Save On Run'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
    object TabControl: TTabSheet
      Caption = 'Control'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 0
        Top = 323
        Width = 761
        Height = 6
        Cursor = crVSplit
        Align = alBottom
        MinSize = 25
        ResizeStyle = rsLine
      end
      object SynEditST: TSynEdit
        Left = 0
        Top = 0
        Width = 761
        Height = 323
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        OnMouseMove = SynEditSTMouseMove
        Gutter.BorderColor = clInactiveCaption
        Gutter.DigitCount = 3
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 20
        Gutter.ZeroStart = True
        Gutter.LineNumberStart = 0
        Highlighter = SynPasSyn
        Lines.Strings = (
          '// '
          '// '
          ''
          '// Global Variables'
          'var Vnom: double;'
          ''
          'procedure Control;'
          'var teta, ref: double;'
          'begin'
          '//  teta := GetLinkTheta(2,0);'
          '//  writeln(floatTostr(teta));'
          '//  ref := -teta * 0.01;'
          '//  setLinkThetaRef(2,0,10);'
          ' setAxisWRef(0, 0, Vnom);'
          ' setAxisWRef(0, 1, Vnom);'
          ' setAxisWRef(0, 2, Vnom);'
          'end;'
          ''
          'procedure Initialize;'
          'begin'
          '  Vnom := 10;'
          'end;'
          '')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollByOneLess, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSpecialLineDefaultFg, eoTabsToSpaces, eoTrimTrailingSpaces]
        ScrollBars = ssVertical
        TabWidth = 2
        WantTabs = True
        OnGutterClick = SynEditSTGutterClick
        OnSpecialLineColors = SynEditSTSpecialLineColors
        OnStatusChange = SynEditSTStatusChange
      end
      object PageControlBottom: TPageControl
        Left = 0
        Top = 329
        Width = 761
        Height = 100
        ActivePage = TabOutput
        Align = alBottom
        TabOrder = 1
        TabPosition = tpBottom
        object TabOutput: TTabSheet
          Caption = 'Output'
          DesignSize = (
            753
            74)
          object MemoResult: TMemo
            Left = 0
            Top = 0
            Width = 752
            Height = 73
            Anchors = [akLeft, akTop, akRight, akBottom]
            PopupMenu = PopupMenuOutput
            ReadOnly = True
            TabOrder = 0
            WordWrap = False
          end
        end
        object TabErrors: TTabSheet
          Caption = 'Errors'
          ImageIndex = 1
          DesignSize = (
            753
            74)
          object LBErrors: TListBox
            Left = 0
            Top = 0
            Width = 752
            Height = 73
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
            OnDblClick = LBErrorsDblClick
          end
        end
        object TabVariables: TTabSheet
          Caption = 'Variables'
          ImageIndex = 2
          DesignSize = (
            753
            74)
          object LBVariables: TListBox
            Left = 0
            Top = 0
            Width = 753
            Height = 73
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
            OnDblClick = LBErrorsDblClick
          end
        end
      end
    end
    object TabPascal: TTabSheet
      Caption = 'Pascal'
      ImageIndex = 2
      DesignSize = (
        761
        429)
      object SynEditPascal: TSynEdit
        Left = 0
        Top = 96
        Width = 752
        Height = 313
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        TabOrder = 0
        Gutter.DigitCount = 3
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 10
        Gutter.LineNumberStart = 100
        Highlighter = SynPasSyn
      end
      object SynMemoHeader: TSynMemo
        Left = 0
        Top = 0
        Width = 753
        Height = 93
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        TabOrder = 1
        Gutter.DigitCount = 3
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Gutter.Width = 0
        Highlighter = SynPasSyn
        Lines.Strings = (
          'const MaxDim  = 8;'
          'const k = MaxDim-1;')
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 456
    Width = 767
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
  object SynPasSyn: TSynPasSyn
    CommentAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clNavy
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    FloatAttri.Foreground = clBlue
    StringAttri.Foreground = clNavy
    DelphiVersion = dvDelphi7
    Left = 192
    Top = 36
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
      object MenuPascal: TMenuItem
        Caption = 'Pascal'
        ShortCut = 24699
        Visible = False
      end
    end
    object MenuProgram: TMenuItem
      Caption = '&Program'
      object MenuCompile: TMenuItem
        Caption = '&Compile'
        ShortCut = 16504
        OnClick = MenuCompileClick
      end
      object MenuRun: TMenuItem
        Caption = 'Ru&n'
        ShortCut = 120
        OnClick = MenuRunClick
      end
      object MenuStop: TMenuItem
        Caption = '&Stop'
        ShortCut = 121
        OnClick = MenuStopClick
      end
      object MenuTest: TMenuItem
        Caption = 'Test'
        ShortCut = 16468
        OnClick = MenuTestClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuSetResetInspector: TMenuItem
        Caption = 'Set/Reset Inspector'
        ShortCut = 116
        OnClick = MenuSetResetInspectorClick
      end
      object MenuShowLocalVariables: TMenuItem
        Caption = 'Show Local Variables'
        ShortCut = 117
        OnClick = MenuShowLocalVariablesClick
      end
      object MenuShowGlobalVariables: TMenuItem
        Caption = 'Show Global Variables'
        ShortCut = 16501
        OnClick = MenuShowGlobalVariablesClick
      end
    end
    object MenuWindow: TMenuItem
      Caption = '&Window'
      object MenuChart: TMenuItem
        Caption = '&Chart...'
        ShortCut = 8308
      end
      object MenuControl: TMenuItem
        Caption = 'Control...'
        ShortCut = 8309
      end
      object MenuLog: TMenuItem
        Caption = '&Log && Trace'
        ShortCut = 8310
        Visible = False
      end
      object MenuCalculator: TMenuItem
        Caption = '&Calculator...'
        OnClick = MenuCalculatorClick
      end
    end
    object MenuHelp: TMenuItem
      Caption = '&Help'
      object MenuAbout: TMenuItem
        Caption = '&About...'
        OnClick = MenuAboutClick
      end
      object MenuLocalHelp: TMenuItem
        Caption = '&Local Help...'
        ShortCut = 112
        OnClick = MenuLocalHelpClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.ulf'
    Filter = 'Unreal Lab File|*.ulf'
    Title = 'Load Unreal Lab Project'
    Left = 288
    Top = 36
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.ulf'
    Filter = 'Unreal Lab File|*.ulf'
    Options = [ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofEnableSizing]
    Title = 'Save Unreal Lab Project'
    Left = 316
    Top = 36
  end
  object FindDialog: TFindDialog
    Left = 352
    Top = 36
  end
  object ReplaceDialog: TReplaceDialog
    Left = 380
    Top = 36
  end
  object PrintDialog: TPrintDialog
    Left = 416
    Top = 36
  end
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 15.000000000000000000
    Margins.Footer = 15.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Colors = True
    TabWidth = 8
    Color = clWhite
    Left = 444
    Top = 36
  end
  object PSScript: TPSScriptDebugger
    CompilerOptions = [icBooleanShortCircuit]
    OnCompile = PSScript_Compile
    OnExecute = PSScript_Execute
    Plugins = <
      item
        Plugin = PSImport_Classes
      end
      item
        Plugin = PSImport_Controls
      end
      item
        Plugin = PSImport_StdCtrls
      end
      item
        Plugin = PSImport_Forms
      end>
    MainFileName = 'unnamed'
    UsePreProcessor = True
    OnNeedFile = PSScriptNeedFile
    OnBreakpoint = PSScriptBreakpoint
    Left = 260
    Top = 36
  end
  object SynCompletionProposal: TSynCompletionProposal
    ItemList.Strings = (
      'Y1'
      'Y2'
      'R1'
      'R2'
      'Sek')
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ShortCut = 16416
    Editor = SynEditST
    Left = 192
    Top = 64
  end
  object PSImport_Classes: TPSImport_Classes
    EnableStreams = True
    EnableClasses = True
    Left = 256
    Top = 68
  end
  object PSImport_Forms: TPSImport_Forms
    EnableForms = True
    EnableMenus = True
    Left = 256
    Top = 96
  end
  object PSImport_Controls: TPSImport_Controls
    EnableStreams = True
    EnableGraphics = True
    EnableControls = True
    Left = 256
    Top = 124
  end
  object PSImport_StdCtrls: TPSImport_StdCtrls
    EnableExtCtrls = True
    EnableButtons = True
    Left = 256
    Top = 152
  end
  object FormStorage: TFormStorage
    IniSection = 'Editor'
    UseRegistry = False
    StoredValues = <
      item
        Name = 'StoredLastProjectName'
        Value = ''
        KeyString = 'LastProjectName'
      end>
    Left = 476
    Top = 36
  end
  object PopupMenuOutput: TPopupMenu
    Left = 192
    Top = 100
    object PopUpClearAll: TMenuItem
      Caption = 'Clear &All'
      ShortCut = 16449
      OnClick = PopUpClearAllClick
    end
  end
end
