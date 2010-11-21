object FParams: TFParams
  Left = 998
  Top = 113
  Width = 281
  Height = 679
  Caption = 'Config'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    273
    652)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 273
    Height = 621
    ActivePage = TabControl
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    object TabControl: TTabSheet
      Caption = 'Control'
      DesignSize = (
        265
        593)
      object Label28: TLabel
        Left = 124
        Top = 18
        Width = 69
        Height = 13
        Caption = 'Robot Position'
      end
      object Label27: TLabel
        Left = 124
        Top = 40
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object Label35: TLabel
        Left = 196
        Top = 40
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object Label36: TLabel
        Left = 196
        Top = 60
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object Label29: TLabel
        Left = 124
        Top = 60
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object Label13: TLabel
        Left = 124
        Top = 100
        Width = 9
        Height = 13
        Caption = 'q:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
      end
      object Label39: TLabel
        Left = 196
        Top = 80
        Width = 10
        Height = 13
        Caption = 'Z:'
      end
      object Label37: TLabel
        Left = 196
        Top = 100
        Width = 9
        Height = 13
        Caption = 'q:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
      end
      object Label49: TLabel
        Left = 124
        Top = 80
        Width = 10
        Height = 13
        Caption = 'Z:'
      end
      object RGControlBlock: TRadioGroup
        Left = 0
        Top = 100
        Width = 113
        Height = 65
        Caption = 'Control Block'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Script'
          'Remote')
        TabOrder = 0
        OnClick = RGControlBlockClick
      end
      object BEditScript: TButton
        Left = 200
        Top = 144
        Width = 59
        Height = 21
        Caption = 'Edit Script'
        TabOrder = 1
        OnClick = BEditScriptClick
      end
      object BTest: TButton
        Left = 138
        Top = 144
        Width = 59
        Height = 21
        Caption = 'Test'
        TabOrder = 2
        OnClick = BTestClick
      end
      object LBRobots: TListBox
        Left = 0
        Top = 0
        Width = 113
        Height = 97
        ItemHeight = 13
        TabOrder = 3
        OnClick = LBRobotsClick
      end
      object PGRobots: TPageControl
        Left = 0
        Top = 168
        Width = 261
        Height = 425
        ActivePage = TabAxis
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 4
        object TabGlobal: TTabSheet
          Caption = 'Global'
          ImageIndex = 2
          DesignSize = (
            253
            397)
          object Label8: TLabel
            Left = 0
            Top = 374
            Width = 14
            Height = 13
            Caption = 'IP:'
          end
          object Label1: TLabel
            Left = 8
            Top = 8
            Width = 64
            Height = 13
            Caption = 'Script Period:'
          end
          object EditRemoteIP: TEdit
            Left = 20
            Top = 370
            Width = 109
            Height = 21
            TabOrder = 0
            Text = '127.0.0.1'
            OnChange = EditRemoteIPChange
          end
          object EditScriptPeriod: TEdit
            Left = 76
            Top = 4
            Width = 41
            Height = 21
            TabOrder = 1
            Text = '40'
          end
          object BGlobalSet: TButton
            Left = 204
            Top = 4
            Width = 47
            Height = 21
            Caption = 'Set'
            TabOrder = 2
            OnClick = BGlobalSetClick
          end
          object SGGlobalSensors: TStringGrid
            Left = 0
            Top = 56
            Width = 157
            Height = 313
            Anchors = [akLeft, akTop, akBottom]
            ColCount = 3
            DefaultColWidth = 61
            DefaultRowHeight = 14
            FixedCols = 0
            RowCount = 128
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
            TabOrder = 3
            ColWidths = (
              53
              31
              49)
          end
        end
        object TabRobot: TTabSheet
          Caption = 'Robot'
          object Label6: TLabel
            Left = 16
            Top = 240
            Width = 56
            Height = 13
            Caption = 'General I/O'
          end
          object Label19: TLabel
            Left = 8
            Top = 56
            Width = 17
            Height = 13
            Caption = 'U0:'
          end
          object Label20: TLabel
            Left = 84
            Top = 56
            Width = 30
            Height = 13
            Caption = 'Odo0:'
          end
          object Label21: TLabel
            Left = 84
            Top = 76
            Width = 30
            Height = 13
            Caption = 'Odo1:'
          end
          object Label18: TLabel
            Left = 8
            Top = 76
            Width = 17
            Height = 13
            Caption = 'U1:'
          end
          object Label14: TLabel
            Left = 184
            Top = 56
            Width = 14
            Height = 13
            Caption = 'I0:'
          end
          object Label15: TLabel
            Left = 184
            Top = 76
            Width = 14
            Height = 13
            Caption = 'I1:'
          end
          object Label16: TLabel
            Left = 8
            Top = 96
            Width = 17
            Height = 13
            Caption = 'U2:'
          end
          object Label17: TLabel
            Left = 84
            Top = 96
            Width = 30
            Height = 13
            Caption = 'Odo2:'
          end
          object Label22: TLabel
            Left = 184
            Top = 96
            Width = 14
            Height = 13
            Caption = 'I2:'
          end
          object Label23: TLabel
            Left = 184
            Top = 116
            Width = 14
            Height = 13
            Caption = 'I3:'
          end
          object Label25: TLabel
            Left = 84
            Top = 116
            Width = 30
            Height = 13
            Caption = 'Odo3:'
          end
          object Label26: TLabel
            Left = 8
            Top = 116
            Width = 17
            Height = 13
            Caption = 'U3:'
          end
          object Label2: TLabel
            Left = 184
            Top = 168
            Width = 16
            Height = 13
            Caption = 'S0:'
          end
          object Label3: TLabel
            Left = 184
            Top = 188
            Width = 16
            Height = 13
            Caption = 'S1:'
          end
          object Label5: TLabel
            Left = 184
            Top = 208
            Width = 16
            Height = 13
            Caption = 'S2:'
          end
          object Label7: TLabel
            Left = 184
            Top = 228
            Width = 16
            Height = 13
            Caption = 'S3:'
          end
          object Label30: TLabel
            Left = 184
            Top = 248
            Width = 16
            Height = 13
            Caption = 'S4:'
          end
          object Label31: TLabel
            Left = 184
            Top = 268
            Width = 16
            Height = 13
            Caption = 'S5:'
          end
          object Label32: TLabel
            Left = 184
            Top = 288
            Width = 16
            Height = 13
            Caption = 'S6:'
          end
          object Label33: TLabel
            Left = 184
            Top = 308
            Width = 16
            Height = 13
            Caption = 'S7:'
          end
          object Label34: TLabel
            Left = 188
            Top = 144
            Width = 42
            Height = 13
            Caption = 'Sensors:'
          end
          object CBIO1: TCheckBox
            Left = 16
            Top = 260
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO1'
            TabOrder = 0
          end
          object CBIO2: TCheckBox
            Left = 16
            Top = 276
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO2'
            TabOrder = 1
          end
          object CBIO3: TCheckBox
            Left = 16
            Top = 292
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO3'
            TabOrder = 2
          end
          object CBIO4: TCheckBox
            Left = 16
            Top = 308
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO4'
            TabOrder = 3
          end
          object CBIO5: TCheckBox
            Left = 64
            Top = 260
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO5'
            TabOrder = 4
          end
          object CBIO6: TCheckBox
            Left = 64
            Top = 276
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO6'
            TabOrder = 5
          end
          object CBIO7: TCheckBox
            Left = 64
            Top = 292
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO7'
            TabOrder = 6
          end
          object CBIO8: TCheckBox
            Left = 64
            Top = 308
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO8'
            TabOrder = 7
          end
          object EditU0: TEdit
            Left = 32
            Top = 52
            Width = 41
            Height = 21
            TabOrder = 8
            Text = '100'
          end
          object EditOdo0: TEdit
            Left = 120
            Top = 52
            Width = 41
            Height = 21
            TabOrder = 9
            Text = '0'
          end
          object EditOdo1: TEdit
            Left = 120
            Top = 72
            Width = 41
            Height = 21
            TabOrder = 10
            Text = '0'
          end
          object EditU1: TEdit
            Left = 32
            Top = 72
            Width = 41
            Height = 21
            TabOrder = 11
            Text = '200'
          end
          object EditI0: TEdit
            Left = 208
            Top = 52
            Width = 41
            Height = 21
            TabOrder = 12
            Text = '0'
          end
          object EditI1: TEdit
            Left = 208
            Top = 72
            Width = 41
            Height = 21
            TabOrder = 13
            Text = '0'
          end
          object EditU2: TEdit
            Left = 32
            Top = 92
            Width = 41
            Height = 21
            TabOrder = 14
            Text = '100'
          end
          object EditOdo2: TEdit
            Left = 120
            Top = 92
            Width = 41
            Height = 21
            TabOrder = 15
            Text = '0'
          end
          object EditI2: TEdit
            Left = 208
            Top = 92
            Width = 41
            Height = 21
            TabOrder = 16
            Text = '0'
          end
          object EditI3: TEdit
            Left = 208
            Top = 112
            Width = 41
            Height = 21
            TabOrder = 17
            Text = '0'
          end
          object EditOdo3: TEdit
            Left = 120
            Top = 112
            Width = 41
            Height = 21
            TabOrder = 18
            Text = '0'
          end
          object EditU3: TEdit
            Left = 32
            Top = 112
            Width = 41
            Height = 21
            TabOrder = 19
            Text = '200'
          end
          object CBPIDsActive: TCheckBox
            Left = 8
            Top = 28
            Width = 53
            Height = 17
            Caption = 'PIDs'
            State = cbGrayed
            TabOrder = 20
            OnClick = CBPIDsActiveClick
          end
          object EditIR0: TEdit
            Left = 208
            Top = 164
            Width = 41
            Height = 21
            TabOrder = 21
            Text = '0'
          end
          object EditIR1: TEdit
            Left = 208
            Top = 184
            Width = 41
            Height = 21
            TabOrder = 22
            Text = '0'
          end
          object EditIR2: TEdit
            Left = 208
            Top = 204
            Width = 41
            Height = 21
            TabOrder = 23
            Text = '0'
          end
          object EditIR3: TEdit
            Left = 208
            Top = 224
            Width = 41
            Height = 21
            TabOrder = 24
            Text = '0'
          end
          object EditIR4: TEdit
            Left = 208
            Top = 244
            Width = 41
            Height = 21
            TabOrder = 25
            Text = '0'
          end
          object EditIR5: TEdit
            Left = 208
            Top = 264
            Width = 41
            Height = 21
            TabOrder = 26
            Text = '0'
          end
          object EditIR6: TEdit
            Left = 208
            Top = 284
            Width = 41
            Height = 21
            TabOrder = 27
            Text = '0'
          end
          object EditIR7: TEdit
            Left = 208
            Top = 304
            Width = 41
            Height = 21
            TabOrder = 28
            Text = '0'
          end
          object CBIRNoise: TCheckBox
            Left = 184
            Top = 332
            Width = 65
            Height = 17
            Caption = 'IR Noise'
            State = cbGrayed
            TabOrder = 29
            OnClick = CBIRNoiseClick
          end
        end
        object TabAxis: TTabSheet
          Caption = 'Axis'
          ImageIndex = 1
          DesignSize = (
            253
            397)
          object Label38: TLabel
            Left = 0
            Top = 62
            Width = 21
            Height = 13
            Caption = 'Pos:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label41: TLabel
            Left = 0
            Top = 32
            Width = 50
            Height = 13
            Caption = 'WayPoint:'
          end
          object SGJoints: TStringGrid
            Left = 4
            Top = 84
            Width = 253
            Height = 313
            Anchors = [akLeft, akTop, akBottom]
            DefaultColWidth = 61
            DefaultRowHeight = 14
            FixedCols = 0
            RowCount = 128
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
            TabOrder = 0
            ColWidths = (
              68
              39
              38
              36
              47)
          end
          object EditJointTeta: TEdit
            Left = 22
            Top = 58
            Width = 41
            Height = 21
            TabOrder = 1
          end
          object EditJointTetaRef: TEdit
            Left = 66
            Top = 58
            Width = 41
            Height = 21
            TabOrder = 2
            Text = '0'
          end
          object BSetJointTetaRef: TButton
            Left = 108
            Top = 58
            Width = 45
            Height = 21
            Caption = 'Set Ref'
            TabOrder = 3
            OnClick = BSetJointTetaRefClick
          end
          object EditLoadJointPoints: TEdit
            Left = 0
            Top = 4
            Width = 153
            Height = 21
            TabOrder = 4
            Text = 'JointWayPoints.xml'
          end
          object BLoadJointWayPoints: TButton
            Left = 204
            Top = 4
            Width = 45
            Height = 21
            Caption = 'Load'
            TabOrder = 5
            OnClick = BLoadJointWayPointsClick
          end
          object BSetAll: TButton
            Left = 204
            Top = 58
            Width = 45
            Height = 21
            Caption = 'Set All'
            TabOrder = 6
            OnClick = BSetAllClick
          end
          object BSetJointWayPointTeta: TButton
            Left = 156
            Top = 58
            Width = 45
            Height = 21
            Caption = 'Set WP'
            TabOrder = 7
            OnClick = BSetJointWayPointTetaClick
          end
          object ComboWayPointName: TComboBox
            Left = 52
            Top = 28
            Width = 149
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 8
          end
          object BJointWayPointsSave: TButton
            Left = 156
            Top = 4
            Width = 45
            Height = 21
            Caption = 'Save'
            TabOrder = 9
            OnClick = BJointWayPointsSaveClick
          end
          object BWayPointEdit: TButton
            Left = 204
            Top = 28
            Width = 45
            Height = 21
            Caption = 'Edit'
            TabOrder = 10
            OnClick = BWayPointEditClick
          end
        end
      end
      object BFreeze: TButton
        Left = 138
        Top = 120
        Width = 59
        Height = 21
        Caption = 'Freeze'
        TabOrder = 5
        OnClick = BFreezeClick
      end
      object BStep: TButton
        Left = 200
        Top = 120
        Width = 59
        Height = 21
        Caption = 'Step'
        TabOrder = 6
        OnClick = BStepClick
      end
      object CBFreeze: TCheckBox
        Left = 124
        Top = 0
        Width = 65
        Height = 17
        Caption = 'Freeze'
        TabOrder = 7
        OnClick = CBFreezeClick
      end
      object BSetPosition: TButton
        Left = 212
        Top = 14
        Width = 45
        Height = 18
        Caption = 'Set'
        TabOrder = 8
        OnClick = BSetPositionClick
      end
      object EditRobotX: TEdit
        Left = 140
        Top = 36
        Width = 45
        Height = 21
        TabOrder = 9
        Text = '0'
      end
      object EditRobotSetX: TEdit
        Left = 212
        Top = 36
        Width = 45
        Height = 21
        TabOrder = 10
        Text = '0'
      end
      object EditRobotSetY: TEdit
        Left = 212
        Top = 56
        Width = 45
        Height = 21
        TabOrder = 11
        Text = '0'
      end
      object EditRobotY: TEdit
        Left = 140
        Top = 56
        Width = 45
        Height = 21
        TabOrder = 12
        Text = '0'
      end
      object EditRobotTeta: TEdit
        Left = 140
        Top = 96
        Width = 45
        Height = 21
        TabOrder = 13
        Text = '0'
      end
      object EditRobotSetZ: TEdit
        Left = 212
        Top = 76
        Width = 45
        Height = 21
        TabOrder = 14
        Text = '0'
      end
      object EditRobotSetTeta: TEdit
        Left = 212
        Top = 96
        Width = 45
        Height = 21
        TabOrder = 15
        Text = '0'
      end
      object EditRobotZ: TEdit
        Left = 140
        Top = 76
        Width = 45
        Height = 21
        TabOrder = 16
        Text = '0'
      end
    end
    object TabGraphics: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 1
      DesignSize = (
        265
        593)
      object Label24: TLabel
        Left = 4
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Target FPS:'
      end
      object Label9: TLabel
        Left = 112
        Top = 218
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object Label10: TLabel
        Left = 112
        Top = 242
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object Label11: TLabel
        Left = 112
        Top = 266
        Width = 10
        Height = 13
        Caption = 'Z:'
      end
      object Label12: TLabel
        Left = 128
        Top = 198
        Width = 37
        Height = 13
        Caption = 'Position'
      end
      object Label50: TLabel
        Left = 176
        Top = 198
        Width = 35
        Height = 13
        Caption = 'Look at'
      end
      object Label51: TLabel
        Left = 4
        Top = 294
        Width = 37
        Height = 13
        Caption = 'Camera'
      end
      object Label52: TLabel
        Left = 8
        Top = 310
        Width = 41
        Height = 13
        Caption = 'Position:'
      end
      object Label53: TLabel
        Left = 8
        Top = 332
        Width = 39
        Height = 13
        Caption = 'Look at:'
      end
      object Label54: TLabel
        Left = 184
        Top = 134
        Width = 29
        Height = 13
        Alignment = taRightJustify
        Caption = 'Trails:'
      end
      object Label55: TLabel
        Left = 168
        Top = 156
        Width = 46
        Height = 13
        Alignment = taRightJustify
        Caption = 'Trail Size:'
      end
      object CBShadows: TCheckBox
        Left = 8
        Top = 68
        Width = 97
        Height = 17
        Caption = 'Shadows'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBShadowsClick
      end
      object CBVsync: TCheckBox
        Left = 8
        Top = 36
        Width = 97
        Height = 17
        Caption = 'Vsync'
        TabOrder = 1
        OnClick = CBVsyncClick
      end
      object CBAntiAliasing: TCheckBox
        Left = 8
        Top = 52
        Width = 97
        Height = 17
        Caption = 'AntiAliasing'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CBAntiAliasingClick
      end
      object CBGrid: TCheckBox
        Left = 8
        Top = 84
        Width = 89
        Height = 17
        Caption = 'Grid'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CBGridClick
      end
      object CBAxis: TCheckBox
        Left = 8
        Top = 100
        Width = 89
        Height = 17
        Caption = 'Axis'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = CBAxisClick
      end
      object CBGroundTexture: TCheckBox
        Left = 8
        Top = 116
        Width = 97
        Height = 17
        Caption = 'Ground Texture'
        TabOrder = 5
        OnClick = CBGroundTextureClick
      end
      object CBSkyDome: TCheckBox
        Left = 8
        Top = 132
        Width = 89
        Height = 17
        Caption = 'SkyDome'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = CBSkyDomeClick
      end
      object CBHotCPU: TCheckBox
        Left = 8
        Top = 148
        Width = 97
        Height = 17
        Caption = 'Hot CPU'
        TabOrder = 7
        OnClick = CBHotCPUClick
      end
      object EditTargetFPS: TEdit
        Left = 64
        Top = 4
        Width = 45
        Height = 21
        TabOrder = 8
        Text = '60'
      end
      object BSetFPS: TButton
        Left = 125
        Top = 2
        Width = 32
        Height = 25
        Caption = 'Set'
        TabOrder = 9
        OnClick = BSetFPSClick
      end
      object RGCamera: TRadioGroup
        Left = 0
        Top = 192
        Width = 109
        Height = 93
        Caption = 'Camera'
        ItemIndex = 0
        Items.Strings = (
          'Fixed'
          'Look at Robot'
          'Move with Robot'
          'Above Robot'
          'Top View')
        TabOrder = 10
      end
      object EditCamX: TEdit
        Left = 128
        Top = 214
        Width = 45
        Height = 21
        TabOrder = 11
      end
      object EditCamY: TEdit
        Left = 128
        Top = 238
        Width = 45
        Height = 21
        TabOrder = 12
      end
      object EditCamZ: TEdit
        Left = 128
        Top = 262
        Width = 45
        Height = 21
        TabOrder = 13
      end
      object SGConf: TStringGrid
        Left = 0
        Top = 384
        Width = 261
        Height = 209
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 4
        DefaultColWidth = 50
        DefaultRowHeight = 14
        FixedCols = 0
        RowCount = 128
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
        TabOrder = 14
        OnDblClick = SGConfDblClick
        ColWidths = (
          87
          50
          50
          50)
      end
      object EditGridX: TEdit
        Left = 76
        Top = 360
        Width = 60
        Height = 21
        TabOrder = 15
        OnKeyPress = EditGridKeyPress
      end
      object EditGridY: TEdit
        Left = 139
        Top = 360
        Width = 60
        Height = 21
        TabOrder = 16
        OnKeyPress = EditGridKeyPress
      end
      object EditGridZ: TEdit
        Left = 202
        Top = 360
        Width = 60
        Height = 21
        TabOrder = 17
        OnKeyPress = EditGridKeyPress
      end
      object BSGConfSet: TButton
        Left = 38
        Top = 360
        Width = 33
        Height = 21
        Caption = 'Set'
        TabOrder = 18
        OnClick = BSGConfSetClick
      end
      object EditCamLookX: TEdit
        Left = 176
        Top = 214
        Width = 45
        Height = 21
        TabOrder = 19
      end
      object EditCamLookY: TEdit
        Left = 176
        Top = 238
        Width = 45
        Height = 21
        TabOrder = 20
      end
      object EditCamLookZ: TEdit
        Left = 176
        Top = 262
        Width = 45
        Height = 21
        TabOrder = 21
      end
      object EditSetCamX: TEdit
        Left = 52
        Top = 306
        Width = 45
        Height = 21
        TabOrder = 22
        Text = '1'
      end
      object EditSetCamY: TEdit
        Left = 100
        Top = 306
        Width = 45
        Height = 21
        TabOrder = 23
        Text = '1'
      end
      object EditSetCamZ: TEdit
        Left = 148
        Top = 306
        Width = 45
        Height = 21
        TabOrder = 24
        Text = '1'
      end
      object BSetCamPars: TButton
        Left = 232
        Top = 306
        Width = 33
        Height = 43
        Caption = 'Set'
        TabOrder = 25
        OnClick = BSetCamParsClick
      end
      object EditSetCamLookX: TEdit
        Left = 52
        Top = 328
        Width = 45
        Height = 21
        TabOrder = 26
        Text = '0'
      end
      object EditSetCamLookY: TEdit
        Left = 100
        Top = 328
        Width = 45
        Height = 21
        TabOrder = 27
        Text = '0'
      end
      object EditSetCamLookZ: TEdit
        Left = 148
        Top = 328
        Width = 45
        Height = 21
        TabOrder = 28
        Text = '0'
      end
      object BGetCamPos: TButton
        Left = 196
        Top = 306
        Width = 33
        Height = 21
        Caption = 'Get'
        TabOrder = 29
        OnClick = BGetCamPosClick
      end
      object Button1: TButton
        Left = 196
        Top = 328
        Width = 33
        Height = 21
        Caption = 'Get'
        TabOrder = 30
        OnClick = Button1Click
      end
      object EditTrailsCount: TEdit
        Left = 216
        Top = 130
        Width = 45
        Height = 21
        TabOrder = 31
        Text = '8'
      end
      object EditTrailSize: TEdit
        Left = 216
        Top = 152
        Width = 45
        Height = 21
        TabOrder = 32
        Text = '200'
      end
      object BSetTrailPars: TButton
        Left = 216
        Top = 104
        Width = 45
        Height = 21
        Caption = 'Set'
        TabOrder = 33
        OnClick = BSetTrailParsClick
      end
      object RGGLObjects: TRadioGroup
        Left = 192
        Top = 34
        Width = 73
        Height = 65
        Caption = 'GLObjects'
        ItemIndex = 1
        Items.Strings = (
          'Original'
          'Mesh'
          'Both')
        TabOrder = 34
        OnClick = RGGLObjectsClick
      end
      object RGSensorGL: TRadioGroup
        Left = 108
        Top = 34
        Width = 81
        Height = 65
        Caption = 'Show Sensors'
        ItemIndex = 0
        Items.Strings = (
          'All'
          'One Ray'
          'None')
        TabOrder = 35
        OnClick = RGSensorGLClick
      end
      object BSGConfGet: TButton
        Left = 1
        Top = 360
        Width = 33
        Height = 21
        Caption = 'Get'
        TabOrder = 36
        OnClick = BSGConfGetClick
      end
    end
    object TabDebug: TTabSheet
      Caption = 'Debug'
      ImageIndex = 2
      object EditDEbug2: TEdit
        Left = 0
        Top = 8
        Width = 269
        Height = 21
        TabOrder = 0
      end
      object EditDebug3: TEdit
        Left = 0
        Top = 60
        Width = 269
        Height = 21
        TabOrder = 1
      end
      object MemoDebug: TMemo
        Left = 0
        Top = 92
        Width = 265
        Height = 349
        TabOrder = 2
        WordWrap = False
      end
      object BExportTrack: TButton
        Left = 0
        Top = 444
        Width = 75
        Height = 25
        Caption = 'Export Track'
        TabOrder = 3
        OnClick = BExportTrackClick
      end
      object CBTags: TCheckBox
        Left = 0
        Top = 472
        Width = 73
        Height = 17
        Caption = 'Tags'
        TabOrder = 4
      end
      object LBSelectedTags: TListBox
        Left = 0
        Top = 492
        Width = 73
        Height = 93
        ItemHeight = 13
        TabOrder = 5
      end
      object LBAllTags: TListBox
        Left = 104
        Top = 492
        Width = 73
        Height = 93
        ItemHeight = 13
        TabOrder = 6
      end
      object BitBtnAddTags: TBitBtn
        Left = 76
        Top = 496
        Width = 25
        Height = 25
        TabOrder = 7
        OnClick = BitBtnAddTagsClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000281C07815A150000000000000000000000000000
          000000000000000000000000000000000000000000000000003E2B0BBE882B85
          5E1A000000000000000000000000000000000000000000000000000000000000
          0000000000005C4112C18C2FC28E33825C1B0000000000000000000000000000
          000000000000000000000000000000000000007C5819C49036C39037C59137AC
          7D277F5A1C86601E86601E86601E86601E86601E8B641F402D0D000000000000
          996F24C7953DC8953CC8953CC8953CC8953CC8953CC8953CC8953CC8953CC895
          3CC8953CC8953E5F4415100C04B58631CB9943C99840CB9941C99840CB9941C9
          9840CB9941C99840CB9941C99840CB9941C99840CB99425E4418070502A67B2E
          CE9D47CE9D46CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D
          45CE9D45CE9D476047190000000000008D6827D1A14BD1A14CCFA04BD1A14CD0
          A14DD1A14ED1A14ED1A14ED1A14ED1A14ED1A14ED1A14F654B1B000000000000
          0000006E521FD4A44FD4A551D4A552B488387154217A5B247A5B247A5B247A5B
          247A5B247D5D25392A100000000000000000000000004F3B17D7A750D5A95893
          6F2E000000000000000000000000000000000000000000000000000000000000
          00000000000000000031250FDAA9529D77350000000000000000000000000000
          000000000000000000000000000000000000000000000000000000001D16098F
          6C2D000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      object BitBtnRemoveTags: TBitBtn
        Left = 76
        Top = 524
        Width = 25
        Height = 25
        TabOrder = 8
        OnClick = BitBtnRemoveTagsClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000825B15271B070000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000865F1ABE882B3D2B0B000000000000000000000000000000000000000000
          000000000000000000000000000000000000825C1AC28E33C18C2F5B40110000
          00000000000000000000402D0D8B641F86601E86601E86601E86601E86601E80
          5C1CAC7D28C59137C59137C490367C58190000000000000000005F4415C8953E
          C8953CC8953CC8953CC8953CC8953CC8953CC8953CC8953CC8953CC8953CC795
          3D996E240000000000005E4517CB9942CB9941C99840CB9941C99840CB9941C9
          9840CB9941C99840CB9941C99840CB9941CB9943B58631100C04604618CE9D47
          CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D45CE9D
          46CE9D47A67B2D070502664B1BD1A14FD1A14ED1A14ED1A14ED1A14ED1A14ED1
          A14ED0A14DCFA04BD1A14CCFA04BD1A14B8B6826000000000000392A107D5D25
          7A5B247A5B247A5B247A5B247A5B24725521B48838D4A552D4A551D4A44F6D52
          1F00000000000000000000000000000000000000000000000000000000000000
          000094702FD5A958D7A7504E3A17000000000000000000000000000000000000
          0000000000000000000000000000000000009F7935DAA95230240F0000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000916D2D1D1609000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      object BitBtnAddAll: TBitBtn
        Left = 76
        Top = 552
        Width = 25
        Height = 25
        Caption = 'All'
        TabOrder = 9
        OnClick = BitBtnAddAllClick
      end
    end
    object TabPhysics: TTabSheet
      Caption = 'Physics'
      ImageIndex = 3
      object Label40: TLabel
        Left = 4
        Top = 8
        Width = 72
        Height = 13
        Caption = 'Default Frition:'
      end
      object Label42: TLabel
        Left = 4
        Top = 32
        Width = 75
        Height = 13
        Caption = 'ODE Time Step:'
      end
      object Label43: TLabel
        Left = 132
        Top = 32
        Width = 21
        Height = 13
        Caption = '(ms)'
      end
      object Label44: TLabel
        Left = 4
        Top = 56
        Width = 59
        Height = 13
        Caption = 'Time Speed:'
      end
      object Label47: TLabel
        Left = 4
        Top = 92
        Width = 49
        Height = 13
        Caption = 'ODE CFM:'
      end
      object Label48: TLabel
        Left = 4
        Top = 116
        Width = 47
        Height = 13
        Caption = 'ODE ERP:'
      end
      object Label4: TLabel
        Left = 4
        Top = 152
        Width = 76
        Height = 13
        Caption = 'Step Iterations:'
      end
      object EditDefaultFriction: TEdit
        Left = 84
        Top = 4
        Width = 45
        Height = 21
        TabOrder = 0
        Text = '0.95'
      end
      object BPhysicsSet: TButton
        Left = 204
        Top = 0
        Width = 55
        Height = 25
        Caption = 'Set'
        TabOrder = 1
        OnClick = BPhysicsSetClick
      end
      object EditOde_dt: TEdit
        Left = 84
        Top = 28
        Width = 45
        Height = 21
        TabOrder = 2
        Text = '1'
      end
      object EditTimeSpeed: TEdit
        Left = 84
        Top = 52
        Width = 45
        Height = 21
        TabOrder = 3
        Text = '1'
      end
      object EditODE_CFM: TEdit
        Left = 84
        Top = 88
        Width = 45
        Height = 21
        TabOrder = 4
        Text = '1e-5'
      end
      object EditODE_ERP: TEdit
        Left = 84
        Top = 112
        Width = 45
        Height = 21
        TabOrder = 5
        Text = '0.4'
      end
      object CBWorldQuickStep: TCheckBox
        Left = 136
        Top = 150
        Width = 97
        Height = 17
        Caption = 'WorldQuickStep'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object EditQuickStepIterations: TEdit
        Left = 84
        Top = 148
        Width = 45
        Height = 21
        TabOrder = 7
        Text = '10'
      end
    end
    object TabIO: TTabSheet
      Caption = 'I/O'
      ImageIndex = 4
      DesignSize = (
        265
        593)
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 265
        Height = 89
        Alignment = taLeftJustify
        BevelOuter = bvLowered
        TabOrder = 0
        object Label45: TLabel
          Left = 8
          Top = 9
          Width = 48
          Height = 13
          Caption = 'Com Port:'
        end
        object BComConf: TButton
          Left = 188
          Top = 6
          Width = 69
          Height = 21
          Caption = 'Configure'
          TabOrder = 0
          OnClick = BComConfClick
        end
        object CBComOpen: TCheckBox
          Left = 64
          Top = 8
          Width = 45
          Height = 17
          Caption = 'Open'
          TabOrder = 1
          OnClick = CBComOpenClick
        end
        object BComWrite: TButton
          Left = 8
          Top = 34
          Width = 44
          Height = 21
          Caption = 'Write'
          TabOrder = 2
          OnClick = BComWriteClick
        end
        object BComRead: TButton
          Left = 8
          Top = 62
          Width = 44
          Height = 21
          Caption = 'Read'
          TabOrder = 3
          OnClick = BComReadClick
        end
        object EditComRead: TEdit
          Left = 64
          Top = 62
          Width = 197
          Height = 21
          TabOrder = 4
        end
        object EditComWrite: TEdit
          Left = 64
          Top = 34
          Width = 197
          Height = 21
          TabOrder = 5
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 92
        Width = 265
        Height = 89
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvLowered
        TabOrder = 1
        object Label46: TLabel
          Left = 12
          Top = 12
          Width = 47
          Height = 13
          Caption = 'UDP Port:'
        end
        object EditUDPPort: TEdit
          Left = 64
          Top = 8
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '9808'
        end
        object CBUDPConnect: TCheckBox
          Left = 128
          Top = 10
          Width = 73
          Height = 17
          Caption = 'Connect'
          TabOrder = 1
          OnClick = CBUDPConnectClick
        end
      end
    end
  end
  object EditDebug: TEdit
    Left = 4
    Top = 628
    Width = 269
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Text = 'EditDebug'
  end
  object UDPServer: TIdUDPServer
    Bindings = <>
    DefaultPort = 9800
    OnUDPRead = UDPServerUDPRead
    Left = 244
  end
  object FormStorage: TFormStorage
    IniSection = 'Config'
    UseRegistry = False
    StoredProps.Strings = (
      'CBAntiAliasing.Checked'
      'CBAxis.Checked'
      'CBHotCPU.Checked'
      'CBGrid.Checked'
      'CBGroundTexture.Checked'
      'CBShadows.Checked'
      'CBSkyDome.Checked'
      'CBVsync.Checked'
      'EditTargetFPS.Text'
      'RGCamera.ItemIndex'
      'CBIRNoise.Checked'
      'RGControlBlock.ItemIndex'
      'CBFreeze.Checked'
      'EditDefaultFriction.Text'
      'EditRobotSetTeta.Text'
      'EditRobotSetX.Text'
      'EditRobotSetY.Text'
      'EditRobotSetZ.Text'
      'EditOde_dt.Text'
      'EditTimeSpeed.Text'
      'PageControl.ActivePage'
      'EditUDPPort.Text'
      'CBUDPConnect.Checked'
      'EditODE_CFM.Text'
      'EditODE_ERP.Text'
      'EditScriptPeriod.Text'
      'CBWorldQuickStep.Checked'
      'EditQuickStepIterations.Text'
      'EditSetCamLookX.Text'
      'EditSetCamLookY.Text'
      'EditSetCamLookZ.Text'
      'EditSetCamX.Text'
      'EditSetCamY.Text'
      'EditSetCamZ.Text'
      'EditTrailsCount.Text'
      'EditTrailSize.Text'
      'RGGLObjects.ItemIndex'
      'PGRobots.TabIndex'
      'CBTags.Checked'
      'LBSelectedTags.Items'
      'RGSensorGL.ItemIndex')
    StoredValues = <>
    Left = 236
    Top = 60
  end
  object ComPort: TComPort
    BaudRate = br115200
    Port = 'COM1'
    Parity.Bits = prNone
    StopBits = sbOneStopBit
    DataBits = dbEight
    Events = []
    FlowControl.OutCTSFlow = False
    FlowControl.OutDSRFlow = False
    FlowControl.ControlDTR = dtrDisable
    FlowControl.ControlRTS = rtsDisable
    FlowControl.XonXoffOut = False
    FlowControl.XonXoffIn = False
    Left = 152
    Top = 28
  end
  object UDPGeneric: TIdUDPServer
    BroadcastEnabled = True
    Bindings = <>
    DefaultPort = 9808
    OnUDPRead = UDPGenericUDPRead
    Left = 180
    Top = 28
  end
end
