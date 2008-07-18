object FParams: TFParams
  Left = 988
  Top = 155
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
        Top = 80
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
        object TabRobot: TTabSheet
          Caption = 'Robot'
          object Label1: TLabel
            Left = 76
            Top = 29
            Width = 40
            Height = 13
            Caption = 'Voltage:'
          end
          object Label4: TLabel
            Left = 168
            Top = 29
            Width = 34
            Height = 13
            Caption = 'Speed:'
          end
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
            Width = 21
            Height = 13
            Caption = 'IR0:'
          end
          object Label3: TLabel
            Left = 184
            Top = 188
            Width = 21
            Height = 13
            Caption = 'IR1:'
          end
          object Label5: TLabel
            Left = 184
            Top = 208
            Width = 21
            Height = 13
            Caption = 'IR2:'
          end
          object Label7: TLabel
            Left = 184
            Top = 228
            Width = 21
            Height = 13
            Caption = 'IR3:'
          end
          object Label30: TLabel
            Left = 184
            Top = 248
            Width = 21
            Height = 13
            Caption = 'IR4:'
          end
          object Label31: TLabel
            Left = 184
            Top = 268
            Width = 21
            Height = 13
            Caption = 'IR5:'
          end
          object Label32: TLabel
            Left = 184
            Top = 288
            Width = 21
            Height = 13
            Caption = 'IR6:'
          end
          object Label33: TLabel
            Left = 184
            Top = 308
            Width = 21
            Height = 13
            Caption = 'IR7:'
          end
          object Label34: TLabel
            Left = 188
            Top = 144
            Width = 42
            Height = 13
            Caption = 'Sensors:'
          end
          object Label8: TLabel
            Left = 16
            Top = 366
            Width = 14
            Height = 13
            Caption = 'IP:'
          end
          object EditMotUnom: TEdit
            Left = 120
            Top = 25
            Width = 41
            Height = 21
            TabOrder = 0
            Text = '24'
          end
          object EditMotSpeedRef: TEdit
            Left = 208
            Top = 25
            Width = 41
            Height = 21
            TabOrder = 1
            Text = '50'
          end
          object CBIO1: TCheckBox
            Left = 16
            Top = 260
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO1'
            TabOrder = 2
          end
          object CBIO2: TCheckBox
            Left = 16
            Top = 276
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO2'
            TabOrder = 3
          end
          object CBIO3: TCheckBox
            Left = 16
            Top = 292
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO3'
            TabOrder = 4
          end
          object CBIO4: TCheckBox
            Left = 16
            Top = 308
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO4'
            TabOrder = 5
          end
          object CBIO5: TCheckBox
            Left = 64
            Top = 260
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO5'
            TabOrder = 6
          end
          object CBIO6: TCheckBox
            Left = 64
            Top = 276
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO6'
            TabOrder = 7
          end
          object CBIO7: TCheckBox
            Left = 64
            Top = 292
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO7'
            TabOrder = 8
          end
          object CBIO8: TCheckBox
            Left = 64
            Top = 308
            Width = 40
            Height = 17
            Alignment = taLeftJustify
            Caption = 'IO8'
            TabOrder = 9
          end
          object EditU0: TEdit
            Left = 32
            Top = 52
            Width = 41
            Height = 21
            TabOrder = 10
            Text = '100'
          end
          object EditOdo0: TEdit
            Left = 120
            Top = 52
            Width = 41
            Height = 21
            TabOrder = 11
            Text = '0'
          end
          object EditOdo1: TEdit
            Left = 120
            Top = 72
            Width = 41
            Height = 21
            TabOrder = 12
            Text = '0'
          end
          object EditU1: TEdit
            Left = 32
            Top = 72
            Width = 41
            Height = 21
            TabOrder = 13
            Text = '200'
          end
          object EditI0: TEdit
            Left = 208
            Top = 52
            Width = 41
            Height = 21
            TabOrder = 14
            Text = '0'
          end
          object EditI1: TEdit
            Left = 208
            Top = 72
            Width = 41
            Height = 21
            TabOrder = 15
            Text = '0'
          end
          object EditU2: TEdit
            Left = 32
            Top = 92
            Width = 41
            Height = 21
            TabOrder = 16
            Text = '100'
          end
          object EditOdo2: TEdit
            Left = 120
            Top = 92
            Width = 41
            Height = 21
            TabOrder = 17
            Text = '0'
          end
          object EditI2: TEdit
            Left = 208
            Top = 92
            Width = 41
            Height = 21
            TabOrder = 18
            Text = '0'
          end
          object EditI3: TEdit
            Left = 208
            Top = 112
            Width = 41
            Height = 21
            TabOrder = 19
            Text = '0'
          end
          object EditOdo3: TEdit
            Left = 120
            Top = 112
            Width = 41
            Height = 21
            TabOrder = 20
            Text = '0'
          end
          object EditU3: TEdit
            Left = 32
            Top = 112
            Width = 41
            Height = 21
            TabOrder = 21
            Text = '200'
          end
          object CBPIDsActive: TCheckBox
            Left = 8
            Top = 28
            Width = 53
            Height = 17
            Caption = 'PIDs'
            Checked = True
            State = cbChecked
            TabOrder = 22
            OnClick = CBPIDsActiveClick
          end
          object EditIR0: TEdit
            Left = 208
            Top = 164
            Width = 41
            Height = 21
            TabOrder = 23
            Text = '0'
          end
          object EditIR1: TEdit
            Left = 208
            Top = 184
            Width = 41
            Height = 21
            TabOrder = 24
            Text = '0'
          end
          object EditIR2: TEdit
            Left = 208
            Top = 204
            Width = 41
            Height = 21
            TabOrder = 25
            Text = '0'
          end
          object EditIR3: TEdit
            Left = 208
            Top = 224
            Width = 41
            Height = 21
            TabOrder = 26
            Text = '0'
          end
          object EditIR4: TEdit
            Left = 208
            Top = 244
            Width = 41
            Height = 21
            TabOrder = 27
            Text = '0'
          end
          object EditIR5: TEdit
            Left = 208
            Top = 264
            Width = 41
            Height = 21
            TabOrder = 28
            Text = '0'
          end
          object EditIR6: TEdit
            Left = 208
            Top = 284
            Width = 41
            Height = 21
            TabOrder = 29
            Text = '0'
          end
          object EditIR7: TEdit
            Left = 208
            Top = 304
            Width = 41
            Height = 21
            TabOrder = 30
            Text = '0'
          end
          object CBIRNoise: TCheckBox
            Left = 184
            Top = 332
            Width = 65
            Height = 17
            Caption = 'IR Noise'
            State = cbGrayed
            TabOrder = 31
            OnClick = CBIRNoiseClick
          end
          object EditRemoteIP: TEdit
            Left = 36
            Top = 362
            Width = 109
            Height = 21
            TabOrder = 32
            Text = '127.0.0.1'
            OnChange = EditRemoteIPChange
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
              31
              89
              38
              36
              35)
          end
          object EditJointTeta: TEdit
            Left = 10
            Top = 58
            Width = 45
            Height = 21
            TabOrder = 1
          end
          object EditJointTetaRef: TEdit
            Left = 58
            Top = 58
            Width = 45
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
        Top = 76
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
    end
    object TabGraphics: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 1
      object Label24: TLabel
        Left = 4
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Target FPS:'
      end
      object Label9: TLabel
        Left = 116
        Top = 212
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object Label10: TLabel
        Left = 116
        Top = 236
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object Label11: TLabel
        Left = 116
        Top = 260
        Width = 10
        Height = 13
        Caption = 'Z:'
      end
      object Label12: TLabel
        Left = 128
        Top = 192
        Width = 37
        Height = 13
        Caption = 'Position'
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
        Text = '25'
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
        Top = 196
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
        Left = 132
        Top = 208
        Width = 34
        Height = 21
        TabOrder = 11
        Text = '1'
      end
      object EditCamY: TEdit
        Left = 132
        Top = 232
        Width = 34
        Height = 21
        TabOrder = 12
        Text = '1'
      end
      object EditCamZ: TEdit
        Left = 132
        Top = 256
        Width = 34
        Height = 21
        TabOrder = 13
        Text = '1'
      end
      object BCamXMLRead: TButton
        Left = 4
        Top = 452
        Width = 75
        Height = 25
        Caption = 'Read'
        TabOrder = 14
      end
      object BXamXMLWrite: TButton
        Left = 184
        Top = 452
        Width = 75
        Height = 25
        Caption = 'Write'
        TabOrder = 15
        OnClick = BXamXMLWriteClick
      end
      object Edit4: TEdit
        Left = 4
        Top = 480
        Width = 121
        Height = 21
        TabOrder = 16
        Text = 'Edit4'
      end
      object MemoCameraConfig: TMemo
        Left = 4
        Top = 308
        Width = 257
        Height = 89
        Lines.Strings = (
          'Camera.Position = [1 1 3]'
          'Camera.OffSet = [0 0 0]'
          'Target.Position = [1 1 3]'
          'Target.OffSet = [0 0 0]')
        TabOrder = 17
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
        Text = 'EditDebug2'
      end
      object EditDebug3: TEdit
        Left = 0
        Top = 60
        Width = 269
        Height = 21
        TabOrder = 1
        Text = 'EditDebug'
      end
      object MemoDebug: TMemo
        Left = 0
        Top = 92
        Width = 265
        Height = 349
        TabOrder = 2
        WordWrap = False
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
    Left = 240
    Top = 4
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
      'EditRemoteIP.Text')
    StoredValues = <>
    Left = 60
    Top = 48
  end
end
