unit Params;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, GLWin32Viewer, GLcontext, Math,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, IdSocketHandle, ODERobots, OdeImport,
  rxPlacemnt, Grids, GLCadencer, CPort, ShellAPI, Sockets, GLShadowVolume, GLScene,
  CheckLst, Buttons, VectorTypes, VectorGeometry;

type
  TFParams = class(TForm)
    PageControl: TPageControl;
    TabControl: TTabSheet;
    RGControlBlock: TRadioGroup;
    TabGraphics: TTabSheet;
    CBShadows: TCheckBox;
    TabDebug: TTabSheet;
    EditDEbug2: TEdit;
    CBVsync: TCheckBox;
    CBAntiAliasing: TCheckBox;
    CBGrid: TCheckBox;
    CBAxis: TCheckBox;
    CBGroundTexture: TCheckBox;
    CBSkyDome: TCheckBox;
    CBHotCPU: TCheckBox;
    Label24: TLabel;
    EditTargetFPS: TEdit;
    BSetFPS: TButton;
    UDPServer: TIdUDPServer;
    BEditScript: TButton;
    BTest: TButton;
    RGCamera: TRadioGroup;
    EditCamX: TEdit;
    EditCamY: TEdit;
    EditCamZ: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    EditDebug3: TEdit;
    MemoDebug: TMemo;
    FormStorage: TFormStorage;
    LBRobots: TListBox;
    PGRobots: TPageControl;
    TabRobot: TTabSheet;
    TabAxis: TTabSheet;
    Label6: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label18: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    CBIO1: TCheckBox;
    CBIO2: TCheckBox;
    CBIO3: TCheckBox;
    CBIO4: TCheckBox;
    CBIO5: TCheckBox;
    CBIO6: TCheckBox;
    CBIO7: TCheckBox;
    CBIO8: TCheckBox;
    EditU0: TEdit;
    EditOdo0: TEdit;
    EditOdo1: TEdit;
    EditU1: TEdit;
    EditI0: TEdit;
    EditI1: TEdit;
    EditU2: TEdit;
    EditOdo2: TEdit;
    EditI2: TEdit;
    EditI3: TEdit;
    EditOdo3: TEdit;
    EditU3: TEdit;
    CBPIDsActive: TCheckBox;
    EditIR0: TEdit;
    EditIR1: TEdit;
    EditIR2: TEdit;
    EditIR3: TEdit;
    EditIR4: TEdit;
    EditIR5: TEdit;
    EditIR6: TEdit;
    EditIR7: TEdit;
    EditDebug: TEdit;
    SGJoints: TStringGrid;
    EditJointTeta: TEdit;
    Label38: TLabel;
    EditJointTetaRef: TEdit;
    BSetJointTetaRef: TButton;
    BFreeze: TButton;
    BStep: TButton;
    CBIRNoise: TCheckBox;
    EditLoadJointPoints: TEdit;
    BLoadJointWayPoints: TButton;
    CBFreeze: TCheckBox;
    TabPhysics: TTabSheet;
    Label40: TLabel;
    EditDefaultFriction: TEdit;
    BPhysicsSet: TButton;
    Label28: TLabel;
    BSetPosition: TButton;
    EditRobotX: TEdit;
    Label27: TLabel;
    Label35: TLabel;
    EditRobotSetX: TEdit;
    EditRobotSetY: TEdit;
    Label36: TLabel;
    EditRobotY: TEdit;
    Label29: TLabel;
    Label13: TLabel;
    EditRobotTeta: TEdit;
    Label39: TLabel;
    EditRobotSetZ: TEdit;
    EditRobotSetTeta: TEdit;
    Label37: TLabel;
    BSetAll: TButton;
    BSetJointWayPointTeta: TButton;
    Label41: TLabel;
    ComboWayPointName: TComboBox;
    BJointWayPointsSave: TButton;
    BWayPointEdit: TButton;
    Label42: TLabel;
    EditOde_dt: TEdit;
    Label43: TLabel;
    Label44: TLabel;
    EditTimeSpeed: TEdit;
    SGConf: TStringGrid;
    EditGridX: TEdit;
    EditGridY: TEdit;
    EditGridZ: TEdit;
    BSGConfSet: TButton;
    ComPort: TComPort;
    TabIO: TTabSheet;
    Panel1: TPanel;
    Label45: TLabel;
    BComConf: TButton;
    CBComOpen: TCheckBox;
    BComWrite: TButton;
    BComRead: TButton;
    EditComRead: TEdit;
    EditComWrite: TEdit;
    Panel2: TPanel;
    EditUDPPort: TEdit;
    Label46: TLabel;
    CBUDPConnect: TCheckBox;
    UDPGeneric: TIdUDPServer;
    Label47: TLabel;
    EditODE_CFM: TEdit;
    Label48: TLabel;
    EditODE_ERP: TEdit;
    TabGlobal: TTabSheet;
    Label8: TLabel;
    EditRemoteIP: TEdit;
    Label1: TLabel;
    EditScriptPeriod: TEdit;
    BGlobalSet: TButton;
    CBWorldQuickStep: TCheckBox;
    Label4: TLabel;
    EditQuickStepIterations: TEdit;
    EditRobotZ: TEdit;
    Label49: TLabel;
    Label50: TLabel;
    EditCamLookX: TEdit;
    EditCamLookY: TEdit;
    EditCamLookZ: TEdit;
    EditSetCamX: TEdit;
    EditSetCamY: TEdit;
    Label51: TLabel;
    Label52: TLabel;
    EditSetCamZ: TEdit;
    BSetCamPars: TButton;
    Label53: TLabel;
    EditSetCamLookX: TEdit;
    EditSetCamLookY: TEdit;
    EditSetCamLookZ: TEdit;
    BGetCamPos: TButton;
    Button1: TButton;
    Label54: TLabel;
    EditTrailsCount: TEdit;
    Label55: TLabel;
    EditTrailSize: TEdit;
    BSetTrailPars: TButton;
    RGGLObjects: TRadioGroup;
    SGGlobalSensors: TStringGrid;
    BExportTrack: TButton;
    CBTags: TCheckBox;
    LBSelectedTags: TListBox;
    LBAllTags: TListBox;
    BitBtnAddTags: TBitBtn;
    BitBtnRemoveTags: TBitBtn;
    BitBtnAddAll: TBitBtn;
    RGSensorGL: TRadioGroup;
    BSGConfGet: TButton;
    procedure CBShadowsClick(Sender: TObject);
    procedure CBVsyncClick(Sender: TObject);
    procedure BSetFPSClick(Sender: TObject);
    procedure CBAntiAliasingClick(Sender: TObject);
    procedure CBGridClick(Sender: TObject);
    procedure CBAxisClick(Sender: TObject);
    procedure CBGroundTextureClick(Sender: TObject);
    procedure CBSkyDomeClick(Sender: TObject);
    procedure BEditScriptClick(Sender: TObject);
    procedure EditRemoteIPChange(Sender: TObject);
    procedure RGControlBlockClick(Sender: TObject);
    procedure BTestClick(Sender: TObject);
    procedure UDPServerUDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);
    procedure CBPIDsActiveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BSetPositionClick(Sender: TObject);
    procedure BSetJointTetaRefClick(Sender: TObject);
    procedure LBRobotsClick(Sender: TObject);
    procedure BFreezeClick(Sender: TObject);
    procedure BStepClick(Sender: TObject);
    procedure CBIRNoiseClick(Sender: TObject);
    procedure BLoadJointWayPointsClick(Sender: TObject);
    procedure CBHotCPUClick(Sender: TObject);
    procedure CBFreezeClick(Sender: TObject);
    procedure BPhysicsSetClick(Sender: TObject);
    procedure BSetJointWayPointTetaClick(Sender: TObject);
    procedure BSetAllClick(Sender: TObject);
    procedure BJointWayPointsSaveClick(Sender: TObject);
    procedure BWayPointEditClick(Sender: TObject);
    procedure BSGConfSetClick(Sender: TObject);
    procedure SGConfDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditGridKeyPress(Sender: TObject; var Key: Char);
    procedure BComConfClick(Sender: TObject);
    procedure CBComOpenClick(Sender: TObject);
    procedure BComReadClick(Sender: TObject);
    procedure BComWriteClick(Sender: TObject);
    procedure CBUDPConnectClick(Sender: TObject);
    procedure UDPGenericUDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);
    procedure BGlobalSetClick(Sender: TObject);
    procedure BSetCamParsClick(Sender: TObject);
    procedure BGetCamPosClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BSetTrailParsClick(Sender: TObject);
    procedure RGGLObjectsClick(Sender: TObject);
    procedure BExportTrackClick(Sender: TObject);
    procedure BitBtnAddTagsClick(Sender: TObject);
    procedure BitBtnRemoveTagsClick(Sender: TObject);
    procedure BitBtnAddAllClick(Sender: TObject);
    procedure RGSensorGLClick(Sender: TObject);
    procedure BSGConfGetClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure FillEditArray(ProtoName: string;
      var EditArray: array of TEdit);
    procedure SGConfRowToVar(SGRow: longword);
    procedure VarToSGConfRow(SGRow: longword);
    procedure FillSGConf;
    procedure FillSGConfRow(varname: string);
    { Private declarations }
  public
    EditsU, EditsI, EditsOdo : array[0..3] of TEdit;
    EditsIR: array[0..7] of TEdit;
    UDPGenData: TMemoryStream;
    UDPGenPackets: TStringList;

    procedure FillLBRobots(LB: TListBox);
    procedure FillLBLinks(LB: TListBox; r: integer);
    procedure ShowRobotState;
    procedure ShowRobotPosition(r: integer);
    procedure ComboWayPointNameUpdate(robot: TRobot);
    procedure ShowGlobalState;

    procedure ShowCameraConfig(GLCamera: TGLCamera);
    procedure ShowParsedScene;
//    procedure ShowIRValues;
  end;


var
  FParams: TFParams;
  dt: double;


implementation

{$R *.dfm}

uses JPeg, GLFile3DS, Viewer, Editor, FastChart, ODERobotsPublished, WayPointsEdit, ProjConfig, utils;

procedure TFParams.BSetFPSClick(Sender: TObject);
var fps: integer;
begin
  fps:=strtointdef(EditTargetFPS.text,25);
  if fps>0 then begin
    FViewer.GLCadencer.FixedDeltaTime := 1/fps;
    //FViewer.GLCadencer.MinDeltaTime := FViewer.GLCadencer.FixedDeltaTime;
    //FViewer.GLCadencer.MaxDeltaTime := FViewer.GLCadencer.FixedDeltaTime;
  end;
end;

procedure TFParams.CBVsyncClick(Sender: TObject);
begin
  if CBVsync.checked then begin
    FViewer.GLSceneViewer.VSync := vsmSync;
  end else begin
    FViewer.GLSceneViewer.VSync := vsmNoSync;
  end;
end;


procedure TFParams.CBAntiAliasingClick(Sender: TObject);
begin
  if CBAntiAliasing.checked then begin
    FViewer.GLSceneViewer.Buffer.AntiAliasing := aa2x;
  end else begin
    FViewer.GLSceneViewer.Buffer.AntiAliasing := aaNone;
  end;
end;

procedure TFParams.CBShadowsClick(Sender: TObject);
begin
  FViewer.GLShadowVolume.Active := CBShadows.Checked;
end;

procedure TFParams.CBGridClick(Sender: TObject);
begin
  FViewer.GLXYZGrid.Visible:= CBGrid.Checked;
end;

procedure TFParams.CBAxisClick(Sender: TObject);
begin
  FViewer.GLDummyCubeAxis.Visible:= CBAxis.Checked;
end;

procedure TFParams.CBGroundTextureClick(Sender: TObject);
begin
  FViewer.GLPlaneFloor.Material.Texture.Disabled:=not CBGroundTexture.Checked;
end;

procedure TFParams.CBSkyDomeClick(Sender: TObject);
begin
  FViewer.GLEarthSkyDome.Visible:= CBSkyDome.Checked;
end;

procedure TFParams.BEditScriptClick(Sender: TObject);
begin
  FEditor.show;
end;

procedure TFParams.EditRemoteIPChange(Sender: TObject);
begin
  RGControlBlock.ItemIndex := 0;
end;

procedure TFParams.RGControlBlockClick(Sender: TObject);
begin
  if RGControlBlock.ItemIndex = 2 then begin
    UDPServer.Active := true;
  end else begin
    UDPServer.Active := false;
  end;
end;

procedure TFParams.BTestClick(Sender: TObject);
var prs: string;
    i: integer;
begin
//  UDPServer.Send(EditRemoteIP.Text, 9801, 'test');
//  FRemoteControl.show;
//  FChart.AddSample(0, WorldODE.physTime);
//  FViewer.GLCadencer.Progress;
  prs := '';
  for i := 1 to ParamCount do begin
    prs := ' ' + ParamStr(i);
  end;
  FViewer.Close;
  ShellExecute(Handle, 'open', pchar(Application.ExeName), pchar(prs), nil,  SW_SHOWNORMAL);
end;

function GetListValue(L: TStrings; QName: string): string;
var i: integer;
    trim_name: string;
begin
  result := '';
  trim_name := AnsiLowerCase(trim(QName));
  if trim_name = '' then exit;
  for i:=0 to L.Count-1 do begin
    if trim_name = AnsiLowerCase(trim(L.Names[i])) then begin
      result := trim(L.ValueFromIndex[i]);
      exit;
    end;
  end;
end;

procedure TFParams.UDPServerUDPRead(Sender: TObject; AData: TStream;  ABinding: TIdSocketHandle);
var cnt: integer;
begin
  //FViewer.rem
  AData.Seek(0, soFromBeginning);
  cnt := AData.Read(RemControl, sizeof(RemControl));
  if cnt <> sizeof(RemControl) then begin
    MemoDebug.Lines.Add(format('Bad packet: size should be %d but is %d',[sizeof(RemControl), cnt]));
  end;
end;


procedure TFParams.ShowRobotState;
var i, idx, wp_idx: integer;
    //str: string;
    theta: double;
begin
  idx := LBRobots.ItemIndex;
  if (idx < 0) or (idx >= WorldODE.Robots.Count) then exit;
  wp_idx := ComboWayPointName.ItemIndex;

  for i := low(EditsU) to high(EditsU) do begin
    if WorldODE.Robots[idx].Links.Count > i then begin
      {if not CBPIDsActive.Checked then
        EditsU[i].Text := format('%5.1f',[WorldODE.Robots[idx].Links[i].Axis.ref.volts])
      else
        EditsU[i].Text := format('%5.1f',[WorldODE.Robots[idx].Links[i].Axis.ref.w]);}
      EditsU[i].Text := format('%5.1f',[WorldODE.Robots[idx].Links[i].Axis[0].Motor.voltage]);
    end else begin
      EditsU[i].Text := '';
    end;
  end;

  for i := low(EditsI) to high(EditsI) do begin
    if WorldODE.Robots[idx].Links.Count > i then begin
      EditsI[i].Text := format('%f',[WorldODE.Robots[idx].Links[i].Axis[0].Motor.Im]);
    end else begin
      EditsI[i].Text := '';
    end;
  end;

  for i := low(EditsOdo) to high(EditsOdo) do begin
    if WorldODE.Robots[idx].Wheels.Count > i then begin
      EditsOdo[i].Text := format('%d',[WorldODE.Robots[idx].Wheels[i].Axle.Axis[0].Odo.Value]);
      //EditsOdo[i].Text := format('%d',[WorldODE.Robots[idx].Links[i].Axis.Odo.Value]);
    end else begin
      EditsOdo[i].Text := '';
    end;
  end;

  for i := low(EditsIR) to high(EditsIR) do begin
    if (WorldODE.Robots[idx].Sensors.Count > i) and
       WorldODE.Robots[idx].Sensors[i].Measures[0].has_measure then begin
      EditsIR[i].Text := format('%.2f',[WorldODE.Robots[idx].Sensors[i].Measures[0].value]);
    end else begin
      EditsIR[i].Text := '';
    end;
  end;

  theta := 0;
  with WorldODE.Robots[idx] do begin
    for i := 0 to Axes.Count -1 do begin
      SGJoints.Cells[0,i+1] := Axes[i].ParentLink.ID;
      SGJoints.Cells[4,i+1] := Axes[i].ParentLink.description;

      if (dJointGetType(Axes[i].ParentLink.joint) = ord(dJointTypeHinge)) or
         (dJointGetType(Axes[i].ParentLink.joint) = ord(dJointTypeUniversal)) then begin
        theta := radtodeg(Axes[i].GetPos);
        SGJoints.Cells[2,i+1] := format('%.2f',[radtodeg(Axes[i].ref.theta)]);
      end else if dJointGetType(Axes[i].ParentLink.joint) = ord(dJointTypeSlider) then begin
        theta := Axes[i].GetPos;
        SGJoints.Cells[2,i+1] := format('%.2f',[Axes[i].ref.theta]);
      end;
      SGJoints.Cells[1,i+1] := format('%.2f',[theta]);

      if wp_idx >= 0 then
        SGJoints.Cells[3,i+1] := format('%.2f',[radtodeg(Axes[i].WayPoints[wp_idx].pos)]);

      if i+1 = SGJoints.Selection.Top then begin
        EditJointTeta.Text := SGJoints.Cells[1,i+1];
      end;
    end;
  end;

end;


procedure TFParams.ShowRobotPosition(r: integer);
var x, y, z, teta: double;
begin
  if r <> LBRobots.ItemIndex then exit;
  WorldODE.Robots[r].GetXYZTeta(x, y, z, teta);
  EditRobotX.text := format('%.3f',[x]);
  EditRobotY.text := format('%.3f',[y]);
  EditRobotZ.text := format('%.3f',[z]);
  EditRobotTeta.text := format('%.3f',[radToDeg(teta)]);
end;



procedure TFParams.CBPIDsActiveClick(Sender: TObject);
var i, idx: integer;
begin
  idx := LBRobots.ItemIndex;
  if (idx < 0) or (idx >= WorldODE.Robots.Count) then exit;

  //for i := 0 to WorldODE.Robots[idx].Wheels.Count - 1 do begin
  //  WorldODE.Robots[idx].Wheels[i].Axle.Axis[0].Motor.Controller.active := CBPIDsActive.Checked;
  //end;
  for i := 0 to WorldODE.Robots[idx].Axes.Count - 1 do begin
    WorldODE.Robots[idx].Axes[i].Motor.Controller.active := CBPIDsActive.Checked;
  end;
end;

procedure TFParams.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName := GetIniFineName;

  SGJoints.Cells[0,0] := 'ID';
  SGJoints.Cells[1,0] := 'Pos';
  SGJoints.Cells[2,0] := 'Ref';
  SGJoints.Cells[3,0] := 'WP';
  SGJoints.Cells[4,0] := 'Description';

  SGGlobalSensors.Cells[0,0] := 'Sensor ID';
  SGGlobalSensors.Cells[1,0] := 'Value';
  SGGlobalSensors.Cells[2,0] := 'Kind';

  UDPGenData := TMemoryStream.Create;
  UDPGenPackets:= TStringList.Create;
end;

procedure TFParams.FormShow(Sender: TObject);
var i: integer;
begin
  FillLBRobots(LBRobots);
  if LBRobots.Count>0 then LBRobots.ItemIndex := 0;
  FillEditArray('EditU', EditsU);
  FillEditArray('EditI', EditsI);
  FillEditArray('EditOdo', EditsOdo);
  FillEditArray('EditIR', EditsIR);
  CBIRNoiseClick(Sender);
  BPhysicsSetClick(Sender);
  RGGLObjectsClick(Sender);
  BGlobalSetClick(Sender);

  try
    if FileExists('params.cfg') then begin
      LoadGridFromfile(SGConf, 'params.cfg');
    end;
    FillSGConf;
  except
    on E: Exception do
    Showmessage(E.Message);
  end;
  for i := 1 to SGConf.RowCount-1 do begin
    SGConfRowToVar(i);
    VarToSGConfRow(i);
  end;

  // Vista hack?
  if PageControl.Height + PageControl.Top > ClientHeight - (EditDebug.Height + 8) then begin
    PageControl.Height := - PageControl.Top + ClientHeight - (EditDebug.Height + 8);
    EditDebug.Top := ClientHeight - (EditDebug.Height + 4);
    //EditDebug.Anchors := EditDebug.Anchors - [akbottom];
    //EditDebug.Anchors := EditDebug.Anchors + [akbottom];
  end;

  LBAllTags.Clear;
  WorldODE.getGLPolygonsTags(LBAllTags.Items);

  try
    BSetCamParsClick(Sender);
  except
    on E: Exception do
    Showmessage(E.Message);
  end;
end;

procedure TFParams.FillEditArray(ProtoName: string; var EditArray: array of TEdit);
var i, cnt: integer;
begin
  cnt := low(EditArray);
  for i := 0 to TabRobot.ControlCount-1 do begin
    if cnt > high(EditArray) then exit;
    if LowerCase(TabRobot.Controls[i].Name) = LowerCase(ProtoName + inttostr(cnt)) then begin
      EditArray[cnt] := TEdit(TabRobot.Controls[i]);
      inc(cnt);
    end;
  end;
end;

procedure TFParams.FillLBRobots(LB: TListBox);
var i: integer;
begin
  for i:=0 to WorldODE.Robots.Count-1 do begin
    with WorldODE.Robots[i] do begin
      LB.Items.Add(format('%s',[Name]));
    end;
  end;
end;

procedure TFParams.ShowParsedScene;
var r, i: integer;
begin
  MemoDebug.Lines.Add(format('Robots: %d',[WorldODE.Robots.Count]));
  for r:=0 to WorldODE.Robots.Count-1 do begin
    MemoDebug.Lines.Add(format('Robot[%d]',[r]));
    MemoDebug.Lines.Add(format('  axes: %d',[WorldODE.Robots[r].Axes.Count]));
    for i:=0 to WorldODE.Robots[r].Axes.Count-1 do begin
      with WorldODE.Robots[r].Axes[i] do begin
        MemoDebug.Lines.Add(format('  axis[%d]: %s',[i, ParentLink.ID]));
        with Motor.Controller do begin
          MemoDebug.Lines.Add(format('    Mode: %s, Active: %d, Period: %g ms',[ControlModeNames[ControlMode], ord(active), 1000*ControlPeriod]));
          MemoDebug.Lines.Add(format('    kp: %g, ki: %g, kd: %g, kf: %g',[Kp, Ki, Kd, Kf]));
        end;
        with Motor do begin
          MemoDebug.Lines.Add(format('    Ri: %g, Li: %g, K: %g Active: %d',[Ri, Li, Ki, ord(active)]));
          MemoDebug.Lines.Add(format('    Gear: %g, Vmax: %g, Imax: %g',[GearRatio, Vmax, Imax]));
        end;
      end;
    end;
  end;
end;

procedure TFParams.FillLBLinks(LB: TListBox; r: integer);
var i: integer;
begin
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[r] do begin
    for i := 0 to Links.Count -1 do begin
      //LB.Items.Add(inttostr(Links[i].ID) + ': ' + Links[i].description);
      LB.Items.Add(Links[i].description);
    end;
  end;
end;

procedure TFParams.BSetPositionClick(Sender: TObject);
var r: integer;
    dx, dy, dz, dteta: double;
begin
  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  dx := StrToFloatDef(EditRobotSetX.Text, 0);
  dy := StrToFloatDef(EditRobotSetY.Text, 0);
  dz := StrToFloatDef(EditRobotSetZ.Text, 0);
  dteta := degtorad(StrToFloatDef(EditRobotSetTeta.Text, 0));

  WorldODE.Robots[r].SetXYZTeta(dx, dy, dz, dteta);
end;

procedure TFParams.BSetJointTetaRefClick(Sender: TObject);
var r, i: integer;
begin
  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  i := SGJoints.Selection.Top - 1;
  if (i < 0) or (i >= WorldODE.Robots[r].Axes.Count) then exit;

  with WorldODE.Robots[r].Axes[i] do begin
    if (dJointGetType(ParentLink.joint) = ord(dJointTypeHinge)) or
       (dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal)) then begin
      ref.theta := degtorad(strtofloatdef(EditJointTetaRef.Text, 0));
    end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
      ref.theta := strtofloatdef(EditJointTetaRef.Text, 0);
    end;
  end;
end;

procedure TFParams.LBRobotsClick(Sender: TObject);
var r, i, c: integer;
begin
  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  // Set the new Camera Target
  WorldODE.SetCameraTarget(r);

  // Fill the Joints names and IDs
  for i := 0 to SGJoints.RowCount -1 do begin
    for c := 0 to SGJoints.ColCount -1 do begin
      SGJoints.Cells[c,i+1] := '';
    end;
  end;
  with WorldODE.Robots[r] do begin
    for i := 0 to Axes.Count -1 do begin
      SGJoints.Cells[0,i+1] := Axes[i].ParentLink.ID;
      SGJoints.Cells[1,i+1] := Axes[i].ParentLink.description;
    end;
  end;
  // Update Robot Tab
  
end;

procedure TFParams.BFreezeClick(Sender: TObject);
begin
//  FViewer.GLCadencer.Enabled := not FViewer.GLCadencer.Enabled;
  if FViewer.GLCadencer.Mode = cmApplicationIdle then begin
    FViewer.GLCadencer.Mode := cmManual;
  end else if FViewer.GLCadencer.Mode = cmManual then begin
    FViewer.GLCadencer.Mode := cmApplicationIdle;
  end;
end;

procedure TFParams.BStepClick(Sender: TObject);
begin
  FViewer.GLCadencer.MaxDeltaTime := FViewer.GLCadencer.FixedDeltaTime;
  FViewer.GLCadencer.Progress;
  FViewer.GLCadencer.MaxDeltaTime := 0;
end;

procedure TFParams.CBIRNoiseClick(Sender: TObject);
var i, r: integer;
begin
  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  for i := 0 to WorldODE.Robots[r].Sensors.Count - 1 do begin
    WorldODE.Robots[r].Sensors[i].Noise.active := CBIRNoise.Checked;
  end;
end;

procedure TFParams.BLoadJointWayPointsClick(Sender: TObject);
var r: integer;
begin
  r := LBRobots.ItemIndex;
  LoadJointWayPoints(r, EditLoadJointPoints.Text);
{  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;
  // clear actual waypoints
  for i := 0 to WorldODE.Robots[r].Axes.Count-1 do begin
    WorldODE.Robots[r].Axes[i].WayPoints.ClearAll
  end;
  // Load new ones
  WorldODE.LoadJointWayPointsFromXML(EditLoadJointPoints.Text, r);}
end;

procedure TFParams.CBHotCPUClick(Sender: TObject);
begin
  if CBHotCPU.Checked then begin
    FViewer.GLCadencer.SleepLength := -1;
  end else begin
    FViewer.GLCadencer.SleepLength := 1;
  end;
end;

procedure TFParams.CBFreezeClick(Sender: TObject);
begin
  if FViewer.GLCadencer.Mode = cmApplicationIdle then begin
    FViewer.GLCadencer.Mode := cmManual;
  end else if FViewer.GLCadencer.Mode = cmManual then begin
    FViewer.GLCadencer.Mode := cmApplicationIdle;
    // Make one step with fixed time and then release it to avoid a "time jump" 
    FViewer.GLCadencer.MaxDeltaTime := FViewer.GLCadencer.FixedDeltaTime;
    FViewer.GLCadencer.Progress;
    FViewer.GLCadencer.MaxDeltaTime := 0;
  end;
end;

procedure TFParams.BPhysicsSetClick(Sender: TObject);
begin
  WorldODE.default_n_mu := strtofloatdef(EditDefaultFriction.Text, WorldODE.default_n_mu);
  WorldODE.Ode_dt := strtofloatdef(EditOde_dt.Text, WorldODE.Ode_dt * 1E3) * 1E-3;
  WorldODE.TimeFactor := strtofloatdef(EditTimeSpeed.Text, WorldODE.TimeFactor);
  WorldODE.Ode_CFM := strtofloatdef(EditODE_CFM.Text, WorldODE.Ode_CFM);
  WorldODE.Ode_ERP := strtofloatdef(EditODE_ERP.Text, WorldODE.Ode_ERP);
  WorldODE.Ode_QuickStepIters := strtointdef(EditQuickStepIterations.Text, WorldODE.Ode_QuickStepIters);
  with WorldODE do begin
    dWorldSetCFM(world, Ode_CFM);
    dWorldSetERP(world, Ode_ERP);
    dWorldSetQuickStepNumIterations(world, Ode_QuickStepIters);
  end;
end;

procedure TFParams.BSetJointWayPointTetaClick(Sender: TObject);
var r, i, wp_idx: integer;
begin
  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  i := SGJoints.Selection.Top - 1;
  if (i < 0) or (i >= WorldODE.Robots[r].Axes.Count) then exit;

  wp_idx := ComboWayPointName.ItemIndex;
  if (wp_idx < 0) then exit;

  WorldODE.Robots[r].Axes[i].WayPoints[wp_idx].pos := degtorad(strtofloatdef(EditJointTetaRef.Text, 0));
end;

procedure TFParams.BSetAllClick(Sender: TObject);
var r, i, wp_idx: integer;
begin
  r := LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  wp_idx := ComboWayPointName.ItemIndex;
  if (wp_idx < 0) then exit;

  for i:= 0 to WorldODE.Robots[r].Axes.Count-1 do begin
    WorldODE.Robots[r].Axes[i].ref.theta := WorldODE.Robots[r].Axes[i].WayPoints[wp_idx].pos;
  end;
end;

procedure TFParams.BJointWayPointsSaveClick(Sender: TObject);
var r: integer;
begin
  r := LBRobots.ItemIndex;
  SaveJointWayPoints(r, EditLoadJointPoints.Text);
end;

procedure TFParams.BWayPointEditClick(Sender: TObject);
begin
  FWayPointsEdit.showmodal;
end;

procedure TFParams.ComboWayPointNameUpdate(robot: TRobot);
var i: integer;
begin
  ComboWayPointName.Clear;
  if Robot.Axes.Count > 0 then begin
    for i := 0 to Robot.AxesWayPointsIDs.Count -1 do begin
      with Robot.Axes[0].WayPoints[i] do
        FParams.ComboWayPointName.AddItem(format('%.2f: %s',[t, Robot.AxesWayPointsIDs[i]]),nil);
    end;
  end;

  if FParams.ComboWayPointName.Items.Count > 0 then
    FParams.ComboWayPointName.ItemIndex := 0;
end;


procedure TFParams.FillSGConfRow(varname: string);
var i: integer;
begin
  for i := 0 to SGConf.RowCount -1 do begin
    if SGConf.Cells[0, i] = varname then exit;
    if SGConf.Cells[0, i] = '' then begin
      SGConf.Cells[0, i] := varname;
      VarToSGConfRow(i);
      break;
    end;
  end;
end;

procedure TFParams.FillSGConf;
begin
  FillSGConfRow('Camera.Position');
  FillSGConfRow('Light.Position');
  FillSGConfRow('Light.Attenuation');
  FillSGConfRow('Floor.Ambient.Color');
end;


procedure TFParams.SGConfRowToVar(SGRow: longword);
var varname: string;
    v: TVector;
begin
  varname := SGConf.Cells[0, SGRow];
  if varname = 'Camera.Position' then begin
    FViewer.GLDummyCamPos.Position.SetPoint(GetVectorFromGrid(SGConf, 'Camera.Position', FViewer.GLDummyCamPos.Position.AsAffineVector));
  end else if varname = 'Light.Position' then begin
    FViewer.GLLightSource.Position.SetPoint(GetVectorFromGrid(SGConf, 'Light.Position', FViewer.GLLightSource.Position.AsAffineVector));
  end else if varname = 'Light.Attenuation' then begin
    FViewer.GLLightSource.ConstAttenuation := GetFloatFromGrid(SGConf, 'Light.Attenuation', 1, FViewer.GLLightSource.ConstAttenuation);
  end else if varname = 'Floor.Ambient.Color' then begin                                     //FViewer.GLPlaneFloor.Material.FrontProperties.Ambient.Color
    SetVector(v,
              TAffineVector(GetVectorFromGrid(SGConf, 'Floor.Ambient.Color', AffineVectorMake(FViewer.GLPlaneFloor.Material.FrontProperties.Ambient.Color))),
              1.0);
    FViewer.GLPlaneFloor.Material.FrontProperties.Ambient.Color := v;
  end;
end;

procedure TFParams.VarToSGConfRow(SGRow: longword);
var varname: string;
begin
  varname := SGConf.Cells[0, SGRow];
  if varname = 'Camera.Position' then begin
    WriteVectorToGrid(SGConf, 'Camera.Position', FViewer.GLDummyCamPos.Position.AsAffineVector);
  end else if varname = 'Light.Position' then begin
    WriteVectorToGrid(SGConf, 'Light.Position', FViewer.GLLightSource.Position.AsAffineVector);
  end else if varname = 'Light.Attenuation' then begin
    WriteFloatToGrid(SGConf, 'Light.Attenuation', 1, FViewer.GLLightSource.ConstAttenuation);
  end else if varname = 'Floor.Ambient.Color' then begin
    WriteVectorToGrid(SGConf, 'Floor.Ambient.Color', AffineVectorMake(FViewer.GLPlaneFloor.Material.FrontProperties.Ambient.Color));
  end;
end;


procedure TFParams.BSGConfSetClick(Sender: TObject);
begin
  SGConf.Cells[1, SGConf.Row] := EditGridX.Text;
  SGConf.Cells[2, SGConf.Row] := EditGridY.Text;
  SGConf.Cells[3, SGConf.Row] := EditGridZ.Text;
  SGConfRowToVar(SGConf.Row);
end;

procedure TFParams.SGConfDblClick(Sender: TObject);
begin
//  VarToSGConfRow(SGConf.Row);
  BSGConfGetClick(Sender);
//  EditGridX.Text := SGConf.Cells[1, SGConf.Row];
//  EditGridY.Text := SGConf.Cells[2, SGConf.Row];
//  EditGridZ.Text := SGConf.Cells[3, SGConf.Row];
end;

procedure TFParams.FormDestroy(Sender: TObject);
begin
  UDPGenPackets.Free;
  UDPGenData.Free;
  try
     SaveGridTofile(SGConf, 'params.cfg');
  except
    on E: Exception do
    Showmessage(E.Message);
  end;
end;

procedure TFParams.EditGridKeyPress(Sender: TObject; var Key: Char);
begin
//  EditGridX.Text := inttostr(ord(key));
  if key = #13 then BSGConfSetClick(FParams);
end;

procedure TFParams.BComConfClick(Sender: TObject);
begin
  ComPort.ShowSetupDialog;
end;

procedure TFParams.CBComOpenClick(Sender: TObject);
begin
  if CBComOpen.Checked then begin
    if not ComPort.Connected then begin
      try
        ComPort.open;
      except
        on E: exception do begin
          CBComOpen.Checked := false;
          showmessage(E.Message);
        end;
      end;
    end;
  end else begin
    ComPort.Close;
  end;
end;


function ReadComPort: string;
begin
  with FParams.ComPort do begin
    try
      ReadStr(result, InputCount);
    except
      on E: exception do begin
        showmessage(E.Message);
      end;
    end;
  end;
end;


procedure WriteComPort(s: string);
begin
  try
    FParams.ComPort.WriteStr(s);
    //FParams.EditComRead.Text := s;
  except
    on E: exception do begin
      showmessage(E.Message);
    end;
  end;
end;


procedure TFParams.BComReadClick(Sender: TObject);
begin
  EditComRead.Text := ReadComPort;
end;

procedure TFParams.BComWriteClick(Sender: TObject);
begin
  WriteComPort(EditComWrite.Text);
end;

procedure TFParams.CBUDPConnectClick(Sender: TObject);
begin
  try
    UDPGeneric.DefaultPort := strtoint(EditUDPPort.Text);
    UDPGeneric.Active := CBUDPConnect.Checked;
  except
    on E: exception do begin
      CBUDPConnect.Checked := UDPGeneric.Active;
      showmessage(E.Message);
    end;
  end;
end;

procedure TFParams.UDPGenericUDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var str: string;
begin
  //UDPGenData.Clear;
  UDPGenData.CopyFrom(AData, 0);

  str := '';
  AData.Position := 0;
  SetLength(str, AData.Size);
  AData.ReadBuffer(Pointer(str)^, AData.Size);
  //AData.Clear;

  UDPGenPackets.Add(str);
end;


procedure TFParams.BGlobalSetClick(Sender: TObject);
begin
  WorldODE.DecPeriod := strtoint(EditScriptPeriod.Text)/1000;
end;

procedure TFParams.ShowCameraConfig(GLCamera: TGLCamera);
begin
  EditCamX.Text:=format('%.2f',[GLCamera.Position.x]);
  EditCamY.Text:=format('%.2f',[GLCamera.Position.y]);
  EditCamZ.Text:=format('%.2f',[GLCamera.Position.z]);

  EditCamLookX.Text:=format('%.2f',[GLCamera.TargetObject.Position.x]);
  EditCamLookY.Text:=format('%.2f',[GLCamera.TargetObject.Position.y]);
  EditCamLookZ.Text:=format('%.2f',[GLCamera.TargetObject.Position.z]);
end;

procedure TFParams.BSetCamParsClick(Sender: TObject);
var x, y, z: double;
begin
  //FViewer.GLCamera.Position.SetPoint
  x := strtofloatdef(EditSetCamX.Text, 1);
  y := strtofloatdef(EditSetCamY.Text, -1);
  z := strtofloatdef(EditSetCamZ.Text, 1);
  FViewer.GLDummyCamPos.Position.SetPoint(x,y,z);

  x := strtofloatdef(EditSetCamLookX.Text, 0);
  y := strtofloatdef(EditSetCamLookY.Text, 0);
  z := strtofloatdef(EditSetCamLookZ.Text, 0);
  FViewer.GLDummyTargetCam.Position.SetPoint(x,y,z);
  //FViewer.GLCamera.TargetObject.Position.SetPoint(x,y,z);
end;

procedure TFParams.BGetCamPosClick(Sender: TObject);
begin
  EditSetCamX.Text := EditCamX.Text;
  EditSetCamY.Text := EditCamY.Text;
  EditSetCamZ.Text := EditCamZ.Text;
end;

procedure TFParams.Button1Click(Sender: TObject);
begin
  EditSetCamLookX.Text := EditCamLookX.Text;
  EditSetCamLookY.Text := EditCamLookY.Text;
  EditSetCamLookZ.Text := EditCamLookZ.Text;
end;

procedure TFParams.BSetTrailParsClick(Sender: TObject);
begin
  FViewer.SetTrailCount(strtointdef(FParams.EditTrailsCount.Text, 8), strtointdef(FParams.EditTrailSize.Text, 200));
end;

procedure TFParams.RGGLObjectsClick(Sender: TObject);
var r, i: integer;
begin
  for r := 0 to WorldODE.Robots.Count - 1 do begin
    for i := 0 to WorldODE.Robots[r].Solids.Count - 1 do begin
      with WorldODE.Robots[r].Solids[i] do begin
        GLObj.Visible := true;
        if AltGLObj <> nil then begin
          AltGLObj.Visible := RGGLObjects.ItemIndex in [1, 2];   // 0 - Original  1 - Mesh  2 - Both
          GLObj.Visible := RGGLObjects.ItemIndex in [0, 2];
        end;
        //if ShadowGlObj <> nil then
        //  ShadowGlObj.Visible := RGGLObjects.ItemIndex > 0;
          //idx := (WorldODE.OdeScene as TGLShadowVolume).Occluders.IndexOfCaster(ShadowGlObj);
          //if idx >= 0 then begin
          //  (WorldODE.OdeScene as TGLShadowVolume).Occluders[idx].CastingMode := scmVisible;
          //end;
      end;
    end;
  end;
end;

procedure TFParams.ShowGlobalState;
var i: integer;
begin
  with WorldODE do begin
    for i := 0 to Sensors.Count - 1 do begin

      SGGlobalSensors.Cells[0,i+1] := Sensors[i].ID;
      SGGlobalSensors.Cells[2,i+1] := SensorKindStrings[Sensors[i].kind];

      if Sensors[i].kind = skIRSharp then begin
        if Sensors[i].Measures[0].has_measure then begin
          SGGlobalSensors.Cells[1,i+1] := format('%.2f', [Sensors[i].Measures[0].value]);
        end else begin
          SGGlobalSensors.Cells[1,i+1] := '';
        end;
      end else begin
        SGGlobalSensors.Cells[1,i+1] := format('%g', [Sensors[i].Measures[0].value]);
      end;

    end;
  end;
end;

procedure TFParams.BExportTrackClick(Sender: TObject);
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if CBTags.Checked then begin
      WorldODE.exportGLPolygonsText(SL, LBSelectedTags.Items);
    end else begin
      WorldODE.exportGLPolygonsText(SL, nil);
    end;
    SL.SaveToFile('track.txt');
  finally
    SL.Free;
  end;
end;


procedure TFParams.BitBtnAddTagsClick(Sender: TObject);
begin
  if LBAllTags.ItemIndex >= 0 then begin
    if LBSelectedTags.Items.indexof(LBAllTags.Items[LBAllTags.Itemindex]) < 0 then
      LBSelectedTags.Items.Add(LBAllTags.Items[LBAllTags.Itemindex]);
  end;
end;

procedure TFParams.BitBtnRemoveTagsClick(Sender: TObject);
begin
  if LBSelectedTags.ItemIndex >= 0 then begin
      LBSelectedTags.Items.Delete(LBSelectedTags.Itemindex);
  end;
end;

procedure TFParams.BitBtnAddAllClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to LBAllTags.Count -1 do begin
    if LBSelectedTags.Items.indexof(LBAllTags.Items[i]) < 0 then
      LBSelectedTags.Items.Add(LBAllTags.Items[i]);
  end;
end;

procedure TFParams.RGSensorGLClick(Sender: TObject);
var r, i: integer;
begin
  for r := 0 to WorldODE.Robots.Count - 1 do begin
    for i := 0 to WorldODE.Robots[r].Sensors.Count - 1 do begin
      with WorldODE.Robots[r].Sensors[i] do begin
        case RGSensorGL.ItemIndex of
          0: GLObj.Visible := true;
          1: GLObj.Visible := (Rays.Count <= 1);
          2: GLObj.Visible := false;
        end;
      end;
    end;
  end;

  for i := 0 to WorldODE.Sensors.Count - 1 do begin
    with WorldODE.Sensors[i] do begin
      case RGSensorGL.ItemIndex of
        0: GLObj.Visible := true;
        1: GLObj.Visible := (Rays.Count <= 1);
        2: GLObj.Visible := false;
      end;
    end;
  end;
end;

procedure TFParams.BSGConfGetClick(Sender: TObject);
begin
  EditGridX.Text := SGConf.Cells[1, SGConf.Row];
  EditGridY.Text := SGConf.Cells[2, SGConf.Row];
  EditGridZ.Text := SGConf.Cells[3, SGConf.Row];
end;

procedure TFParams.FormResize(Sender: TObject);
begin
  if IsIconic(handle) then begin  //... form was minimized.
    hide;
  end;
end;

end.


