unit Params;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, GLWin32Viewer, GLcontext, Math,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, IdSocketHandle, ODERobots, OdeImport,
  rxPlacemnt, Grids, GLCadencer, CPort;

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
    MemoCameraConfig: TMemo;
    EditDebug3: TEdit;
    MemoDebug: TMemo;
    FormStorage: TFormStorage;
    LBRobots: TListBox;
    PGRobots: TPageControl;
    TabRobot: TTabSheet;
    TabAxis: TTabSheet;
    Label1: TLabel;
    Label4: TLabel;
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
    EditMotUnom: TEdit;
    EditMotSpeedRef: TEdit;
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
    EditRemoteIP: TEdit;
    Label8: TLabel;
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
    CBGLObject: TCheckBox;
    CBAltGLObject: TCheckBox;
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
    procedure SGConfSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
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
    procedure CBGLObjectClick(Sender: TObject);
  private
    procedure FillEditArray(ProtoName: string;
      var EditArray: array of TEdit);
    procedure SGConfRowToVar(SGRow: longword);
    procedure VarToSGConfRow(SGRow: longword);
    procedure FillSGConf;
    procedure FillSGConfRow(var SGRow: longword; varname: string);
    { Private declarations }
  public
    EditsU, EditsI, EditsOdo : array[0..3] of TEdit;
    EditsIR: array[0..7] of TEdit;
    UDPGenData: TMemoryStream;

    procedure FillLBRobots(LB: TListBox);
    procedure FillLBLinks(LB: TListBox; r: integer);
    procedure ShowRobotState;
    procedure ShowRobotRemState(r: integer);
    procedure ComboWayPointNameUpdate(robot: TRobot);
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
begin
//  UDPServer.Send(EditRemoteIP.Text, 9801, 'test');
//  FRemoteControl.show;
  FChart.AddSample(0, WorldODE.physTime);
//  FViewer.GLCadencer.Progress;
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
    if (WorldODE.Robots[idx].IRSensors.Count > i) and
       WorldODE.Robots[idx].IRSensors[i].has_measure then begin
      EditsIR[i].Text := format('%.2f',[WorldODE.Robots[idx].IRSensors[i].measure]);
    end else begin
      EditsIR[i].Text := '';
    end;
  end;

  with WorldODE.Robots[idx] do begin
    for i := 0 to Axes.Count -1 do begin
      SGJoints.Cells[0,i+1] := Axes[i].ParentLink.ID;
      SGJoints.Cells[1,i+1] := Axes[i].ParentLink.description;

      if dJointGetType(Axes[i].ParentLink.joint) = ord(dJointTypeHinge) then begin
        theta := radtodeg(Axes[i].GetPos);
        SGJoints.Cells[3,i+1] := format('%.1f',[radtodeg(Axes[i].ref.theta)]);
      end else if dJointGetType(Axes[i].ParentLink.joint) = ord(dJointTypeSlider) then begin
        theta := Axes[i].GetPos;
        SGJoints.Cells[3,i+1] := format('%.1f',[Axes[i].ref.theta]);
      end;
      SGJoints.Cells[2,i+1] := format('%.1f',[theta]);

      if wp_idx >= 0 then
        SGJoints.Cells[4,i+1] := format('%.1f',[radtodeg(Axes[i].WayPoints[wp_idx].pos)]);

      if i+1 = SGJoints.Selection.Top then begin
        EditJointTeta.Text := SGJoints.Cells[2,i+1];
      end;
    end;
  end;

end;


procedure TFParams.ShowRobotRemState(r: integer);
begin
  if r <> LBRobots.ItemIndex then exit;
  with RemState do begin
    EditRobotX.text := format('%.3f',[Robot.x]);
    EditRobotY.text := format('%.3f',[Robot.y]);
    EditRobotTeta.text := format('%.3f',[radToDeg(Robot.teta)]);
  end;
end;



procedure TFParams.CBPIDsActiveClick(Sender: TObject);
var i, idx: integer;
begin
  idx := LBRobots.ItemIndex;
  if (idx < 0) or (idx >= WorldODE.Robots.Count) then exit;

  for i := 0 to WorldODE.Robots[idx].Wheels.Count - 1 do begin
    WorldODE.Robots[idx].Wheels[i].Axle.Axis[0].Motor.Controller.active := CBPIDsActive.Checked;
  end;
end;

procedure TFParams.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName := GetIniFineName;

  SGJoints.Cells[0,0] := 'ID';
  SGJoints.Cells[1,0] := 'Description';
  SGJoints.Cells[2,0] := 'Pos';
  SGJoints.Cells[3,0] := 'Ref';
  SGJoints.Cells[4,0] := 'WP';

  UDPGenData := TMemoryStream.Create;
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
  try
    if FileExists('params.cfg') then begin
      LoadGridFromfile(SGConf, 'params.cfg');
    end else begin;
      FillSGConf;
    end;
  except
    on E: Exception do
    Showmessage(E.Message);
  end;
  for i := 1 to SGConf.RowCount-1 do begin
    SGConfRowToVar(i);
    VarToSGConfRow(i);
  end;
  CBGLObjectClick(Sender);
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
    if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
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

  for i := 0 to WorldODE.Robots[r].IRSensors.Count - 1 do begin
    WorldODE.Robots[r].IRSensors[i].Noise.active := CBIRNoise.Checked;
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
  with WorldODE do begin
    dWorldSetCFM(world, Ode_CFM);
    dWorldSetERP(world, Ode_ERP);
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


procedure TFParams.SGConfSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
//  EditGridX.Text := SGConf.Cells[1, Arow];
//  EditGridY.Text := SGConf.Cells[2, Arow];
//  EditGridZ.Text := SGConf.Cells[3, Arow];
end;

procedure TFParams.FillSGConfRow(var SGRow: longword; varname: string);
begin
  SGConf.Cells[0, SGRow] := varname;
  VarToSGConfRow(SGRow);
  inc(SGRow);
end;

procedure TFParams.FillSGConf;
var SGRow: longword;
begin
  SGRow := 1;
  FillSGConfRow(SGRow, 'Camera.Position');
  FillSGConfRow(SGRow, 'Light.Position');
  FillSGConfRow(SGRow, 'Light.Attenuation');
end;


procedure TFParams.SGConfRowToVar(SGRow: longword);
var varname: string;
begin
  varname := SGConf.Cells[0, SGRow];
  if varname = 'Camera.Position' then begin
    FViewer.GLDummyCamPos.Position.SetPoint(GetVectorFromGrid(SGConf, 'Camera.Position', FViewer.GLDummyCamPos.Position.AsAffineVector));
  end else if varname = 'Light.Position' then begin
    FViewer.GLLightSource.Position.SetPoint(GetVectorFromGrid(SGConf, 'Light.Position', FViewer.GLLightSource.Position.AsAffineVector));
  end else if varname = 'Light.Attenuation' then begin
    FViewer.GLLightSource.ConstAttenuation := GetFloatFromGrid(SGConf, 'Light.Attenuation', 1, FViewer.GLLightSource.ConstAttenuation);
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
  EditGridX.Text := SGConf.Cells[1, SGConf.Row];
  EditGridY.Text := SGConf.Cells[2, SGConf.Row];
  EditGridZ.Text := SGConf.Cells[3, SGConf.Row];
end;

procedure TFParams.FormDestroy(Sender: TObject);
begin
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
begin
  //UDPGenData.Clear;
  UDPGenData.CopyFrom(AData, 0);
end;

procedure TFParams.CBGLObjectClick(Sender: TObject);
var r, i: integer;
begin
  for r := 0 to WorldODE.Robots.Count - 1 do begin
    for i := 0 to WorldODE.Robots[r].Solids.Count - 1 do begin
      with WorldODE.Robots[r].Solids[i] do begin
        if AltGLObj <> nil then GLObj.Visible := CBGLObject.Checked;
      end;
    end;
  end;
end;

end.


