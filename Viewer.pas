unit Viewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLWin32Viewer, GLCadencer, ODEImport,
  GLShadowPlane, VectorGeometry, GLGeomObjects, ExtCtrls, ComCtrls,
  GLWindowsFont, keyboard, GLTexture, math, GLSpaceText, Remote,
  GLShadowVolume, GLSkydome, GLGraph, OmniXML, OmniXMLUtils, Contnrs, ODERobots,
  rxPlacemnt, ProjConfig, GLHUDObjects, Menus, GLVectorFileObjects,
  GLFireFX, GlGraphics, OpenGL1x, SimpleParser, GLBitmapFont,
  GLMultiPolygon, GLMesh, jpeg;

type
  TRGBfloat = record
    r, g, b: single;
  end;

//Physic World ODE
type
  TWorld_ODE = class
    //SampleCount: integer;
    Ode_dt, TimeFactor: double;
    Ode_ERP, Ode_CFM: double;
    Ode_QuickStepIters: integer;
    OdeScene: TGLBaseSceneObject;
    MaxWorldX, MaxWorldY, MinWorldX, MinWorldY: double;
    world: PdxWorld;
    space: PdxSpace;
    contactgroup: TdJointGroupID;
    default_n_mu: double;
    AirDensity: double;

    Environment: TSolid;
    Robots: TRobotList;
    Obstacles: TSolidList;
    Things: TSolidList;

    PickSolid: TSolid;
    PickJoint: TdJointID;
    PickPoint, TargetPickPoint: TVector;
    PickDist, PickLinDamping, PickAngularDamping: double;
    TestBody: TSolid;

    ground_box : PdxGeom;
    ground_box2 : PdxGeom;

    ODEEnable : boolean;
    physTime : double;
    SecondsCount, DecPeriod: double;
    Ode_UseQuickStep: boolean;

    XMLFiles, XMLErrors: TStringList;

    destructor destroy; override;
    constructor create;
    procedure WorldUpdate;
  private
    //procedure CreateSubGeomBox(var body: PdxBody; var geom: PdxGeom; xsize, ysize, zsize, x, y, z: double);
    //procedure CreateGlBox(var GLCube: TGLCube; var geom: PdxGeom);

    procedure CreateSolidBox(var Solid: TSolid; bmass, posX, posY, posZ, L, W, H: double);
    procedure CreateSolidCylinder(var Solid: TSolid; cmass, posX, posY, posZ: double; c_radius, c_length: double);
    procedure CreateSolidSphere(var Solid: TSolid; bmass, posX, posY, posZ: double; c_radius: double);

    procedure CreateHingeJoint(var Link: TSolidLink; Solid1, Solid2: TSolid;
      anchor_x, anchor_y, anchor_z, axis_x, axis_y, axis_z: double);
    procedure SetHingeLimits(var Link: TSolidLink; LimitMin, LimitMax: double);

    procedure CreateBoxObstacle(var Obstacle: TSolid; sizeX, sizeY, sizeZ, posX, posY, posZ: double);

    procedure CreateShellBox(var Solid: TSolid; motherbody: PdxBody; posX, posY, posZ, L, W, H: double);
    procedure CreateShellCylinder(var Solid: TSolid; motherbody: PdxBody; posX, posY, posZ, R, H: double);

    procedure UpdateOdometry(Axis: TAxis);
    procedure CreateWheel(Robot: TRobot; Wheel: TWheel; const Pars: TWheelPars; const wFriction: TFriction; const wMotor: TMotor);
    procedure CreateIRSensor(motherbody: PdxBody; IRSensor: TSensor; posX, posY, posZ,
      IR_teta, IR_length, InitialWidth, FinalWidth: double);

    procedure LoadObstaclesFromXML(XMLFile: string; Parser: TSimpleParser);
    procedure LoadSensorsFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
    //procedure LoadHumanoidJointsFromXML(Robot: TRobot; XMLFile: string);
    procedure LoadLinksFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
    procedure ReadFrictionFromXMLNode(var Friction: TFriction; sufix: string; const prop: IXMLNode; Parser: TSimpleParser);
    procedure ReadSpringFromXMLNode(var Spring: TSpring; sufix: string; const prop: IXMLNode; Parser: TSimpleParser);
    procedure ReadMotorFromXMLNode(var Motor: TMotor; sufix: string; const prop: IXMLNode; Parser: TSimpleParser);

    procedure CreateUniversalJoint(var Link: TSolidLink; Solid1,
      Solid2: TSolid; anchor_x, anchor_y, anchor_z, axis_x, axis_y, axis_z,
      axis2_x, axis2_y, axis2_z: double);
    procedure SetUniversalLimits(var Link: TSolidLink; LimitMin, LimitMax,
      Limit2Min, Limit2Max: double);
    procedure LoadSolidsFromXML(SolidList: TSolidList; const root: IXMLNode; Parser: TSimpleParser);
//    procedure CreateBall(bmass, radius, posX, posY, posZ: double);
    procedure LoadWheelsFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
    procedure LoadShellsFromXML(Robot: TRobot;  const root: IXMLNode; Parser: TSimpleParser);
    procedure LoadSceneFromXML(XMLFile: string);
    function LoadRobotFromXML(XMLFile: string; Parser: TSimpleParser): TRobot;
    procedure CreateSliderJoint(var Link: TSolidLink; Solid1,
      Solid2: TSolid; axis_x, axis_y, axis_z: double);
    procedure SetSliderLimits(var Link: TSolidLink; LimitMin, LimitMax: double);
    procedure UpdatePickJoint;
    procedure LoadThingsFromXML(XMLFile: string; Parser: TSimpleParser);
    procedure CreateFixedJoint(var Link: TSolidLink; Solid1,  Solid2: TSolid);
    function GetNodeAttrRealParse(parentNode: IXMLNode; attrName: string; defaultValue: double; const Parser: TSimpleParser): double;
    procedure LoadDefinesFromXML(Parser: TSimpleParser; const root: IXMLNode);
    procedure LoadTrackFromXML(XMLFile: string; Parser: TSimpleParser);
    procedure LoadPolygonFromXML(const root: IXMLNode; Parser: TSimpleParser);
    procedure LoadArcFromXML(const root: IXMLNode; Parser: TSimpleParser);
    procedure LoadLineFromXML(const root: IXMLNode; Parser: TSimpleParser);
    procedure CreateShellSphere(var Solid: TSolid; motherbody: PdxBody;
      posX, posY, posZ, R: double);
//    procedure AxisGLCreate(axis: Taxis; aRadius, aHeight: double);
//    procedure AxisGLSetPosition(axis: Taxis);
  public

    procedure LoadJointWayPointsFromXML(XMLFile: string; r: integer);
    procedure SaveJointWayPointsToXML(XMLFile: string; r: integer);
    procedure SetCameraTarget(r: integer);

    procedure CreatePickJoint(Solid: TSolid; anchor_x, anchor_y, anchor_z: double);
    procedure MovePickJoint(anchor_x, anchor_y, anchor_z: double);
    procedure DestroyPickJoint;
  end;


//Form
type
  TFViewer = class(TForm)
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLSceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLLightSource: TGLLightSource;
    GLDummyTargetCam: TGLDummyCube;
    Timer: TTimer;
    GLSphere_Ball: TGLSphere;
    GLShadowVolume: TGLShadowVolume;
    GLPlaneFloor: TGLPlane;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCylinder1: TGLCylinder;
    GLEarthSkyDome: TGLEarthSkyDome;
    GLXYZGrid: TGLXYZGrid;
    GLDummyCamPos: TGLDummyCube;
    GLDummyCamPosRel: TGLDummyCube;
    GLDummyTargetCamRel: TGLDummyCube;
    GLStoredBitmapFont: TGLStoredBitmapFont;
    GLDummyCubeAxis: TGLDummyCube;
    GLFlatText_X: TGLFlatText;
    GLFlatText_Y: TGLFlatText;
    GLFlatTextOrigin: TGLFlatText;
    GLPolygonArrowX: TGLPolygon;
    GLPolygonArrowY: TGLPolygon;
    GLCylinder2: TGLCylinder;
    FormStorage: TFormStorage;
    GLLinesXY: TGLLines;
    GLHUDTextObjName: TGLHUDText;
    PopupMenu: TPopupMenu;
    MenuConfig: TMenuItem;
    MenuChart: TMenuItem;
    MenuEditor: TMenuItem;
    GLMaterialLibrary3ds: TGLMaterialLibrary;
    GLFireFXManager: TGLFireFXManager;
    GLDummyCFire: TGLDummyCube;
    GLCube1: TGLCube;
    MenuScene: TMenuItem;
    GLHUDTextGeneric: TGLHUDText;
    GLDTrails: TGLDummyCube;
    GLFreeForm1: TGLFreeForm;
    MenuSheets: TMenuItem;
    MenuSnapshot: TMenuItem;
    MenuChangeScene: TMenuItem;
    GLCone1: TGLCone;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuChartClick(Sender: TObject);
    procedure MenuConfigClick(Sender: TObject);
    procedure MenuEditorClick(Sender: TObject);
    procedure MenuSceneClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuSheetsClick(Sender: TObject);
    procedure MenuSnapshotClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuChangeSceneClick(Sender: TObject);
  private
    { Private declarations }
    OldPick : TGLCustomSceneObject;
    my,mx: integer;
    t_delta, t_last, t_act: int64;
    KeyVals: TKeyVals;
    procedure UpdateCamPos(CMode: integer);
    procedure FillRemote(r: integer);
    procedure IRSharpNoiseModel(r, i: integer);
    procedure UpdateGLScene;
    function GLSceneViewerPick(X, Y: Integer): TGLCustomSceneObject;
    procedure TestTexture;
    procedure ShowOrRestoreForm(Fm: TForm);
  public
    HUDStrings: TStringList;
    TrailsCount: integer;
    CurrentProject: string;
    
    procedure SetTrailCount(NewCount, NodeCount: integer);
    procedure AddTrailNode(T: integer; x, y, z: double);
    procedure DelTrailNode(T: integer);
  end;


procedure RFromZYXRotRel(var R: TdMatrix3; angX, angY, angZ: TDreal);
function LoadXML(XMLFile: string; ErrorList: TStringList): IXMLDocument;

var
  FViewer: TFViewer;
  WorldODE: TWorld_ODE;
  RemControl: TRemControl;
  RemState: TRemState;

implementation

{$R *.dfm}

uses ODEGL, Params, Editor, FastChart, RemoteControl, utils, StdCtrls, VerInfo,
  SceneEdit, Sheets;



// this is called by dSpaceCollide when two objects in space are
// potentially colliding.

procedure nearCallback (data : pointer; o1, o2 : PdxGeom); cdecl;
var
  i,n : integer;
  b1, b2: PdxBody;
  c : TdJointID;
  contact : array[0..MAX_CONTACTS-1] of TdContact;
  n_mode: cardinal;
  n_mu, n_mu2, n_soft_cfm, tmp: double;
  n_fdir1 : TdVector3;
  n_motion1: double;
begin
  //exit;
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  // do not collide static objects
  if not assigned(b1) and not assigned(b2) then exit;

  if assigned(b1) and assigned(b2) then begin
    //exit without doing anything if the two bodies are connected by a joint
    //if (dAreConnected(b1, b2)<>0) then exit;
    if (dGeomGetClass(o1) <> dRayClass) and (dGeomGetClass(o2) <> dRayClass) then
      if (dAreConnectedExcluding(b1, b2, dJointTypeContact)<>0) then exit;
  end;

  n := dCollide(o1, o2, MAX_CONTACTS, contact[0].geom, sizeof(TdContact));
  if (n > 0) then  begin
    //FParams.EditDEbug2.Text := '';
    //if((dGeomGetClass(o1) = dRayClass) or (dGeomGetClass(o2) = dRayClass)) then begin

    if dGeomGetClass(o1) = dRayClass then begin
      if (o1.data <> nil) then begin
        with TSensor(o1.data) do begin
          pos := contact[0].geom.pos;
          normal := contact[0].geom.normal;
          dist := contact[0].geom.depth;
          //measure := min(measure, contact[0].geom.depth);
          has_measure:= true;
          MeasuredSolid := o2.data;
        end;
      end;
      //FParams.EditDEbug2.Text := format('%.2f, %.2f %.2f',[contact[0].geom.pos[0], contact[0].geom.pos[1], contact[0].geom.pos[2]]);;
      exit;
    end;
    if dGeomGetClass(o2) = dRayClass then begin
      if (o2.data <> nil) then begin
        with TSensor(o2.data) do begin
          pos := contact[0].geom.pos;
          normal := contact[0].geom.normal;
          dist := contact[0].geom.depth;
          //measure := min(measure, contact[0].geom.depth);
          has_measure:= true;
          MeasuredSolid := o1.data;
        end;
      end;
      exit;
    end;

    n_mode := dContactBounce or
              dContactSoftCFM or
              dContactApprox1;

    n_mu := WorldODE.default_n_mu;
    n_mu2 := n_mu;
    //n_soft_cfm := 0.001;
    n_soft_cfm := 1e-5;

    n_motion1 := 0;
    if (o1.data <> nil) and (TSolid(o1.data).ParSurface.mode <> 0) then begin
      n_mu := TSolid(o1.data).ParSurface.mu;
      n_mu2 := TSolid(o1.data).ParSurface.mu2;
      //n_soft_cfm := max(n_soft_cfm, TSolid(o1.data).ParSurface.soft_cfm);
      n_soft_cfm := TSolid(o1.data).ParSurface.soft_cfm;
    end;
    if (o2.data <> nil) and (TSolid(o2.data).ParSurface.mode <> 0) then begin
      n_mu := sqrt(n_mu * TSolid(o2.data).ParSurface.mu);
      n_mu2 := sqrt(n_mu2 * TSolid(o2.data).ParSurface.mu2);
      n_soft_cfm := max(n_soft_cfm, TSolid(o2.data).ParSurface.soft_cfm);
    end;


    {if((dGeomGetClass(o1) = dSphereClass) and (dGeomGetClass(o2) <> dPlaneClass)) or
      ((dGeomGetClass(o2) = dSphereClass) and (dGeomGetClass(o1) <> dPlaneClass)) then begin
      //n_mode := 0.9;
      n_mu2 := 0.0;
      n_mu := 0.1;
      zeromemory(@n_fdir1[0], sizeof(n_fdir1));
    end else}

    //FParams.EditDebug.Text := format('%d- %.2f %.2f %.2f',[n, n_fdir1[0], n_fdir1[1], n_fdir1[2]]);
    if (o1.data <> nil) and (TSolid(o1.data).kind = skOmniWheel) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1);
        dBodyVectorToWorld(b1, 0, 0, 1, n_fdir1);
        //n_fdir1 := Vector3Cross(contact[0].geom.normal, n_fdir1);
        //dNormalize3(n_fdir1);
        tmp := n_mu2;
        n_mu2 := n_mu;
        n_mu := tmp; //0.001;
    end else if (o2.data <> nil) and (TSolid(o2.data).kind = skOmniWheel) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1);
        dBodyVectorToWorld(b2, 0, 0, 1, n_fdir1);
        //n_fdir1 := Vector3Cross(contact[0].geom.normal, n_fdir1);
        //dNormalize3(n_fdir1);
        tmp := n_mu2;
        n_mu2 := n_mu;
        n_mu := tmp; //0.001;
    end;

    // Conveyor belt case
    if (o1.data <> nil) and (TSolid(o1.data).kind = skMotorBelt) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1 or dContactMotion1);
        dBodyVectorToWorld(b1, 1, 0, 0, n_fdir1);
        n_mu2 := n_mu; //0.9
        n_motion1 := TSolid(o1.data).BeltSpeed;
    end else if (o2.data <> nil) and (TSolid(o2.data).kind = skMotorBelt) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1 or dContactMotion1);
        dBodyVectorToWorld(b2, 1, 0, 0, n_fdir1);
        n_mu2 := n_mu;
        n_motion1 := TSolid(o2.data).BeltSpeed;
    end;

    for i := 0 to n-1 do begin
      with contact[i].surface do begin
        mode := n_mode;
        mu := n_mu;
        mu2 := n_mu2;
        contact[i].fdir1 := n_fdir1;

        //soft_cfm := 0.001;
        soft_cfm := n_soft_cfm;
        bounce := 0.6;
        bounce_vel := 0.001;
        motion1 := n_motion1;
      end;
      c := dJointCreateContact(WorldODE.world, WorldODE.contactgroup, @contact[i]);
      dJointAttach(c, dGeomGetBody(contact[i].geom.g1), dGeomGetBody(contact[i].geom.g2));
    end;
  end;
end;


procedure RFromZYXRotRel(var R: TdMatrix3; angX, angY, angZ: TDreal);
var Rx, Ry, Rz, Ryx: TdMatrix3;
begin
  dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
  dRFromAxisAndAngle(Ry, 0, 1, 0, angY);
  dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

  dMULTIPLY0_333(Ryx, Ry, Rx);
  dMULTIPLY0_333(R, Rz, Ryx);
end;


function CalcPID(var PID: TMotController; ref, ek: double): double;
var {ek,} dek, mk: double;
begin
  result := 0;
  if not PID.active then exit;

  //ek := ref - yk;
  dek := ek - PID.ek_1;
  PID.Sek := PID.Sek + ek;
  mk := PID.Kp * ek + PID.Ki * PID.Sek + PID.Kd * dek + PID.Kf * ref;

  // Anti Windup
  if abs(mk) >= PID.y_sat then begin
    PID.Sek := PID.Sek - ek;
    mk := max(-PID.y_sat, min(PID.y_sat, mk));
  end;

  PID.ek_1 := ek;
  result := mk;
end;

function CalcPD(var PID: TMotController; ref, refw, yk, ywk: double): double;
var ek, ewk, mk: double;
begin
  result := 0;
  if not PID.active then exit;

  ek := ref - yk;
  PID.Sek := PID.Sek + ek;
  //if abs(ek) < degtorad(1) then exit;
  ewk := refw - ywk;
  mk := PID.Ki * PID.Sek + PID.Kp * ek + PID.Kd * ewk + PID.Kf * ref;

  // Anti Windup
 { if abs(mk) >= PID.y_sat then begin
    PID.Sek := PID.Sek - ek;
    mk := max(-PID.y_sat, min(PID.y_sat, mk));
  end;}

  result := mk;
end;


procedure JointGLPosition(var joint: TdJointID; var jointGL: TGLCylinder);
var vtmp: TdVector3;
begin
    with TGLCylinder(jointGL) do begin
      dJointGetHingeAnchor(joint,vtmp);
      Position.x := vtmp[0];
      Position.y := vtmp[1];
      Position.z := vtmp[2];

      dJointGetHingeAxis(joint,vtmp);
      up.x := vtmp[0];
      up.y := vtmp[1];
      up.z := vtmp[2];
    end;
end;

{
function GetNodeAttrRealExpr(parentNode: IXMLNode; attrName: string; defaultValue: real): real;
var attrValue, s: WideString;
begin
  if not GetNodeAttr(parentNode, attrName, attrValue) then begin
    Result := defaultValue;
  end else begin
    s := StringReplace(attrValue, DEFAULT_DECIMALSEPARATOR, DecimalSeparator, [rfReplaceAll]);
    try
      Result := SimpleCalc(s);
    except on E: Exception do
      Result := defaultValue;
    end;
  end;
end; // GetNodeAttrReal }


function TWorld_ODE.GetNodeAttrRealParse(parentNode: IXMLNode; attrName: string; defaultValue: double; const Parser: TSimpleParser): double;
var attrValue, s: WideString;
    err: string;
begin
  if not GetNodeAttr(parentNode, attrName, attrValue) then begin
    Result := defaultValue;
  end else begin
    s := StringReplace(attrValue, DEFAULT_DECIMALSEPARATOR, DecimalSeparator, [rfReplaceAll]);
    try
      Result := Parser.Calc(s);
    except on E: Exception do begin
      Result := defaultValue;
      err := '[Expression error] ' + format('%s(%d): ', [XMLFiles[XMLFiles.count - 1], -1]) + #$0d+#$0A
             + E.Message + ': ' +#$0d+#$0A
             + '"'+ s + '"';

      if XMLErrors <> nil then begin
        XMLErrors.Add(err);
      end else begin
        showmessage(err);
      end;
      end;
    end;
  end;
end; { GetNodeAttrReal }

{
procedure TWorld_ODE.CreateSubGeomBox(var body: PdxBody; var geom: PdxGeom; xsize, ysize, zsize: double; x,y,z: double);
begin
  // create geom in current space
  geom := dCreateBox(space, xsize, ysize, zsize);
  dGeomSetBody(geom, body);
  dGeomSetOffsetPosition(geom, x, y, z);
end;


procedure TWorld_ODE.CreateGlBox(var GLCube: TGLCube; var geom: PdxGeom);
begin
  // gutter_base GLScene
  GLCube := TGLCube(ODEScene.AddNewChild(TGLCube));
  geom.data := GLCube;
  CopyCubeSizeFromBox(GLCube, geom);
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(GLCube);
//  PositionSceneObject(GLCube, geom);
end;
}

procedure TWorld_ODE.CreateSolidBox(var Solid: TSolid; bmass, posX, posY, posZ, L, W, H: double);
var m: TdMass;
begin
  Solid.kind := skDefault;
  Solid.Body := dBodyCreate(world);
  dBodySetPosition(Solid.Body, posX, posY, posZ);

  //dRFromAxisAndAngle (R,0,1,0,0);
  //dBodySetRotation (Solid.Body, R);

  dMassSetBox(m, 1, L, W, H);
  dMassAdjust(m, bmass);
  dBodySetMass(Solid.Body, @m);

  Solid.Geom := dCreateBox(space, L, W, H);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  Solid.Volume := L * W * H;
  Solid.Ax := W * H;
  Solid.Ay := L * H;
  Solid.Az := L * W;
  dBodySetDamping(Solid.Body, Solid.StokesDrag, Solid.RollDrag); // TODO anisotripic rolldrag
//  dBodySetDamping(Solid.Body, Solid.StokesDrag * (L + W + H), Solid.RollDrag * (L + W + H));
//  dBodySetDamping(Solid.Body, 1e-1, 1e-1);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCube));

  PositionSceneObject(Solid.GLObj, Solid.Geom);
  with (Solid.GLObj as TGLCube) do begin
    TagObject := Solid;
    //Scale.x := L;
    //Scale.y := W;
    //Scale.z := H;
    CubeDepth := H;
    CubeHeight := W;
    CubeWidth := L;
    Material.MaterialLibrary := FViewer.GLMaterialLibrary;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);
end;


procedure TWorld_ODE.CreateSolidCylinder(var Solid: TSolid; cmass, posX, posY, posZ: double; c_radius, c_length: double);
var m: TdMass;
//    R: TdMatrix3;
begin
  Solid.kind := skDefault;
  Solid.Body := dBodyCreate(world);
  dBodySetPosition(Solid.Body, posX, posY, posZ);

  dMassSetCylinder(m, 1, 3, c_radius, c_length);
  dMassAdjust(m, cmass);
  dBodySetMass(Solid.Body, @m);

  Solid.Geom := dCreateCylinder(space, c_radius, c_length);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  Solid.Volume :=  Pi* sqr(c_radius) * c_length;
  //TODO falta acertar ax, ay, az
  Solid.Ax := 2 * c_radius * c_length;
  Solid.Ay := 2 * c_radius * c_length;
  Solid.Az := pi * sqr(c_radius);
  dBodySetDamping(Solid.Body, Solid.StokesDrag, Solid.RollDrag); // TODO anisotripic drag
//  dBodySetDamping(Solid.Body, Solid.StokesDrag * (c_radius + c_length), Solid.RollDrag * (c_radius + c_length));
//  dRFromAxisAndAngle(R,1,0,0,pi/2);
//  dGeomSetOffsetRotation(Solid.Geom, R);
//  dBodySetRotation(Solid.Body, R);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCylinder));

  with (Solid.GLObj as TGLCylinder) do begin
    TagObject := Solid;
    TopRadius := c_radius;
    BottomRadius := c_radius;
    Height := c_length;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
    Material.MaterialLibrary := FViewer.GLMaterialLibrary;
    //Material.LibMaterialName := 'LibMaterialBumps';
    //Material.LibMaterialName := 'LibMaterialFeup';
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);

  //PositionSceneObject(Solid.GLObj, Solid.Geom);
  //if Solid.GLObj is TGLCylinder then Solid.GLObj.pitch(90);
end;


procedure TWorld_ODE.CreateSolidSphere(var Solid: TSolid; bmass, posX, posY, posZ, c_radius: double);
var m: TdMass;
begin
  Solid.kind := skDefault;
  Solid.Body := dBodyCreate(world);
  dBodySetPosition(Solid.Body, posX, posY, posZ);

  dMassSetSphereTotal(m, bmass, c_radius);
  dBodySetMass(Solid.Body, @m);

  Solid.Geom := dCreateSphere(space, c_radius);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  Solid.Volume :=  4/3 * Pi * sqr(c_radius) * c_radius;
  Solid.Ax := pi * sqr(c_radius);
  Solid.Ay := pi * sqr(c_radius);
  Solid.Az := pi * sqr(c_radius);
  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLSphere));
  dBodySetDamping(Solid.Body, Solid.StokesDrag, Solid.RollDrag);
//  dBodySetDamping(Solid.Body, Solid.StokesDrag * pi * sqr(c_radius), Solid.RollDrag * c_radius);

  with (Solid.GLObj as TGLSphere) do begin
    TagObject := Solid;
    Radius := c_radius;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
    Material.MaterialLibrary := FViewer.GLMaterialLibrary;
    //Material.LibMaterialName := 'LibMaterialBumps';
    //Material.LibMaterialName := 'LibMaterialFeup';
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);

  //PositionSceneObject(Solid.GLObj, Solid.Geom);
end;



procedure TWorld_ODE.CreateShellBox(var Solid: TSolid; motherbody: PdxBody; posX, posY, posZ, L, W, H: double);
begin
  Solid.kind := skDefault;
  Solid.Body := motherbody;

  Solid.Geom := dCreateBox(space, L, W, H);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  dGeomSetOffsetPosition(Solid.Geom, posX, posY, posZ);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCube));

  with (Solid.GLObj as TGLCube) do begin
    TagObject := Solid;
    Scale.x := L;
    Scale.y := W;
    Scale.z := H;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);
end;



procedure TWorld_ODE.CreateShellCylinder(var Solid: TSolid; motherbody: PdxBody; posX, posY, posZ, R, H: double);
begin
  Solid.kind := skDefault;
  Solid.Body := motherbody;

  Solid.Geom := dCreateCylinder(space, R, H);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  dGeomSetOffsetPosition(Solid.Geom, posX, posY, posZ);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCylinder));

  with (Solid.GLObj as TGLCylinder) do begin
    TagObject := Solid;
    TopRadius := R;
    BottomRadius := R;
    Height := H;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);
end;


procedure TWorld_ODE.CreateShellSphere(var Solid: TSolid; motherbody: PdxBody; posX, posY, posZ, R: double);
begin
  Solid.kind := skDefault;
  Solid.Body := motherbody;

  Solid.Geom := dCreateSphere(space, R);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  dGeomSetOffsetPosition(Solid.Geom, posX, posY, posZ);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLSphere));

  with (Solid.GLObj as TGLSphere) do begin
    TagObject := Solid;
    Radius := R;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);
end;


procedure TWorld_ODE.CreateIRSensor(motherbody: PdxBody; IRSensor: TSensor; posX, posY, posZ: double; IR_teta, IR_length, InitialWidth, FinalWidth: double);
//var R: TdMatrix3;
begin
  //IRSensor.Geom := dCreateRay(space, IR_length);
  IRSensor.Geoms.Add(dCreateRay(space, IR_length));
  dGeomSetBody(IRSensor.Geoms[0], motherbody);
  IRSensor.Geoms[0].data := IRSensor;

//  dRFromAxisAndAngle(R, -sin(IR_teta), cos(IR_teta), 0, pi/2);
//  dGeomSetOffsetRotation(IRSensor.Geom, R);
//  dGeomSetOffsetPosition(IRSensor.Geom, posX, posY, posZ);
  IRSensor.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCylinder));

  with (IRSensor.GLObj as TGLCylinder) do begin
    TopRadius := InitialWidth;
    BottomRadius := FinalWidth;
    Height := IR_length;
    Alignment := caTop;
    Material.FrontProperties.Diffuse.AsWinColor := clred;
    Material.FrontProperties.Diffuse.Alpha := 0.5;
    Material.BlendingMode := bmTransparency;
  end;

end;


procedure TWorld_ODE.CreateBoxObstacle(var Obstacle: TSolid; sizeX, sizeY, sizeZ, posX, posY, posZ: double);
//var R: TdMatrix3;
begin
  Obstacle.kind := skDefault;
  // Create 1 GLSCube and a box space.
  Obstacle.Geom := dCreateBox(space, sizeX, sizeY, sizeZ);
//  dRFromAxisAndAngle(R, 0, 0, 1, obs_teta);
//  dGeomSetRotation(Obstacle.Geom,R);
  dGeomSetPosition(Obstacle.Geom, posX, posY, posZ);
  Obstacle.GLObj := TGLCube(ODEScene.AddNewChild(TGLCube));
  Obstacle.GLObj.TagObject := Obstacle;
  Obstacle.Geom.data := Obstacle;

  CopyCubeSizeFromBox(TGLCube(Obstacle.GLObj), Obstacle.Geom);
  TGLCube(Obstacle.GLObj).Material.MaterialLibrary := FViewer.GLMaterialLibrary;
  PositionSceneObject(Obstacle.GLObj, Obstacle.Geom);
//  PositionSceneObject(TGLBaseSceneObject(PdxGeom(ground_box).data), ground_box);
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Obstacle.GLObj);
end;


{procedure TWorld_ODE.AxisGLCreate(axis: TAxis; aRadius, aHeight: double);
begin
  axis.GLObj := TGLCylinder(ODEScene.AddNewChild(TGLCylinder));
  with TGLCylinder(axis.GLObj) do begin
    BottomRadius := aRadius;
    TopRadius := aRadius;
    height := aHeight;
    Material.FrontProperties.Diffuse.AsWinColor := clred;
  end;

//  AxisGLSetPosition(axis);
end;

procedure TWorld_ODE.AxisGLSetPosition(axis: Taxis);
var vtmp: TdVector3;
begin
  with TGLCylinder(axis.GLObj) do begin
    axis.GetAnchor(vtmp);
    Position.x := vtmp[0];
    Position.y := vtmp[1];
    Position.z := vtmp[2];

    axis.GetDir(vtmp);
    up.x := vtmp[0];
    up.y := vtmp[1];
    up.z := vtmp[2];
  end;
end;}

procedure TWorld_ODE.CreateHingeJoint(var Link: TSolidLink; Solid1, Solid2: TSolid; anchor_x, anchor_y,
  anchor_z, axis_x, axis_y, axis_z: double);
begin
  Link.joint:= dJointCreateHinge(world, nil);
  dJointAttach(Link.joint, Solid1.body, Solid2.body);
  dJointSetHingeAnchor(Link.joint, anchor_x, anchor_y, anchor_z);
  dJointSetHingeAxis(Link.joint, axis_x, axis_y, axis_z);
//  JointGLCreate(idx);

//  dJointSetHingeParam (joint[idx], dParamStopERP, );
//  dJointSetHingeParam(Link.joint, dParamCFM, 1e-8);
//  dJointSetHingeParam(Link.joint, dParamFudgeFactor, 0.1);

  dJointSetHingeParam(Link.joint, dParamStopCFM, 1e-5);
end;

procedure TWorld_ODE.SetHingeLimits(var Link: TSolidLink; LimitMin, LimitMax: double);
begin
  dJointSetHingeParam(Link.joint, dParamLoStop, DegToRad(LimitMin));
  dJointSetHingeParam(Link.joint, dParamHiStop, DegToRad(LimitMax));
end;


procedure TWorld_ODE.CreateSliderJoint(var Link: TSolidLink; Solid1, Solid2: TSolid; axis_x, axis_y, axis_z: double);
begin
  Link.joint:= dJointCreateSlider(world, nil);
  dJointAttach(Link.joint, Solid1.body, Solid2.body);
  //dJointSetHingeAnchor(Link.joint, anchor_x, anchor_y, anchor_z);
  dJointSetSliderAxis(Link.joint, axis_x, axis_y, axis_z);

//  dJointSetHingeParam (joint[idx], dParamStopERP, );
  dJointSetSliderParam(Link.joint, dParamStopCFM, 1e-5);
end;

procedure TWorld_ODE.SetSliderLimits(var Link: TSolidLink; LimitMin, LimitMax: double);
begin
  dJointSetSliderParam(Link.joint, dParamLoStop, LimitMin);
  dJointSetSliderParam(Link.joint, dParamHiStop, LimitMax);
end;

procedure TWorld_ODE.CreateFixedJoint(var Link: TSolidLink; Solid1, Solid2: TSolid);
begin
  Link.joint:= dJointCreateFixed(world, nil);
  dJointAttach(Link.joint, Solid1.body, Solid2.body);
  dJointSetFixed(Link.joint);

//  dJointSetHingeParam (joint[idx], dParamStopERP, );
//  dJointSetF Param(Link.joint, dParamStopCFM, 1e-5);
end;


procedure TWorld_ODE.CreateUniversalJoint(var Link: TSolidLink; Solid1, Solid2: TSolid; anchor_x, anchor_y,
  anchor_z, axis_x, axis_y, axis_z, axis2_x, axis2_y, axis2_z: double);
begin
  Link.joint:= dJointCreateUniversal(world, nil);
  dJointAttach(Link.joint, Solid1.body, Solid2.body);
  dJointSetUniversalAnchor(Link.joint, anchor_x, anchor_y, anchor_z);
  dJointSetUniversalAxis1(Link.joint, axis_x, axis_y, axis_z);
  dJointSetUniversalAxis2(Link.joint, axis2_x, axis2_y, axis2_z);
//  JointGLCreate(idx);

//  dJointSetUniversalParam(joint[idx], dParamStopERP, );
  dJointSetUniversalParam(Link.joint, dParamStopCFM, 1e-5);
end;

procedure TWorld_ODE.SetUniversalLimits(var Link: TSolidLink; LimitMin, LimitMax, Limit2Min, Limit2Max: double);
begin
  dJointSetUniversalParam(Link.joint, dParamLoStop, DegToRad(LimitMin));
  dJointSetUniversalParam(Link.joint, dParamHiStop, DegToRad(LimitMax));

  dJointSetUniversalParam(Link.joint, dParamLoStop2, DegToRad(Limit2Min));
  dJointSetUniversalParam(Link.joint, dParamHiStop2, DegToRad(Limit2Max));
end;

procedure TWorld_ODE.CreatePickJoint(Solid: TSolid; anchor_x, anchor_y, anchor_z: double);
begin
  if Solid = nil then exit;
  if Solid.Body = nil then exit;
  if PickJoint <> nil then dJointDestroy(PickJoint);

  PickJoint:= dJointCreateBall(world, nil);
  makevector(PickPoint, anchor_x, anchor_y, anchor_z);
  makevector(TargetPickPoint, anchor_x, anchor_y, anchor_z);

  dJointAttach(PickJoint, Solid.body, nil);
  dJointSetBallAnchor(PickJoint, anchor_x, anchor_y, anchor_z);
  PickLinDamping := dBodyGetLinearDamping(Solid.Body);
  PickAngularDamping := dBodyGetAngularDamping(Solid.Body);
  dBodySetDamping(Solid.Body, 1e-2, 1e-1);
//  JointGLCreate(idx);

//  dJointSetBallParam(PickJoint, dParamStopERP, );
//  dJointSetBallParam(PickJoint, dParamStopCFM, 1e-5);
end;

procedure TWorld_ODE.MovePickJoint(anchor_x, anchor_y, anchor_z: double);
begin
  if PickJoint = nil then exit;
  if PickSolid = nil then exit;
  if PickSolid.Body = nil then exit;

  MakeVector(targetPickPoint, anchor_x, anchor_y, anchor_z);
end;

procedure TWorld_ODE.UpdatePickJoint;
var //R: TdMatrix3;
    P: TdVector3;
    oP: TdVector3;
    lambda: double;
    anchor_x, anchor_y, anchor_z: double;
begin
  if PickJoint = nil then exit;
  if PickSolid = nil then exit;
  if PickSolid.Body = nil then exit;

  //R := PickSolid.GetRotation;
  P := PickSolid.GetPosition;
  dJointGetBallAnchor(PickJoint, oP);

  lambda := 0.9;

  anchor_x := (1-lambda) * targetPickPoint[0] + lambda * oP[0];
  anchor_y := (1-lambda) * targetPickPoint[1] + lambda * oP[1];
  anchor_z := (1-lambda) * targetPickPoint[2] + lambda * oP[2];

  PickSolid.SetPosition(P[0] + (anchor_x - oP[0]), P[1] + (anchor_y - oP[1]), P[2] + (anchor_z - oP[2]));

  dJointSetBallAnchor(PickJoint, anchor_x, anchor_y, anchor_z);
  PickSolid.SetPosition(P[0], P[1], P[2]);
  //PickSolid.SetRotation(R);
end;


procedure TWorld_ODE.DestroyPickJoint;
begin
  //dJointAttach(PickJoint, nil, nil);
  if PickJoint <> nil then begin
    //dJointAttach(PickJoint, nil, nil);
    dJointDestroy(PickJoint);
    dBodySetDamping(PickSolid.Body, PickLinDamping, PickAngularDamping);
    PickJoint := nil;
    PickSolid := nil;
  end;
end;

{
procedure TPlayer_Ragdoll.JointGLCreate(idx: integer);
begin
  jointGL[idx] := TGLCylinder(frmRagdoll.ODEScene.AddNewChild(TGLCylinder));
  with TGLCylinder(jointGL[idx]) do begin
    BottomRadius := 0.2*uHead;
    TopRadius := 0.2*uHead;
    height := 0.7*uHead;
    Material.FrontProperties.Diffuse.AsWinColor := clRed;
  end;

  JointGLPosition(idx);
end;


procedure TPlayer_Ragdoll.JointGLPosition(idx: integer);
var vtmp: TdVector3;
begin
    with TGLCylinder(jointGL[idx]) do begin
      dJointGetHingeAnchor(joint[idx],vtmp);
      Position.x := vtmp[0];
      Position.y := vtmp[1];
      Position.z := vtmp[2];

      dJointGetHingeAxis(joint[idx],vtmp);
      up.x := vtmp[0];
      up.y := vtmp[1];
      up.z := vtmp[2];
    end;
end;
}

procedure TWorld_ODE.UpdateOdometry(Axis: TAxis);
var rot: double;
begin
  with Axis do begin
    Odo.LastAngle := Odo.Angle;
    Odo.Angle := Axis.GetPos;
    rot := Odo.Angle - Odo.LastAngle;
    if rot >= pi then rot := rot - 2*pi;
    if rot <= -pi then rot := rot + 2*pi;

    Odo.Residue :=  Odo.Residue + Motor.Encoder.PPR * rot / (2*pi);// + randg(0,OdoNoise);
    Odo.LastValue := Odo.Value;
    odo.Value := floor(Odo.Residue);
    Odo.Residue := Odo.Residue - Odo.Value;
    Odo.AccValue := Odo.AccValue + odo.Value;
    //FParams.editdebug2.Text := format('%d %d',[odo.Value, Odo.AccValue]);
  end;
end;



procedure TWorld_ODE.CreateWheel(Robot: TRobot; Wheel: TWheel; const Pars: TWheelPars; const wFriction: TFriction; const wMotor: TMotor);
var wdx,wdy: double;
    newTyre: TSolid;
    newLink: TSolidLink;
begin
  wdx := Pars.CenterDist * cos(Pars.Angle);
  wdy := Pars.CenterDist * sin(Pars.Angle);

  newTyre := TSolid.Create;
  Robot.Solids.Add(newTyre);
  newTyre.ID := '-1';
  newTyre.description := 'Tyre '+inttostr(round(deg(Pars.Angle)));
  // The cylinder is created with its axis vertical (Z alligned)
  CreateSolidCylinder(newTyre, Pars.mass, wdx, wdy, Pars.Radius, Pars.Radius, Pars.Width);
  newTyre.SetRotation(-sin(Pars.Angle), cos(Pars.Angle), 0, pi/2);
  newTyre.MovePosition(Pars.offsetX, Pars.offsetY, Pars.offsetZ);
  newTyre.SetZeroState();

  if Pars.omni then begin
    newTyre.kind := skOmniWheel;
  end;
  newTyre.ParSurface.mode := $FF;
  newTyre.ParSurface.mu := Pars.Mu;
  newTyre.ParSurface.mu2 := Pars.Mu2;
  newTyre.ParSurface.soft_cfm := Pars.soft_cfm;

  newLink := TSolidLink.Create;
  Robot.Links.Add(newLink);
  CreateHingeJoint(newLink, newTyre, Robot.MainBody,
                   Pars.offsetX + wdx, Pars.offsetY + wdy, Pars.offsetZ + Pars.Radius,
                   -wdx, -wdy, 0);

  newLink.ID := inttostr(Robot.Links.Count);
  newLink.description := 'Wheel' + newLink.ID;
  newLink.Axis[0] := TAxis.Create;
  newLink.Axis[0].ParentLink := newLink;
  Robot.Axes.Add(newLink.Axis[0]);

  with newLink.Axis[0] do begin
    Friction := wFriction;
    Motor := wMotor;
    //Odo :=  wAxis.Odo;
  end;

  Wheel.Pars := Pars;
  Wheel.active := true;
  Wheel.Tyre := newTyre;
  Wheel.Axle := newLink;
end;

{function LoadXMLattrXYZ(const node: IXMLNode; var x,y,z: double): boolean;
var at: IXMLNode;
begin
  result := false;
  at := prop.Attributes.GetNamedItem('x');
  if at <> nil then exit;
  x := strtofloatdef(at.NodeValue, x);
  //if not TryStrToFloat(at.NodeValue, x) then exit;
end;}


//procedure TWorld_ODE.LoadHumanoidBonesFromXML(Robot: TRobot; XMLFile: string);
//procedure TWorld_ODE.LoadSolidsFromXML(Robot: TRobot; const root: IXMLNode);
procedure TWorld_ODE.LoadSolidsFromXML(SolidList: TSolidList; const root: IXMLNode; Parser: TSimpleParser);
var bone, prop: IXMLNode;
    radius, sizeX, sizeY, sizeZ, posX, posY, posZ, angX, angY, angZ, mass: double;
    I11, I22, I33, I12, I13, I23: double;
    BuoyantMass, Drag, StokesDrag, RollDrag: double;
    colorR, colorG, colorB: double;
    ID: string;
    R: TdMatrix3;
    newBone: TSolid;
    descr: string;
    TextureName: string;
    TextureScale: double;
    MeshFile, MeshShadowFile: string;
    MeshScale: double;
    MeshCastsShadows: boolean;
    Surf: TdSurfaceParameters;
    dMass: TdMass;
    MatterProps: TMatterProperties;
begin
  if root = nil then exit;

  bone := root.FirstChild;
  while bone <> nil do begin
    if pos(bone.NodeName, 'cuboid <> cylinder <> sphere <> belt <> propeller') <> 0 then begin // 'bone'
      prop := bone.FirstChild;
      // default values
      mass := 1;  ID := '-1';
      I11 := -1; I22 := -1; I33 := -1; I12 := 0; I13 := 0; I23 := 0;
      MatterProps := [];
      MeshFile := '';
      MeshShadowFile := '';
      MeshScale := 1;
      MeshCastsShadows := true;
      BuoyantMass := 0;
      Drag := 0; StokesDrag := 1e-5; RollDrag := 1e-3;
      radius := 1;
      sizeX := 1; sizeY := 1; sizeZ := 1;
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;
      TextureName := ''; TextureScale := 1;
      descr := bone.NodeName + inttostr(SolidList.Count);
      Surf.mu := -1; Surf.mu2 := -1;
      Surf.soft_cfm := 1e-5;
      //Surf.soft_erp := 0.2;
      Surf.bounce := 0; Surf.bounce_vel := 0;
      while prop <> nil do begin
        if prop.NodeName = 'metallic' then begin
          MatterProps := MatterProps + [smMetallic];
        end;
        if prop.NodeName = 'ferromagnetic' then begin
          MatterProps := MatterProps + [smFerromagnetic];
        end;
        if prop.NodeName = 'surface' then begin
          Surf.mu := GetNodeAttrRealParse(prop, 'mu', Surf.mu, Parser);
          Surf.mu2 := GetNodeAttrRealParse(prop, 'mu2', Surf.mu2, Parser);
          Surf.soft_cfm := GetNodeAttrRealParse(prop, 'softness', Surf.soft_cfm, Parser);
          Surf.bounce := GetNodeAttrRealParse(prop, 'bounce', Surf.bounce, Parser);
          Surf.bounce_vel := GetNodeAttrRealParse(prop, 'bounce_tresh', Surf.bounce_vel, Parser);
        end;
        if prop.NodeName = 'mesh' then begin
          MeshFile := GetNodeAttrStr(prop, 'file', MeshFile);
          MeshShadowFile := GetNodeAttrStr(prop, 'shadowfile', MeshShadowFile);
          MeshScale := GetNodeAttrRealParse(prop, 'scale', MeshScale, Parser);
          MeshCastsShadows := GetNodeAttrBool(prop, 'shadow', MeshCastsShadows);
        end;
        if prop.NodeName = 'drag' then begin
          Drag := GetNodeAttrRealParse(prop, 'coefficient', Drag, Parser);
          StokesDrag := GetNodeAttrRealParse(prop, 'stokes', StokesDrag, Parser);
          RollDrag := GetNodeAttrRealParse(prop, 'roll', RollDrag, Parser);
        end;
        if prop.NodeName = 'buoyant' then begin
          BuoyantMass := GetNodeAttrRealParse(prop, 'mass', BuoyantMass, Parser);
        end;
        if prop.NodeName = 'radius' then begin
          radius := GetNodeAttrRealParse(prop, 'value', radius, Parser);
        end;
        if prop.NodeName = 'size' then begin
          sizeX := GetNodeAttrRealParse(prop, 'x', sizeX, Parser);
          sizeY := GetNodeAttrRealParse(prop, 'y', sizeY, Parser);
          sizeZ := GetNodeAttrRealParse(prop, 'z', sizeZ, Parser);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
          posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
          posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrRealParse(prop, 'x', angX, Parser));
          angY := degToRad(GetNodeAttrRealParse(prop, 'y', angY, Parser));
          angZ := degToRad(GetNodeAttrRealParse(prop, 'z', angZ, Parser));
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        if prop.NodeName = 'mass' then begin
          mass := GetNodeAttrRealParse(prop, 'value', mass, Parser);
          //I11, I22, I33, I12, I13, I23
          I11 := GetNodeAttrRealParse(prop, 'I11', I11, Parser);
          I22 := GetNodeAttrRealParse(prop, 'I22', I22, Parser);
          I33 := GetNodeAttrRealParse(prop, 'I33', I33, Parser);
          I12 := GetNodeAttrRealParse(prop, 'I12', I12, Parser);
          I13 := GetNodeAttrRealParse(prop, 'I13', I13, Parser);
          I23 := GetNodeAttrRealParse(prop, 'I23', I23, Parser);
        end;
        if prop.NodeName = 'ID' then begin
          ID := GetNodeAttrStr(prop, 'value', ID);
        end;
        if prop.NodeName = 'desc' then begin
          descr := GetNodeAttrStr(prop, 'Eng', descr);
        end;
        if prop.NodeName = 'texture' then begin
          TextureName := GetNodeAttrStr(prop, 'name', TextureName);
          TextureScale := GetNodeAttrRealParse(prop, 'scale', TextureScale, Parser);
        end;
        prop := prop.NextSibling;
      end;

      if ID <> '-1' then begin
        // Create and position the solid
        newBone := TSolid.Create;
        SolidList.Add(newBone);
        newBone.ID := ID;
        newBone.description := ID;
        newBone.BuoyantMass := BuoyantMass;
        newBone.Drag := Drag;
        newBone.StokesDrag := StokesDrag;
        newBone.RollDrag := RollDrag;

        if (bone.NodeName = 'cuboid') or (bone.NodeName = 'belt') or (bone.NodeName = 'propeller')then begin
          CreateSolidBox(newBone, mass, posX, posY, posZ, sizeX, sizeY, sizeZ);
          //if TextureName <> '' then begin
          //  newBone.SetTexture(TextureName, TextureScale); //'LibMaterialFeup'
          //end;
        end else if bone.NodeName = 'sphere' then begin
          CreateSolidSphere(newBone, mass, posX, posY, posZ, radius);
        end else if bone.NodeName = 'cylinder' then begin
          CreateSolidCylinder(newBone, mass, posX, posY, posZ, sizeX, sizeZ);
          //if TextureName <> '' then begin
          //  newBone.SetTexture(TextureName, TextureScale); //'LibMaterialFeup'
          //end;
        end;
        if (I11 > 0) and (I22 > 0) and (I33 > 0)  then begin
          dMass := newBone.Body.mass;
          dMassSetParameters(dmass, dmass.mass,
                             dmass.c[0],
                             dmass.c[1],
                             dmass.c[2],
                             I11, I22, I33, I12, I13, I23);
          dBodySetMass(newBone.Body, @dMass);
        end;

        if TextureName <> '' then begin
          newBone.SetTexture(TextureName, TextureScale); //'LibMaterialFeup'
        end;

        if BuoyantMass <> 0 then begin
          dBodySetGravityMode(newBone.Body, 0);
        end;
        if bone.NodeName = 'belt' then newBone.kind := skMotorBelt;
        if bone.NodeName = 'propeller' then newBone.kind := skPropeller;

        newBone.MatterProperties := MatterProps;

        // create mesh file
        if MeshFile <> '' then begin
          //newBone.GLObj.Visible := false;
          newBone.AltGLObj := TGLSceneObject(ODEScene.AddNewChild(TGLFreeForm));
          with (newBone.AltGLObj as TGLFreeForm) do begin
            TagObject := newBone;
            MaterialLibrary := FViewer.GLMaterialLibrary3ds;
            try
              LoadFromFile(MeshFile);
            except on e: Exception do
              showmessage(E.Message);
            end;
            Scale.x := MeshScale;
            Scale.y := MeshScale;
            Scale.z := MeshScale;
          end;
          PositionSceneObject(newBone.AltGLObj, newBone.Geom);

          if MeshCastsShadows and (MeshShadowFile = '') then
            (OdeScene as TGLShadowVolume).Occluders.AddCaster(newBone.AltGLObj);
        end;

        // create shadow mesh file
        if MeshShadowFile <> '' then begin
          newBone.ShadowGlObj := TGLSceneObject(ODEScene.Parent.AddNewChild(TGLFreeForm));
          //newBone.ShadowGlObj := TGLSceneObject(ODEScene.AddNewChild(TGLFreeForm));
          with (newBone.ShadowGlObj as TGLFreeForm) do begin
            Visible := false;
            TagObject := newBone;
            MaterialLibrary := FViewer.GLMaterialLibrary3ds;
            try
              LoadFromFile(MeshShadowFile);
            except on e: Exception do
              showmessage(E.Message);
            end;
            Scale.x := MeshScale;
            Scale.y := MeshScale;
            Scale.z := MeshScale;
          end;
          PositionSceneObject(newBone.ShadowGlObj, newBone.Geom);
          with (OdeScene as TGLShadowVolume).Occluders.AddCaster(newBone.ShadowGlObj) do begin
            CastingMode := scmParentVisible;// scmAlways; //scmVisible;
          end;
        end;

        if Surf.mu >= 0 then begin
          newBone.ParSurface.mode := $FF;
          newBone.ParSurface.mu := Surf.mu;
          if Surf.mu2 >= 0 then newBone.ParSurface.mu2 := Surf.mu2;
          newBone.ParSurface.soft_cfm := Surf.soft_cfm;
          newBone.ParSurface.bounce := Surf.bounce;
          newBone.ParSurface.bounce_vel := Surf.bounce_vel;
        end;
        RFromZYXRotRel(R, angX, angY, AngZ);
        dBodySetRotation(newBone.Body, R);

        newBone.SetZeroState();
        newBone.SetColor(colorR, colorG, colorB);
        //PositionSceneObject(newBone.GLObj, newBone.Geom);
      end;

    end;

    bone := bone.NextSibling;
  end;
end;

{
  <joint>
    <ID value='8'/>
    <Pos x='-55' y='32' z='-103'/>
    <Axis x='1' y='0' z='0'/>      <!-- lateral-->
    <Axis2 x='0' y='0' z='1'/>     <!-- rotação-->
    <Connect B1='6' B2='7'/>
	<Limits Min='-16' Max='53'/>     <!-- lateral-->
    <Limits2 Min='-75' Max='11'/>    <!-- rotação-->
    <Type value='Universal'/>
    <Descr Pt='Anca Esq Lat/Rot'/>
    <Descr Eng='Left Lat/Rot Buttock'/>
  </joint>}

//procedure TWorld_ODE.LoadHumanoidJointsFromXML(Robot: TRobot; XMLFile: string);
procedure TWorld_ODE.LoadLinksFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
var JointNode, prop: IXMLNode;
    posX, posY, posZ: double;
    axisX, axisY, axisZ: double;
    axis2X, axis2Y, axis2Z: double;
    aGL, DefaGL: TAxisGLPars;
    LimitMin, LimitMax: double;
    Limit2Min, Limit2Max: double;
    LinkBody1, LinkBody2: string;
    LinkType: string;
    //colorR, colorG, colorB: double;
    SolidIndex1, SolidIndex2: integer;
    Solid1, Solid2: TSolid;
    i: integer;
    IDvalue: string;
    newLink: TSolidLink;
    newAxis: TAxis;
    Friction, DefFriction: TFriction;
    Friction2: TFriction;
    Spring, DefSpring: TSpring;
    Spring2: TSpring;
    Motor, DefMotor: TMotor;
    Motor2: TMotor;
    descr: string;
begin
  if root = nil then exit;

  // Initialize default parameters
  DefaGL.Radius := -1;
  DefaGL.height:= 0.05;
  DefaGL.Color := RGB(0, 0, $80);

  with DefMotor do begin
    active := true;
    simple := false;
    Encoder.PPR := 1000;
    Encoder.NoiseMean := 0;
    Encoder.NoiseStDev := -1;
    Ri := 1;
    Li := 0;
    Ki := 1.4e-2;
    Imax := 4;
    Vmax := 24;
    GearRatio := 10;

    Controller.active := false;
    //Controller.active := true;
    with Controller do begin
      Kp := 0.5;
      Ki := 0;
      Kd := 0;
      Kf := 0.5;
      Y_sat := 24;
      ticks := 0;
      Sek := 0;
      controlPeriod := 10;
      ControlMode := cmPIDSpeed;
    end;
  end;

  with DefFriction do begin
    Bv := 1e-5;
    Fc := 1e-3;
    CoulombLimit := 1e-2;
  end;

  with DefSpring do begin
    K := 0;
    ZeroPos := 0;
  end;

  JointNode := root.FirstChild;
  while JointNode <> nil do begin
    if (JointNode.NodeName = 'joint') or (JointNode.NodeName = 'default') then begin //'joint'
      prop := JointNode.FirstChild;
      // default values
      posX := 0; posY := 0; posZ := 0;
      axisX := 1; axisY := 0; axisZ := 0;
      axis2X := 0; axis2Y := 0; axis2Z := 1;
      aGL := DefaGL;
      LimitMin:= -360; LimitMax := 360;
      Limit2Min:= -360; Limit2Max := 360;
      LinkBody1 := '-1'; LinkBody2 := '-1';
      LinkType :=' ';
      descr := '';
      IDvalue := '-1';
      //colorR := 128/255; colorG := 128/255; colorB := 128/255;
      newLink := nil;
      motor := DefMotor;
      motor2 := DefMotor;
      Friction := DefFriction;
      Friction2 := DefFriction;
      Spring := DefSpring;
      Spring2 := DefSpring;

      while prop <> nil do begin
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
          posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
          posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
        end;
        if prop.NodeName = 'axis' then begin
          axisX := GetNodeAttrRealParse(prop, 'x', axisX, Parser);
          axisY := GetNodeAttrRealParse(prop, 'y', axisY, Parser);
          axisZ := GetNodeAttrRealParse(prop, 'z', axisZ, Parser);
        end;
        if prop.NodeName = 'axis2' then begin
          axis2X := GetNodeAttrRealParse(prop, 'x', axis2X, Parser);
          axis2Y := GetNodeAttrRealParse(prop, 'y', axis2Y, Parser);
          axis2Z := GetNodeAttrRealParse(prop, 'z', axis2Z, Parser);
        end;
        if prop.NodeName = 'limits' then begin
          LimitMin := GetNodeAttrRealParse(prop, 'Min', LimitMin, Parser);
          LimitMax := GetNodeAttrRealParse(prop, 'Max', LimitMax, Parser);
        end;
        if prop.NodeName = 'limits2' then begin
          Limit2Min := GetNodeAttrRealParse(prop, 'Min', Limit2Min, Parser);
          Limit2Max := GetNodeAttrRealParse(prop, 'Max', Limit2Max, Parser);
        end;
        if prop.NodeName = 'connect' then begin
          LinkBody1 := GetNodeAttrStr(prop, 'B1', LinkBody1);
          LinkBody2 := GetNodeAttrStr(prop, 'B2', LinkBody2);
        end;
        //<draw radius='0.01' height='0.1' rgb24='8F0000'/>
        if prop.NodeName = 'draw' then begin
          aGL.Radius := GetNodeAttrRealParse(prop, 'radius', aGL.Radius, Parser);
          aGL.height := GetNodeAttrRealParse(prop, 'height', aGL.height, Parser);
          aGL.Color := StrToIntDef('$'+GetNodeAttrStr(prop, 'rgb24', inttohex(aGL.color,6)), aGL.color);
        end;
        {if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;}
        if prop.NodeName = 'type' then begin
          LinkType := GetNodeAttrStr(prop, 'value', LinkType);
        end;
        if prop.NodeName = 'desc' then begin
          descr := GetNodeAttrStr(prop, 'Eng', descr);
        end;
        if prop.NodeName = 'ID' then begin
          IDValue := GetNodeAttrStr(prop, 'value', IDValue);
        end;
        ReadFrictionFromXMLNode(Friction, '', prop, Parser);
        ReadFrictionFromXMLNode(Friction2, '2', prop, Parser);
        ReadSpringFromXMLNode(Spring, '', prop, Parser);
        ReadSpringFromXMLNode(Spring2, '2', prop, Parser);
        ReadMotorFromXMLNode(Motor, '', prop, Parser);
        ReadMotorFromXMLNode(Motor2, '2', prop, Parser);
        prop := prop.NextSibling;
      end;

      if JointNode.NodeName = 'default' then begin
        // Set the new default Link parameters
        DefFriction := Friction;
        DefSpring := Spring;
        DefMotor := Motor;
        DefaGL := aGL;
      end else begin
        // Create a new Link
        if (LinkBody1 <> '-1') and (LinkBody2 <> '-1') then begin
          // Find the solids with this IDs
          SolidIndex1 := -1;
          SolidIndex2 := -1;
          for i := 0 to Robot.Solids.Count-1 do begin
            if Robot.Solids[i].ID = LinkBody1 then SolidIndex1 := i;
            if Robot.Solids[i].ID = LinkBody2 then SolidIndex2 := i;
          end;

          // ID = '0' or 'world' mean the world;
          if LinkBody1 = 'world' then LinkBody1 := '0';
          if LinkBody2 = 'world' then LinkBody2 := '0';
          if (((SolidIndex1 <> -1) or (LinkBody1 = '0')) and (SolidIndex2 <> -1)) or
             (((SolidIndex2 <> -1) or (LinkBody2 = '0')) and (SolidIndex1 <> -1)) then begin
            if LinkBody1 = '0' then begin // It only works when the SECOND body is the Environment
              Solid1 := Robot.Solids[SolidIndex2];
              Solid2 := Environment;
            end else if LinkBody2 = '0' then begin
              Solid1 := Robot.Solids[SolidIndex1];
              Solid2 := Environment;
            end else begin
              Solid1 := Robot.Solids[SolidIndex1];
              Solid2 := Robot.Solids[SolidIndex2];
            end;
            newLink := TSolidLink.Create;
            Robot.Links.Add(newLink);

            if LinkType ='Hinge' then begin
              CreateHingeJoint(newLink, Solid1, Solid2, posX, posY, posZ, axisX,axisY,axisZ);
              SetHingeLimits(newLink, LimitMin, LimitMax);
              if Friction.Fc > 0 then begin
                dJointSetHingeParam(newLink.joint, dParamVel , 0);
                dJointSetHingeParam(newLink.joint, dParamFMax, Friction.Fc);
              end;
            end;

            if LinkType ='Slider' then begin
              CreateSliderJoint(newLink, Solid1, Solid2, axisX,axisY,axisZ);
              SetSliderLimits(newLink, LimitMin, LimitMax);
              if Friction.Fc > 0 then begin
                dJointSetSliderParam(newLink.joint, dParamVel , 0);
                dJointSetSliderParam(newLink.joint, dParamFMax, Friction.Fc);
              end;
            end;

            if LinkType ='Fixed' then begin
              CreateFixedJoint(newLink, Solid1, Solid2);
              aGL.Radius := -1;
            end;

            if LinkType ='Universal' then begin
              CreateUniversalJoint(newLink, Solid1, Solid2, posX, posY, posZ, axisX,axisY,axisZ, axis2X,axis2Y,axis2Z);
              SetUniversalLimits(newLink, LimitMin, LimitMax, Limit2Min, Limit2Max);

              if Friction.Fc > 0 then begin
                dJointSetUniversalParam(newLink.joint, dParamVel , 0);
                dJointSetUniversalParam(newLink.joint, dParamFMax, Friction.Fc);
              end;

              if Friction2.Fc > 0 then begin
                dJointSetUniversalParam(newLink.joint, dParamVel2 , 0);
                dJointSetUniversalParam(newLink.joint, dParamFMax2, Friction2.Fc);
              end;

              newAxis := TAxis.Create;
              Robot.Axes.Add(newAxis);
              newAxis.ParentLink := newLink;
              newAxis.Friction := Friction2;
              newAxis.Spring := Spring; //TODO Deal with different parameters for each axis
              newAxis.Motor := Motor2;
              newLink.Axis[1] := newAxis;

              if aGL.Radius > 0 then begin
                newAxis.GLCreate(OdeScene, aGL.Radius, aGL.height);
                (newAxis.GLObj as TGLSceneObject).Material.FrontProperties.Diffuse.AsWinColor := aGL.color;
                newAxis.GLSetPosition;
                newAxis.GLObj.TagObject := newAxis;
              end;
            end;

            newAxis := TAxis.Create;
            Robot.Axes.Add(newAxis);
            newAxis.ParentLink := newLink;
            newAxis.Friction := Friction;
            newAxis.Spring := Spring;
            newAxis.Motor := Motor;
            newLink.Axis[0] := newAxis;

            if aGL.Radius > 0 then begin
              newAxis.GLCreate(OdeScene, aGL.Radius, aGL.height);
              (newAxis.GLObj as TGLSceneObject).Material.FrontProperties.Diffuse.AsWinColor := aGL.color;
              newAxis.GLSetPosition;
              newAxis.GLObj.TagObject := newAxis;
            end;

            if newLink <> nil then begin
              newLink.ID := IDValue;
              newLink.description := IDValue;
            end;
          end;
        end;
      end;

    end;

    JointNode := JointNode.NextSibling;
  end;

end;

function LoadXML(XMLFile: string; ErrorList: TStringList): IXMLDocument;
var XML: IXMLDocument;
    err: string;
begin
  result := nil;
  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason <> '' then begin
    //with FParams.MemoDebug.Lines do begin
      //Add('XML file error:' + XMLFile);
      //Add(XML.ParseError.Reason);
    //end;
    err := '[XML error] ' + format('%s(%d): ', [XMLFile, XML.ParseError.Line]) + #$0d+#$0A
    //err := format('%s(%d): ', [XMLFile, XML.ParseError.Line]) + #$0d+#$0A
           + format('"%s": ',[trim(XML.ParseError.SrcText)]) + #$0d+#$0A
           + XML.ParseError.Reason ;

    if ErrorList <> nil then begin
      ErrorList.Add(err);
    end else begin
      showmessage(err);
    end;
    exit;
  end;
  result := XML;
end;


procedure TWorld_ODE.SaveJointWayPointsToXML(XMLFile: string; r: integer);
var XML: IXMLDocument;
    root, node, prop: IXMLElement;
    PI: IXMLProcessingInstruction;
    wp_idx, j, axis_idx: integer;
    eps: double;
begin
  if (r < 0) or (r >= Robots.Count) then exit;
  if Robots[r].Axes.Count = 0 then exit;

  XML := CreateXMLDoc;
  //PI := XML.CreateProcessingInstruction('xml', 'version="1.0" encoding="UTF-8"');
  PI := XML.CreateProcessingInstruction('xml', 'version="1.0"');
  XML.InsertBefore(PI, XML.DocumentElement);

  root := XML.CreateElement('joint_waypoints');
  XML.DocumentElement := root;
  for wp_idx := 0 to Robots[r].AxesWayPointsIDs.Count - 1 do begin
    //  <state ID='1' final_time='1'>
    node := XML.CreateElement('state');
    node.SetAttribute('ID', Robots[r].AxesWayPointsIDs[wp_idx]);
    node.SetAttribute('final_time', format('%g',[Robots[r].Axes[0].WayPoints[wp_idx].t]));
    root.AppendChild(node);

    eps := 1e-8;
    for j := 0 to Robots[r].Axes.Count -1 do begin
      with Robots[r].Axes[j] do begin
        if wp_idx > 0 then begin
          if (abs(WayPoints[wp_idx - 1].pos - WayPoints[wp_idx].pos) < eps) and
             (abs(WayPoints[wp_idx - 1].speed - WayPoints[wp_idx].speed) < eps) then continue;
        end;
        //  <joint ID='0' axis='1' theta='0' w='0'/>
        prop := XML.CreateElement('joint');
        prop.SetAttribute('ID', ParentLink.ID);

        // Find which axis is this
        axis_idx := 0;
        while axis_idx < MaxAxis do begin
          if ParentLink.Axis[axis_idx] = Robots[r].Axes[j] then break;
          inc(axis_idx);
        end;
        if axis_idx <> 0 then
          prop.SetAttribute('axis', format('%d',[axis_idx + 1]));

        prop.SetAttribute('theta', format('%g',[radtodeg(WayPoints[wp_idx].pos)]));
        prop.SetAttribute('w', format('%g',[radtodeg(WayPoints[wp_idx].speed)]));
        node.AppendChild(prop);
      end;
    end;
  end;
  XML.Save(XMLFile, ofIndent);
end;

procedure TWorld_ODE.LoadJointWayPointsFromXML(XMLFile: string; r: integer);
var XML: IXMLDocument;
    root, node, prop: IXMLNode;
    ang, speed, final_time: double;
    i, j, axis_idx: integer;
    jointID, trajID: string;
    NewPoint: TAxisTraj;
    way_point_idx: integer;
begin
  if (r < 0) or (r >= Robots.Count) then exit;

  ang := 0;
  speed := 0;

{  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason<>'' then begin
    exit;
  end;}
  XML := LoadXML(XMLFile, XMLErrors);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/joint_waypoints');
  if root = nil then exit;

  node := root.FirstChild;
  while node <> nil do begin
    if node.NodeName = 'state' then begin

      trajID := GetNodeAttrStr(node, 'ID', '');
      final_time := GetNodeAttrReal(node, 'final_time', 0);

      prop := node.FirstChild;
      while prop <> nil do begin
        // default values
        ang := 0; speed := 0;
        axis_idx := 1;

        if prop.NodeName = 'joint' then begin
          jointID := GetNodeAttrStr(prop, 'ID', '-1');
          axis_idx := GetNodeAttrInt(prop, 'axis', axis_idx) - 1;
          ang := DegToRad(GetNodeAttrReal(prop, 'theta', ang));
          speed := GetNodeAttrReal(prop, 'w', speed);

          i :=  Robots[r].Links.IndexOf(jointID);
          if i >= 0 then begin
            if Robots[r].Links[i].Axis[axis_idx] <> nil then begin
              // Create and insert the point
              NewPoint := TAxisTraj.Create;
              NewPoint.pos := ang;
              NewPoint.speed := speed;
              NewPoint.t := final_time;
              Robots[r].Links[i].Axis[axis_idx].WayPoints.Add(NewPoint)
            end;
          end;
        end;

        prop := prop.NextSibling;
      end;

      WorldODE.Robots[r].AxesWayPointsIDs.Add(trajID);
      // insert the remaining points that weren't specified in this state
      for i := 0 to Robots[r].Axes.Count -1 do begin
        way_point_idx := Robots[r].Axes[i].WayPoints.Count -1;
        if (way_point_idx >= 0) then begin
          if abs(Robots[r].Axes[i].WayPoints[way_point_idx].t - final_time) < 1e-5 then
            continue;

          ang := Robots[r].Axes[i].WayPoints[way_point_idx].pos;
          speed := Robots[r].Axes[i].WayPoints[way_point_idx].speed;
        end else begin
          ang := 0;
          speed := 0;
        end;
        // Create and insert the point
        NewPoint := TAxisTraj.Create;
        NewPoint.pos := ang;
        NewPoint.speed := speed;
        NewPoint.t := final_time;
        Robots[r].Axes[i].WayPoints.Add(NewPoint)
      end;
    end;

    node := node.NextSibling;
  end;

  FParams.ComboWayPointNameUpdate(Robots[r]);

  for i := 0 to Robots[r].Links.Count -1 do begin
    for j := 0 to Robots[r].Links[i].Axis[0].WayPoints.Count -1 do begin
      with Robots[r].Links[i].Axis[0].WayPoints[j] do
        FParams.MemoDebug.Lines.Add(format('[%d,%d] %.2f: %.2f %.2f',[i,j, t, pos, speed]));
    end;
  end;

end;




procedure TWorld_ODE.LoadObstaclesFromXML(XMLFile: string; Parser: TSimpleParser);
var XML: IXMLDocument;
    root, obstacle, prop: IXMLNode;
    sizeX, sizeY, sizeZ, posX, posY, posZ, angX, angY, angZ: double;
    colorR, colorG, colorB: double;
    TextureName: string;
    TextureScale: double;
    R: TdMatrix3;
    NewObstacle: TSolid;
begin
{  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason<>'' then begin
    exit;
  end;}
  XML := LoadXML(XMLFile, XMLErrors);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/obstacles');
  if root = nil then exit;

  obstacle := root.FirstChild;
  while obstacle <> nil do begin
    if obstacle.NodeName = 'cuboid' then begin
      prop := obstacle.FirstChild;
      // default values
      sizeX := 1; sizeY := 1; sizeZ := 1;
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;
      TextureName := ''; TextureScale := 1;
      while prop <> nil do begin
        if prop.NodeName = 'size' then begin
          sizeX := GetNodeAttrRealParse(prop, 'x', sizeX, Parser);
          sizeY := GetNodeAttrRealParse(prop, 'y', sizeY, Parser);
          sizeZ := GetNodeAttrRealParse(prop, 'z', sizeZ, Parser);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
          posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
          posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrRealParse(prop, 'x', angX, Parser));
          angY := degToRad(GetNodeAttrRealParse(prop, 'y', angY, Parser));
          angZ := degToRad(GetNodeAttrRealParse(prop, 'z', angZ, Parser));
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        if prop.NodeName = 'texture' then begin
          TextureName := GetNodeAttrStr(prop, 'name', TextureName);
          TextureScale := GetNodeAttrRealParse(prop, 'scale', TextureScale, Parser);
        end;
        prop := prop.NextSibling;
      end;
      // Create and position the obstacle
      NewObstacle := TSolid.Create;
      Obstacles.Add(NewObstacle);
      NewObstacle.description := format('Obstacle at (%.1f, %.1f, %.1f)',[posX, posY, posZ]);
      CreateBoxObstacle(NewObstacle, sizeX, sizeY, sizeZ, posX, posY, posZ);

      RFromZYXRotRel(R, angX, angY, AngZ);
      dGeomSetRotation(NewObstacle.Geom, R);

      if TextureName <> '' then begin
        NewObstacle.SetTexture(TextureName, TextureScale); //'LibMaterialFeup'
      end;
      NewObstacle.SetColor(colorR, colorG, colorB);
      PositionSceneObject(NewObstacle.GLObj, NewObstacle.Geom);
    end;

    obstacle := obstacle.NextSibling;
  end;

end;


procedure TWorld_ODE.LoadSensorsFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
var sensor, prop: IXMLNode;
    SLen, SInitialWidth, SFinalWidth, posX, posY, posZ, angX, angY, angZ: double;
    Noise: TSensorNoise;
    colorR, colorG, colorB: double;
    R: TdMatrix3;
    newSensor: TSensor;

    MotherSolidId: string;
    MotherSolid: TSolid;
    SolidIdx: integer;
    AbsoluteCoords: boolean;
begin
  if root = nil then exit;

  sensor := root.FirstChild;
  while sensor <> nil do begin
    if (sensor.NodeName = 'IR') or
       (sensor.NodeName = 'IRSharp') or
       (sensor.NodeName = 'capacitive') or
       (sensor.NodeName = 'inductive') then begin
      // default values
      MotherSolidId := '';
      SLen := 0.8; SInitialWidth := 0.01; SFinalWidth := 0.015;
      AbsoluteCoords := false;
      with Noise do begin
        var_k := 0; var_d := 0; offset := 0; gain := 1; active := false;
      end;
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;

      prop := sensor.FirstChild;
      while prop <> nil do begin
        if prop.NodeName = 'absolute_coords' then begin
          AbsoluteCoords := true;
        end;
        if prop.NodeName = 'solid' then begin
          MotherSolidId := GetNodeAttrStr(prop, 'id', MotherSolidId);
        end;
        if prop.NodeName = 'beam' then begin
          SLen := GetNodeAttrRealParse(prop, 'length', SLen, Parser);
          SInitialWidth := GetNodeAttrRealParse(prop, 'initial_width', SInitialWidth, Parser);
          SFinalWidth := GetNodeAttrRealParse(prop, 'final_width', SFinalWidth, Parser);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
          posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
          posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrRealParse(prop, 'x', angX, Parser));
          angY := degToRad(GetNodeAttrRealParse(prop, 'y', angY, Parser));
          angZ := degToRad(GetNodeAttrRealParse(prop, 'z', angZ, Parser));
        end;
        if prop.NodeName = 'noise' then with Noise do begin
          active := true;  // if the tag 'noise' is present then it is active
          var_k := GetNodeAttrRealParse(prop, 'var_k', var_k, Parser);
          var_d := GetNodeAttrRealParse(prop, 'var_d', var_d, Parser);
          offset := GetNodeAttrRealParse(prop, 'offset', offset, Parser);
          gain := GetNodeAttrRealParse(prop, 'gain', gain, Parser);
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        prop := prop.NextSibling;
      end;
      // Create and position the sensor
      newSensor := TSensor.Create;
      newSensor.kind := skIR;
      if sensor.NodeName = 'IRSharp' then newSensor.kind := skIRSharp
      else if sensor.NodeName = 'capacitive' then newSensor.kind := skCapacitive
      else if sensor.NodeName = 'inductive' then newSensor.kind := skInductive;

      newSensor.Noise := Noise;
      Robot.Sensors.Add(newSensor);

      SolidIdx := Robot.Solids.IndexFromID(MotherSolidId);
      if SolidIdx = -1 then  begin
        MotherSolid := Robot.MainBody;
      end else begin
        MotherSolid := Robot.Solids[SolidIdx];
      end;
      CreateIRSensor(MotherSolid.Body, newSensor, posX, posY, posZ, angZ, SLen, SInitialWidth, SFinalWidth);

      RFromZYXRotRel(R, angX, angY + pi/2, AngZ);

      if AbsoluteCoords then begin
        dGeomSetOffsetWorldRotation(newSensor.Geoms[0], R);
        dGeomSetOffsetWorldPosition(newSensor.Geoms[0], posX, posY, posZ);
      end else begin
        dGeomSetOffsetRotation(newSensor.Geoms[0], R);
        dGeomSetOffsetPosition(newSensor.Geoms[0], posX, posY, posZ);
      end;

      newSensor.SetColor(colorR, colorG, colorB);
    end;

    sensor := sensor.NextSibling;
  end;

end;

procedure TWorld_ODE.ReadFrictionFromXMLNode(var Friction: TFriction; sufix: string; const prop: IXMLNode; Parser: TSimpleParser);
begin
  with Friction do begin
    if prop.NodeName = 'friction' + sufix then begin
      Bv := GetNodeAttrRealParse(prop, 'bv', Bv, Parser);
      Fc := GetNodeAttrRealParse(prop, 'fc', Fc, Parser);
      //CoulombLimit := GetNodeAttrReal(prop, 'coulomblimit', CoulombLimit);
    end;
  end;
end;

procedure TWorld_ODE.ReadSpringFromXMLNode(var Spring: TSpring; sufix: string; const prop: IXMLNode; Parser: TSimpleParser);
begin
  with Spring do begin
    if prop.NodeName = 'spring' + sufix then begin
      K := GetNodeAttrRealParse(prop, 'k', K, Parser);
      ZeroPos := degtorad(GetNodeAttrRealParse(prop, 'zeropos', ZeroPos, Parser));
    end;
  end;
end;


procedure TWorld_ODE.ReadMotorFromXMLNode(var Motor: TMotor; sufix: string; const prop: IXMLNode; Parser: TSimpleParser);
var str: string;
begin
  with Motor do begin
    if prop.NodeName = 'motor' + sufix then begin
      Ri := GetNodeAttrRealParse(prop, 'ri', Ri, Parser);
      Li := GetNodeAttrRealParse(prop, 'li', Li, Parser);
      Ki := GetNodeAttrRealParse(prop, 'ki', Ki, Parser);
      Vmax := GetNodeAttrRealParse(prop, 'vmax', Vmax, Parser);
      Controller.y_sat := Vmax;
      Imax := GetNodeAttrRealParse(prop, 'imax', Imax, Parser);
      active := GetNodeAttrBool(prop, 'active', active);
      simple := GetNodeAttrBool(prop, 'simple', simple);
    end;
    if prop.NodeName = 'gear' + sufix then begin
      GearRatio := GetNodeAttrRealParse(prop, 'ratio', GearRatio, Parser);
    end;
    if prop.NodeName = 'encoder' + sufix then begin
      Encoder.PPR := GetNodeAttrInt(prop, 'ppr', Encoder.PPR);
      Encoder.NoiseMean := GetNodeAttrRealParse(prop, 'mean', Encoder.NoiseMean, Parser);
      Encoder.NoiseStDev := GetNodeAttrRealParse(prop, 'stdev', Encoder.NoiseStDev, Parser);
    end;

    if prop.NodeName = 'controller' + sufix then begin
      with Controller do begin
        Kp := GetNodeAttrRealParse(prop, 'kp', Kp, Parser);
        Ki := GetNodeAttrRealParse(prop, 'ki', Ki, Parser);
        Kd := GetNodeAttrRealParse(prop, 'kd', Kd, Parser);
        Kf := GetNodeAttrRealParse(prop, 'kf', Kf, Parser);
        //Y_sat := GetNodeAttrRealParse(prop, 'ysat', Y_sat, Parser);
        controlPeriod := GetNodeAttrRealParse(prop, 'period', controlPeriod, Parser)/1000;
        str := GetNodeAttrStr(prop, 'mode', 'pidspeed');
        if str = 'pidspeed' then ControlMode := cmPIDSpeed
        else if str = 'pidposition' then ControlMode := cmPIDPosition
        else if str = 'state' then ControlMode := cmState;
        active := GetNodeAttrBool(prop, 'active', active);
      end;
    end;
  end;
end;

procedure TWorld_ODE.LoadWheelsFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
var wheelnode, prop: IXMLNode;
    //offX, offY, offZ,
    angX, angY, angZ: double;
    //R, Rx, Ry, Rz, Ryx: TdMatrix3;
    newWheel: TWheel;
    Pars, DefPars: TWheelPars;
    Friction, DefFriction: TFriction;
    Motor, DefMotor: TMotor;
    RGB, DefRGB: TRGBfloat;
begin
  if root = nil then exit;

  // Initialize default parameters
  with DefPars do begin
    offsetX := 0;
    offsetY := 0;
    offsetZ := 0;
    Radius := 0.09;
    Width := 0.03;
    mass := 0.13;
    CenterDist := 0.2;
    Mu := 1;
    Mu2:= 0.001;
    soft_cfm := 0.001;
    Omni := false;
  end;

  with DefMotor do begin
    active := true;
    Encoder.PPR := 1000;
    Encoder.NoiseMean := 0;
    Encoder.NoiseStDev := -1;
    Ri := 1;
    Li := 0;
    Ki := 1.4e-2;
    Imax := 4;
    Vmax := 24;
    GearRatio := 10;

    Controller.active := false;
    //Controller.active := true;
    with Controller do begin
      Kp := 0.5;
      Ki := 0;
      Kd := 0;
      Kf := 0.5;
      Y_sat := 24;
      ticks := 0;
      Sek := 0;
      controlPeriod := 10;
      ControlMode := cmPIDSpeed;
    end;
  end;

  with DefFriction do begin
    Bv := 1e-5;
    Fc := 1e-3;
    CoulombLimit := 1e-2;
  end;

  with RGB do begin
    R := 128/255;
    G := 128/255;
    B := 128/255;
  end;

{ <wheel>
    <omni/>
*    <tyre mass='0.1' radius='0.1' width='0.01' centerdist='0.015'/>
*    <axis angle='-90'/>
*    <motor ri='0' ki='0.3' vmax='0' imax='0' active='1'/>
*    <gear ratio='10'/>
    <friction bv='0' fc='0' coulomblimit='0'/>
-    <encoder ppr='1000' mean='0' stdev='0'/>
*    <controller type='PIDspeed' ki='1' ki='0' kd='0.1' kf='1' active='1' period='10'/>
*    <color_rgb r='128' g='0' b='0'/>
  </wheel> }

  wheelnode := root.FirstChild;
  while wheelnode <> nil do begin
    if (wheelnode.NodeName = 'wheel') or (wheelnode.NodeName = 'default') then begin
      // default values
      //offX := 0; offY := 0; offZ := 0;
      angX := 0; angY := 0; angZ := 0;
      Pars := DefPars;
      Friction := DefFriction;
      Motor := DefMotor;
      RGB := DefRGB;

      prop := wheelnode.FirstChild;
      while prop <> nil do begin

        if prop.NodeName = 'omni' then begin
          Pars.omni := true;
        end;
        if prop.NodeName = 'tyre' then begin
          Pars.mass := GetNodeAttrRealParse(prop, 'mass', Pars.mass, Parser);
          Pars.Radius := GetNodeAttrRealParse(prop, 'radius', Pars.Radius, Parser);
          Pars.Width := GetNodeAttrRealParse(prop, 'width', Pars.Width, Parser);
          Pars.CenterDist := GetNodeAttrRealParse(prop, 'centerdist', Pars.CenterDist, Parser);
          //Pars.Mu := GetNodeAttrReal(prop, 'mu', Pars.mu);
          //Pars.Mu2 := GetNodeAttrReal(prop, 'mu2', Pars.mu2);
        end;

        if prop.NodeName = 'surface' then begin
          Pars.Mu := GetNodeAttrRealParse(prop, 'mu', Pars.mu, Parser);
          Pars.Mu2 := GetNodeAttrRealParse(prop, 'mu2', Pars.mu2, Parser);
          Pars.soft_cfm := GetNodeAttrRealParse(prop, 'softness', Pars.soft_cfm, Parser);
        end;

        ReadFrictionFromXMLNode(Friction, '', prop, Parser);

        ReadMotorFromXMLNode(Motor, '', prop, Parser);

        if prop.NodeName = 'offset' then begin
          Pars.offsetX := GetNodeAttrRealParse(prop, 'x', Pars.offsetX, Parser);
          Pars.offsetY := GetNodeAttrRealParse(prop, 'y', Pars.offsetY, Parser);
          Pars.offsetZ := GetNodeAttrRealParse(prop, 'z', Pars.offsetZ, Parser);
        end;

        if prop.NodeName = 'axis' then begin
          angX := degToRad(GetNodeAttrRealParse(prop, 'x', angX, Parser));
          angY := degToRad(GetNodeAttrRealParse(prop, 'y', angY, Parser));
          angZ := degToRad(GetNodeAttrRealParse(prop, 'angle', angZ, Parser));
        end;

        if prop.NodeName = 'color_rgb' then begin
          RGB.R := GetNodeAttrInt(prop, 'r', 128)/255;
          RGB.G := GetNodeAttrInt(prop, 'g', 128)/255;
          RGB.B := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        prop := prop.NextSibling;
      end;

      Pars.Angle := angZ;
      if wheelnode.NodeName = 'default' then begin
        // Set the new default wheel
        DefPars := Pars;
        DefFriction := Friction;
        DefMotor := Motor;
        DefRGB := RGB;
      end else begin
        // Create and position the wheel
        newWheel := TWheel.Create;
        Robot.Wheels.Add(newWheel);
        CreateWheel(Robot, newWheel, Pars, Friction, Motor);
        newWheel.Tyre.SetTexture('MatBumps', 4); //TODO
      end;

    end;

    wheelnode := wheelnode.NextSibling;
  end;

end;


procedure TWorld_ODE.LoadShellsFromXML(Robot: TRobot; const root: IXMLNode; Parser: TSimpleParser);
var ShellNode, prop: IXMLNode;
    sizeX, sizeY, sizeZ, posX, posY, posZ, angX, angY, angZ: double;
    mass, radius: double;
    colorR, colorG, colorB: double;
    R: TdMatrix3;
    newShell: TSolid;
    MotherSolidId: string;
    MotherSolid: TSolid;
    SolidIdx: integer;
    IDValue: string;
    CompMass, NewMass: TdMass;
    PGeomPos: PdVector3;
    i: integer;
    MatterProps: TMatterProperties;
begin
  if root = nil then exit;

  ShellNode := root.FirstChild;
  while ShellNode <> nil do begin
    if pos(ShellNode.NodeName, 'cuboid<>cylinder<>sphere') <> 0 then begin
      prop := ShellNode.FirstChild;
      // default values
      mass := 0;
      sizeX := 1; sizeY := 1; sizeZ := 1;
      radius := 1;
      IDValue := '';
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;
      MatterProps := [];
      MotherSolidId := '';
      while prop <> nil do begin
        if prop.NodeName = 'ID' then begin
          IDValue := GetNodeAttrStr(prop, 'value', IDValue);
        end;
        if prop.NodeName = 'solid' then begin
          MotherSolidId := GetNodeAttrStr(prop, 'id', MotherSolidId);
        end;
        if prop.NodeName = 'mass' then begin
          mass := GetNodeAttrRealParse(prop, 'value', mass, Parser);
        end;
        if prop.NodeName = 'radius' then begin
          radius := GetNodeAttrRealParse(prop, 'value', radius, Parser);
        end;
        if prop.NodeName = 'size' then begin
          sizeX := GetNodeAttrRealParse(prop, 'x', sizeX, Parser);
          sizeY := GetNodeAttrRealParse(prop, 'y', sizeY, Parser);
          sizeZ := GetNodeAttrRealParse(prop, 'z', sizeZ, Parser);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
          posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
          posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrRealParse(prop, 'x', angX, Parser));
          angY := degToRad(GetNodeAttrRealParse(prop, 'y', angY, Parser));
          angZ := degToRad(GetNodeAttrRealParse(prop, 'z', angZ, Parser));
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        if prop.NodeName = 'metallic' then begin
          MatterProps := MatterProps + [smMetallic];
        end;
        if prop.NodeName = 'ferromagnetic' then begin
          MatterProps := MatterProps + [smFerromagnetic];
        end;
        prop := prop.NextSibling;
      end;
      // Create and position the shell
      newShell := TSolid.Create;
      Robot.Shells.Add(newShell);
      if IDValue = '' then begin
        newShell.description := 'Shell';
      end else begin
        newShell.description := IDValue;
      end;

      SolidIdx := Robot.Solids.IndexFromID(MotherSolidId);
      if SolidIdx = -1 then  begin
        MotherSolid := Robot.MainBody;
      end else begin
        MotherSolid := Robot.Solids[SolidIdx];
      end;

      if mass > 0 then begin  // Offset from previous compositions
        PGeomPos := dGeomGetOffsetPosition(MotherSolid.Geom);
        posX := posX + PGeomPos^[0];
        posY := posY + PGeomPos^[1];
        posZ := posZ + PGeomPos^[2];
      end;

      if ShellNode.NodeName = 'cuboid' then begin
        CreateShellBox(newShell, MotherSolid.Body, posX, posY, posZ, sizeX, sizeY, sizeZ);
        if mass > 0 then dMassSetBoxTotal(NewMass, mass, sizeX, sizeY, sizeZ);
      end else if ShellNode.NodeName = 'cylinder' then begin
        CreateShellCylinder(newShell, MotherSolid.Body, posX, posY, posZ, sizeX, sizeZ);
        if mass > 0 then dMassSetCylinderTotal(NewMass, mass, 1, sizeX, sizeZ);
      end else if ShellNode.NodeName = 'sphere' then begin
        CreateShellSphere(newShell, MotherSolid.Body, posX, posY, posZ, radius);
        if mass > 0 then dMassSetSphereTotal(NewMass, mass, radius);
      end;
      newShell.MatterProperties := MatterProps;

      RFromZYXRotRel(R, angX, angY, AngZ);
      dGeomSetOffsetRotation(newShell.Geom, R);

      if mass > 0 then begin  // Affect the body center of mass and Inertia matrix
        CompMass := MotherSolid.Body.mass;
        dMassTranslate(NewMass, posX, posY, posZ);
        dMassRotate(NewMass, R);
        dMassAdd(CompMass, NewMass);

        // Reposition the objects to set a center of gravity to 0,0,0
        //dGeomSetOffsetPosition(newShell.Geom, posX - CompMass.c[0],
        //                                      posY - CompMass.c[1],
        //                                      posZ - CompMass.c[2]);
        PGeomPos := dGeomGetOffsetPosition(MotherSolid.Geom);
        dGeomSetOffsetPosition(MotherSolid.Geom, PGeomPos^[0] - CompMass.c[0],
                                                 PGeomPos^[1] - CompMass.c[1],
                                                 PGeomPos^[2] - CompMass.c[2]);
        // Update offset of all connected geoms
        for i:= 0 to Robot.Shells.count-1 do begin
          if Robot.Shells[i].Body = MotherSolid.Body then begin
            PGeomPos := dGeomGetOffsetPosition(Robot.Shells[i].Geom);
            dGeomSetOffsetPosition(Robot.Shells[i].Geom, PGeomPos^[0] - CompMass.c[0],
                                                         PGeomPos^[1] - CompMass.c[1],
                                                         PGeomPos^[2] - CompMass.c[2]);
          end;
        end;

        MotherSolid.MovePosition(CompMass.c[0], CompMass.c[1], CompMass.c[2]);
        MotherSolid.SetZeroState;

        dMassTranslate(CompMass, -CompMass.c[0], - CompMass.c[1], - CompMass.c[2]);
        dBodySetMass(MotherSolid.Body, @CompMass);
      end;

      with newShell.GLObj as TGLSceneObject do begin
        Material.FrontProperties.Diffuse.SetColor(colorR, colorG, colorB);
      end;
      PositionSceneObject(newShell.GLObj, newShell.Geom);
    end;

    ShellNode := ShellNode.NextSibling;
  end;

end;


procedure TWorld_ODE.LoadThingsFromXML(XMLFile: string; Parser: TSimpleParser);
var XML: IXMLDocument;
    root: IXMLNode;
    LocalParser: TSimpleParser;
begin
  XML := LoadXML(XMLFile, XMLErrors);
  if XML = nil then exit;


  root:=XML.SelectSingleNode('/things');
  if root = nil then exit;

  LocalParser:= TSimpleParser.Create;
  try
    LocalParser.CopyVarList(Parser);
    LoadSolidsFromXML(Things, root, LocalParser);
  finally
    LocalParser.Free;
  end;
end;



procedure TWorld_ODE.LoadTrackFromXML(XMLFile: string; Parser: TSimpleParser);
var XML: IXMLDocument;
    root, objNode: IXMLNode;
    LocalParser: TSimpleParser;
begin
  XML := LoadXML(XMLFile, XMLErrors);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/track');
  if root = nil then exit;

  LocalParser:= TSimpleParser.Create;
  LocalParser.CopyVarList(Parser);

  try
    objNode := root.FirstChild;
    while objNode <> nil do begin

      if objNode.NodeName = 'defines' then begin
        LoadDefinesFromXML(LocalParser, objnode);
      end else if objNode.NodeName = 'polygon' then begin
        LoadPolygonFromXML(objNode, LocalParser);
      end else if objNode.NodeName = 'line' then begin
        LoadLineFromXML(objNode, LocalParser);
      end else if objNode.NodeName = 'arc' then begin
        LoadArcFromXML(objNode, LocalParser);
      end;

      objNode := objNode.NextSibling;
    end;

  finally
    LocalParser.Free;
  end;
end;

{
  <arc>
    <color rgb24='8F8F8F'/>
    <center x='0' y='0' z='0'/>
    <radius inner='1.3' outer='1.4'/>
    <angle_deg start='0' stop='90' steps='15'/>
  </arc>
}
procedure TWorld_ODE.LoadArcFromXML(const root: IXMLNode; Parser: TSimpleParser);
var PolyNode: IXMLNode;
    Xc, Yc, Zc: double;
    innerRadius, outerRadius: double;
    StartAngle, StopAngle, step: double;
    aWinColor: longWord;
    GLPolygon: TGLPolygon;
    ang: double;
    over: boolean;
    i: integer;
begin
  if root = nil then exit;

  PolyNode := root.FirstChild;
  if PolyNode = nil then exit;

  // Start a new contour
  //GLPolygon := TGLPolygon.CreateAsChild(FViewer.GLPlaneFloor);
  GLPolygon := TGLPolygon.CreateAsChild(OdeScene.FindChild('GLPlaneFloor',false));
  GLPolygon.Position.Z := 0.0005;

  // default values
  Xc := 0; Yc := 0; Zc := 0;
  aWinColor := $808080;
  innerRadius := 0; outerRadius := 1;
  StartAngle := 0; StopAngle := 180; step := 15;

  while PolyNode <> nil do begin
    if PolyNode.NodeName = 'center' then begin
      Xc := GetNodeAttrRealParse(PolyNode, 'x', Xc, Parser);
      Yc := GetNodeAttrRealParse(PolyNode, 'y', Yc, Parser);
      Zc := GetNodeAttrRealParse(PolyNode, 'z', Zc, Parser);
    end else if PolyNode.NodeName = 'radius' then begin
      innerRadius := GetNodeAttrRealParse(PolyNode, 'inner', innerRadius, Parser);
      outerRadius := GetNodeAttrRealParse(PolyNode, 'outer', outerRadius, Parser);
    end else if PolyNode.NodeName = 'angle_deg' then begin
      StartAngle := GetNodeAttrRealParse(PolyNode, 'start', StartAngle, Parser);
      StopAngle := GetNodeAttrRealParse(PolyNode, 'stop', StopAngle, Parser);
      step := GetNodeAttrRealParse(PolyNode, 'step', step, Parser);
    end else if PolyNode.NodeName = 'color' then begin
      aWinColor := StrToIntDef('$'+GetNodeAttrStr(PolyNode, 'rgb24', inttohex(aWinColor,6)), aWinColor);
      GLPolygon.Material.FrontProperties.Diffuse.AsWinColor := aWinColor;
    end;

    PolyNode := PolyNode.NextSibling;
  end;

  StartAngle := DegToRad(StartAngle);
  StopAngle := DegToRad(StopAngle);
  step := DegToRad(step);

  // Draw outer arc
  GLPolygon.BeginUpdate;
  ang := StartAngle;
  over := false;
  while true do begin
    GLPolygon.AddNode(Xc + outerRadius * cos(ang), Yc + outerRadius * sin(ang), ang); // Z stores the angle temporarily
    if over then break;
    ang := ang + step;
    if ang > StopAngle then begin
      ang :=  StopAngle;
      over := true;
    end;
  end;

  // Draw inner arc
  if innerRadius <= 0 then begin
    GLPolygon.AddNode(Xc, Yc, Zc);
  end else begin
    for i := GLPolygon.Nodes.Count-1 downto 0 do begin
      ang := GLPolygon.Nodes[i].Z;
      GLPolygon.Nodes[i].Z := Zc;
      GLPolygon.AddNode(Xc + innerRadius * cos(ang), Yc + innerRadius * sin(ang), Zc);
    end;
  end;
  GLPolygon.EndUpdate;

end;

procedure TWorld_ODE.LoadPolygonFromXML(const root: IXMLNode; Parser: TSimpleParser);
var PolyNode: IXMLNode;
    posX, posY, posZ: double;
    aWinColor: longWord;
    GLPolygon: TGLPolygon;
begin
  if root = nil then exit;

  PolyNode := root.FirstChild;
  if PolyNode = nil then exit;

  // Start a new contour
  //GLPolygon := TGLPolygon.CreateAsChild(FViewer.GLPlaneFloor);
  GLPolygon := TGLPolygon.CreateAsChild(OdeScene.FindChild('GLPlaneFloor',false));
  GLPolygon.Position.Z := 0.0005;

  while PolyNode <> nil do begin
    // default values
    posX := 0; posY := 0; posZ := 0;
    aWinColor := $808080;

    if PolyNode.NodeName = 'vertice' then begin
      posX := GetNodeAttrRealParse(PolyNode, 'x', posX, Parser);
      posY := GetNodeAttrRealParse(PolyNode, 'y', posY, Parser);
      posZ := GetNodeAttrRealParse(PolyNode, 'z', posZ, Parser);
      GLPolygon.AddNode(posX, posY, posZ);
    end else if PolyNode.NodeName = 'color' then begin
      aWinColor := StrToIntDef('$'+GetNodeAttrStr(PolyNode, 'rgb24', inttohex(aWinColor,6)), aWinColor);
      GLPolygon.Material.FrontProperties.Diffuse.AsWinColor := aWinColor;
    end;

    PolyNode := PolyNode.NextSibling;
  end;
end;

{  <line>
    <color rgb24='8F8F8F'/>
    <position x='0' y='0' z='0' angle='0'/>
    <size width='0' lenght='0'/>
  </line>
}
procedure TWorld_ODE.LoadLineFromXML(const root: IXMLNode; Parser: TSimpleParser);
var PolyNode: IXMLNode;
    posX, posY, posZ, angle: double;
    lineWidth, lineLength: double;
    aWinColor: longWord;
    GLPolygon: TGLPolygon;
//    x, y: double;
begin
  if root = nil then exit;

  PolyNode := root.FirstChild;
  if PolyNode = nil then exit;

  // Start a new contour
  //GLPolygon := TGLPolygon.CreateAsChild(FViewer.GLPlaneFloor);
  GLPolygon := TGLPolygon.CreateAsChild(OdeScene.FindChild('GLPlaneFloor',false));
  GLPolygon.Position.Z := 0.0005;

  // default values
  posX := 0; posY := 0; posZ := 0; angle := 0;
  lineWidth := 0.1; lineLength := 1;
  aWinColor := $808080;

  while PolyNode <> nil do begin
    if PolyNode.NodeName = 'position' then begin
      posX := GetNodeAttrRealParse(PolyNode, 'x', posX, Parser);
      posY := GetNodeAttrRealParse(PolyNode, 'y', posY, Parser);
      posZ := GetNodeAttrRealParse(PolyNode, 'z', posZ, Parser);
      angle := GetNodeAttrRealParse(PolyNode, 'angle', angle, Parser);
    end else if PolyNode.NodeName = 'size' then begin
      lineWidth := GetNodeAttrRealParse(PolyNode, 'width', lineWidth, Parser);
      lineLength := GetNodeAttrRealParse(PolyNode, 'length', lineLength, Parser);
    end else if PolyNode.NodeName = 'color' then begin
      aWinColor := StrToIntDef('$'+GetNodeAttrStr(PolyNode, 'rgb24', inttohex(aWinColor,6)), aWinColor);
      GLPolygon.Material.FrontProperties.Diffuse.AsWinColor := aWinColor;
    end;

    PolyNode := PolyNode.NextSibling;
  end;

  //angle := DegToRad(angle);

  {x := posX;
  y := posY;
  GLPolygon.AddNode(x, y, posZ);

  x := x + lineLength * cos(angle);
  y := y + lineLength * sin(angle);
  GLPolygon.AddNode(x, y, posZ);

  x := x - lineWidth * sin(angle);
  y := y + lineWidth * cos(angle);
  GLPolygon.AddNode(x, y, posZ);

  x := posX - lineWidth * sin(angle);
  y := posY + lineWidth * cos(angle);
  GLPolygon.AddNode(x, y, posZ);}

  GLPolygon.AddNode(0, 0, 0);
  GLPolygon.AddNode(lineLength, 0, 0);
  GLPolygon.AddNode(lineLength, lineWidth, 0);
  GLPolygon.AddNode(0, lineWidth, 0);

  GLPolygon.RotateAbsolute(zvector, -angle);
  GLPolygon.Position.SetPoint(posX, posY, posZ);

 { GLPolygon.Material.Texture.Image.LoadFromFile('..\grad.jpg');
  if lineWidth > 1e-10 then begin
    GLPolygon.Material.Texture.MappingSCoordinates.X := 0;
    GLPolygon.Material.Texture.MappingSCoordinates.Y := 1/lineWidth;
  end;
  GLPolygon.Material.Texture.MappingMode := tmmObjectLinear;
  GLPolygon.Material.Texture.TextureWrap := twNone;
  GLPolygon.Material.Texture.Disabled := false;}
end;


procedure TWorld_ODE.LoadDefinesFromXML(Parser: TSimpleParser; const root: IXMLNode);
var prop: IXMLNode;
    ConstName: string;
    ConstValue: double;
begin
  if root = nil then exit;

  // default values
  ConstName:='';
  ConstValue := 0;

  prop := root.FirstChild;
  while prop <> nil do begin
    if prop.NodeName = 'const' then begin
      ConstName := GetNodeAttrStr(prop, 'name', ConstName);
      ConstValue := GetNodeAttrRealParse(prop, 'value', ConstValue, Parser);
      if ConstName <> '' then
        Parser.RegisterConst(ConstName, ConstValue);
    end;
    prop := prop.NextSibling;
  end;
end;


procedure TWorld_ODE.LoadSceneFromXML(XMLFile: string);
var XML: IXMLDocument;
    root, objNode, prop: IXMLNode;
    posX, posY, posZ, angX, angY, angZ: double;
    newRobot: TRobot;
//    thing: TSolid;
    name, filename: string;
    mass, radius: double;
    Parser: TSimpleParser;
//    ConstName: string;
//    ConstValue: double;
begin
{  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason<>'' then begin
    with FParams.MemoDebug.Lines do begin
      Add('XML file error:' + XMLFile);
      Add(XML.ParseError.Reason);
    end;
    exit;
  end;}
  XMLFiles.Add(XMLFile);
  XML := LoadXML(XMLFile, XMLErrors);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/scene');
  if root = nil then exit;

  Parser := TSimpleParser.Create;
  try
    objNode := root.FirstChild;
    while objNode <> nil do begin
      if objNode.NodeName = 'robot' then begin
        prop := objNode.FirstChild;
        // default values
        name:='robot' + inttostr(Robots.count);
        filename := '';
        posX := 0; posY := 0; posZ := 0;
        angX := 0; angY := 0; angZ := 0;
        while prop <> nil do begin
          if prop.NodeName = 'ID' then begin
            name := GetNodeAttrStr(prop, 'name', name);
          end;
          if prop.NodeName = 'body' then begin
            filename := GetNodeAttrStr(prop, 'file', filename);
          end;
          if prop.NodeName = 'pos' then begin
            posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
            posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
            posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
          end;
          if prop.NodeName = 'rot_deg' then begin
            angX := degToRad(GetNodeAttrRealParse(prop, 'x', angX, Parser));
            angY := degToRad(GetNodeAttrRealParse(prop, 'y', angY, Parser));
            angZ := degToRad(GetNodeAttrRealParse(prop, 'z', angZ, Parser));
          end;
          prop := prop.NextSibling;
        end;

        //if filename <> '' then begin
        if fileexists(filename) then begin
          XMLFiles.Add(filename);
          newRobot := LoadRobotFromXML(filename, Parser);
          if newRobot <> nil then begin
            newRobot.Name := name;
            newRobot.SetXYZTeta(posX, posY, posZ, angZ);
          end;
        end;

      end else if objNode.NodeName = 'defines' then begin
        LoadDefinesFromXML(Parser, objnode);

      end else if objNode.NodeName = 'obstacles' then begin
        // Create static obstacles
        filename := GetNodeAttrStr(objNode, 'file', filename);
        if fileexists(filename) then begin
          XMLFiles.Add(filename);
          LoadObstaclesFromXML(filename, Parser);
        end;

      end else if objNode.NodeName = 'things' then begin
        // Create things
        filename := GetNodeAttrStr(objNode, 'file', filename);
        if fileexists(filename) then begin
          XMLFiles.Add(filename);
          LoadThingsFromXML(filename, Parser);
        end;

      end else if objNode.NodeName = 'track' then begin
        // Create track
        filename := GetNodeAttrStr(objNode, 'file', filename);
        if fileexists(filename) then begin
          XMLFiles.Add(filename);
          LoadTrackFromXML(filename, Parser);
        end;

      end else if objNode.NodeName = 'ball' then begin
        prop := objNode.FirstChild;
        // default values
        radius := -1; mass := -1;
        posX := 0; posY := 0; posZ := 0;

        while prop <> nil do begin

          if prop.NodeName = 'radius' then begin
            radius := GetNodeAttrRealParse(prop, 'value', radius, Parser);
          end;
          if prop.NodeName = 'mass' then begin
            mass := GetNodeAttrRealParse(prop, 'value', mass, Parser);
          end;
          if prop.NodeName = 'pos' then begin
            posX := GetNodeAttrRealParse(prop, 'x', posX, Parser);
            posY := GetNodeAttrRealParse(prop, 'y', posY, Parser);
            posZ := GetNodeAttrRealParse(prop, 'z', posZ, Parser);
          end;
          prop := prop.NextSibling;

        end;
      end;

      objNode := objNode.NextSibling;
    end;

  finally
    Parser.Free;
  end;

end;


function TWorld_ODE.LoadRobotFromXML(XMLFile: string; Parser: TSimpleParser): TRobot;
var XML: IXMLDocument;
    root, objNode: IXMLNode;
    newRobot: TRobot;
    str: string;
    LocalParser: TSimpleParser;
begin
  result := nil;

  XML := LoadXML(XMLFile, XMLErrors);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/robot');
  if root = nil then exit;

  LocalParser:= TSimpleParser.Create;
  LocalParser.CopyVarList(Parser);

  try
    newRobot := TRobot.Create;
    Robots.Add(newRobot);

    objNode := root.FirstChild;
    while objNode <> nil do begin
      if objNode.NodeName = 'kind' then begin
        str := GetNodeAttrStr(objNode, 'value', '');
        if lowercase(str)='omni3' then newRobot.Kind := rkOmni3
        else if lowercase(str)='omni4' then newRobot.Kind := rkOmni4
        else if lowercase(str)='wheelchair' then newRobot.Kind := rkWheelChair
        else if lowercase(str)='humanoid' then newRobot.Kind := rkHumanoid
        else if lowercase(str)='belt' then newRobot.Kind := rkConveyorBelt;
      end;

      if objNode.NodeName = 'defines' then begin
        LoadDefinesFromXML(LocalParser, objnode);
      end;

      if objNode.NodeName = 'solids' then begin
        LoadSolidsFromXML(newRobot.Solids, objNode, LocalParser);
        if newRobot.Solids.Count = 0 then exit;
        newRobot.MainBody := newRobot.Solids[0];
      end;

      if objNode.NodeName = 'wheels' then begin
        LoadWheelsFromXML(newRobot, objNode, LocalParser);
      end;

      if objNode.NodeName = 'shells' then begin
        LoadShellsFromXML(newRobot, objNode, LocalParser);
      end;

      if objNode.NodeName = 'sensors' then begin
        LoadSensorsFromXML(newRobot, objNode, LocalParser);
      end;

      if objNode.NodeName = 'articulations' then begin
        LoadLinksFromXML(newRobot, objNode, LocalParser);
      end;

      objNode := objNode.NextSibling;
    end;

  finally
    LocalParser.Free;
  end;

  result := newRobot;
end;


constructor TWorld_ODE.create;
begin
  AirDensity := 1.293; //kg/m3
  default_n_mu := 0.95;
  Ode_dt := 1/1000;
  TimeFactor := 1;
  ODEEnable := False;
  PhysTime := 0;

  SecondsCount := 0;
  DecPeriod := 0.04;

  OdeScene := FViewer.GLShadowVolume;

  Environment := TSolid.Create;
  Environment.Body := nil;

  Robots := TRobotList.Create;
  Obstacles := TSolidList.Create;
  Things := TSolidList.Create;

  XMLFiles := TStringList.Create;
  XMLErrors := TStringList.Create;

  //Create physic
  world := dWorldCreate();
//  dWorldSetQuickStepNumIterations(world, 10);
  Ode_QuickStepIters := 10;
  dWorldSetQuickStepNumIterations(world, Ode_QuickStepIters);
  space := dHashSpaceCreate(nil);
  //dHashSpaceSetLevels(space, -4, 1);
  contactgroup := dJointGroupCreate(0);
  dWorldSetGravity(world, 0, 0, -9.81);

  Ode_CFM := 1e-5;
  Ode_ERP := 0.4;
  dWorldSetCFM(world, Ode_CFM);
  dWorldSetERP(world, Ode_ERP);

  MaxWorldX := 12;
  MinWorldX := -12;
  MaxWorldY := 12;
  MinWorldY := -12;

  LoadSceneFromXML('scene.xml');
  SetCameraTarget(0);

  //Floor
  dCreatePlane(space, 0, 0, 1, 0);
  //Box wall limit
  dCreatePlane(space,  0, 1, 0, -MaxWorldY);
  dCreatePlane(space,  1, 0, 0, -MaxWorldX);
  dCreatePlane(space,  0,-1, 0, MinWorldY);
  dCreatePlane(space, -1, 0, 0, MinWorldX);

{  if Robots[0] <> nil then begin
    if Robots[0].MainBody.GLObj<>nil then begin
      FViewer.GLDummyCamPosRel.MoveTo(Robots[0].MainBody.GLObj);
      FViewer.GLDummyTargetCamRel.MoveTo(Robots[0].MainBody.GLObj);
    end;
  end;}
 {
  //CreateSolidCylinder(TestBody, 1, 1.12, -0.1, 1.5, 0.1, 1);
  //RotateSolid(TestBody, 0, 1, 0, pi/2);
}

end;

destructor TWorld_ODE.Destroy;
begin
  //Destroy the physic
  dJointGroupDestroy(contactgroup);

  Things.ClearAll;
  Things.Free;

  Robots.ClearAll;
  Robots.Free;

  Obstacles.ClearAll;
  Obstacles.Free;

  Environment.Free;

  dSpaceDestroy(space);
  //TODO Destroy the bodies
  dWorldDestroy(world);

  XMLErrors.Free;
  XMLFiles.Free;

  inherited;
end;

procedure TWorld_ODE.WorldUpdate;
begin
  //Update the physic
  dSpaceCollide(space, nil, nearCallback);
  if FParams.CBWorldQuickStep.Checked then begin
    //dWorldSetQuickStepNumIterations(world, 10);
    dWorldQuickStep(world, Ode_dt);
  end else begin
    dWorldStep(world, Ode_dt);
  end;
  //dWorldStep (world, Ode_dt);
  physTime := physTime + Ode_dt;

  // remove all contact joints
  dJointGroupEmpty (contactgroup);
end;


procedure TFViewer.FormCreate(Sender: TObject);
var s, fl: string;
    Slist: TStringList;
begin
  if ParamCount > 0 then begin
    s := ParamStr(1);
  end else begin
    s := 'default';
    fl := extractfilepath(application.exename) + 'Scene.cfg';
    if fileexists(extractfilepath(application.exename) + 'Scene.cfg') then begin
      Slist := TStringList.Create;
      try
        try
          Slist.LoadFromFile(fl);
          if Slist.Count > 0 then begin
            s := Slist[0];
          end;
        finally
          Slist.Free;
        end;
      except
        on E: Exception do showmessage(E.Message);
      end;
    end;
  end;

  //showmessage(getCurrentDir + ' ' + s);
  if DirectoryExists(s) then begin
    SetCurrentDir(s);
    CurrentProject := s;
  end;

  FormStorage.IniFileName := GetIniFineName;
//  GetProcessAffinityMask(
  SetThreadAffinityMask(GetCurrentThreadId(), 1);
//  SetThreadAffinityMask(GetCurrentProcessId(), 1);
  QueryPerformanceFrequency(t_delta);
  t_delta := t_delta div 1000;

  //Execute Create physic
  WorldODE := TWorld_ODE.create;

  //WorldODE.SampleCount := 0;
  WorldODE.ODEEnable := True;

  GLHUDTextObjName.Text := '';
  HUDStrings := TStringlist.Create;
  GetVersionInfo;
  SimTwoVersion := 'SimTwo v' + InfoData[3];

  Timer.Enabled := true;
end;

procedure TFViewer.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pick : TGLCustomSceneObject;
    vs, CamPos, hitPoint: TVector;
begin
  pick := GLSceneViewerPick(x, y);
  if Assigned(pick) and not (ssCtrl in shift) then begin
     vs :=  GLSceneViewer.Buffer.ScreenToVector(x, GLSceneViewer.Buffer.ViewPort.Height - y);
     NormalizeVector(vs);
     //FParams.EditDebug3.Text := format('%.2f,%.2f,%.2f', [vs[0], vs[1], vs[2]]);
    if pick.TagObject is TSolid then with WorldODE do begin
      if TSolid(pick.TagObject).Body <> nil then begin
        PickSolid := TSolid(pick.TagObject);
        CamPos := GLSceneViewer.Buffer.Camera.Position.AsVector;
        if PickSolid.GLObj.RayCastIntersect(CamPos, vs, @hitPoint[0]) then begin
          CreatePickJoint(PickSolid, hitPoint[0], hitPoint[1], hitPoint[2]);
          PickDist := sqrt(sqr(hitPoint[0]-CamPos[0])+sqr(hitPoint[1]-CamPos[1])+sqr(hitPoint[2]-CamPos[2]));
        end;
      end;
    end;
  end;

  my := y;
  mx := x;
end;

procedure TFViewer.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if [ssShift, ssleft, ssCtrl] <= shift then begin
    GLScene.CurrentGLCamera.MoveTargetInEyeSpace(0, -0.03*(mx-x), 0.03*(my-y));
    //GLDummyTargetCam.Position := GLScene.CurrentGLCamera.TargetObject.Position;
    //UpdateCamPos(FParams.RGCamera.ItemIndex);
    my := y;
    mx := x;
  end else if [ssleft, ssCtrl] <= shift then begin
    GLScene.CurrentGLCamera.MoveAroundTarget(my-y,mx-x);
    GLDummyCamPos.Position := GLScene.CurrentGLCamera.Position;
    //UpdateCamPos(FParams.RGCamera.ItemIndex);
    my := y;
    mx := x;
  end;

//  if WorldODE.PickSolid = nil then begin
//    GLSceneViewerPick(x, y);
//  end;

end;

procedure TFViewer.GLSceneViewerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  WorldODE.DestroyPickJoint;
end;

function TFViewer.GLSceneViewerPick(X, Y: Integer): TGLCustomSceneObject;
var pick : TGLCustomSceneObject;
begin
  pick:=(GLSceneViewer.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);

  if Assigned(pick) then begin
    if pick.TagObject is TSolid then begin
      GLHUDTextObjName.Text := TSolid(pick.TagObject).description;
      GLHUDTextObjName.TagFloat := 10;
      if Assigned(OldPick) then OldPick.Material.FrontProperties.Emission.Color:=clrBlack;
      pick.Material.FrontProperties.Emission.Color:=clrRed;
      OldPick := Pick;
    end else begin
      GLHUDTextObjName.Text := '';
      GLHUDTextObjName.TagFloat := 0;
      if Assigned(OldPick) then OldPick.Material.FrontProperties.Emission.Color:=clrBlack;
      OldPick := nil;
    end;
    if pick.TagObject is TAxis then begin
      GLHUDTextObjName.Text := TAxis(pick.TagObject).ParentLink.description;
      GLHUDTextObjName.TagFloat := 10;
    end;
  end;
  result := pick;
end;

procedure AxisTorqueModel(axis: TAxis; Theta, w, dt: double; var T: double);
var old_Im, dI, err: double;
    max_delta_Im: double;
    minLim, maxLim: double;
    ev, ebt: double;
begin
  max_delta_Im := 0.15; // A/ms
  with Axis do begin
    // If active use PID controller
    if Motor.active then begin
      with Motor.Controller do begin
        if active then begin

          ticks := ticks + WorldODE.Ode_dt;
          if ticks >= ControlPeriod then begin
            ticks := ticks - ControlPeriod;
          //inc(ticks);
          //if ticks >= ControlPeriod then begin
            case ControlMode of
              cmPIDPosition: begin
                if axis.isCircular then begin   // If it is a circular joint
                  axis.GetLimits(minLim, maxLim);
                  if (minlim < -pi) and (maxlim > pi) then begin  // and it has no limits
                    err := DiffAngle(ref.theta, theta);           // Use the shortest path solution
                  end else begin
                    err := ref.theta - theta;                     // Else use the standard path
                  end;
                end else begin
                  err := ref.theta - theta;
                end;
                ref.volts := CalcPID(Motor.Controller, ref.theta, err);
              end;

              cmPIDSpeed: begin
                //ref.volts := CalcPID(Motor.Controller, ref.w, w);
                ref.volts := CalcPID(Motor.Controller, ref.w, ref.w - filt_speed);
              end;

              cmState: begin
                //ref.volts := CalcPD(Motor.Controller, ref.theta, ref.w, theta, w);
                ref.volts := CalcPD(Motor.Controller, ref.theta, ref.w, theta, filt_speed);
              end;
            end;
            //ticks := 0;
          end;
        end;
      end;
      // Voltage with saturation
      Motor.voltage := max(-Motor.Vmax, min(Motor.Vmax, ref.volts));
      // Motor Model
      {if Motor.Vmax <> 0 then begin
        duty := abs(Motor.voltage / Motor.Vmax);
      end else begin
        duty := 0;
      end;}

      old_Im := Motor.Im;

      if not Motor.simple then begin
        ev := w * Motor.GearRatio * Motor.Ki ;
        ev := max(-Motor.Vmax, min(Motor.Vmax, ev));

        if Motor.Li <> 0 then begin
          if Motor.Ri <> 0 then begin
            ebt := exp(-dt*Motor.Ri / Motor.Li);
            Motor.Im := ebt * Motor.Im + (1 - ebt) * (Motor.voltage - ev) / Motor.Ri;
          end else begin
            dI := dt / Motor.Li * (Motor.voltage - ev);
            Motor.Im := Motor.Im + dI;
          end;
        end else begin
          if Motor.Ri <> 0 then begin
            Motor.Im := (Motor.voltage - ev) / Motor.Ri;
          end else begin
            Motor.Im := Motor.Imax * sign(Motor.voltage);
          end;
        end;

{        if Motor.Ri <> 0 then begin
          if Motor.Li/Motor.Ri > 1/(5*dt) then begin
            Motor.Im := (Motor.voltage - ev) / Motor.Ri;
          end else begin
            //dI := dt / Motor.Li * (Motor.voltage - ev - Motor.Ri* Motor.Im);
            //Motor.Im := Motor.Im + dI;
            ebt := exp(-dt*Motor.Ri / Motor.Li);
            Motor.Im := ebt * Motor.Im + (1 - ebt) * (Motor.voltage - ev) / Motor.Ri;
          end;
        end else begin
          if Motor.Li <> 0 then begin
            dI := dt / Motor.Li * (Motor.voltage - ev);
            Motor.Im := Motor.Im + dI;
          end else begin
            Motor.Im := Motor.Imax * sign(Motor.voltage);
          end;
        end;
}
      end else begin
        Motor.Im := Motor.voltage / Motor.Ri;
      end;


      if Motor.Im > old_Im + max_delta_Im then Motor.Im := old_Im + max_delta_Im;
      if Motor.Im < old_Im - max_delta_Im then Motor.Im := old_Im - max_delta_Im;

      if abs(Motor.voltage)>1e-3 then begin
        //iw := max(-Pars.Imax , min(Pars.Imax , iw));
        // this limit is dependent on the current sensor placement
        // Here is assumed that it is only active on the "on" time of the PWM
        //Motor.Im := max(-Motor.Imax / duty, min(Motor.Imax / duty, Motor.Im));
        Motor.Im := max(-Motor.Imax, min(Motor.Imax, Motor.Im));
      end;
    end else begin
      Motor.Im := 0;
    end;

    Motor.PowerDrain := Motor.Im * Motor.voltage * WorldODE.Ode_dt;
    if Motor.PowerDrain > 0 then begin
      Motor.EnergyDrain := Motor.EnergyDrain + Motor.PowerDrain;
    end;

    // coulomb friction
    // Tq := Friction.Fc * sign(w);
    // Limit it to avoid instability
    //if Friction.CoulombLimit >= 0 then
    //  Tq := max(-Friction.CoulombLimit * abs(w), min(Friction.CoulombLimit * abs(w), Tq));
    //T := Motor.Im * Motor.Ki * Motor.GearRatio - Friction.Bv * w - Tq - Spring.K * diffangle(Theta, Spring.ZeroPos);
    //T := Motor.Im * Motor.Ki * Motor.GearRatio - Friction.Bv * w - Tq - Spring.K * (Theta - Spring.ZeroPos);
    T := Motor.Im * Motor.Ki * Motor.GearRatio - Friction.Bv * filt_speed - Spring.K * (Theta - Spring.ZeroPos);
    //T := Motor.Im * Motor.Ki * Motor.GearRatio - Friction.Bv * w - Spring.K * (Theta - Spring.ZeroPos);

  end;
end;



procedure ReadKeyVals(var KeyVals: TKeyVals);
begin
  if IsKeyDown(VK_UP) then begin
    KeyVals[0] := 1;
    KeyVals[1] := -1;
  end;

  if IsKeyDown(VK_DOWN) then begin
    KeyVals[0] := -1;
    KeyVals[1] := 1;
  end;

  if IsKeyDown(VK_RIGHT) then begin
    if not IsKeyDown(VK_LCONTROL) then begin
      KeyVals[0] := -1;
      KeyVals[1] := -1;
      KeyVals[2] := -1;
    end else begin
      KeyVals[0] := 0.16;
      KeyVals[1] := 0.16;
      KeyVals[2] := -0.83;
    end;
  end;

  if IsKeyDown(VK_LEFT) then begin
    if not IsKeyDown(VK_LCONTROL) then begin
      KeyVals[0] := 1;
      KeyVals[1] := 1;
      KeyVals[2] := 1;
    end else begin
      KeyVals[0] := -0.16;
      KeyVals[1] := -0.16;
      KeyVals[2] := 0.83;
    end;
  end;
end;

procedure TFViewer.IRSharpNoiseModel(r, i: integer);
var v, iv, var_v, m, var_dist: double;
begin
  //v := 0;
  with WorldODE.Robots[r].Sensors[i] do begin
    if kind <> skIRSharp then exit;
    if not has_measure then exit;
    if not Noise.active then exit;

    //var_dist := 0;
    iv := Noise.gain * measure + Noise.offset;
    if iv <> 0 then
      v := 1 / iv
    else exit;
    var_v := Noise.var_d * measure + Noise.var_k;
    //m := - Noise.gain / sqr(iv);
    m := - Noise.gain * sqr(v);
    if m <> 0 then begin
      var_dist := var_v / sqr(m);
      measure := measure + RandG(0, sqrt(var_dist));
    end;

  end;
end;


procedure TFViewer.FillRemote(r: integer);
var i: integer;
  v1, v2: TdVector3;
begin
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[r], RemState do begin
    id:=$5db0;
    // Calculate robot position and orientation
    if (MainBody <> nil) and
       (MainBody.Body <> nil) then begin
      v1 := dBodyGetPosition(MainBody.Body)^;
      dBodyGetRelPointPos(MainBody.Body, 1,0,0, v2);
      Robot.x := v1[0];
      Robot.y := v1[1];
      Robot.teta := atan2(v2[1]-v1[1], v2[0]-v1[0]);
    end;

    // Fill Remote Odometry
    for i := 0 to min(MaxRemWheels, Wheels.Count) - 1 do begin
      Robot.Odos[i] := Wheels[i].Axle.Axis[0].Odo.Value;
    end;

    // Fill remote IR sensors
    for i:=0 to min(Sensors.Count, MaxRemIrSensors)-1 do begin
      if Sensors[i].has_measure then begin
        Robot.IRSensors[i] := Sensors[i].measure;
      end else begin
        Robot.IRSensors[i] := 0;
      end;
    end;

    {Ball.x:=BallsState[i].x;
    Ball.y:=BallsState[i].y;
    Ball.vx:=BallsState[i].vx;
    Ball.vy:=BallsState[i].vy;
    Ball.bcolor:=BallsState[i].bcolor;
    Ball.goal:=BallsState[i].goal;}
  end;
end;

procedure TFViewer.UpdateGLScene;
var r, i: integer;
begin
    if GLHUDTextObjName.TagFloat > 0 then begin
      GLHUDTextObjName.TagFloat := GLHUDTextObjName.TagFloat - GLCadencer.FixedDeltaTime;
      GLHUDTextObjName.ModulateColor.Alpha := max(0, min(1, GLHUDTextObjName.TagFloat / 2));
    end else begin
      GLHUDTextObjName.Text := '';
    end;
    GLHUDTextGeneric.Text := HUDStrings.Text;

    for r := 0 to WorldODE.Robots.Count-1 do begin
      with WorldODE.Robots[r] do begin
        // Shells
        for i := 0 to Shells.Count-1 do begin
          if Shells[i].GLObj = nil then continue;
          PositionSceneObject(Shells[i].GLObj, Shells[i].Geom);
          if Shells[i].GLObj is TGLCylinder then Shells[i].GLObj.pitch(90);
        end;
        //Sensors
        for i := 0 to Sensors.Count-1 do begin
          if Sensors[i].GLObj = nil then continue;
          PositionSceneObject(Sensors[i].GLObj, Sensors[i].Geoms[0]);
          if Sensors[i].GLObj is TGLCylinder then Sensors[i].GLObj.pitch(90);
        end;

        // Solids
        for i := 0 to Solids.Count-1 do begin
          if Solids[i].GLObj <> nil then begin
            if Solids[i].kind = skMotorBelt then begin  // make the texture slide if there is a belt speed <> 0
              if Solids[i].GLObj.Material.TextureEx.Count > 0 then begin
                with Solids[i].GLObj.Material.TextureEx.Items[0] do begin
                  TextureOffset.Y := TextureOffset.Y - TextureScale.Y * GLCadencer.FixedDeltaTime * WorldODE.TimeFactor * Solids[i].BeltSpeed;
                  if TextureOffset.Y > 1 then TextureOffset.Y := TextureOffset.Y - 1;
                  if TextureOffset.Y < -1 then TextureOffset.Y := TextureOffset.Y + 1;
                end;
              end;
            end;
            PositionSceneObject(Solids[i].GLObj, Solids[i].Geom);
            if Solids[i].GLObj is TGLCylinder then Solids[i].GLObj.pitch(90);
          end;

          if Solids[i].AltGLObj <> nil then begin
            PositionSceneObject(Solids[i].AltGLObj, Solids[i].Geom);
          end;
          if Solids[i].ShadowGlObj <> nil then begin
            PositionSceneObject(Solids[i].ShadowGlObj, Solids[i].Geom);
          end;
        end;

        // Axis
        for i := 0 to Axes.Count-1 do begin
          if Axes[i].GLObj = nil then continue;
          Axes[i].GLSetPosition;
        end;

        //PositionSceneObject(WorldODE.TestBody.GLObj, WorldODE.TestBody.Geom);
        //if WorldODE.testBody.GLObj is TGLCylinder then WorldODE.testBody.GLObj.pitch(90);
      end;
    end;

    with WorldODE do begin
      for i := 0 to Things.Count-1 do begin
        if Things[i].GLObj = nil then continue;
        PositionSceneObject(Things[i].GLObj, Things[i].Geom);
        if Things[i].GLObj is TGLCylinder then Things[i].GLObj.pitch(90);
      end;
    end;
//    if WorldODE.ball_geom <> nil then
//      PositionSceneObject(FViewer.GLSphere_ball, WorldODE.ball_geom);
end;


procedure TFViewer.GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var theta, w, Tq: double;
    i, r: integer;
    t_start, t_end, t_end_gl: int64;
    t_i1, t_i2, t_itot: int64;
    v1, v2: TdVector3;
//    txt: string;
    vs: TVector;
    Curpos: TPoint;
    newFixedTime: double;
begin
  GLScene.CurrentGLCamera.Position := GLDummyCamPos.Position;
  if WorldODE.ODEEnable <> False then begin
    QueryPerformanceCounter(t_start);
    t_itot := 0;

    // while WorldODE.physTime < newtime do begin
    newFixedTime := WorldODE.physTime + GLCadencer.FixedDeltaTime * WorldODE.TimeFactor;
    while WorldODE.physTime < newFixedTime do begin

      // Higher level controller (subsampled)
      WorldODE.SecondsCount := WorldODE.SecondsCount + WorldODE.Ode_dt;
      if WorldODE.SecondsCount > WorldODE.DecPeriod then begin
        WorldODE.SecondsCount := WorldODE.SecondsCount - WorldODE.DecPeriod;

        t_last := t_act;
        QueryPerformanceCounter(t_act);

        // Before control block
        for r := 0 to WorldODE.Robots.Count-1 do begin
          with WorldODE.Robots[r] do begin

            // Read Odometry
            for i := 0 to Axes.Count-1 do begin
              WorldODE.UpdateOdometry(Axes[i]);
            end;

            for i := 0 to Sensors.Count - 1 do begin
              if Sensors[i].kind = skIRSharp then begin
                // Process IR sensors noise
                IRSharpNoiseModel(r, i);
              end;
            end;

            // Fill RemState
            if r = Fparams.LBRobots.ItemIndex then begin
              //UDP.RemoteHost:=EditIP.text;
              FillRemote(r);
            end;

            FParams.ShowRobotPosition(r);

            // Default Control values are zero
            for i := 0 to Axes.Count-1 do begin
              Axes[i].ref.volts := 0;
              Axes[i].ref.w := 0;
              Axes[i].ref.Torque := 0;
            end;

          end;
        end;

        for r := 0 to WorldODE.Robots.Count-1 do begin
          with WorldODE.Robots[r] do begin

            // Call the selected Decision System
            if Fparams.RGControlBlock.ItemIndex = 0 then begin
              zeromemory(@KeyVals[0], sizeof(KeyVals));
              ReadKeyVals(KeyVals);
              if r = Fparams.LBRobots.ItemIndex then begin
                for i := 0 to WorldODE.Robots[r].Wheels.Count-1 do begin
                  Wheels[i].Axle.Axis[0].ref.volts := Keyvals[i] * Wheels[i].Axle.Axis[0].Motor.Vmax;
                  Wheels[i].Axle.Axis[0].ref.w := Keyvals[i]*30; //TODO: angular speed constant
                end;
              end;
            end else if Fparams.RGControlBlock.ItemIndex = 1 then begin  // Script controller
              if r = 0 then FEditor.RunOnce; // One call to the script, in the begining, for all robots
              if FEditor.SimTwoCloseRequested then begin
                close;
                exit;
              end;
            end else if Fparams.RGControlBlock.ItemIndex = 2 then begin  // LAN controller
              Fparams.UDPServer.SendBuffer(Fparams.EditRemoteIP.Text, 9801, RemState, sizeof(RemState));
              // Test if A new position was requested
              if RemControl.num = r+1 then begin
                SetXYZTeta(RemControl.x, RemControl.y, RemControl.z, RemControl.teta);
              end;
              // Copy Remote Control values to Wheel Structs
              for i := 0 to Wheels.Count-1 do begin
                Wheels[i].Axle.Axis[0].ref.volts := RemControl.u[i];
                Wheels[i].Axle.Axis[0].ref.w := RemControl.Wref[i];
              end;
              // Default Remote Control values is zero
              ZeroMemory(@RemControl,sizeof(RemControl));
            end;
            //Sleep(1);

            Fparams.ShowRobotState;

            FChart.AddSample(r, WorldODE.physTime);
          end;
        end;
      end;
      // End of High level (and subsampled) control

      //Use motors and frictions to calculate axis torques and forces
      for r := 0 to WorldODE.Robots.Count-1 do begin
        //if WorldODE.Robots[r].ForceMoved then begin
          //WorldODE.Robots[r].ForceMoved := false;
          //GLCadencer.Mode := cmManual;
          //continue;
        //end;

        // Motores no eixo das rodas e atritos
        for i := 0 to WorldODE.Robots[r].Axes.Count-1 do begin
          with WorldODE.Robots[r] do begin
            //if not active then continue; //TODO
            theta := Axes[i].GetPos();
            w := Axes[i].GetSpeed();

            //AxisTorqueModel(Axes[i], Theta, Axes[i].filt_speed, Tq);
            AxisTorqueModel(Axes[i], Theta, w, WorldODE.Ode_dt ,Tq);
            // Apply it to the axis
            Axes[i].AddTorque(Tq);
            // Apply Extra torque
            Axes[i].AddTorque(Axes[i].ref.Torque);
          end;
        end;

        // Robot Main Body extra Frictions
        {if (WorldODE.Robots[r].MainBody <> nil) and
           (WorldODE.Robots[r].MainBody.Body <> nil) then begin
          // Robot Body Linear Friction
          v2 := Vector3ScalarMul(WorldODE.Robots[r].MainBody.Body.lvel, -1e-2);
          dBodyAddForce(WorldODE.Robots[r].MainBody.Body, v2[0], v2[1], v2[2]);

          // Robot Body Angular Friction
          v1 := Vector3ScalarMul(WorldODE.Robots[r].MainBody.Body.avel, -1e-2);
          dBodyAddTorque(WorldODE.Robots[r].MainBody.Body, v1[0], v1[1], v1[2]);
        end;}

        // Drag
        for i:=0 to WorldODE.Robots[r].Solids.Count-1 do begin
          with WorldODE.Robots[r].Solids[i] do begin
            if Drag <> 0 then begin
               v1 := dBodyGetLinearVel(Body)^;
               dBodyVectorFromWorld(Body, v1[0], v1[1], v1[2], v2);
               // Air Drag
               v1[0] := -0.5 * WorldODE.AirDensity * v2[0] * abs(v2[0])* Ax * Drag;
               v1[1] := -0.5 * WorldODE.AirDensity * v2[1] * abs(v2[1])* Ay * Drag;
               v1[2] := -0.5 * WorldODE.AirDensity * v2[2] * abs(v2[2])* Az * Drag;
               dBodyAddRelForce(Body, v1[0], v1[1], v1[2]);
                  //dBodyVectorToWorld(Body, v1[0], v1[1], v1[2], v2);
                  //dBodyAddForce(Body, -v2[0], -v2[1], -v2[2]);
               v1 := dBodyGetAngularVel(Body)^;
               dBodyVectorFromWorld(Body, v1[0], v1[1], v1[2], v2);
               v1[0] := -0.5 * WorldODE.AirDensity * v2[0] * abs(v2[0])* Ax * Drag;
               v1[1] := -0.5 * WorldODE.AirDensity * v2[1] * abs(v2[1])* Ay * Drag;
               v1[2] := -0.5 * WorldODE.AirDensity * v2[2] * abs(v2[2])* Az * Drag;

               dBodyVectorToWorld(Body, v1[0], v1[1], v1[2], v2);
               v2[0] := max(-100,min(100,v2[0]));
               v2[1] := max(-100,min(100,v2[1]));
               v2[2] := max(-100,min(100,v2[2]));
               dBodyAddTorque(Body, v2[0], v2[1], v2[2]);
               //dBodyAddRelTorque(Body, v1[0], v1[1], v1[2]);
            end;
          end;
        end;

        // Buoyancy
        dWorldGetGravity(WorldODE.world, v1);
        dNormalize3(v1);
        for i:=0 to WorldODE.Robots[r].Solids.Count-1 do begin
          with WorldODE.Robots[r].Solids[i] do begin
            if BuoyantMass <> 0 then begin
               v2 := Vector3ScalarMul(v1, BuoyantMass);
               //v2 := Vector3ScalarMul(v1, BuoyantMass * Volume);
               //v2 := Vector3ScalarMul(v1, BuoyantMass );
               //FParams.EditDebug3.text := format('%g',[Volume]);
               dBodyAddForce(Body, v2[0], v2[1], v2[2]);
            end;
          end;
        end;

        // Thrusters
        for i:=0 to WorldODE.Robots[r].Solids.Count-1 do begin
          with WorldODE.Robots[r].Solids[i] do begin
            if kind = skPropeller then begin
               v1 := dBodyGetAngularVel(Body)^;
               dBodyVectorFromWorld(Body, v1[0], v1[1], v1[2], v2);
               dBodyAddRelForce(Body, 0.01*v2[0], 0, 0);
            end;
          end;
        end;


        // Reset sensors measure
        for i:=0 to WorldODE.Robots[r].Sensors.Count-1 do begin
          WorldODE.Robots[r].Sensors[i].measure := 1e6;
          WorldODE.Robots[r].Sensors[i].has_measure := false;
        end;
      end; //end robot loop

      if WorldODE.PickSolid <> nil then with WorldODE do begin
        if Focused then begin
          if IsKeyDown('q') then begin
            PickDist := PickDist * 1.001;
          end else if IsKeyDown('a') then begin
            PickDist := PickDist / 1.001;
          end;
        end;
        Curpos:= ScreenToClient(Mouse.CursorPos);
        vs :=  GLSceneViewer.Buffer.ScreenToVector(Curpos.X, GLSceneViewer.Buffer.ViewPort.Height - Curpos.y);
        NormalizeVector(Vs);
        scalevector(Vs, PickDist);
        AddVector(Vs, GLSceneViewer.Buffer.Camera.Position.AsVector);
        WorldODE.movePickJoint(vs[0], vs[1], vs[2]);
        WorldODE.UpdatePickJoint;
      end;

      QueryPerformanceCounter(t_i1);
      WorldODE.WorldUpdate;
      QueryPerformanceCounter(t_i2);
      t_itot := t_itot + t_i2 - t_i1;

      // Process Sensors
      for r := 0 to WorldODE.Robots.Count-1 do begin
        for i := 0 to WorldODE.Robots[r].Sensors.Count - 1 do begin
          with WorldODE.Robots[r].Sensors[i] do begin
            //measure := min(measure, dist);
            case kind of
              skIRSharp:
                measure := min(measure, dist);
              skCapacitive: begin
                if has_measure then begin
                  measure := 1
                end else begin
                  measure := 0;
                  has_measure := true;
                end;
              end;
              skInductive: begin
                if has_measure then begin
                  if (MeasuredSolid <> nil) and
                     (smMetallic in MeasuredSolid.MatterProperties) then
                        measure := 1
                  else measure := 0;
                end else begin
                  measure := 0;
                  has_measure := true;
                end;
              end;
            end;
          end;
        end;
      end;

      // Debug IR sensors
      {txt := '';
      for r := 0 to WorldODE.Robots.Count-1 do begin
        txt := txt + format('[%d] ',[r]);
        for i := 0 to WorldODE.Robots[r].IRSensors.Count - 1 do begin
          if WorldODE.Robots[r].IRSensors[i].has_measure then begin
            txt := txt + format('%d: (%.2f) ',[i, WorldODE.Robots[r].IRSensors[i].measure]);;
          end;
        end;
      end;
      FParams.EditDEbug2.Text := txt;}

    end;
    //End Physics Loop

    QueryPerformanceCounter(t_end);
    //FParams.EditDebug.text := format('%.2f (%.2f)[%.2f]',[(t_end - t_start)/t_delta, t_itot/t_delta, (t_act - t_last)/t_delta]);

    // GLScene

    //Update all GLscene Parts.
    UpdateGLScene;
    QueryPerformanceCounter(t_end_gl);
    FParams.EditDebug.text := format('%.2f (%.2f) %.2f [%.2f]',[(t_end - t_start)/t_delta, t_itot/t_delta, (t_end_gl - t_end)/t_delta, (t_act - t_last)/t_delta]);

    // Update Camera Position
    UpdateCamPos(FParams.RGCamera.ItemIndex);

    FParams.ShowCameraConfig(GLCamera);

    GLSceneViewer.Invalidate;

    //GLMemoryViewer.Render(nil);
  end;
end;


procedure TFViewer.UpdateCamPos(CMode: integer);
begin
  case CMode of
    0: begin
         GLCamera.TargetObject := GLDummyTargetCam;
         GLCamera.Position := GLDummyCamPos.Position;
       end;
    1: begin
         GLCamera.TargetObject := GLDummyTargetCamRel;
         GLCamera.Position := GLDummyCamPos.Position;
       end;
    2: begin
         GLCamera.TargetObject := GLDummyTargetCamRel;
         GLCamera.Position.X := GLCamera.TargetObject.AbsolutePosition[0] + GLDummyCamPosRel.Position.X;
         GLCamera.Position.Y := GLCamera.TargetObject.AbsolutePosition[1] + GLDummyCamPosRel.Position.Y;
       end;
    3: begin
         GLCamera.TargetObject := GLDummyTargetCamRel;
         with RemState do begin
           GLCamera.Position.X := GLCamera.TargetObject.AbsolutePosition[0] - 3*cos(Robot.teta);
           GLCamera.Position.Y := GLCamera.TargetObject.AbsolutePosition[1] - 3*sin(Robot.teta);
         end;
       end;
    4: begin
         GLCamera.TargetObject := GLDummyTargetCam;
         GLCamera.Position.x := 0;
         GLCamera.Position.y := -0.001;
         //GLCamera.Position.z := 2;
       end;
  end;
end;

function PosLastChar(c: char; S: string): Integer;
var i: integer;
begin
//  result := 0;
  i := length(s);
  while i > 0 do begin
    if s[i] = c then break;
  end;
  result := i;
end;


procedure TFViewer.FormClose(Sender: TObject; var Action: TCloseAction);
var fl: string;
    Slist: TStringList;
begin
  FSheets.Close;
  FSceneEdit.Close;

  //Execute Destroy Physics
  GLCadencer.Enabled := False;
  WorldODE.ODEEnable := False;
  WorldODE.destroy;

  fl := extractfilepath(application.exename) + 'Scene.cfg';
  Slist := TStringList.Create;
  try
    try
      Slist.Add(extractfilename(GetCurrentDir));
      Slist.SaveToFile(fl);
    finally
      Slist.Free;
    end;
  except
    on E: Exception do showmessage(E.Message);
  end;

  FSceneEdit.ReSpawn;
end;

procedure TFViewer.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //Mouse wheel zoom + -
  if (WorldODE.PickSolid = nil) or (WorldODE.PickJoint = nil ) then begin
    GLScene.CurrentGLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
    GLDummyCamPos.Position := GLScene.CurrentGLCamera.Position;
    UpdateCamPos(FParams.RGCamera.ItemIndex);
  //Mouse wheel object distance + -
  end else with WorldODE do begin
    PickDist := PickDist * Power(1.1, WheelDelta / 120);
  end;
end;

procedure TFViewer.FormShow(Sender: TObject);
begin
  FParams.show;
  FEditor.show;
  FChart.show;
  FSheets.show;

  FSceneEdit.Show;
  if WorldODE.XMLErrors.Count > 0 then begin
    FSceneEdit.LBErrors.Items.AddStrings(WorldODE.XMLErrors);
  end;

  //SetTrailCount(strtointdef(FParams.EditTrailsCount.Text, 8), strtointdef(FParams.EditTrailSize.Text, 200));
  FParams.BSetTrailParsClick(Sender);

  FParams.ShowParsedScene;

  MakeFullyVisible();
  UpdateGLScene;

  GLCadencer.enabled := true;
  TestTexture;
  //GLHUDTextGeneric.Text := 'Test' + #13 + '2nd line?'; 

end;

procedure TFViewer.TimerTimer(Sender: TObject);
var fps: double;
    script: string;
begin
  fps := GLSceneViewer.FramesPerSecond;
  GLSceneViewer.ResetPerformanceMonitor;
  if FParams.RGControlBlock.ItemIndex = 1 then begin
    script := ' - Script Running';
  end else begin
    script := '';
  end;
  Caption:=Format('SimTwo - v%s [%s] (%.1f FPS)%s', [InfoData[3], CurrentProject, fps, script]);
end;


procedure TFViewer.TestTexture;
var img: TGLBitmap32;
    thebmp: TBitmap;
begin
  thebmp := TBitmap.Create;
  thebmp.Width := 256;
  thebmp.Height := 256;
  thebmp.PixelFormat := pf24bit;
  thebmp.Canvas.TextOut(10,10,'Hello World!');
  //img := GLCube1.Material.Texture.Image.GetBitmap32(GL_TEXTURE_2D);  tgltexture
  img := GLCube1.Material.TextureEx.Items[0].Texture.Image.GetBitmap32(GL_TEXTURE_2D);
  img.AssignFromBitmap24WithoutRGBSwap(thebmp);
  thebmp.Free;
end;

procedure TWorld_ODE.SetCameraTarget(r: integer);
begin
  if (r < 0) or (r >= Robots.Count) then exit;
  if Robots[r].MainBody <> nil then begin
    if Robots[r].MainBody.GLObj<>nil then begin
      FViewer.GLDummyCamPosRel.MoveTo(Robots[r].MainBody.GLObj);
      FViewer.GLDummyTargetCamRel.MoveTo(Robots[r].MainBody.GLObj);
    end;
  end;
end;

procedure TFViewer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FEditor.FormCloseQuery(Sender, CanClose);
  if CanClose then
    FSceneEdit.FormCloseQuery(Sender, CanClose);
end;

procedure TFViewer.ShowOrRestoreForm(Fm: TForm);
begin
  Fm.Show;
  if Fm.WindowState = wsMinimized then
    Fm.WindowState := wsNormal;
end;


procedure TFViewer.MenuChartClick(Sender: TObject);
begin
  ShowOrRestoreForm(FChart);
end;

procedure TFViewer.MenuConfigClick(Sender: TObject);
begin
  ShowOrRestoreForm(FParams);
end;

procedure TFViewer.MenuEditorClick(Sender: TObject);
begin
  ShowOrRestoreForm(FEditor);
end;


procedure TFViewer.MenuSceneClick(Sender: TObject);
begin
  ShowOrRestoreForm(FSceneEdit);
end;

procedure TFViewer.MenuSheetsClick(Sender: TObject);
begin
  ShowOrRestoreForm(FSheets);
end;


procedure TFViewer.FormDestroy(Sender: TObject);
begin
  HUDStrings.Free;
end;

procedure TFViewer.SetTrailCount(NewCount, NodeCount: integer);
var i, OldCount: integer;
    GLLines: TGLLines;
begin
  oldCount := GLDTrails.Count;
  if OldCount = NewCount then exit;
  if OldCount > NewCount then begin
    for i := 1 to OldCount - NewCount do begin
      GLDTrails.Children[NewCount-1].Free;
    end;
  end;
  if OldCount < NewCount then begin
    for i := 1 to NewCount - OldCount  do begin
      GLLines := TGLLines.CreateAsChild(GLDTrails);
      GLLines.LineWidth := 1;
      GLLines.NodesAspect := lnaInvisible;
      GLLines.Tag := NodeCount; //Stores maximum number of nodes
    end;
  end;
end;

procedure TFViewer.AddTrailNode(T: integer; x, y, z: double);
var GLLines: TGLLines;
begin
  GLLines := (GLDTrails.Children[T] as TGLLines);
  GLLines.AddNode(x, y, z);
  while GLLines.Nodes.Count > GLLines.Tag do GLLines.Nodes.Delete(0);
end;


procedure TFViewer.DelTrailNode(T: integer);
begin
  (GLDTrails.Children[T] as TGLLines).Nodes.Delete(0);
end;


procedure TFViewer.MenuSnapshotClick(Sender: TObject);
var GLBitmap: TBitmap;
    JPEGImage: TJPEGImage;  //Requires the "jpeg" unit added to "uses" clause.
begin
  GLBitmap := GLSceneViewer.Buffer.CreateSnapShotBitmap;
  try
    //GLBitmap.SaveToFile('snapshot.bmp');
    JPEGImage := TJPEGImage.Create;
    try
      JPEGImage.Assign(GLBitmap);
      JPEGImage.CompressionQuality := 90;
      JPEGImage.SaveToFile('snapshot.jpg');
    finally
      JPEGImage.free;
    end;
  finally
    GLBitmap.free;
  end;
end;

procedure TFViewer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssctrl in Shift) then begin
    if (key = ord('I')) then MenuSnapshot.Click;
    if (key = ord('G')) then MenuChangeScene.Click;
  end;
end;

procedure TFViewer.MenuChangeSceneClick(Sender: TObject);
begin
  FSceneEdit.MenuChange.Click;
end;

end.


{


procedure TFMain.GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var acttime: Dword;
    dteta,droR,droL,dsR,dsL: double;
    droF,droB,dsF,dsB: double;
    buf:array[0..2047] of char;
    wpole_n: double;
    dsbx,dsby: double;

    vec: TAffineVector;
    tmpx,tmpy:double;

    i: integer;
    RealOdo, OdoNoise, droR_ODO, droL_ODO: double;
begin
  with RobotState, RobotPars, FieldPars do begin
    EditOdo1.Text := inttostr(Odos[1]);
    EditOdo2.Text := inttostr(Odos[2]);
    u[1] := 0;
    u[2] := 0;

    // Line Scanner
    if CBLineScanner.Checked then begin
      with RemState.Robot do begin
        ScannerAngle := strtofloatdef(EditScannerAngle.Text, 15);
        ScannerAngle := max(min(ScannerAngle, 70), 10);
        GLSphereRobotLineSensor.PitchAngle := ScannerAngle;
      end;
      BuildLineScan;
    end;
  end;


  // Log
  if CBLogActive.Checked then with RobotStateLog do begin
    Points[index]:=RobotState;
    inc(PointsCount);
    inc(index);
    if index>=MaxLogPoints then index:=0;
  end;

  // Trail
  TrailSize:=strtointdef(EditTrailSize.Text,TrailSize);
  if (GLLinesXY.Nodes.Count>0) then begin
    if (dist(RobotState.x-GLLinesXY.Nodes[GLLinesXY.Nodes.Count-1].X,
          RobotState.y-GLLinesXY.Nodes[GLLinesXY.Nodes.Count-1].y)>0.05) then
    GLLinesXY.Nodes.AddNode(RobotState.x,RobotState.y,0.001);
  end else
    GLLinesXY.Nodes.AddNode(RobotState.x,RobotState.y,0.001);
  while GLLinesXY.Nodes.Count>TrailSize do GLLinesXY.Nodes.Delete(0);

  acttime:=GetTickcount;
//  Caption := format('SimOne - %d',[acttime-LastCadencerTime]);
//  LastCadencerTime:=ActTime;

  if acttime>LastCadencerTime+1000 then begin
    Caption := format(SimOneVersion + ' (c) Paulo Costa - %.1f (%d)',[GLSceneViewer.FramesPerSecond*1000/(acttime-LastCadencerTime),acttime-LastCadencerTime]);
    GLSceneViewer.ResetPerformanceMonitor;
    LastCadencerTime:=ActTime;
  end;
end;

procedure TFMain.BuildLineScan;
const ScanRes = 64;
var Vo, Vp, V, V1, V2 : TAffineVector;
    iPoint: TVector;
//    iObj : TGLBaseSceneObject;
    i, j, ico, ins: integer;
    b, inside: boolean;
    X,Y: single;
    sens: array[1..ScanRes] of integer;
begin
  //ZeroMemory(@(RemState.Robot.Scanner), sizeof(RemState.Robot.Scanner));
  ZeroMemory(@(sens), sizeof(sens));
  Vo := GLPolygonScanner.LocalToAbsolute(GLPolygonScanner.Nodes.Items[0].AsAffineVector);
  V1 := GLPolygonScanner.LocalToAbsolute(GLPolygonScanner.Nodes.Items[2].AsAffineVector);
  V1 := VectorSubtract(V1, Vo);
  V2 := GLPolygonScanner.LocalToAbsolute(GLPolygonScanner.Nodes.Items[1].AsAffineVector);
  V2 := VectorSubtract(V2, Vo);
  V := VectorSubtract(V2, V1);
  for i:=0 to ScanRes-1 do begin
    Vp := VectorCombine(V1, V, 1, i/ScanRes);
    NormalizeVector(Vp);
    b :=GLPlaneFloor.RayCastIntersect(VectorMake(Vo), VectorMake(Vp), @iPoint, nil);
    if b then begin
      X:=iPoint[0];
      Y:=iPoint[1];
      inside:= false;
      // test if point is inside contours
      for ico:=0 to GLMultiPolygonTrack.Contours.Count -1 do begin
        j := GLMultiPolygonTrack.Contours.Items[ico].Nodes.Count - 1;
        with GLMultiPolygonTrack.Contours.Items[ico] do begin
          for ins:=0 to Nodes.Count -1 do begin
            if ((((Nodes.Items[ins].Y <= Y) and (Y<Nodes.Items[j].Y)) or ((Nodes.Items[j].Y <= Y) and (Y<Nodes.Items[ins].Y)) )
                 and (X<(Nodes.Items[j].X-Nodes.Items[ins].X)*(Y-Nodes.Items[ins].Y)/(Nodes.Items[j].Y-Nodes.Items[ins].Y)+Nodes.Items[ins].X)) then
                inside:=not inside;
             j:=ins;
          end;
          if inside then break;
        end;
      end;
      EditDebug.Text :=  format(' %.2f, %.2f, %.2f',[iPoint[0], iPoint[1], iPoint[2]]);
      //EditDebug.Text :=  iObj.Name + format(' %.2f, %.2f, %.2f',[Vo[0], Vo[1], Vo[2]]);
      if inside then begin
        //RemState.Robot.Scanner[i]:=255;
        sens[i]:=255;
      end else begin
        //RemState.Robot.Scanner[i]:=0;
        sens[i]:=0;
      end;
    end else begin
      //RemState.Robot.Scanner[i]:=0;
      sens[i]:=0;
    end;
  end;


  // Add noise
  for i:=0 to ScanRes-1 do begin
    Sens[i] :=  round(Sens[i] + 10 *randg(0,2));
  end;

  // Filter
  for i:=1 to ScanRes-2 do begin
    RemState.Robot.Scanner[i] := round(max(0,min(255,(sens[i-1] + 2*sens[i] + sens[i+1]) div 4)));
  end;

  // Show In Chart
  Series1.BeginUpdate;
  Series1.Clear;
  for i:=0 to ScanRes-1 do begin
    Series1.AddXY(i,RemState.Robot.Scanner[i]);
  end;
  Series1.EndUpdate;

end;
}


// TODO
// Things scriptable
// Sensores sem ser num robot  (???)
// Zona morta no PID
// Calcular centro de gravidade
// -Passadeiras- (falta controlar a aceleração delas)
// -Thrusters- + turbulence
// alternate globject {For TSolids only now}
// world wind
// Channels
// yasml
// Texture panel
// Scale not
// rotation only in z for the robots
// 3ds offset

// Optional walls ??

// Sensores de linha branca  1
// Sonar 1-n
// Beacons  0
// Receptores de beacons  1-nb
//  digitais
//  analógicos
//  indicandor de direcção
// Bussola       0
// Giroscopios   0
// Acelerometros 0

// Solve color input confusion
// Show scene tree
// Show tags not used

// Quaternions
// charts for the spreadsheet
// multiple spreadsheets
