unit Viewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLWin32Viewer, GLCadencer, ODEImport,
  GLShadowPlane, VectorGeometry, GLGeomObjects, ExtCtrls, ComCtrls,
  GLBitmapFont, GLWindowsFont, keyboard, GLTexture, math, GLSpaceText, Remote,
  GLShadowVolume, GLSkydome, GLGraph, OmniXML, OmniXMLUtils, Contnrs, ODERobots,
  rxPlacemnt, ProjConfig, GLHUDObjects, Menus;

type
  TRGBfloat = record
    r, g, b: single;
  end;

//Physic World ODE
type
  TWorld_ODE = class
    //SampleCount: integer;
    Ode_dt, TimeFactor: double;
    OdeScene: TGLBaseSceneObject;
    world: PdxWorld;
    space: PdxSpace;
    contactgroup: TdJointGroupID;
    default_n_mu: double;

    Environment: TSolid;
    Robots: TRobotList;
    Obstacles: TSolidList;
    Things: TSolidList;

    PickSolid: TSolid;
    PickJoint: TdJointID;
    PickPoint, TargetPickPoint: TVector;
    PickDist: double;
    TestBody: TSolid;

    ground_box : PdxGeom;
    ground_box2 : PdxGeom;

    ODEEnable : boolean;
    physTime : double;
    destructor destroy; override;
    constructor create;
    procedure WorldUpdate;
  private
    procedure CreateSubGeomBox(var body: PdxBody; var geom: PdxGeom; xsize, ysize, zsize, x, y, z: double);
    procedure CreateGlBox(var GLCube: TGLCube; var geom: PdxGeom);

    procedure CreateSolidBox(var Solid: TSolid; bmass, posX, posY, posZ, L, W, H: double);
    procedure CreateSolidCylinder(var Solid: TSolid; cmass, posX, posY, posZ: double; c_radius, c_length: double);
    procedure CreateSolidBall(var Solid: TSolid; bmass, posX, posY, posZ: double; c_radius: double);

    procedure CreateHingeJoint(var Link: TSolidLink; Solid1, Solid2: TSolid;
      anchor_x, anchor_y, anchor_z, axis_x, axis_y, axis_z: double);
    procedure CreateBoxObstacle(var Obstacle: TSolid; sizeX, sizeY, sizeZ, posX, posY, posZ: double);
    procedure CreateShellBox(var Solid: TSolid; motherbody: PdxBody; posX, posY, posZ, L, W, H: double);

    procedure UpdateOdometry(var Link: TSolidLink);
    procedure CreateWheel(Robot: TRobot; Wheel: TWheel; const Pars: TWheelPars; const wFriction: TFriction; const wMotor: TMotor);
    procedure CreateIRSensor(motherbody: PdxBody; IRSensor: TSensor; posX, posY, posZ,
      IR_teta, IR_length, InitialWidth, FinalWidth: double);

    procedure LoadObstaclesFromXML(XMLFile: string);
    procedure LoadIRSensorsFromXML(Robot: TRobot; const root: IXMLNode);
    //procedure LoadHumanoidJointsFromXML(Robot: TRobot; XMLFile: string);
    procedure LoadLinksFromXML(Robot: TRobot; const root: IXMLNode);
    procedure CreateUniversalJoint(var Link: TSolidLink; Solid1,
      Solid2: TSolid; anchor_x, anchor_y, anchor_z, axis_x, axis_y, axis_z,
      axis2_x, axis2_y, axis2_z: double);
    procedure SetUniversalLimits(var Link: TSolidLink; LimitMin, LimitMax,
      Limit2Min, Limit2Max: double);
    procedure SetHingeLimits(var Link: TSolidLink; LimitMin,
      LimitMax: double);
    procedure LoadSolidsFromXML(SolidList: TSolidList; const root: IXMLNode);
//    procedure CreateBall(bmass, radius, posX, posY, posZ: double);
    procedure LoadWheelsFromXML(Robot: TRobot; const root: IXMLNode);
    procedure LoadShellsFromXML(Robot: TRobot;  const root: IXMLNode);
    procedure LoadSceneFromXML(XMLFile: string);
    function LoadRobotFromXML(XMLFile: string): TRobot;
    procedure CreateSliderJoint(var Link: TSolidLink; Solid1,
      Solid2: TSolid; axis_x, axis_y, axis_z: double);
    procedure SetSliderLimits(var Link: TSolidLink; LimitMin,
      LimitMax: double);
    procedure UpdatePickJoint;
    procedure LoadThingsFromXML(XMLFile: string);
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
  private
    { Private declarations }
    OldPick : TGLCustomSceneObject;
    my,mx: integer;
    t_delta: int64;
    KeyVals: TKeyVals;
    procedure UpdateCamPos(CMode: integer);
    procedure FillRemote(r: integer);
    procedure IRSharpNoiseModel(r: integer);
    procedure UpdateGLScene;
    function GLSceneViewerPick(X, Y: Integer): TGLCustomSceneObject;
  public
  end;

procedure ReadFrictionFromXMLNode(var Friction: TFriction; const prop: IXMLNode);
procedure ReadSpringFromXMLNode(var Spring: TSpring; const prop: IXMLNode);
procedure ReadMotorFromXMLNode(var Motor: TMotor; const prop: IXMLNode);

var
  FViewer: TFViewer;
  WorldODE: TWorld_ODE;
  RemControl: TRemControl;
  RemState: TRemState;

implementation

{$R *.dfm}

uses ODEGL, Params, Editor, FastChart, RemoteControl, utils, StdCtrls;



// this is called by dSpaceCollide when two objects in space are
// potentially colliding.

procedure nearCallback (data : pointer; o1, o2 : PdxGeom); cdecl;
var
  i,n : integer;
  b1, b2{, tmp} : PdxBody;
  c : TdJointID;
  contact : array[0..MAX_CONTACTS-1] of TdContact;
  n_mode: cardinal;
  n_mu, n_mu2: double;
  n_fdir1 : TdVector3;
  n_motion1: double;
begin
  //exit;
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if assigned(b1) and assigned(b2) then begin
    //exit without doing anything if the two bodies are connected by a joint
    //if (dAreConnected(b1, b2)<>0) then exit;
    if (dAreConnectedExcluding(b1, b2, dJointTypeContact)<>0) then exit;
  end;
  // do not collide static objects
  if not assigned(b1) and not assigned(b2) then exit;

  n := dCollide(o1, o2, MAX_CONTACTS, contact[0].geom, sizeof(TdContact));
  if (n > 0) then  begin
    //FParams.EditDEbug2.Text := '';
    //if((dGeomGetClass(o1) = dRayClass) or (dGeomGetClass(o2) = dRayClass)) then begin
    if dGeomGetClass(o1) = dRayClass then begin
      if (o1.data <> nil) then begin
        with TSensor(o1.data) do begin
          pos := contact[0].geom.pos;
          measure := min(measure, contact[0].geom.depth);
          has_measure:= true;
        end;
      end;
      //FParams.EditDEbug2.Text := format('%.2f, %.2f %.2f',[contact[0].geom.pos[0], contact[0].geom.pos[1], contact[0].geom.pos[2]]);;
      exit;
    end;
    if dGeomGetClass(o2) = dRayClass then begin
      if (o2.data <> nil) then begin
        with TSensor(o2.data) do begin
          pos := contact[0].geom.pos;
          measure := min(measure, contact[0].geom.depth);
          has_measure:= true;
        end;
      end;
      exit;
    end;

    n_mode := dContactBounce or
              dContactSoftCFM or
              dContactApprox1;
    n_mu := WorldODE.default_n_mu;
    n_mu2 := n_mu;
    n_motion1 := 0;

    if((dGeomGetClass(o1) = dSphereClass) and (dGeomGetClass(o2) <> dPlaneClass)) or
      ((dGeomGetClass(o2) = dSphereClass) and (dGeomGetClass(o1) <> dPlaneClass)) then begin
      //n_mode := 0.9;
      n_mu2 := 0.0;
      n_mu := 0.1;
      zeromemory(@n_fdir1[0], sizeof(n_fdir1));
    end else

    //FParams.EditDebug.Text := format('%d- %.2f %.2f %.2f',[n, n_fdir1[0], n_fdir1[1], n_fdir1[2]]);
    if (o1.data <> nil) and (TSolid(o1.data).kind = skOmniWheel) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1);
        dBodyVectorToWorld(b1, 0, 0, 1, n_fdir1);
        //n_fdir1 := Vector3Cross(contact[0].geom.normal, n_fdir1);
        //dNormalize3(n_fdir1);
        n_mu2 := 2;
        n_mu := 0.001;
    end else if (o2.data <> nil) and (TSolid(o2.data).kind = skOmniWheel) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1);
        dBodyVectorToWorld(b2, 0, 0, 1, n_fdir1);
        //n_fdir1 := Vector3Cross(contact[0].geom.normal, n_fdir1);
        //dNormalize3(n_fdir1);
        n_mu2 := 2;
        n_mu := 0.001;
    end;

    // Conveyor belt case
    if (o1.data <> nil) and (TSolid(o1.data).kind = skMotorBelt) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1 or dContactMotion1);
        dBodyVectorToWorld(b1, 1, 0, 0, n_fdir1);
        n_mu2 := 0.9;
        n_mu := 0.9;
        n_motion1 := TSolid(o1.data).BeltSpeed;
    end else if (o2.data <> nil) and (TSolid(o2.data).kind = skMotorBelt) then begin
        n_mode := n_mode or cardinal(dContactMu2 or dContactFDir1 or dContactMotion1);
        dBodyVectorToWorld(b2, 1, 0, 0, n_fdir1);
        n_mu2 := 0.9;
        n_mu := 0.9;
        n_motion1 := TSolid(o2.data).BeltSpeed;
    end;

    for i := 0 to n-1 do begin
      with contact[i].surface do begin
        mode := n_mode;
        mu := n_mu;
        mu2 := n_mu2;
        contact[i].fdir1 := n_fdir1;

        //soft_cfm := 0.2;
        soft_cfm := 0.001;
        bounce := 0.1;
        bounce_vel := 0.2;
        motion1 := n_motion1;
      end;
      c := dJointCreateContact(WorldODE.world, WorldODE.contactgroup, @contact[i]);
      dJointAttach(c, dGeomGetBody(contact[i].geom.g1), dGeomGetBody(contact[i].geom.g2));
    end;
  end;
end;


function CalcPID(var PID: TMotController; ref, yk: double): double;
var ek, dek, mk: double;
begin
  result := 0;
  if not PID.active then exit;

  ek := ref - yk;
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

//  dBodySetDamping(Solid.Body, 1e-1, 1e-1);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCube));

  PositionSceneObject(Solid.GLObj, Solid.Geom);
  with (Solid.GLObj as TGLCube) do begin
    TagObject := Solid;
    Scale.x := L;
    Scale.y := W;
    Scale.z := H;
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

  dMassSetCylinder(m, 1, 1, c_radius, c_length);
  dMassAdjust(m, cmass);
  dBodySetMass(Solid.Body, @m);

  Solid.Geom := dCreateCylinder(space, c_radius, c_length);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

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
    Material.LibMaterialName := 'LibMaterialBumps';
    //Material.LibMaterialName := 'LibMaterialFeup';
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);

  //PositionSceneObject(Solid.GLObj, Solid.Geom);
  //if Solid.GLObj is TGLCylinder then Solid.GLObj.pitch(90);
end;


procedure TWorld_ODE.CreateSolidBall(var Solid: TSolid; bmass, posX, posY, posZ, c_radius: double);
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

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLSphere));

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

  //dRFromAxisAndAngle (R,0,1,0,0);
  //dBodySetRotation (Solid.Body, R);

  Solid.Geom := dCreateBox(space, L, W, H);
  dGeomSetBody(Solid.Geom, Solid.Body);
  Solid.Geom.data := Solid;

  dGeomSetOffsetPosition(Solid.Geom, posX, posY, posZ);

  Solid.GLObj := TGLSceneObject(ODEScene.AddNewChild(TGLCube));

  //PositionSceneObject(Solid.GLObj, Solid.Geom);
  with (Solid.GLObj as TGLCube) do begin
    TagObject := Solid;
    Scale.x := L;
    Scale.y := W;
    Scale.z := H;
    //Material.FrontProperties.Diffuse.AsWinColor := clyellow;
  end;
  (OdeScene as TGLShadowVolume).Occluders.AddCaster(Solid.GLObj);
end;


procedure TWorld_ODE.CreateIRSensor(motherbody: PdxBody; IRSensor: TSensor; posX, posY, posZ: double; IR_teta, IR_length, InitialWidth, FinalWidth: double);
//var R: TdMatrix3;
begin
  IRSensor.Geom := dCreateRay(space, IR_length);
  dGeomSetBody(IRSensor.Geom, motherbody);
  IRSensor.Geom.data := IRSensor;

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
  dBodySetDamping(PickSolid.Body, 1e-2, 1e-1);
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
    dJointDestroy(PickJoint);
    dBodySetDamping(PickSolid.Body, 0, 0);
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

procedure TWorld_ODE.UpdateOdometry(var Link: TSolidLink);
var rot: double;
begin
  with Link.Axis[0] do begin
    Odo.LastAngle := Odo.Angle;
    Odo.Angle := dJointGetHingeAngle(Link.joint);
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

  if Pars.omni then newTyre.kind := skOmniWheel;

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
procedure TWorld_ODE.LoadSolidsFromXML(SolidList: TSolidList; const root: IXMLNode);
var bone, prop: IXMLNode;
    radius, sizeX, sizeY, sizeZ, posX, posY, posZ, angX, angY, angZ, mass: double;
    BuoyantMass: double;
    colorR, colorG, colorB: double;
    ID: string;
    R, Rx, Ry, Rz, Ryx: TdMatrix3;
    newBone: TSolid;
    descr: string;
    TextureName: string;
    TextureScale: double;
begin
  if root = nil then exit;

  bone := root.FirstChild;
  while bone <> nil do begin
    if pos(bone.NodeName, 'cuboid, cylinder, sphere, belt') <> 0 then begin // 'bone'
      prop := bone.FirstChild;
      // default values
      mass := 1;  ID := '-1';
      BuoyantMass := 0;
      radius := 1;
      sizeX := 1; sizeY := 1; sizeZ := 1;
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;
      TextureName := ''; TextureScale := 1;
      descr := bone.NodeName + inttostr(SolidList.Count);
      while prop <> nil do begin
        if prop.NodeName = 'buoyant' then begin
          BuoyantMass := GetNodeAttrReal(prop, 'mass', BuoyantMass);
        end;
        if prop.NodeName = 'radius' then begin
          radius := GetNodeAttrReal(prop, 'value', radius);
        end;
        if prop.NodeName = 'size' then begin
          sizeX := GetNodeAttrReal(prop, 'x', sizeX);
          sizeY := GetNodeAttrReal(prop, 'y', sizeY);
          sizeZ := GetNodeAttrReal(prop, 'z', sizeZ);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrReal(prop, 'x', angX));
          angY := degToRad(GetNodeAttrReal(prop, 'y', angY));
          angZ := degToRad(GetNodeAttrReal(prop, 'z', angZ));
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        if prop.NodeName = 'mass' then begin
          mass := GetNodeAttrReal(prop, 'value', mass);
        end;
        if prop.NodeName = 'ID' then begin
          ID := GetNodeAttrStr(prop, 'value', ID);
        end;
        if prop.NodeName = 'desc' then begin
          descr := GetNodeAttrStr(prop, 'Eng', descr);
        end;
        if prop.NodeName = 'texture' then begin
          TextureName := GetNodeAttrStr(prop, 'name', TextureName);
          TextureScale := GetNodeAttrReal(prop, 'scale', TextureScale);
        end;
        prop := prop.NextSibling;
      end;

      if ID <> '-1' then begin
        // Create and position the solid
        newBone := TSolid.Create;
        SolidList.Add(newBone);
        newBone.ID := ID;
        newBone.description := descr;
        if (bone.NodeName = 'cuboid') or (bone.NodeName = 'belt') then begin
          CreateSolidBox(newBone, mass, posX, posY, posZ, sizeX, sizeY, sizeZ);
        end else if bone.NodeName = 'sphere' then begin
          CreateSolidBall(newBone, mass, posX, posY, posZ, radius);
        end else if bone.NodeName = 'cylinder' then begin
          CreateSolidCylinder(newBone, mass, posX, posY, posZ, sizeX, sizeZ);
          if TextureName <> '' then begin
            newBone.SetTexture(TextureName, TextureScale); //'LibMaterialFeup'
          end;
        end;
        if bone.NodeName = 'belt' then newBone.kind := skMotorBelt;

        if BuoyantMass <> 0 then dBodySetGravityMode(newBone.Body, 0);
        
        dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
        dRFromAxisAndAngle(Ry, 0, 1, 0, angY);
        dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

        dMULTIPLY0_333(Ryx, Ry, Rx);
        dMULTIPLY0_333(R, Rz, Ryx);

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
procedure TWorld_ODE.LoadLinksFromXML(Robot: TRobot; const root: IXMLNode);
var JointNode, prop: IXMLNode;
    posX, posY, posZ: double;
    axisX, axisY, axisZ: double;
    axis2X, axis2Y, axis2Z: double;
    aGL, DefaGL: TAxisGLPars;
    LimitMin, LimitMax: double;
    Limit2Min, Limit2Max: double;
    LinkBody1, LinkBody2: string;
    LinkType: string;
    colorR, colorG, colorB: double;
    SolidIndex1, SolidIndex2: integer;
    Solid1, Solid2: TSolid;
    i: integer;
    IDvalue: string;
    newLink: TSolidLink;
    newAxis: TAxis;
    Friction, DefFriction: TFriction;
    Spring, DefSpring: TSpring;
    Motor, DefMotor: TMotor;
    descr: string;
begin
  if root = nil then exit;

  // Initialize default parameters
  DefaGL.Radius := -1;
  DefaGL.height:= 0.05;
  DefaGL.Color := RGB(0, 0, $80);

  with DefMotor do begin
    active := true;
    Encoder.PPR := 1000;
    Encoder.NoiseMean := 0;
    Encoder.NoiseStDev := -1;
    Ri := 0.3;
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
      LimitMin:= 0; LimitMax := 0;
      Limit2Min:= 0; Limit2Max := 0;
      LinkBody1 := '-1'; LinkBody2 := '-1';
      LinkType :=' ';
      descr := '';
      IDvalue := '-1';
      colorR := 128/255; colorG := 128/255; colorB := 128/255;
      newLink := nil;
      motor := DefMotor;
      Friction := DefFriction;
      Spring := DefSpring;

      while prop <> nil do begin
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        if prop.NodeName = 'axis' then begin
          axisX := GetNodeAttrReal(prop, 'x', axisX);
          axisY := GetNodeAttrReal(prop, 'y', axisY);
          axisZ := GetNodeAttrReal(prop, 'z', axisZ);
        end;
        if prop.NodeName = 'axis2' then begin
          axis2X := GetNodeAttrReal(prop, 'x', axis2X);
          axis2Y := GetNodeAttrReal(prop, 'y', axis2Y);
          axis2Z := GetNodeAttrReal(prop, 'z', axis2Z);
        end;
        if prop.NodeName = 'limits' then begin
          LimitMin := GetNodeAttrReal(prop, 'Min', LimitMin);
          LimitMax := GetNodeAttrReal(prop, 'Max', LimitMax);
        end;
        if prop.NodeName = 'limits2' then begin
          Limit2Min := GetNodeAttrReal(prop, 'Min', Limit2Min);
          Limit2Max := GetNodeAttrReal(prop, 'Max', Limit2Max);
        end;
        if prop.NodeName = 'connect' then begin
          LinkBody1 := GetNodeAttrStr(prop, 'B1', LinkBody1);
          LinkBody2 := GetNodeAttrStr(prop, 'B2', LinkBody2);
        end;
        //<draw radius='0.01' height='0.1' rgb24='8F0000'/>
        if prop.NodeName = 'draw' then begin
          aGL.Radius := GetNodeAttrReal(prop, 'radius', aGL.Radius);
          aGL.height := GetNodeAttrReal(prop, 'height', aGL.height);
          aGL.Color := StrToIntDef('$'+GetNodeAttrStr(prop, 'rgb24', inttohex(aGL.color,6)), aGL.color);
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        if prop.NodeName = 'type' then begin
          LinkType := GetNodeAttrStr(prop, 'value', LinkType);
        end;
        if prop.NodeName = 'desc' then begin
          descr := GetNodeAttrStr(prop, 'Eng', descr);
        end;
        if prop.NodeName = 'ID' then begin
          IDValue := GetNodeAttrStr(prop, 'value', IDValue);
        end;
        ReadFrictionFromXMLNode(Friction, prop);
        ReadSpringFromXMLNode(Spring, prop);
        ReadMotorFromXMLNode(Motor, prop);
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

          // ID = 0 mean the world TODO: use a bettter name
          if (((SolidIndex1 <> -1) or (LinkBody1 = '0')) and (SolidIndex2 <> -1)) or
             (((SolidIndex2 <> -1) or (LinkBody2 = '0')) and (SolidIndex1 <> -1)) then begin
            if LinkBody1 = '0' then begin
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
              //CreateHingeJoint(newLink, Robot.Solids[SolidIndex1], Robot.Solids[SolidIndex2], posX, posY, posZ, axisX,axisY,axisZ);
              CreateHingeJoint(newLink, Solid1, Solid2, posX, posY, posZ, axisX,axisY,axisZ);
              SetHingeLimits(newLink, LimitMin, LimitMax);
            end;

            if LinkType ='Slider' then begin
              CreateSliderJoint(newLink, Solid1, Solid2, axisX,axisY,axisZ);
              SetSliderLimits(newLink, LimitMin, LimitMax);
            end;

            if LinkType ='Universal' then begin
              CreateUniversalJoint(newLink, Solid1, Solid2, posX, posY, posZ, axisX,axisY,axisZ, axis2X,axis2Y,axis2Z);
              SetUniversalLimits(newLink, LimitMin, LimitMax, Limit2Min, Limit2Max);

              newAxis := TAxis.Create; //TODO Deal with different parameters for each axis
              Robot.Axes.Add(newAxis);
              newAxis.ParentLink := newLink;
              newAxis.Friction := Friction;
              newAxis.Spring := Spring;
              newAxis.Motor := Motor;
              newLink.Axis[1] := newAxis;

              if aGL.Radius > 0 then begin
                newAxis.GLCreate(OdeScene, aGL.Radius, aGL.height);
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
              newAxis.GLSetPosition;
              newAxis.GLObj.TagObject := newAxis;
            end;

            if newLink <> nil then begin
              newLink.ID := IDValue;
              newLink.description := descr;
            end;
          end;
        end;
      end;

    end;

    JointNode := JointNode.NextSibling;
  end;

end;

function LoadXML(XMLFile: string): IXMLDocument;
var XML: IXMLDocument;
begin
  result := nil;
  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason<>'' then begin
    with FParams.MemoDebug.Lines do begin
      Add('XML file error:' + XMLFile);
      Add(XML.ParseError.Reason);
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
  XML := LoadXML(XMLFile);
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




procedure TWorld_ODE.LoadObstaclesFromXML(XMLFile: string);
var XML: IXMLDocument;
    root, obstacle, prop: IXMLNode;
    sizeX, sizeY, sizeZ, posX, posY, posZ, angX, angY, angZ: double;
    colorR, colorG, colorB: double;
    R, Rx, Ry, Rz, Ryx: TdMatrix3;
    NewObstacle: TSolid;
begin
{  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason<>'' then begin
    exit;
  end;}
  XML := LoadXML(XMLFile);
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
      while prop <> nil do begin
        if prop.NodeName = 'size' then begin
          sizeX := GetNodeAttrReal(prop, 'x', sizeX);
          sizeY := GetNodeAttrReal(prop, 'y', sizeY);
          sizeZ := GetNodeAttrReal(prop, 'z', sizeZ);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrReal(prop, 'x', angX));
          angY := degToRad(GetNodeAttrReal(prop, 'y', angY));
          angZ := degToRad(GetNodeAttrReal(prop, 'z', angZ));
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        prop := prop.NextSibling;
      end;
      // Create and position the obstacle
      NewObstacle := TSolid.Create;
      Obstacles.Add(NewObstacle);
      NewObstacle.description := format('Obstacle at (%.1f, %.1f, %.1f)',[posX, posY, posZ]);
      CreateBoxObstacle(NewObstacle, sizeX, sizeY, sizeZ, posX, posY, posZ);

      dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
      dRFromAxisAndAngle(Ry, 0, 1, 0, angY);
      dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

      dMULTIPLY0_333(Ryx, Ry, Rx);
      dMULTIPLY0_333(R, Rz, Ryx);

      dGeomSetRotation(NewObstacle.Geom, R);
      NewObstacle.SetColor(colorR, colorG, colorB);
      PositionSceneObject(NewObstacle.GLObj, NewObstacle.Geom);
    end;

    obstacle := obstacle.NextSibling;
  end;

end;


procedure TWorld_ODE.LoadIRSensorsFromXML(Robot: TRobot; const root: IXMLNode);
var sensor, prop: IXMLNode;
    SLen, SInitialWidth, SFinalWidth, posX, posY, posZ, angX, angY, angZ: double;
    Noise: TSensorNoise;
    colorR, colorG, colorB: double;
    R, Rx, Ry, Rz, Ryx: TdMatrix3;
    newIRSensor: TSensor;

    MotherSolidId: string;
    MotherSolid: TSolid;
    SolidIdx: integer;
begin
  if root = nil then exit;

  sensor := root.FirstChild;
  while sensor <> nil do begin
    if (sensor.NodeName = 'IR') or (sensor.NodeName = 'IRSharp') then begin
      // default values
      MotherSolidId := '';
      SLen := 0.8; SInitialWidth := 0.01; SFinalWidth := 0.015;
      with Noise do begin
        var_k := 0; var_d := 0; offset := 0; gain := 1; active := false;
      end;
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;

      prop := sensor.FirstChild;
      while prop <> nil do begin
        if prop.NodeName = 'solid' then begin
          MotherSolidId := GetNodeAttrStr(prop, 'id', MotherSolidId);
        end;
        if prop.NodeName = 'beam' then begin
          SLen := GetNodeAttrReal(prop, 'length', SLen);
          SInitialWidth := GetNodeAttrReal(prop, 'initial_width', SInitialWidth);
          SFinalWidth := GetNodeAttrReal(prop, 'final_width', SFinalWidth);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrReal(prop, 'x', angX));
          angY := degToRad(GetNodeAttrReal(prop, 'y', angY));
          angZ := degToRad(GetNodeAttrReal(prop, 'z', angZ));
        end;
        if prop.NodeName = 'noise' then with Noise do begin
          active := true;  // if the tag 'noise' is present then it is active
          var_k := GetNodeAttrReal(prop, 'var_k', var_k);
          var_d := GetNodeAttrReal(prop, 'var_d', var_d);
          offset := GetNodeAttrReal(prop, 'offset', offset);
          gain := GetNodeAttrReal(prop, 'gain', gain);
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        prop := prop.NextSibling;
      end;
      // Create and position the sensor
      newIRSensor := TSensor.Create;
      newIRSensor.kind := skIR;
      if sensor.NodeName = 'IRSharp' then newIRSensor.kind := skIRSharp;
      newIRSensor.Noise := Noise;
      Robot.IRSensors.Add(newIRSensor);

      SolidIdx := Robot.Solids.IndexFromID(MotherSolidId);
      if SolidIdx = -1 then  begin
        MotherSolid := Robot.MainBody;
      end else begin
        MotherSolid := Robot.Solids[SolidIdx];
      end;
      CreateIRSensor(MotherSolid.Body, newIRSensor, posX, posY, posZ, angZ, SLen, SInitialWidth, SFinalWidth);

      dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
      dRFromAxisAndAngle(Ry, 0, 1, 0, angY + pi/2);
      dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

      dMULTIPLY0_333(Ryx, Ry, Rx);
      dMULTIPLY0_333(R, Rz, Ryx);

      dGeomSetOffsetRotation(newIRSensor.Geom, R);
      dGeomSetOffsetPosition(newIRSensor.Geom, posX, posY, posZ);

      newIRSensor.SetColor(colorR, colorG, colorB);
    end;

    sensor := sensor.NextSibling;
  end;

end;

procedure ReadFrictionFromXMLNode(var Friction: TFriction; const prop: IXMLNode);
begin
  with Friction do begin
    if prop.NodeName = 'friction' then begin
      Bv := GetNodeAttrReal(prop, 'bv', Bv);
      Fc := GetNodeAttrReal(prop, 'fc', Fc);
      CoulombLimit := GetNodeAttrReal(prop, 'coulomblimit', CoulombLimit);
    end;
  end;
end;

procedure ReadSpringFromXMLNode(var Spring: TSpring; const prop: IXMLNode);
begin
  with Spring do begin
    if prop.NodeName = 'spring' then begin
      K := GetNodeAttrReal(prop, 'k', K);
      ZeroPos := degtorad(GetNodeAttrReal(prop, 'zeropos', ZeroPos));
    end;
  end;
end;


procedure ReadMotorFromXMLNode(var Motor: TMotor; const prop: IXMLNode);
var str: string;
begin
  with Motor do begin
    if prop.NodeName = 'motor' then begin
      Ri := GetNodeAttrReal(prop, 'ri', Ri);
      Ki := GetNodeAttrReal(prop, 'ki', Ki);
      Vmax := GetNodeAttrReal(prop, 'vmax', Vmax);
      Imax := GetNodeAttrReal(prop, 'imax', Imax);
      active := GetNodeAttrBool(prop, 'active', active);
    end;
    if prop.NodeName = 'gear' then begin
      GearRatio := GetNodeAttrReal(prop, 'ratio', GearRatio);
    end;
    if prop.NodeName = 'encoder' then begin
      Encoder.PPR := GetNodeAttrInt(prop, 'ppr', Encoder.PPR);
      Encoder.NoiseMean := GetNodeAttrReal(prop, 'mean', Encoder.NoiseMean);
      Encoder.NoiseStDev := GetNodeAttrReal(prop, 'stdev', Encoder.NoiseStDev);
    end;

    if prop.NodeName = 'controller' then begin
      with Controller do begin
        Kp := GetNodeAttrReal(prop, 'kp', Kp);
        Ki := GetNodeAttrReal(prop, 'ki', Ki);
        Kd := GetNodeAttrReal(prop, 'kd', Kd);
        Kf := GetNodeAttrReal(prop, 'kf', Kf);
        Y_sat := GetNodeAttrReal(prop, 'ysat', Y_sat);
        controlPeriod := GetNodeAttrInt(prop, 'period', controlPeriod);
        str := GetNodeAttrStr(prop, 'mode', 'pidspeed');
        if str = 'pidspeed' then ControlMode := cmPIDSpeed
        else if str = 'pidposition' then ControlMode := cmPIDPosition
        else if str = 'state' then ControlMode := cmState;
        active := GetNodeAttrBool(prop, 'active', active);
      end;
    end;
  end;
end;

procedure TWorld_ODE.LoadWheelsFromXML(Robot: TRobot; const root: IXMLNode);
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
    Omni := false;
  end;

  with DefMotor do begin
    active := true;
    Encoder.PPR := 1000;
    Encoder.NoiseMean := 0;
    Encoder.NoiseStDev := -1;
    Ri := 0.3;
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
          Pars.mass := GetNodeAttrReal(prop, 'mass', Pars.mass);
          Pars.Radius := GetNodeAttrReal(prop, 'radius', Pars.Radius);
          Pars.Width := GetNodeAttrReal(prop, 'width', Pars.Width);
          Pars.CenterDist := GetNodeAttrReal(prop, 'centerdist', Pars.CenterDist);
        end;

        ReadFrictionFromXMLNode(Friction, prop);

        ReadMotorFromXMLNode(Motor, prop);

        if prop.NodeName = 'offset' then begin
          Pars.offsetX := GetNodeAttrReal(prop, 'x', Pars.offsetX);
          Pars.offsetY := GetNodeAttrReal(prop, 'y', Pars.offsetY);
          Pars.offsetZ := GetNodeAttrReal(prop, 'z', Pars.offsetZ);
        end;

        if prop.NodeName = 'axis' then begin
          angX := degToRad(GetNodeAttrReal(prop, 'x', angX));
          angY := degToRad(GetNodeAttrReal(prop, 'y', angY));
          angZ := degToRad(GetNodeAttrReal(prop, 'angle', angZ));
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
      end;

    end;

    wheelnode := wheelnode.NextSibling;
  end;

end;


procedure TWorld_ODE.LoadShellsFromXML(Robot: TRobot; const root: IXMLNode);
var ShellNode, prop: IXMLNode;
    sizeX, sizeY, sizeZ, posX, posY, posZ, angX, angY, angZ: double;
    colorR, colorG, colorB: double;
    R, Rx, Ry, Rz, Ryx: TdMatrix3;
    newShell: TSolid;
    MotherSolidId: string;
    MotherSolid: TSolid;
    SolidIdx: integer;
begin
  if root = nil then exit;

  ShellNode := root.FirstChild;
  while ShellNode <> nil do begin
    if ShellNode.NodeName = 'cuboid' then begin
      prop := ShellNode.FirstChild;
      // default values
      sizeX := 1; sizeY := 1; sizeZ := 1;
      posX := 0; posY := 0; posZ := 0;
      angX := 0; angY := 0; angZ := 0;
      colorR := 128/255; colorG := 128/255; colorB := 128/255;
      MotherSolidId := '';
      while prop <> nil do begin
        if prop.NodeName = 'solid' then begin
          MotherSolidId := GetNodeAttrStr(prop, 'id', MotherSolidId);
        end;
        if prop.NodeName = 'size' then begin
          sizeX := GetNodeAttrReal(prop, 'x', sizeX);
          sizeY := GetNodeAttrReal(prop, 'y', sizeY);
          sizeZ := GetNodeAttrReal(prop, 'z', sizeZ);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrReal(prop, 'x', angX));
          angY := degToRad(GetNodeAttrReal(prop, 'y', angY));
          angZ := degToRad(GetNodeAttrReal(prop, 'z', angZ));
        end;
        if prop.NodeName = 'color_rgb' then begin
          colorR := GetNodeAttrInt(prop, 'r', 128)/255;
          colorG := GetNodeAttrInt(prop, 'g', 128)/255;
          colorB := GetNodeAttrInt(prop, 'b', 128)/255;
        end;
        prop := prop.NextSibling;
      end;
      // Create and position the obstacle
      newShell := TSolid.Create;
      Robot.Shells.Add(newShell);
      newShell.description := 'Shell';

      SolidIdx := Robot.Solids.IndexFromID(MotherSolidId);
      if SolidIdx = -1 then  begin
        MotherSolid := Robot.MainBody;
      end else begin
        MotherSolid := Robot.Solids[SolidIdx];
      end;
      CreateShellBox(newShell, MotherSolid.Body, posX, posY, posZ, sizeX, sizeY, sizeZ);

      dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
      dRFromAxisAndAngle(Ry, 0, 1, 0, angY);
      dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

      dMULTIPLY0_333(Ryx, Ry, Rx);
      dMULTIPLY0_333(R, Rz, Ryx);

      dGeomSetOffsetRotation(newShell.Geom, R);

      with newShell.GLObj as TGLCube do begin
        Material.FrontProperties.Diffuse.SetColor(colorR, colorG, colorB);
      end;
      PositionSceneObject(newShell.GLObj, newShell.Geom);
    end;

    ShellNode := ShellNode.NextSibling;
  end;

end;


procedure TWorld_ODE.LoadThingsFromXML(XMLFile: string);
var XML: IXMLDocument;
    root: IXMLNode;
begin
  XML := LoadXML(XMLFile);
  if XML = nil then exit;


  root:=XML.SelectSingleNode('/things');
  if root = nil then exit;
  LoadSolidsFromXML(Things, root);
end;


procedure TWorld_ODE.LoadSceneFromXML(XMLFile: string);
var XML: IXMLDocument;
    root, objNode, prop: IXMLNode;
    posX, posY, posZ, angX, angY, angZ: double;
    newRobot: TRobot;
    thing: TSolid;
    name, filename: string;
    mass, radius: double;
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
  XML := LoadXML(XMLFile);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/scene');
  if root = nil then exit;

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
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        if prop.NodeName = 'rot_deg' then begin
          angX := degToRad(GetNodeAttrReal(prop, 'x', angX));
          angY := degToRad(GetNodeAttrReal(prop, 'y', angY));
          angZ := degToRad(GetNodeAttrReal(prop, 'z', angZ));
        end;
        prop := prop.NextSibling;
      end;

      //if filename <> '' then begin
      if fileexists(filename) then begin
        newRobot := LoadRobotFromXML(filename);
        if newRobot <> nil then begin
          newRobot.Name := name;
          newRobot.SetXYZTeta(posX, posY, posZ, angZ);
        end;
      end;

    end else if objNode.NodeName = 'obstacles' then begin
      // Create static obstacles
      filename := GetNodeAttrStr(objNode, 'file', filename);
      if fileexists(filename) then begin
        LoadObstaclesFromXML(filename);
      end;

    end else if objNode.NodeName = 'things' then begin
      // Create things
      filename := GetNodeAttrStr(objNode, 'file', filename);
      if fileexists(filename) then begin
        LoadThingsFromXML(filename);
      end;


    end else if objNode.NodeName = 'ball' then begin
      prop := objNode.FirstChild;
      // default values
      radius := -1; mass := -1;
      posX := 0; posY := 0; posZ := 0;

      while prop <> nil do begin

        if prop.NodeName = 'radius' then begin
          radius := GetNodeAttrReal(prop, 'value', radius);
        end;
        if prop.NodeName = 'mass' then begin
          mass := GetNodeAttrReal(prop, 'value', mass);
        end;
        if prop.NodeName = 'pos' then begin
          posX := GetNodeAttrReal(prop, 'x', posX);
          posY := GetNodeAttrReal(prop, 'y', posY);
          posZ := GetNodeAttrReal(prop, 'z', posZ);
        end;
        prop := prop.NextSibling;

      end;
      if (mass > 0) and (radius > 0) then begin
        Thing := TSolid.Create;
        Things.Add(Thing);
        Thing.description := 'Ball';
        CreateSolidBall(Thing, mass, posX, posY, posZ, radius);
      end;
    end;

    objNode := objNode.NextSibling;
  end;

end;


function TWorld_ODE.LoadRobotFromXML(XMLFile: string): TRobot;
var XML: IXMLDocument;
    root, objNode, prop: IXMLNode;
    newRobot: TRobot;
    str: string;
begin
  result := nil;
{  XML:=CreateXMLDoc;
  XML.Load(XMLFile);
  if XML.ParseError.Reason<>'' then begin
    with FParams.MemoDebug.Lines do begin
      Add('XML file error:' + XMLFile);
      Add(XML.ParseError.Reason);
    end;
    exit;
  end;}
  XML := LoadXML(XMLFile);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/robot');
  if root = nil then exit;

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

    if objNode.NodeName = 'solids' then begin
      LoadSolidsFromXML(newRobot.Solids, objNode);
      if newRobot.Solids.Count = 0 then exit;
      newRobot.MainBody := newRobot.Solids[0];
    end;

    if objNode.NodeName = 'wheels' then begin
      LoadWheelsFromXML(newRobot, objNode);
    end;

    if objNode.NodeName = 'shells' then begin
      LoadShellsFromXML(newRobot, objNode);
    end;

    if objNode.NodeName = 'sensors' then begin
      LoadIRSensorsFromXML(newRobot, objNode);
    end;

    if objNode.NodeName = 'articulations' then begin
      LoadLinksFromXML(newRobot, objNode);
    end;

    objNode := objNode.NextSibling;
  end;
  result := newRobot;
end;


constructor TWorld_ODE.create;
begin
  default_n_mu := 0.95;
  Ode_dt := 1/1000;
  TimeFactor := 1;
  ODEEnable := False;
  PhysTime := 0;
  OdeScene := FViewer.GLShadowVolume;

  Environment := TSolid.Create;
  Environment.Body := nil;

  Robots := TRobotList.Create;
  Obstacles := TSolidList.Create;
  Things := TSolidList.Create;


  //Create physic
  world := dWorldCreate();
//  dWorldSetQuickStepNumIterations(world, 10);
  dWorldSetQuickStepNumIterations(world, 10);
  space := dHashSpaceCreate(nil);
  //dHashSpaceSetLevels(space, -4, 1);
  contactgroup := dJointGroupCreate(0);
  dWorldSetGravity(world, 0, 0, -9.81);

//  dWorldSetCFM(world, 1e-5);
  dWorldSetCFM(world, 1e-5);
  dWorldSetERP(world, 0.1);

  //Floor
  dCreatePlane(space, 0, 0, 1, 0);
  //Box wall limit
  dCreatePlane(space,  0, 1, 0, -10.00);
  dCreatePlane(space,  1, 0, 0, -10.00);
  dCreatePlane(space,  0,-1, 0, -10.00);
  dCreatePlane(space, -1, 0, 0, -10.00);

//  FViewer.GLMaterialLibrary;

  LoadSceneFromXML('scene.xml');
  SetCameraTarget(0);

{  if Robots[0] <> nil then begin
    if Robots[0].MainBody.GLObj<>nil then begin
      FViewer.GLDummyCamPosRel.MoveTo(Robots[0].MainBody.GLObj);
      FViewer.GLDummyTargetCamRel.MoveTo(Robots[0].MainBody.GLObj);
    end;
  end;}

//  if ball_geom = nil then
//    FViewer.GLSphere_ball.Visible := false;

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
  inherited;
end;

procedure TWorld_ODE.WorldUpdate;
begin
  //Update the physic
  dSpaceCollide(space, nil, nearCallback);
  dWorldQuickStep (world, Ode_dt);
  //dWorldStep (world, Ode_dt);
  physTime := physTime + Ode_dt;

  // remove all contact joints
  dJointGroupEmpty (contactgroup);
end;


procedure TFViewer.FormCreate(Sender: TObject);
begin
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
  //GLCadencer.enabled := true;

  GLHUDTextObjName.Text := '';
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
var vs, CamPos, hitPoint: TVector;
    hit: boolean;
begin
  if [ssleft, ssCtrl] <= shift then begin
    GLScene.CurrentGLCamera.MoveAroundTarget(my-y,mx-x);
    GLDummyCamPos.Position := GLScene.CurrentGLCamera.Position;
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
      if Assigned(OldPick) then OldPick.Material.FrontProperties.Emission.Color:=clrBlack;
      pick.Material.FrontProperties.Emission.Color:=clrRed;
      OldPick := Pick;
    end else begin
      GLHUDTextObjName.Text := '';
      if Assigned(OldPick) then OldPick.Material.FrontProperties.Emission.Color:=clrBlack;
      OldPick := nil;
    end;
    if pick.TagObject is TAxis then begin
      GLHUDTextObjName.Text := TAxis(pick.TagObject).ParentLink.description;
    end;
  end;
  result := pick;
end;

procedure AxisTorqueModel(axis: TAxis; Theta, w: double; var T: double);
var duty, Tq, old_Im: double;
    max_delta_Im: double;
begin
  max_delta_Im := 0.15; // A/ms
  with Axis do begin
    // If active use PID controller
    if Motor.active then begin
      with Motor.Controller do begin
        if active then begin
          inc(ticks);
          if ticks >= ControlPeriod then begin
            case ControlMode of
              cmPIDPosition: begin
                ref.volts := CalcPID(Motor.Controller, ref.theta, theta);
              end;

              cmPIDSpeed: begin
                //ref.volts := CalcPID(Motor.Controller, ref.w, w);
                ref.volts := CalcPID(Motor.Controller, ref.w, filt_speed);
              end;

              cmState: begin
                //ref.volts := CalcPD(Motor.Controller, ref.theta, ref.w, theta, w);
                ref.volts := CalcPD(Motor.Controller, ref.theta, ref.w, theta, filt_speed);
              end;
            end;
            ticks := 0;
          end;
        end;
      end;

      // Voltage with saturation
      Motor.voltage := max(-Motor.Vmax, min(Motor.Vmax, ref.volts));
      // Motor Model
      if Motor.Vmax <> 0 then begin
        duty := abs(Motor.voltage / Motor.Vmax);
      end else begin
        duty := 0;
      end;

      old_Im := Motor.Im;
      if Motor.Ri <> 0 then begin
        Motor.Im := (Motor.voltage - w * Motor.GearRatio * Motor.Ki) / Motor.Ri;
      end else begin
        Motor.Im := Motor.Imax;
      end;

      if Motor.Im > old_Im + max_delta_Im then Motor.Im := old_Im + max_delta_Im;
      if Motor.Im < old_Im - max_delta_Im then Motor.Im := old_Im - max_delta_Im;

      if abs(Motor.voltage)>1e-3 then begin
        //iw := max(-Pars.Imax , min(Pars.Imax , iw));
        // this limit is dependent on the current sensor placement
        // Here is assumed that it is only active on the "on" time of the PWM
        Motor.Im := max(-Motor.Imax / duty, min(Motor.Imax / duty, Motor.Im));
        //Motor.Im := max(-Motor.Imax, min(Motor.Imax, Motor.Im));
      end;
    end else begin
      Motor.Im := 0;
    end;
    Motor.PowerDrain := Motor.Im * Motor.voltage * WorldODE.Ode_dt;
    if Motor.PowerDrain > 0 then begin
      Motor.EnergyDrain := Motor.EnergyDrain + Motor.PowerDrain;
    end;
    // coulomb friction
    Tq := Friction.Fc * sign(w);
    // Limit it to avoid instability
    if Friction.CoulombLimit >= 0 then
      Tq := max(-Friction.CoulombLimit * abs(w), min(Friction.CoulombLimit * abs(w), Tq));
    //T := Motor.Im * Motor.Ki * Motor.GearRatio - Friction.Bv * w - Tq - Spring.K * diffangle(Theta, Spring.ZeroPos);
    T := Motor.Im * Motor.Ki * Motor.GearRatio - Friction.Bv * w - Tq - Spring.K * (Theta - Spring.ZeroPos);
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

procedure TFViewer.IRSharpNoiseModel(r: integer);
var i: integer;
    v, iv, var_v, m, var_dist: double;
begin
  with WorldODE.Robots[r] do begin
    for i := 0 to IRSensors.Count - 1 do begin
      with WorldODE.Robots[r].IRSensors[i] do begin
        if kind <> skIRSharp then continue;
        if not has_measure then continue;
        if not Noise.active then continue;

        //var_dist := 0;
        iv := Noise.gain * measure + Noise.offset;
        if iv <> 0 then
          v := 1 / iv
        else continue;
        var_v := Noise.var_d * measure + Noise.var_k;
        m := - Noise.gain / sqr(iv);
        if m <> 0 then
          var_dist := var_v / sqr(m)
        else continue;

        measure := measure + RandG(0, sqrt(var_dist));

      end;
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
    for i:=0 to min(IRSensors.Count, MaxRemIrSensors)-1 do begin
      if IRSensors[i].has_measure then begin
        Robot.IRSensors[i] := IRSensors[i].measure;
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
    for r := 0 to WorldODE.Robots.Count-1 do begin
      with WorldODE.Robots[r] do begin
        // Shells
        for i := 0 to Shells.Count-1 do begin
          if Shells[i].GLObj = nil then continue;
          PositionSceneObject(Shells[i].GLObj, Shells[i].Geom);
          if Shells[i].GLObj is TGLCylinder then Shells[i].GLObj.pitch(90);
        end;
        //IRSensors
        for i := 0 to IRSensors.Count-1 do begin
          if IRSensors[i].GLObj = nil then continue;
          PositionSceneObject(IRSensors[i].GLObj, IRSensors[i].Geom);
          if IRSensors[i].GLObj is TGLCylinder then IRSensors[i].GLObj.pitch(90);
        end;

        // Solids
        for i := 0 to Solids.Count-1 do begin
          if Solids[i].GLObj = nil then continue;
          PositionSceneObject(Solids[i].GLObj, Solids[i].Geom);
          if Solids[i].GLObj is TGLCylinder then Solids[i].GLObj.pitch(90);
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
    t_start, t_end: int64;
    v1, v2: TdVector3;
    txt: string;
    vs: TVector;
    Curpos: TPoint;
    newFixedTime: double;
begin
  GLScene.CurrentGLCamera.Position := GLDummyCamPos.Position;
  if WorldODE.ODEEnable <> False then begin
    QueryPerformanceCounter(t_start);

    // while WorldODE.physTime < newtime do begin
    newFixedTime := WorldODE.physTime + GLCadencer.FixedDeltaTime * WorldODE.TimeFactor;
    while WorldODE.physTime < newFixedTime do begin

      {if Focused then begin
        if IsKeyDown('r') then begin
          if WorldODE.ball_body <> nil then begin
            dBodySetPosition(WorldODE.ball_body, 0, 0 ,1.2);
            dBodySetLinearVel(WorldODE.ball_body, 0, 0 , 0);
          end;
        end;
      end;}

      // Higher level controller (subsampled)
      for r := 0 to WorldODE.Robots.Count-1 do begin
        with WorldODE.Robots[r] do begin
          SecondsCount := SecondsCount + WorldODE.Ode_dt;
          if SecondsCount > DecPeriod then begin
            SecondsCount := SecondsCount - DecPeriod;

            if Focused then begin
              // Keyboard injection defaults to zero
              zeroMemory(@KeyVals, sizeof(KeyVals));
              ReadKeyVals(KeyVals);
            end;

            // Read Odometry
            for i := 0 to Wheels.Count-1 do begin
              WorldODE.UpdateOdometry(Wheels[i].axle);
            end;

            // Fill remote IR sensors noise
            IRSharpNoiseModel(r);

            // Fill RemState
            if r = Fparams.LBRobots.ItemIndex then begin
              //UDP.RemoteHost:=EditIP.text;
              FillRemote(r);
              FParams.ShowRobotRemState(r);
            end;

            // Default Control values are zero
            for i := 0 to Wheels.Count-1 do begin
              Wheels[i].Axle.Axis[0].ref.volts := 0;
              Wheels[i].Axle.Axis[0].ref.w := 0;
            end;

            // Call the selected Decision System
            if Fparams.RGControlBlock.ItemIndex = 0 then begin
              if r = Fparams.LBRobots.ItemIndex then begin
                for i := 0 to WorldODE.Robots[r].Wheels.Count-1 do begin
                  RemControl.u[i] := Keyvals[i] * Wheels[i].Axle.Axis[0].Motor.Vmax;
                  RemControl.Wref[i] := Keyvals[i]*30; //TODO: angular speed constant
                end;
              end;
            end else if Fparams.RGControlBlock.ItemIndex = 1 then begin  // Script controller
              FEditor.RunOnce;
            end else if Fparams.RGControlBlock.ItemIndex = 2 then begin  // LAN controller
              Fparams.UDPServer.SendBuffer(Fparams.EditRemoteIP.Text, 9801, RemState, sizeof(RemState));
            end;
            //Sleep(1);

            // Copy Remote Control values to Wheel Structs
            for i := 0 to Wheels.Count-1 do begin
              Wheels[i].Axle.Axis[0].ref.volts := RemControl.u[i];
              Wheels[i].Axle.Axis[0].ref.w := RemControl.Wref[i];
              //FParams.Edit4.Text := inttostr(round(WorldODE.Robots[r].Wheels[i].Axle.Axis.ref.volts));
            end;

            // Default Remote Control values is zero
            ZeroMemory(@RemControl,sizeof(RemControl));

            Fparams.ShowRobotState;


            //FChart.AddPoint(WorldODE.physTime, RemControl.Vref[1], WorldODE.Robots[0].Wheels[0].Axle.Axis.Motor.Im,
             //                dJointGetHingeAngleRate(WorldODE.Robots[0].Wheels[0].Axle.joint)  );
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
            AxisTorqueModel(Axes[i], Theta, w, Tq);
            // Apply it to the axis
            Axes[i].AddTorque(Tq);
          end;
        end;

        // Robot Main Body extra Frictions
        if (WorldODE.Robots[r].MainBody <> nil) and
           (WorldODE.Robots[r].MainBody.Body <> nil) then begin
          // Robot Body Linear Friction
          v2 := Vector3ScalarMul(WorldODE.Robots[r].MainBody.Body.lvel, -1e-2);
          dBodyAddForce(WorldODE.Robots[r].MainBody.Body, v2[0], v2[1], v2[2]);

          // Robot Body Angular Friction
          v1 := Vector3ScalarMul(WorldODE.Robots[r].MainBody.Body.avel, -1e-2);
          dBodyAddTorque(WorldODE.Robots[r].MainBody.Body, v1[0], v1[1], v1[2]);
        end;

        // Reset IR sensors
        for i:=0 to WorldODE.Robots[r].IRSensors.Count-1 do begin
          WorldODE.Robots[r].IRSensors[i].measure := 1e6;
          WorldODE.Robots[r].IRSensors[i].has_measure := false;
        end;
      end;

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

      WorldODE.WorldUpdate;

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
    FParams.EditDebug.text := format('%.2f',[(t_end - t_start)/t_delta]);

    // GLScene

    //Update all GLscene Parts.
    UpdateGLScene;

    // Update Camera Position
   { if WorldODE.PickSolid <> nil then begin
      GLDummyCamPosRel.MoveTo(GLShadowVolume);
      GLDummyTargetCamRel.MoveTo(GLShadowVolume);
    end;}

    UpdateCamPos(FParams.RGCamera.ItemIndex);

    with FParams do begin
      EditCamX.Text:=format('%.2f',[GLCamera.Position.x]);
      EditCamY.Text:=format('%.2f',[GLCamera.Position.y]);
      EditCamZ.Text:=format('%.2f',[GLCamera.Position.z]);
    end;

    GLSceneViewer.Invalidate;
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

procedure TFViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Execute Destroy Physics
  GLCadencer.Enabled := False;
  WorldODE.ODEEnable := False;
  WorldODE.destroy;
end;

procedure TFViewer.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //Mouse wheel zoom + -
  GLScene.CurrentGLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  GLDummyCamPos.Position := GLScene.CurrentGLCamera.Position;
  UpdateCamPos(FParams.RGCamera.ItemIndex);
end;

procedure TFViewer.FormShow(Sender: TObject);
begin
  FParams.show;
  FEditor.show;
  FChart.show;
  //FLog.show;
  //FRemoteControl.show;

  MakeFullyVisible();
  UpdateGLScene;

  GLCadencer.enabled := true;
end;

procedure TFViewer.TimerTimer(Sender: TObject);
var fps: double;
begin
  fps := GLSceneViewer.FramesPerSecond;
  GLSceneViewer.ResetPerformanceMonitor;
  Caption:=Format('SimTwo - v0.9 (%.1f FPS)', [fps]);
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
end;


procedure TFViewer.MenuChartClick(Sender: TObject);
begin
  FChart.show;
end;

procedure TFViewer.MenuConfigClick(Sender: TObject);
begin
  FParams.Show;
end;

procedure TFViewer.MenuEditorClick(Sender: TObject);
begin
  FEditor.Show;
end;

end.



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


// TODO
// trails  (2D( height), 3D (offset), length, color, tolerance)
// Things.xml e scriptable
// Sensores sem ser num robot
// Zona morta no PID
// PDI para controlar o robot
// alternate globject
// Calcular centro de gravidade
// -Passadeiras- (falta controlar a aceleração delas, falta uma textura ou tecnica que indique o movimento)
// Controlo integral na realimentaçao de estado
// Materiais { surface }
// Surfaces
