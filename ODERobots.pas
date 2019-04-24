unit ODERobots;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, {GLMisc,} GLLCLViewer, ODEImport, OpenGL1x,
  GLVectorGeometry, GLGeomObjects, ExtCtrls, ComCtrls, GLTexture, GLGraphics,
  keyboard, math, GLMaterial;

const
  //ODE world constants
  MAX_CONTACTS = 7;

  MaxJointSamples = 256;
  MaxKeyVals = 8;
  MaxAxis = 3;

const MaxDim = 8;

{const
  skDefault   = 0;
  skOmniWheel = 1;
  skMotorBelt = 2;}
type
  TSolidKind = (skDefault, skOmniWheel, skOmniSurface, skMotorBelt, skPropeller, skFloor, skWall);

type
  TControlMode = (cmPIDPosition, cmPIDSpeed, cmState);
const
  ControlModeNames: array[TControlMode] of string = ('PIDPosition', 'PIDSpeed', 'State');

type

  TFriction = record
    Bv, Fc: double;
//    CoulombLimit: double;
  end;

  TSpring = record
    K, ZeroPos: double;
  end;

  TMotController = record
    Ki, Kd, Kp, Kf: double;
    Sek, ek_1: double;
    y_sat: double;
    Ticks, ControlPeriod: double;
    ControlMode: TControlMode;
    active: boolean;
  end;

  TEncoder = record
    PPR: integer;
    NoiseMean: double;
    NoiseStDev: double;
  end;

  TMotor = record
    Ri, Li, Ki, Vmax, Imax: double;
    Im: double;
    GearRatio: double;
    JRotor, teta, w: double;
    BRotor, QRotor: double;
    KGearBox, BGearBox: double;
    KGearBox2, BGearBox2: double;
    Encoder: TEncoder;
    Controller: TMotController;
    voltage, PowerDrain, EnergyDrain: double;
    active, simple: boolean;
  end;

  TMatterProperty = (smMetallic, smFerroMagnetic, smRFIDTag);
  TMatterProperties = set of TMatterProperty;

  { TSolid }

  TSolid = class
    Body: PdxBody;
    Geom : PdxGeom;
    GLObj, AltGLObj, ShadowGlObj, CanvasGLObj, extraGLObj: TGLSceneObject;
    PaintBitmap: TBitmap;
    PaintBitmapCorner: TdVector3;
    kind: TSolidKind;
    MatterProperties: TMatterProperties;
    BeltSpeed: double;
    ParSurface{, MaxParSurface} : TdSurfaceParameters;
    ID: string;
    //description: string;
    BuoyantMass, Volume, Drag, StokesDrag, RollDrag: double;
    BuoyanceCenter: TdVector3;
    Thrust: double;
    Ax, Ay, Az: double;
    ZeroPosition: TdVector3;
    ZeroRotation: TdMatrix3;
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPosition(posX, posY, posZ: double);
    procedure SetLinSpeed(vX, vY, vZ: double);
    procedure MovePosition(dX, dY, dZ: double);
    procedure SetRotation(axisX, axisY, axisZ, rot_angle: double);  overload;
    procedure SetRotation(R: TdMatrix3); overload;
    procedure SetZeroState;
    procedure SetColor(R, G, B: single; A: single = 1);
    procedure GetColor(out R, G, B, A: integer);
    procedure SetColorRGB(RGB: TColor);
    procedure SetTexture(TextureName: string; TextureScale: double);
    function GetPosition: TdVector3;
    function GetRotation: TdMatrix3;
    function GetLinSpeed: TdVector3;
    function GetAngularVel: TdVector3;
    procedure SetAngularVel(wX, wY, wZ: double);
    procedure SetSize(sizeX, sizeY, sizeZ: double);
    procedure GetSize(out sizeX, sizeY, sizeZ: double);
    procedure SetForce(FX, FY, FZ: double);
    procedure SetSurfaceFriction(mu, mu2: double);
    procedure SetSurfacePars(mu, mu2, softness, bounce, bounce_tresh: double);
    procedure UpdateGLCanvas;
  end;

  TSolidList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TSolid;
    procedure SetItems(Index: Integer; ASolid: TSolid);
  public
    function Add(ASolid: TSolid): Integer;
    function Extract(Item: TSolid): TSolid;
    function Remove(ASolid: TSolid): Integer;
    function IndexOf(ASolid: TSolid): Integer;
    function First: TSolid;
    function Last: TSolid;
    procedure Insert(Index: Integer; ASolid: TSolid);
    property Items[Index: Integer]: TSolid read GetItems write SetItems; default;
    procedure ClearAll;
    function IndexFromID(aID: string): integer;
  end;

  TAxisTraj = class
  public
    pos, speed, t: double;
    constructor Create;
    destructor Destroy; override;
  end;

  TAxisTrajList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TAxisTraj;
    procedure SetItems(Index: Integer; AAxisTraj: TAxisTraj);
  public
    function Add(AAxisTraj: TAxisTraj): Integer;
    function Extract(Item: TAxisTraj): TAxisTraj;
    function Remove(AAxisTraj: TAxisTraj): Integer;
    function IndexOf(AAxisTraj: TAxisTraj): Integer;
    function First: TAxisTraj;
    function Last: TAxisTraj;
    procedure Insert(Index: Integer; AAxisTraj: TAxisTraj);
    property Items[Index: Integer]: TAxisTraj read GetItems write SetItems; default;
    procedure ClearAll;
  end;

  TAxisGLPars = record
    radius, height: double;
    color: longWord;
  end;

  TAxisInputs = record
    theta, w: double;
    volts, Torque: double;
  end;

  TOdoState = record
    Angle, LastAngle: double;
    Residue: double;
    Value, LastValue, AccValue: integer;
  end;

  TSolidLink = class;

  TAxis = class
    ParentLink: TSolidLink;
    GLObj: TGLBaseSceneObject;
    Friction: TFriction;
    Spring: TSpring;
    Motor: TMotor;
    TrajectPoints, WayPoints: TAxisTrajList;
    Odo: TOdoState;
    ref: TAxisInputs;
    torque: double;
    speed_lambda, filt_speed: double;
    canWrap: boolean;
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetAnchor(var result: TdVector3);
    procedure SetAnchor(const nAnchor: TdVector3);
    procedure GetDir(var result: TdVector3);
    procedure SetDir(const nDir: TdVector3);
    function GetPos: double;
    function GetSpeed: double;
    procedure AddTorque(Tq: Double);
    procedure GLCreateCylinder(Parent: TGLBaseSceneObject; aRadius, aHeight: double);
    procedure GLCreateBall(Parent: TGLBaseSceneObject; aRadius: double);
    procedure GLSetPosition;
    function isCircular: boolean;
    procedure GetLimits(out MinLimit, Maxlimit: double);
  end;


  TAxisList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TAxis;
    procedure SetItems(Index: Integer; AAxis: TAxis);
  public
    function Add(AAxis: TAxis): Integer;
    function Extract(Item: TAxis): TAxis;
    function Remove(AAxis: TAxis): Integer;
    function IndexOf(AAxis: TAxis): Integer;
    function First: TAxis;
    function Last: TAxis;
    procedure Insert(Index: Integer; AAxis: TAxis);
    property Items[Index: Integer]: TAxis read GetItems write SetItems; default;
    procedure ClearAll;

    function IndexFromAxisID(aID: string; ith: integer): integer;
  end;

  TSolidLink = class
    joint: TdJointID;
    GLObj: TGLBaseSceneObject;
    //Axis, Axis2: TAxis;
    Axis: array[0..MaxAxis-1] of TAxis;
    ID: string;
    description: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TSolidLinkList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TSolidLink;
    procedure SetItems(Index: Integer; ASolidLink: TSolidLink);
  public
    function Add(ASolidLink: TSolidLink): Integer;
    function Extract(Item: TSolidLink): TSolidLink;
    function Remove(ASolidLink: TSolidLink): Integer;
    function IndexOf(ASolidLink: TSolidLink): Integer; overload;
    function First: TSolidLink;
    function Last: TSolidLink;
    procedure Insert(Index: Integer; ASolidLink: TSolidLink);
    property Items[Index: Integer]: TSolidLink read GetItems write SetItems; default;
    procedure ClearAll;
    function IndexOf(aID: string): integer; overload;
  end;


  TWheelPars = record
    offsetX, offsetY, offsetZ: double;
    Radius, Width, mass, CenterDist, Angle: double;
    Mu, Mu2, soft_cfm: double;
    omni: boolean;
  end;

  TWheel = class
    Pars: TWheelPars;
    Tyre: TSolid;     //Not owned by the class
    Axle: TSolidLink; //Not owned by the class
    active: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TWheelList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TWheel;
    procedure SetItems(Index: Integer; AWheel: TWheel);
  public
    function Add(AWheel: TWheel): Integer;
    function Extract(Item: TWheel): TWheel;
    function Remove(AWheel: TWheel): Integer;
    function IndexOf(AWheel: TWheel): Integer;
    function First: TWheel;
    function Last: TWheel;
    procedure Insert(Index: Integer; AWheel: TWheel);
    property Items[Index: Integer]: TWheel read GetItems write SetItems; default;
    procedure ClearAll;
  end;

  TSensor = class;

  TSensorMeasure = class
    value, dist: double;
    pos, normal: TdVector3;
    HitSolid: TSolid;
    has_measure: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSensorRay = class
    ParentSensor, TargetBeacon: TSensor;
    Geom: PdxGeom;
    Measure: TSensorMeasure;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Place(posX, posY, posZ, angX, angY, AngZ: double; AbsoluteCoords: boolean = false);
  end;

  TSensorRayList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TSensorRay;
    procedure SetItems(Index: Integer; ASensorRay: TSensorRay);
  public
    function Add(ASensorRay: TSensorRay): Integer;
    function Extract(Item: TSensorRay): TSensorRay;
    function Remove(ASensorRay: TSensorRay): Integer;
    function IndexOf(ASensorRay: TSensorRay): Integer;
    function First: TSensorRay;
    function Last: TSensorRay;
    procedure Insert(Index: Integer; ASensorRay: TSensorRay);
    property Items[Index: Integer]: TSensorRay read GetItems write SetItems; default;
    procedure ClearAll;
  end;

  TSensorNoise = record
    var_k, var_d, offset, gain: double;
    std_a, std_p: double;
    active: boolean;
  end;

  TSensorKind = (skGeneric, skIR, skIRSharp, skSonar, skCapacitive, skInductive,
                 skBeacon, skFloorLine, skRanger2D, skPenTip, skIMU, skSolenoid, skRFID);

  TSensor = class
    ID: string;
    kind: TSensorKind;
    Noise: TSensorNoise;
    Rays: TSensorRayList;
    GLObj: TGLSceneObject;
    Period, TimeFromLastMeasure: double;
    Tags: TStringlist;
    Fmax, k1,k2: double;
    Vin: double;
    MaxDist, MinDist, StartAngle, EndAngle: double;

  private
    function InsideGLPolygonsTaged(x, y: double; GLFloor: TGLBaseSceneObject): boolean;
    procedure FreeMeasures;
  protected
    function GetMeasureCount: integer;
    //function GetMeasure(Index: Integer): TSensorMeasure;
    //procedure SetMeasure(Index: Integer; AMeasure: TSensorMeasure);

  public
    Measures: array of TSensorMeasure;

    constructor Create(MeasuresCount: integer = 1);
    destructor Destroy; override;
    procedure SetColor(R, G, B: single; A: single = -1);
    //property Measures[Index: Integer]: TSensorMeasure read GetMeasure write SetMeasure; default;
    property MeasuresCount: integer read GetMeasureCount;
    procedure PreProcess(dt: double);
    procedure PostProcess;
    procedure UpdateMeasures;
    procedure NoiseModel;
    procedure SetColorRGB(RGB: TColor);
    function SwapRGB(RGB: TColor): TColor;
    procedure SetVin(aVin: double);
    procedure CreateMeasures(Count: integer = 1);
  end;

const
  SensorKindStrings: array[TSensorKind] of string =
  ('Generic', 'IR', 'IRSharp', 'Sonar', 'Capacitive', 'Inductive',
   'Beacon', 'FloorLine', 'Ranger2D', 'PenTip', 'IMU', 'Solenoid', 'RFID');

type
  TSensorList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TSensor;
    procedure SetItems(Index: Integer; ASensor: TSensor);
  public
    function Add(ASensor: TSensor): Integer;
    function Extract(Item: TSensor): TSensor;
    function Remove(ASensor: TSensor): Integer;
    function IndexOf(ASensor: TSensor): Integer;
    function First: TSensor;
    function Last: TSensor;
    procedure Insert(Index: Integer; ASensor: TSensor);
    property Items[Index: Integer]: TSensor read GetItems write SetItems; default;
    procedure ClearAll;
    function IndexFromID(aID: string): integer;
  end;


  TKeyVals = array[0..MaxKeyVals-1] of single; //TODO

  TRobotKind = (rkUnknown, rkWheelChair, rkOmni3, rkOmni4, rkHumanoid, rkArm, rkConveyorBelt, rkOther);

  TRobot = class
    Solids: TSolidList;
    Links: TSolidLinkList;
    Axes: TAxisList; // TAxisList does not owns the axis
    AxesWayPointsIDs: TStringlist;

    MainBody: TSolid;
    Shells: TSolidList;
    Wheels: TWheelList;
    Sensors: TSensorList;
    Kind: TRobotKind;
    //SamplesCount, DecPeriodSamples: integer;
    //SecondsCount, DecPeriod: double;
    ID: string;
    ForceMoved: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetXYZTeta(new_x, new_y, new_z, new_teta: double);
    procedure GetXYZTeta(out x, y, z, teta: double);
    function CalcCenterOfMass: TdVector3;
    procedure AddXY(add_x, add_y: double);
  end;

  TRobotList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TRobot;
    procedure SetItems(Index: Integer; ARobot: TRobot);
  public
    function Add(ARobot: TRobot): Integer;
    function Extract(Item: TRobot): TRobot;
    function Remove(ARobot: TRobot): Integer;
    function IndexOf(ARobot: TRobot): Integer;
    function First: TRobot;
    function Last: TRobot;
    procedure Insert(Index: Integer; ARobot: TRobot);
    property Items[Index: Integer]: TRobot read GetItems write SetItems; default;
    procedure ClearAll;
    function IndexFromID(aID: string): integer;
  end;

procedure RFromZYXRotRel(var R: TdMatrix3; angX, angY, angZ: TDreal);
procedure RFromZYXRotRelRay(var R: TdMatrix3; angX, angY, angZ: TDreal);

implementation

uses Utils;


procedure RFromZYXRotRel(var R: TdMatrix3; angX, angY, angZ: TDreal);
var Rx, Ry, Rz, Ryx: TdMatrix3;
begin
  dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
  dRFromAxisAndAngle(Ry, 0, 1, 0, angY);
  dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

  dMULTIPLY0_333(Ryx, Ry, Rx);
  dMULTIPLY0_333(R, Rz, Ryx);
end;


procedure RFromZYXRotRelRay(var R: TdMatrix3; angX, angY, angZ: TDreal);
var Rx, Ry, Rz, Raux1, Raux2, Ry2: TdMatrix3;
begin
  dRFromAxisAndAngle(Rz, 0, 0, 1, angZ);
  dRFromAxisAndAngle(Ry, 0, 1, 0, angY);
  dRFromAxisAndAngle(Ry2, 0, 1, 0, pi/2);
  dRFromAxisAndAngle(Rx, 1, 0, 0, angX);

  dMULTIPLY0_333(Raux1, Rz, Ry2);
  dMULTIPLY0_333(Raux2, Ry, Raux1);
  dMULTIPLY0_333(R, Rx, Raux2);
end;


{ TRobot }


constructor TRobot.Create;
begin
  Solids := TSolidList.Create;
  Shells := TSolidList.Create;
  Axes := TAxisList.Create;
  AxesWayPointsIDs := TStringList.Create;
  Links := TSolidLinkList.Create;
  Wheels := TWheelList.Create;
  Sensors := TSensorList.Create;
end;

destructor TRobot.Destroy;
begin
//  Sensors.ClearAll;
  Sensors.Free;
  AxesWayPointsIDs.Free;
  //Axes.ClearAll; // TAxisList does not owns the axis
  Axes.Free;
  Wheels.ClearAll;
  Wheels.Free;
  Links.ClearAll;
  Links.Free;
  Shells.ClearAll;
  Shells.Free;
  Solids.ClearAll;
  Solids.free;
  inherited;
end;

procedure TRobot.GetXYZTeta(out x, y, z, teta: double);
var v0, v1, v2: TdVector3;
begin
  if (MainBody <> nil) and
     (MainBody.Body <> nil) then begin
    v0 := MainBody.ZeroPosition;
    v1 := dBodyGetPosition(MainBody.Body)^;
    dBodyGetRelPointPos(MainBody.Body, 1,0,0, v2);
    x := v1[0] - V0[0];
    y := v1[1] - V0[1];
    z := v1[2] - V0[2];
    teta := atan2(v2[1]-v1[1], v2[0]-v1[0]);
  end;
end;


procedure TRobot.SetXYZTeta(new_x, new_y, new_z, new_teta: double);
var i, j, k, n: integer;
    Rteta, Ra: TdMatrix3;
    Pr, P0, Pd: TdVector3;
begin
  if MainBody <> nil then begin
    P0 := MainBody.ZeroPosition;
    //P0 := Solids[0].ZeroPosition;
  end else begin
    P0 := Vector3Make(0, 0, 0);
  end;
  dRFromAxisAndAngle(Rteta, 0, 0, 1, new_teta);

  for i := 0 to Solids.Count - 1 do begin
    // Rotate to the original pose
    dMULTIPLY0_333(Ra, Rteta, Solids[i].ZeroRotation);
    dBodySetRotation(Solids[i].Body, Ra);

    // Move to the original pose counting with the relative offset from the mainbody
    Pd := Vector3SUB(Solids[i].ZeroPosition, P0);
    dMULTIPLY0_331(Pr, Rteta, Pd);
    dBodySetPosition(Solids[i].Body, Pr[0] + P0[0] + new_x, Pr[1] + P0[1]  + new_y, Pr[2] + P0[2] + new_z);

    // Nullify the velocities
    dBodySetLinearVel(Solids[i].Body, 0, 0, 0);
    dBodySetAngularVel(Solids[i].Body, 0, 0, 0);
  end;

  // Reset motor position
  for j := 0 to Links.Count - 1 do begin
    //for k := 0 to MaxAxis - 1 do begin
    //  Links[j].Axis[0].Motor.teta := 0;
    n := 0;
    if  (dJointGetType(Links[j].joint) = ord(dJointTypeHinge)) or
        (dJointGetType(Links[j].joint) = ord(dJointTypeSlider)) then n := 1
    else if (dJointGetType(Links[j].joint) = ord(dJointTypeUniversal)) then n := 2;

    for k := 0 to n - 1 do begin
      Links[j].Axis[k].Motor.teta := 0;
    end;
  end;

  // deal with links to the world
  for i := 0 to Solids.Count - 1 do begin
    for j := 0 to Links.Count - 1 do begin
      if (dJointGetBody(Links[j].joint, 0) = Solids[i].Body) and (dJointGetBody(Links[j].joint, 1)= nil) then begin
        {Links[j].Axis[0].GetAnchor(Pr);
        Pd := Vector3SUB(Pr, P0);
        dMULTIPLY0_331(Pr, Rteta, Pd);
        Pd[0] := Pr[0] + P0[0];
        Pd[1] := Pr[1] + P0[1];
        Pd[2] := Pr[2] + P0[2];
        Links[j].Axis[0].SetAnchor(Pd)}

        {Pd := Vector3SUB(Pr, P0);
        dMULTIPLY0_331(Pr, Rteta, Pd);
        Pd[0] := Pr[0] + P0[0] + new_x;
        Pd[1] := Pr[1] + P0[1] + new_y;
        Pd[2] := Pr[2] + P0[2] + new_z;}
        //Links[j].Axis[0].SetAnchor(Pd);

        //Links[j].Axis[0].GetAnchor(Pr);
        //dMULTIPLY0_331(Pd, Rteta, Pr);
        //Pr[0] := Pr[0] + new_x;
        //Pr[1] := Pr[1] + new_y;
        //Pr[2] := Pr[2] + new_z;
        //Links[j].Axis[0].SetAnchor(Pr);

        Links[j].Axis[0].GetAnchor(Pr);  // TWICE!!!
       { dMULTIPLY0_331(Pd, Rteta, Pr);
        Pr[0] := Pd[0];// + new_x;
        Pr[1] := Pd[1];// + new_y;
        Pr[2] := Pd[2];// + new_z;}
        Links[j].Axis[0].SetAnchor(Pr);
        Links[j].Axis[0].GetAnchor(Pr);
        Links[j].Axis[0].SetAnchor(Pr);

        Links[j].Axis[0].GetDir(Pr);
        dMULTIPLY0_331(Pd, Rteta, Pr); // Double angle!!!
        dMULTIPLY0_331(Pr, Rteta, Pd);
        {Pr[0] := Pd[0];// + new_x;
        Pr[1] := Pd[1];// + new_y;
        Pr[2] := Pd[2];// + new_z;}

        Links[j].Axis[0].Setdir(Pr);
      end;
    end;
  end;

  ForceMoved := true;
end;

function TRobot.CalcCenterOfMass: TdVector3;
var v0, vi: TdVector3;
    m0, mi: double;
    i: integer;
begin
  v0 := Vector3Make(0, 0, 0);
  m0 := 0;

  for i := 0 to Solids.Count - 1 do begin
    vi := dBodyGetPosition(Solids[i].Body)^;
    mi := Solids[i].Body.mass.mass;
    v0 := Vector3ADD(v0, Vector3ScalarMul(vi, mi));
    m0 := m0 + mi;
  end;
  if m0 > 0 then begin
    v0 := Vector3ScalarMul(v0, 1/m0);
  end;
  result := v0;
end;


procedure TRobot.AddXY(add_x, add_y: double);
var i: integer;
    Pr: TdVector3;
begin

  for i := 0 to Solids.Count - 1 do begin
    Pr := dBodygetPosition(Solids[i].Body)^;
    dBodySetPosition(Solids[i].Body, Pr[0] + add_x, Pr[1] + add_y, Pr[2]);
    //dBodySetPosition(Solids[i].Body, Pr[0], Pr[1], Pr[2]);
  end;
end;


{ TRobotList }

function TRobotList.IndexFromID(aID: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].ID = aID then begin
      result := i;
      exit;
    end;
  end;
end;


function TRobotList.Add(ARobot: TRobot): Integer;
begin
  Result := inherited Add(ARobot);
end;

procedure TRobotList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;

function TRobotList.Extract(Item: TRobot): TRobot;
begin
  Result := TRobot(inherited Extract(Item));
end;

function TRobotList.First: TRobot;
begin
  Result := TRobot(inherited First);
end;

function TRobotList.GetItems(Index: Integer): TRobot;
begin
  Result := TRobot(inherited Items[Index]);
end;

function TRobotList.IndexOf(ARobot: TRobot): Integer;
begin
  Result := inherited IndexOf(ARobot);
end;

procedure TRobotList.Insert(Index: Integer; ARobot: TRobot);
begin
  inherited Insert(Index, ARobot);
end;

function TRobotList.Last: TRobot;
begin
  Result := TRobot(inherited Last);
end;

function TRobotList.Remove(ARobot: TRobot): Integer;
begin
  Result := inherited Remove(ARobot);
end;

procedure TRobotList.SetItems(Index: Integer; ARobot: TRobot);
begin
  inherited Items[Index] := ARobot;
end;


{ TSolid }

constructor TSolid.Create;
begin
  BeltSpeed := 0.5;
  PaintBitmap := nil;
end;

destructor TSolid.Destroy;
begin
  if assigned(PaintBitmap) then PaintBitmap.Free;
  inherited;
end;


{ TSolidsList }

procedure TSolidList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;


function TSolidList.Add(ASolid: TSolid): Integer;
begin
  Result := inherited Add(ASolid);
end;

function TSolidList.Extract(Item: TSolid): TSolid;
begin
  Result := TSolid(inherited Extract(Item));
end;

function TSolidList.First: TSolid;
begin
  Result := TSolid(inherited First);
end;

function TSolidList.GetItems(Index: Integer): TSolid;
begin
  Result := TSolid(inherited Items[Index]);
end;

function TSolidList.IndexOf(ASolid: TSolid): Integer;
begin
  Result := inherited IndexOf(ASolid);
end;

procedure TSolidList.Insert(Index: Integer; ASolid: TSolid);
begin
  inherited Insert(Index, ASolid);
end;

function TSolidList.Last: TSolid;
begin
  Result := TSolid(inherited Last);
end;

function TSolidList.Remove(ASolid: TSolid): Integer;
begin
  Result := inherited Remove(ASolid);
end;

procedure TSolidList.SetItems(Index: Integer; ASolid: TSolid);
begin
  inherited Items[Index] := ASolid;
end;

function TSolid.GetPosition: TdVector3;
begin
  result := dBodyGetPosition(Body)^;
end;

function TSolid.GetRotation: TdMatrix3;
begin
  result := dBodyGetRotation(Body)^;
end;

procedure TSolid.MovePosition(dX, dY, dZ: double);
var Vpos: TdVector3;
begin
  Vpos := dBodyGetPosition(Body)^;
  dBodySetPosition(Body, Vpos[0] + dX, Vpos[1] + dY, Vpos[2] + dZ);
end;

procedure TSolid.SetColor(R, G, B, A: single);
begin
  if GLObj = nil then exit;
  if A < 1 then GLObj.Material.BlendingMode := bmTransparency;
  GLObj.Material.FrontProperties.Diffuse.SetColor(R, G, B, A);
end;


procedure TSolid.SetColorRGB(RGB: TColor);
begin
  if GLObj = nil then exit;
  GLObj.Material.FrontProperties.Diffuse.AsWinColor := RGB;
end;

procedure TSolid.GetColor(out R, G, B, A: integer);
begin
  if GLObj = nil then exit;
  with GLObj.Material.FrontProperties.Diffuse do begin
    R := round(255 * Red);
    G := round(255 * green);
    B := round(255 * Blue);
    A := round(255 * Alpha);
  end;
end;

procedure TSolid.SetPosition(posX, posY, posZ: double);
begin
  dBodySetPosition(Body, posX, posY, posZ);
end;

procedure TSolid.SetRotation(axisX, axisY, axisZ, rot_angle: double);
var R: TdMatrix3;
begin
  dRFromAxisAndAngle(R, axisX, axisY, axisZ, rot_angle);
  dBodySetRotation(Body, R);
end;

procedure TSolid.SetRotation(R: TdMatrix3);
begin
  dBodySetRotation(Body, R);
end;

procedure TSolid.SetTexture(TextureName: string; TextureScale: double);
var LM: TGLLibMaterial;
begin
  exit; //TODO
  if GLObj = nil then exit;
  //LM := GLObj.Material.MaterialLibrary.Materials.GetLibMaterialByName(TextureName);
  if LM <> nil then begin;
    GLObj.Material.LibMaterialName := TextureName;
    LM.TextureScale.x := TextureScale;
    LM.TextureScale.y := TextureScale;
  end else begin
    GLObj.Material.TextureEx.Add;
    GLObj.Material.TextureEx.Items[0].Texture.Image.LoadFromFile(TextureName);
    with GLObj.Material.TextureEx.Items[0] do begin
      TextureIndex := 0;

      Texture.Disabled := false;
      Texture.TextureMode := tmModulate;
      //Texture.TextureMode := tmDecal;
      //Texture.TextureMode := tmReplace;
      //Texture.TextureMode := tmBlend;
      Texture.MappingMode := tmmObjectLinear;
      with Texture.MappingSCoordinates do begin
        W := 0; X := 0; Y := 1; Z := 0;
      end;
      with Texture.MappingTCoordinates do begin
        W := 0; X := 1; Y := 0; Z := 0;
      end;
      with TextureOffset do begin
        X := 0.5; Y := 0.5; Z := 0;
      end;
    end;
    GLObj.Material.TextureEx.Items[0].TextureScale.x := TextureScale;
    GLObj.Material.TextureEx.Items[0].TextureScale.y := TextureScale;
  end;
end;

procedure TSolid.SetZeroState;
begin
  ZeroPosition := dBodyGetPosition(Body)^;
  ZeroRotation :=  dBodyGetRotation(Body)^;
end;



procedure TSolid.SetLinSpeed(vX, vY, vZ: double);
begin
  dBodySetLinearVel(Body, vX, vY, vZ);
end;

procedure TSolid.SetAngularVel(wX, wY, wZ: double);
begin
  dBodySetAngularVel(Body, wX, wY, wZ);
end;


procedure TSolid.SetSize(sizeX, sizeY, sizeZ: double);
begin
  if dGeomGetClass(Geom) = dBoxClass then begin
    dGeomBoxSetLengths(Geom, sizeX, sizeY, sizeZ);
    with (GLObj as TGLCube) do begin
      CubeDepth := sizeZ;
      CubeHeight := sizeY;
      CubeWidth := sizeX;
    end;
  end else if dGeomGetClass(Geom) = dCylinderClass then begin
    dGeomCylinderSetParams(Geom, sizeX, sizeZ);
    with (GLObj as TGLCylinder) do begin
      TopRadius := sizeX;
      BottomRadius := sizeX;
      Height := sizeZ;
    end;
  end else if dGeomGetClass(Geom) = dSphereClass then begin
    dGeomSphereSetRadius(Geom, sizeX);
    with (GLObj as TGLSphere) do begin
      Radius := sizeX;
    end;
  end;
  // TODO more geom classes
end;


procedure TSolid.GetSize(out sizeX, sizeY, sizeZ: double);
var Vsize: TdVector3;
    r, h: TdReal;
begin
  if dGeomGetClass(Geom) = dBoxClass then begin
    dGeomBoxGetLengths(Geom, Vsize);
    SizeX := Vsize[0];
    SizeY := Vsize[1];
    SizeZ := Vsize[2];
  end else if dGeomGetClass(Geom) = dCylinderClass then begin
    dGeomCylinderGetParams(Geom, r, h);
    SizeX := r;
    SizeY := r;
    SizeZ := h;
  end else if dGeomGetClass(Geom) = dSphereClass then begin
    sizeX := dGeomSphereGetRadius(Geom);
    SizeY := sizeX;
    SizeZ := sizeX;
  end;
end;

procedure TSolid.SetForce(FX, FY, FZ: double);
begin
  dBodySetForce(Body, Fx, Fy, Fz);
end;


function TSolid.GetLinSpeed: TdVector3;
begin
  result :=dBodyGetLinearVel(Body)^;
end;

procedure TSolid.UpdateGLCanvas;
var img: TGLBitmap32;
begin
  if assigned(CanvasGLObj) and assigned(PaintBitmap) then begin
    CanvasGLObj.Material.Texture.Image.BeginUpdate;
    img := CanvasGLObj.Material.Texture.Image.GetBitmap32();
    img.AssignFromBitmap24WithoutRGBSwap(PaintBitmap);
    CanvasGLObj.Material.Texture.Image.endUpdate;
  end;
end;


procedure TSolid.SetSurfacePars(mu, mu2, softness, bounce, bounce_tresh: double);
begin
  if mu >= 0 then begin
    ParSurface.mode := $FF;
    ParSurface.mu := mu;
    if mu2 >= 0 then begin
      ParSurface.mu2 := mu2;
      kind := skOmniSurface;
    end else begin
      kind := skDefault;
    end;
    ParSurface.soft_cfm := softness;
    ParSurface.bounce := bounce;
    ParSurface.bounce_vel := bounce_tresh;
  end else begin
    ParSurface.mode := 0;
    kind := skDefault;
  end;
end;


procedure TSolid.SetSurfaceFriction(mu, mu2: double);
begin
  if mu >= 0 then begin
    ParSurface.mode := $FF;
    ParSurface.mu := mu;
    if mu2 >= 0 then begin
      ParSurface.mu2 := mu2;
      kind := skOmniSurface;
    end else begin
      kind := skDefault;
    end;
  end else begin
    ParSurface.mode := 0;
    kind := skDefault;
  end;
end;


function TSolid.GetAngularVel: TdVector3;
begin
  result :=dBodyGetAngularVel(Body)^;
end;

{ TAxis }


constructor TAxis.Create;
begin
  TrajectPoints := TAxisTrajList.Create;
  WayPoints := TAxisTrajList.Create;
  speed_lambda := 0.95;
  canWrap := true;
end;

destructor TAxis.Destroy;
begin
  WayPoints.ClearAll;
  TrajectPoints.ClearAll;
  WayPoints.Free;
  TrajectPoints.Free;
  inherited;
end;

procedure TAxis.GetAnchor(var result: TdVector3);
begin
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    dJointGetHingeAnchor(ParentLink.joint, result);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      dJointGetUniversalAnchor(ParentLink.joint, result);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      dJointGetUniversalAnchor2(ParentLink.joint, result);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    //ZeroMemory(@(result[0]), sizeof(result));
    result := dBodyGetPosition(dJointGetBody(ParentLink.joint,0))^;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeFixed) then begin
    result := dBodyGetPosition(dJointGetBody(ParentLink.joint,0))^;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeBall) then begin
    dJointGetBallAnchor(ParentLink.joint, result);
    //result := dBodyGetPosition(dJointGetb Body(ParentLink.joint,0))^;
  end;
  //TODO more Joint types
end;

procedure TAxis.SetAnchor(const nAnchor: TdVector3);
var v: TdVector3;
begin
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    dJointSetHingeAnchor(ParentLink.joint, nAnchor[0], nAnchor[1], nAnchor[2]);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    dJointSetUniversalAnchor(ParentLink.joint, nAnchor[0], nAnchor[1], nAnchor[2]);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    // Read and set the axis to reset the anchor point
    dJointGetSliderAxis(ParentLink.joint, v);
    dJointSetSliderAxis(ParentLink.joint, v[0], v[1], v[2]);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeFixed) then begin
    dJointSetFixed(ParentLink.joint);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeBall) then begin
    dJointSetBallAnchor(ParentLink.joint, nAnchor[0], nAnchor[1], nAnchor[2]);
  end;
  //TODO more Joint types
end;


procedure TAxis.GetDir(var result: TdVector3);
begin
  result[0] := 0;
  result[1] := 0;
  result[2] := 0;
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    dJointGetHingeAxis(ParentLink.joint, result);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      dJointGetUniversalAxis1(ParentLink.joint, result);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      dJointGetUniversalAxis2(ParentLink.joint, result);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    dJointGetSliderAxis(ParentLink.joint, result);
    //ZeroMemory(@(result[0]), sizeof(result));
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeFixed) then begin
    ZeroMemory(@(result[0]), sizeof(result));
    result[2] := 1; //TODO set the direction to the vector from one solid to the other
  end;
  //TODO more Joint types
end;

procedure TAxis.SetDir(const nDir: TdVector3);
begin
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    dJointSetHingeAxis(ParentLink.joint, nDir[0], nDir[1], nDir[2]);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      dJointSetUniversalAxis1(ParentLink.joint, nDir[0], nDir[1], nDir[2]);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      dJointSetUniversalAxis2(ParentLink.joint, nDir[0], nDir[1], nDir[2]);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    dJointSetSliderAxis(ParentLink.joint, nDir[0], nDir[1], nDir[2]);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeFixed) then begin
    // Nothing to do here
  end;
  //TODO more Joint types
end;



// Get Axis "angle"
function TAxis.GetPos: double;
begin
  result := 0;
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    result := dJointGetHingeAngle(ParentLink.joint);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      result := dJointGetUniversalAngle1(ParentLink.joint);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      result := dJointGetUniversalAngle2(ParentLink.joint);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    result := dJointGetSliderPosition(ParentLink.joint);
  end;
  //TODO more Joint types
end;

function TAxis.GetSpeed: double;
begin
  result := 0;
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    result := dJointGetHingeAngleRate(ParentLink.joint);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      result := dJointGetUniversalAngle1Rate(ParentLink.joint);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      result := dJointGetUniversalAngle2Rate(ParentLink.joint);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    result := dJointGetSliderPositionRate(ParentLink.joint);
  end;
  //lambda := 0.9;
  filt_speed := (1 - speed_lambda)* result + speed_lambda * filt_speed;
  //TODO more Joint types
end;

procedure TAxis.AddTorque(Tq: Double);
begin
  torque := Tq;
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    dJointAddHingeTorque(ParentLink.joint, Tq);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      dJointAddUniversalTorques(ParentLink.joint, Tq, 0);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      dJointAddUniversalTorques(ParentLink.joint, 0, Tq);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    dJointAddSliderForce(ParentLink.joint, Tq);
  end;
  //TODO more Joint types
end;

procedure TAxis.GLCreateCylinder(Parent: TGLBaseSceneObject; aRadius, aHeight: double);
begin
  GLObj := TGLCylinder(Parent.AddNewChild(TGLCylinder));
  with TGLCylinder(GLObj) do begin
    BottomRadius := aRadius;
    TopRadius := aRadius;
    height := aHeight;
    Material.FrontProperties.Diffuse.AsWinColor := clred;
  end;

//  AxisGLSetPosition(axis);
end;

procedure TAxis.GLCreateBall(Parent: TGLBaseSceneObject; aRadius: double);
begin
  GLObj := TGLSphere(Parent.AddNewChild(TGLSphere));
  with TGLSphere(GLObj) do begin
    Radius := aRadius;
    Material.FrontProperties.Diffuse.AsWinColor := clred;
  end;

//  AxisGLSetPosition(axis);
end;


procedure TAxis.GLSetPosition;
var vtmp: TdVector3;
begin
  if GLObj = nil then exit;
  with TGLSceneObject(GLObj) do begin
    GetAnchor(vtmp);
    Position.x := vtmp[0];
    Position.y := vtmp[1];
    Position.z := vtmp[2];

    GetDir(vtmp);
    up.x := vtmp[0];
    up.y := vtmp[1];
    up.z := vtmp[2];
  end;
end;

function TAxis.isCircular: boolean;
begin
  result := true;
  if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    result := false;
  end;
  //TODO more Joint types
end;

procedure TAxis.GetLimits(out MinLimit, Maxlimit: double);
begin
  if dJointGetType(ParentLink.joint) = ord(dJointTypeHinge) then begin
    MinLimit := dJointGetHingeParam(ParentLink.joint, dParamLoStop);
    MaxLimit := dJointGetHingeParam(ParentLink.joint, dParamHiStop);
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeUniversal) then begin
    if self = ParentLink.Axis[0] then begin //if it is the first axis in the universal joint..
      MinLimit := dJointGetUniversalParam(ParentLink.joint, dParamLoStop);
      MaxLimit := dJointGetUniversalParam(ParentLink.joint, dParamHiStop);
    end else if self = ParentLink.Axis[1] then begin //if it is the second axis in the universal joint..
      MinLimit := dJointGetUniversalParam(ParentLink.joint, dParamLoStop2);
      MaxLimit := dJointGetUniversalParam(ParentLink.joint, dParamHiStop2);
    end;
  end else if dJointGetType(ParentLink.joint) = ord(dJointTypeSlider) then begin
    MinLimit := dJointGetHingeParam(ParentLink.joint, dParamLoStop);
    MaxLimit := dJointGetHingeParam(ParentLink.joint, dParamHiStop);
  end;
  //TODO more Joint types
end;

{ TSolidLink }

constructor TSolidLink.Create;
begin
  //Axis := nil;
  //Axis2 := nil;
end;

destructor TSolidLink.Destroy;
var i: integer;
begin
  for i := 0 to 2 do Axis[i].Free;

//  Axis.Free;
//  Axis2.Free;
  inherited;
end;



{ TSolidLinkList }

procedure TSolidLinkList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;


function TSolidLinkList.Add(ASolidLink: TSolidLink): Integer;
begin
  Result := inherited Add(ASolidLink);
end;

function TSolidLinkList.Extract(Item: TSolidLink): TSolidLink;
begin
  Result := TSolidLink(inherited Extract(Item));
end;

function TSolidLinkList.First: TSolidLink;
begin
  Result := TSolidLink(inherited First);
end;

function TSolidLinkList.GetItems(Index: Integer): TSolidLink;
begin
  Result := TSolidLink(inherited Items[Index]);
end;

function TSolidLinkList.IndexOf(ASolidLink: TSolidLink): Integer;
begin
  Result := inherited IndexOf(ASolidLink);
end;

procedure TSolidLinkList.Insert(Index: Integer; ASolidLink: TSolidLink);
begin
  inherited Insert(Index, ASolidLink);
end;

function TSolidLinkList.Last: TSolidLink;
begin
  Result := TSolidLink(inherited Last);
end;

function TSolidLinkList.Remove(ASolidLink: TSolidLink): Integer;
begin
  Result := inherited Remove(ASolidLink);
end;

procedure TSolidLinkList.SetItems(Index: Integer; ASolidLink: TSolidLink);
begin
  inherited Items[Index] := ASolidLink;
end;

function TSolidLinkList.IndexOf(aID: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].ID = aID then begin
      result := i;
      exit;
    end;
  end;
end;

{ TWheel }

constructor TWheel.Create;
begin
  Tyre := nil;
  Axle := nil;
end;

destructor TWheel.Destroy;
begin
  //Tyre.Free;
  //Axle.Free;
  inherited;
end;

{ TWheelList }

procedure TWheelList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;


function TWheelList.Add(AWheel: TWheel): Integer;
begin
  Result := inherited Add(AWheel);
end;

function TWheelList.Extract(Item: TWheel): TWheel;
begin
  Result := TWheel(inherited Extract(Item));
end;

function TWheelList.First: TWheel;
begin
  Result := TWheel(inherited First);
end;

function TWheelList.GetItems(Index: Integer): TWheel;
begin
  Result := TWheel(inherited Items[Index]);
end;

function TWheelList.IndexOf(AWheel: TWheel): Integer;
begin
  Result := inherited IndexOf(AWheel);
end;

procedure TWheelList.Insert(Index: Integer; AWheel: TWheel);
begin
  inherited Insert(Index, AWheel);
end;

function TWheelList.Last: TWheel;
begin
  Result := TWheel(inherited Last);
end;

function TWheelList.Remove(AWheel: TWheel): Integer;
begin
  Result := inherited Remove(AWheel);
end;

procedure TWheelList.SetItems(Index: Integer; AWheel: TWheel);
begin
  inherited Items[Index] := AWheel;
end;

{ TSensor }

constructor TSensor.Create(MeasuresCount: integer = 1);
begin
  Tags := TStringlist.Create;

  Rays := TSensorRayList.Create;

  CreateMeasures(MeasuresCount);
end;

destructor TSensor.Destroy;
begin
  FreeMeasures;

  Rays.ClearAll;
  Rays.Free;

  Tags.Free;
  inherited;
end;

procedure TSensor.CreateMeasures(Count: integer = 1);
var i: integer;
begin
  SetLength(Measures, Count);
  for i := 0 to Count - 1 do begin
    Measures[i] := TSensorMeasure.Create;
  end;
end;

procedure TSensor.FreeMeasures;
var i: integer;
begin
  for i := 0 to length(Measures) - 1 do begin
    Measures[i].Free;
  end;
end;

{procedure TSensor.SetMeasure(Index: Integer; AMeasure: TSensorMeasure);
begin
  if (Index < 0) or (Index >= length(SensorMeasures)) then
    raise ERangeError.CreateFmt('%d is not within the valid range of 0..%d', [Index, length(SensorMeasures)-1]);
  SensorMeasures[Index] := AMeasure;
end;

function TSensor.GetMeasure(Index: Integer): TSensorMeasure;
begin
  if (Index < 0) or (Index >= length(SensorMeasures)) then
    raise ERangeError.CreateFmt('%d is not within the valid range of 0..%d', [Index, length(SensorMeasures)-1]);
  result := SensorMeasures[Index];
end;}

procedure TSensor.SetColor(R, G, B: single; A: single = -1);
begin
  if GLObj = nil then exit;

  if A = -1 then A := GLObj.Material.FrontProperties.Diffuse.Alpha;
  GLObj.Material.FrontProperties.Diffuse.SetColor(R, G, B, A);
end;


procedure TSensor.PreProcess(dt: double);
var i, j: integer;
    pers: double;
    active_rays, first_active_ray: integer;
begin

  case kind of

    skRanger2D: begin
      pers := dt / Period;
      active_rays := round(Rays.Count * pers) + 1;

      first_active_ray :=  round(Rays.Count * TimeFromLastMeasure / Period);
      for i := 0 to Rays.Count - 1 do begin
        if (i >= first_active_ray) and (i <= first_active_ray + active_rays) then begin
          dGeomEnable(Rays[i].Geom);
        end else begin
          dGeomDisable(Rays[i].Geom);
        end;
      end;
    end;

  end;

  for j := 0 to Rays.Count - 1 do begin
    if dGeomIsEnabled(Rays[j].Geom) = 0 then continue;
    Rays[j].Measure.dist := -1;
    Rays[j].Measure.has_measure := false;
    Rays[j].Measure.HitSolid := nil;
  end;

end;



procedure TSensor.PostProcess;
var HitSolid: TSolid;
    SensorPos, HitSolidPos: PdVector3;
    Vec: TdVector3;
    f, d: double;
begin
  case kind of

    skSolenoid: begin
      with Measures[0] do begin
        HitSolid := Rays[0].Measure.HitSolid;
        if assigned(HitSolid) and (HitSolid.Body <> nil) and (value > 0) then begin
          SensorPos := dGeomGetPosition(Rays[0].Geom);
          HitSolidPos := dBodyGetPosition(HitSolid.Body);
          Vec := Vector3SUB(SensorPos^, Rays[0].Measure.pos);
          //Vec := Vector3SUB(SensorPos^, HitSolidPos^);
          f := 0.5 * Vin * Fmax /sqr(1 + max(0 ,Rays[0].Measure.dist) * k2);
          d := Vector3Length(Vec);
          if d > 0 then f := f / d;
          Vec := Vector3ScalarMul(Vec, f);
          HitSolid.SetForce(Vec[0], Vec[1], Vec[2]);                     // Action
          if assigned(Rays[0].Geom) and assigned(Rays[0].Geom.Body) then
            dBodySetForce(Rays[0].Geom.Body, -Vec[0], -Vec[1], -Vec[2]);   // Reaction
        end;
      end;
    end;

  end;
end;


function TSensor.InsideGLPolygonsTaged(x, y: double; GLFloor: TGLBaseSceneObject): boolean;
var n, i, j: integer;
    GLPolygon: TGLPolygon;
begin
  result := false;
  if GLFloor = nil then exit;
  for n := 0 to GLFloor.Count - 1 do begin
    if not (GLFloor.Children[n] is TGLPolygon) then continue;
    GLPolygon := TGLPolygon(GLFloor.Children[n]);
    if tags <> nil then
      if tags.IndexOf(GLPolygon.Hint) < 0 then continue;

    j := GLPolygon.Nodes.Count - 1;
    for i := 0 to GLPolygon.Nodes.Count - 1 do begin
      //result := false;
      // test if point is inside polygon
      with GLPolygon do begin
        if ((((Nodes[i].Y <= Y) and (Y < Nodes[j].Y)) or ((Nodes[j].Y <= Y) and (Y < Nodes[i].Y)) )
             and (X < (Nodes[j].X - Nodes[i].X) * (Y - Nodes[i].Y) / (Nodes[j].Y - Nodes[i].Y) + Nodes[i].X))
        then result := not result;
        j:=i;
      end;
    end;
    if result then break;
  end;
end;



function TSensor.GetMeasureCount: integer;
begin
  result := length(Measures);
end;


procedure TSensor.UpdateMeasures;
var relpos, worldPos: TdVector3;
    x, y, d: double;
    i, n: integer;
begin
  case kind of

    skIRSharp: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := Rays[0].Measure.has_measure;
        if has_measure then begin
          value := dist;
        end else begin
          value := 0;
        end;
      end;
    end;

    skCapacitive: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := true;
        if Rays[0].Measure.has_measure then begin
          value := 1
        end else begin
          value := 0;
        end;
      end;
    end;

    skInductive: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := true;
        HitSolid := Rays[0].Measure.HitSolid;
        value := 0;
        if (Rays[0].Measure.has_measure) and
           (HitSolid <> nil) and
           (smMetallic in HitSolid.MatterProperties) then begin
          value := 1
        end;
      end;
    end;

    skSolenoid: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := true;
        HitSolid := Rays[0].Measure.HitSolid;
        value := 0;
        if (Rays[0].Measure.has_measure) and
           (HitSolid <> nil) and
           (smFerroMagnetic in HitSolid.MatterProperties) then begin
          value := 1
        end;
      end;
    end;


    skFloorLine: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := true;
        HitSolid := Rays[0].Measure.HitSolid;
        value := 0;
        n := Rays.Count;
        for i := 0 to n - 1 do begin
          if (Rays[1].Measure.has_measure) and
             (HitSolid <> nil) and
             (HitSolid.kind = skFloor) then begin
            if InsideGLPolygonsTaged(Rays[i].Measure.pos[0], Rays[i].Measure.pos[1], HitSolid.AltGLObj) then begin
              value := value + 1;
            end;
          end;
        end;
        value := value / n;


         //if (Rays[0].Measure.has_measure) and
         //  (HitSolid <> nil) and
         //  (HitSolid.kind = skFloor) then begin
         // if InsideGLPolygonsTaged(Rays[0].Measure.pos[0], Rays[0].Measure.pos[1], HitSolid.AltGLObj) then
         // value := 1
         //end;
      end;
    end;

    skPenTip: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := true;
        HitSolid := Rays[0].Measure.HitSolid;
        value := 0;
        //if (HitSolid <> nil) then Fparams.EditDebug3.Text := HitSolid.ID else Fparams.EditDebug3.Text := '';

        if (Rays[0].Measure.has_measure) and
           (HitSolid <> nil) and
           (assigned(HitSolid.CanvasGLObj)) then begin
          worldPos := Rays[0].Measure.pos;
          dBodyGetPosRelPoint(HitSolid.Body, worldPos[0], worldPos[1], worldPos[2], relpos);
          if abs(relpos[2] - HitSolid.PaintBitmapCorner[2]) < 1e-4 then begin //Test if is the right face
            x := (HitSolid.PaintBitmapCorner[0] + relpos[0])/(2*HitSolid.PaintBitmapCorner[0]) * HitSolid.PaintBitmap.Width;
            y := (HitSolid.PaintBitmapCorner[1] - relpos[1])/(2*HitSolid.PaintBitmapCorner[1]) * HitSolid.PaintBitmap.Height;
            d := 1;
            if assigned(GLObj) then begin // Get the color and size from the GLObject
              HitSolid.PaintBitmap.Canvas.Brush.Color := swapRGB(GLObj.Material.FrontProperties.Diffuse.AsWinColor);
              HitSolid.PaintBitmap.Canvas.pen.Color := swapRGB(GLObj.Material.FrontProperties.Diffuse.AsWinColor);
              if GLObj is TGLCylinder then
                d :=  0.5 * TGLCylinder(GLObj).BottomRadius / HitSolid.PaintBitmapCorner[0] * HitSolid.PaintBitmap.Width;
            end;
            HitSolid.PaintBitmap.Canvas.Ellipse(rect(round(x-d), round(y-d), round(x+d), round(y+d)));
          end;
          value := 1; // Can be used as an on/off presure sensor
        end;
      end;
    end;

    skIMU: begin
    end;

    skRanger2D: begin
      n := MeasuresCount;
      for i := 0 to n - 1 do begin
        Measures[i].value := Rays[i].Measure.dist;
        {if assigned(Rays[i].Measure.HitSolid) then begin
          G := 0;
          Rays[i].Measure.HitSolid.GetColor(R, G, B, A);
          Measures[i].value := Measures[i].value + G / (10000); // TODO
          //Measures[i].value := G/1e6;
        end;}
      end;
    end;

    skRFID: begin
      with Measures[0] do begin
        dist := Rays[0].Measure.dist;
        has_measure := true;
        HitSolid := Rays[0].Measure.HitSolid;
        value := 0;
        if (Rays[0].Measure.has_measure) and
           (HitSolid <> nil) and
           (smRFIDTag in HitSolid.MatterProperties) then begin
          value := StrToIntDef(HitSolid.ID, 0);
        end;
      end;
    end;
  end;
end;

procedure TSensor.NoiseModel;
var v, iv, var_v, m, var_dist: double;
    i, n: integer;
begin
  if not Noise.active then exit;

  if kind = skIRSharp then begin
    if not Measures[0].has_measure then exit;

    iv := Noise.gain * Measures[0].value + Noise.offset;
    if iv <> 0 then
      v := 1 / iv
    else exit;
    var_v := Noise.var_d * Measures[0].value + Noise.var_k;
    m := - Noise.gain * sqr(v);
    if m <> 0 then begin
      var_dist := var_v / sqr(m);
      Measures[0].value := Measures[0].value + RandG(0, sqrt(var_dist));
    end;
  end else begin
    n := MeasuresCount;
    for i := 0 to n - 1 do begin
      v := Noise.offset;
      if Noise.std_a <> 0 then begin
        v := v + RandG(0, Noise.std_a);
      end;
      if Noise.std_p <> 0 then begin
        v := v + RandG(0, Measures[i].value * Noise.std_p);
      end;
      Measures[i].value := Noise.gain * Measures[i].value + v;
    end;
  end;
end;



procedure TSensor.SetColorRGB(RGB: TColor);
begin
  if GLObj = nil then exit;
  GLObj.Material.FrontProperties.Diffuse.AsWinColor := RGB;
end;

function TSensor.SwapRGB(RGB: TColor): TColor;
begin
  result := ((RGB and $FF0000) shr 16)  or (RGB and $00FF00) or ((RGB and $0000FF) shl 16);
end;

procedure TSensor.SetVin(aVin: double);
begin
  Vin := aVin;
end;

{ TSensorList }

procedure TSensorList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;


function TSensorList.Add(ASensor: TSensor): Integer;
begin
  Result := inherited Add(ASensor);
end;

function TSensorList.Extract(Item: TSensor): TSensor;
begin
  Result := TSensor(inherited Extract(Item));
end;

function TSensorList.First: TSensor;
begin
  Result := TSensor(inherited First);
end;

function TSensorList.GetItems(Index: Integer): TSensor;
begin
  Result := TSensor(inherited Items[Index]);
end;

function TSensorList.IndexOf(ASensor: TSensor): Integer;
begin
  Result := inherited IndexOf(ASensor);
end;

procedure TSensorList.Insert(Index: Integer; ASensor: TSensor);
begin
  inherited Insert(Index, ASensor);
end;

function TSensorList.Last: TSensor;
begin
  Result := TSensor(inherited Last);
end;

function TSensorList.Remove(ASensor: TSensor): Integer;
begin
  Result := inherited Remove(ASensor);
end;

procedure TSensorList.SetItems(Index: Integer; ASensor: TSensor);
begin
  inherited Items[Index] := ASensor;
end;

function TSensorList.IndexFromID(aID: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].ID = aID then begin
      result := i;
      exit;
    end;
  end;
end;

{ TAxisTrajList }

procedure TAxisTrajList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;


function TAxisTrajList.Add(AAxisTraj: TAxisTraj): Integer;
begin
  Result := inherited Add(AAxisTraj);
end;

function TAxisTrajList.Extract(Item: TAxisTraj): TAxisTraj;
begin
  Result := TAxisTraj(inherited Extract(Item));
end;

function TAxisTrajList.First: TAxisTraj;
begin
  Result := TAxisTraj(inherited First);
end;

function TAxisTrajList.GetItems(Index: Integer): TAxisTraj;
begin
  Result := TAxisTraj(inherited Items[Index]);
end;

function TAxisTrajList.IndexOf(AAxisTraj: TAxisTraj): Integer;
begin
  Result := inherited IndexOf(AAxisTraj);
end;

procedure TAxisTrajList.Insert(Index: Integer; AAxisTraj: TAxisTraj);
begin
  inherited Insert(Index, AAxisTraj);
end;

function TAxisTrajList.Last: TAxisTraj;
begin
  Result := TAxisTraj(inherited Last);
end;

function TAxisTrajList.Remove(AAxisTraj: TAxisTraj): Integer;
begin
  Result := inherited Remove(AAxisTraj);
end;

procedure TAxisTrajList.SetItems(Index: Integer; AAxisTraj: TAxisTraj);
begin
  inherited Items[Index] := AAxisTraj;
end;

{ TAxisTraj }

constructor TAxisTraj.Create;
begin

end;

destructor TAxisTraj.Destroy;
begin

  inherited;
end;

{ TAxisList }

function TAxisList.Add(AAxis: TAxis): Integer;
begin
  Result := inherited Add(AAxis);
end;

procedure TAxisList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;

function TAxisList.Extract(Item: TAxis): TAxis;
begin
  Result := TAxis(inherited Extract(Item));
end;

function TAxisList.First: TAxis;
begin
  Result := TAxis(inherited First);
end;

function TAxisList.GetItems(Index: Integer): TAxis;
begin
  Result := TAxis(inherited Items[Index]);
end;

function TAxisList.IndexFromAxisID(aID: string; ith: integer): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].ParentLink.ID = aID then begin
      if Items[i] = Items[i].ParentLink.Axis[ith] then begin
        result := i;
        exit;
      end;
    end;
  end;
end;

function TAxisList.IndexOf(AAxis: TAxis): Integer;
begin
  Result := inherited IndexOf(AAxis);
end;

procedure TAxisList.Insert(Index: Integer; AAxis: TAxis);
begin
  inherited Insert(Index, AAxis);
end;

function TAxisList.Last: TAxis;
begin
  Result := TAxis(inherited Last);
end;

function TAxisList.Remove(AAxis: TAxis): Integer;
begin
  Result := inherited Remove(AAxis);
end;

procedure TAxisList.SetItems(Index: Integer; AAxis: TAxis);
begin
  inherited Items[Index] := AAxis;
end;


function TSolidList.IndexFromID(aID: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].ID = aID then begin
      result := i;
      exit;
    end;
  end;
end;

{ TSensorRay }

constructor TSensorRay.Create;
begin
  Measure := TSensorMeasure.Create;
end;

destructor TSensorRay.Destroy;
begin
  Measure.Free;
  if geom <> nil then dGeomDestroy(geom);
  inherited;
end;

procedure TSensorRay.Place(posX, posY, posZ, angX, angY, AngZ: double;  AbsoluteCoords: boolean);
var R: TdMatrix3;
    body: PdxBody;
begin
  RFromZYXRotRelRay(R, angX, angY, AngZ);
  //RFromZYXRotRel(R, angX, angY + pi/2, AngZ);
  body := dGeomGetBody(Geom);
  if assigned(body) then begin // if the sensor ray is attached to a body (a robot sensor)
    if AbsoluteCoords then begin
      dGeomSetOffsetWorldRotation(Geom, R);
      dGeomSetOffsetWorldPosition(Geom, posX, posY, posZ);
    end else begin
      dGeomSetOffsetRotation(Geom, R);
      dGeomSetOffsetPosition(Geom, posX, posY, posZ);
    end;
  end else begin // if the sensor ray is NOT attached to a body (a world sensor)
    //RFromZYXRotRel(R, angX, angY + pi/2, AngZ);
    dGeomSetRotation(Geom, R);
    dGeomSetPosition(Geom, posX, posY, posZ);
  end;
end;



{ TSensorRayList }

function TSensorRayList.Add(ASensorRay: TSensorRay): Integer;
begin
  Result := inherited Add(ASensorRay);
end;

procedure TSensorRayList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;

function TSensorRayList.Extract(Item: TSensorRay): TSensorRay;
begin
  Result := TSensorRay(inherited Extract(Item));
end;

function TSensorRayList.First: TSensorRay;
begin
  Result := TSensorRay(inherited First);
end;

function TSensorRayList.GetItems(Index: Integer): TSensorRay;
begin
  Result := TSensorRay(inherited Items[Index]);
end;

function TSensorRayList.IndexOf(ASensorRay: TSensorRay): Integer;
begin
  Result := inherited IndexOf(ASensorRay);
end;

procedure TSensorRayList.Insert(Index: Integer; ASensorRay: TSensorRay);
begin
  inherited Insert(Index, ASensorRay);
end;

function TSensorRayList.Last: TSensorRay;
begin
  Result := TSensorRay(inherited Last);
end;

function TSensorRayList.Remove(ASensorRay: TSensorRay): Integer;
begin
  Result := inherited Remove(ASensorRay);
end;

procedure TSensorRayList.SetItems(Index: Integer; ASensorRay: TSensorRay);
begin
  inherited Items[Index] := ASensorRay;
end;

{ TSensorMeasure }

constructor TSensorMeasure.Create;
begin

end;

destructor TSensorMeasure.Destroy;
begin

  inherited;
end;

end.

