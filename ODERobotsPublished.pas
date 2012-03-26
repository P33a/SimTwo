unit ODERobotsPublished;

interface

uses Graphics, Types, ODERobots, PathFinder, dynmatrix;

type
  TAxisPoint = record
    pos: double;
    speed: double;
    final_time: double;
  end;

  TAxisState = record
    pos: double;
    vel: double;
    Torque: double;
    Vm: double;
    Im: double;
  end;

  TState2D = record
    x: double;
    y: double;
    angle: double;
  end;

  TPoint3D = record
    x: double;
    y: double;
    z: double;
  end;

  TSpringDef = record
    K: double;
    ZeroPos: double;
  end;

  TMotorPars = record
    Ri: double;
    Li: double;
    Ki: double;
    Vmax: double;
    Imax: double;
    GearRatio: double;
    simple: boolean;
  end;

  TMotorControllerPars = record
    Ki: double;
    Kd: double;
    Kp: double;
    Kf: double;
  end;

  TFrictionDef = record
    Bv: double;
    Fc: double;
    CoulombLimit: double;
  end;

  TRGBAColor = record
    Red: integer;
    Green: integer;
    Blue: integer;
    alpha: integer;
  end;

procedure SetFireScale(x, y, z: double);
procedure SetFirePosition(x, y, z: double);
procedure StartFire;
procedure StopFire;
procedure StopSolidFire(R, I: integer);
procedure StartSolidFire(R, I: integer);

function GetSceneConstant(constantName: string; defaultValue: double): double;

procedure SetRobotPos(R: integer; x, y, z, teta: double);

function GetRobotPos2D(R: integer): TState2D;
function GetRobotVel2D(R: integer): TState2D;

function GetRobotCenterOfMass(R: integer): TPoint3D;

function GetSolidIndex(R: integer; ID: string): integer;

procedure SetSolidMass(R, i: integer; nmass: double);
function GetSolidMass(R, i: integer): double;
function GetSolidCenterOfMass(R, i: integer): TPoint3D;

procedure SetSolidPos(R, i: integer; x, y, z: double);
procedure SetSolidPosMat(R, i: integer; P: Matrix);
procedure SetSolidRotationMat(R, i: integer; Rot: Matrix);

function GetSolidPos(R, i: integer): TPoint3D;
function GetSolidLinearVel(R, i: integer): TPoint3D;


//Matrix versions
function GetSolidPosMat(R, i: integer): Matrix;
function GetSolidLinearVelMat(R, i: integer): Matrix;
function GetSolidRotMat(R, i: integer): Matrix;
function GetSolidAgularVelMat(R, i: integer): Matrix;


// Flat Functions
function GetRobotX(R: integer): double;
function GetRobotY(R: integer): double;
function GetRobotTheta(R: integer): double;

function GetRobotVx(R: integer): double;
function GetRobotVy(R: integer): double;
function GetRobotW(R: integer): double;

// Flat Regular

function GetSolidX(R, i: integer): double;
function GetSolidY(R, i: integer): double;
function GetSolidZ(R, i: integer): double;
function GetSolidTheta(R, i: integer): double;

function GetSolidVx(R, i: integer): double;
function GetSolidVy(R, i: integer): double;
function GetSolidVz(R, i: integer): double;

function GetSolidColor(R, i: integer): TRGBAColor;
procedure SetSolidColor(R, I: integer; Red, Green, Blue: byte);
function GetSolidCanvas(R, i: integer): TCanvas;
procedure SolidCanvasClear(R, i: integer);
//function GetSolidBitmap(R, i: integer): TBitmap;
//procedure SolidCanvasDrawText(R, i: integer; x, y: integer; txt: string);

procedure SetSolidForce(R, i: integer; Fx, Fy, Fz: double);

function GetGlobalSensorIndex(ID: string): integer;
function GetGlobalSensorVal(i: integer): double;

function GetSensorIndex(R: integer;ID: string): integer;
function GetSensorVal(R, i: integer): double;
procedure SetSensorColor(R, i: integer; Red, Green, Blue: byte);

function GetThingIndex(ID: string): integer;
function GetThingsCount: integer;

function GetThingColor(T, c: integer): TRGBAColor;
procedure SetThingColor(T, c: integer; Red, Green, Blue: byte);

function GetThingPos(T: integer): TPoint3D;
procedure SetThingPos(T: integer; x, y, z: double);

function GetThingRotMat(T: integer): Matrix;
procedure SetThingRotationMat(T: integer; Rot: Matrix);

function GetThingAgularVelMat(T: integer): Matrix;


function GetThingSize(T: integer): TPoint3D;
procedure SetThingSize(T: integer; x, y, z: double);

function AddThingBox(ID: string; mass, posx, posY, posZ, sizeX, sizeY, sizeZ: double; rgb24: integer): integer;
function AddThingSphere(ID: string; mass, posX, posY, posZ, radius: double; rgb24: integer): integer;
function AddThingCylinder(ID: string; mass, posx, posY, posZ, radius, len: double; rgb24: integer): integer;
function DeleteThing(ID: string): integer;
procedure MeshThing(ID, MeshFile, MeshShadowFile: string; MeshScale: double; MeshCastsShadows: boolean);

procedure SetThingForce(T: integer; Fx, Fy, Fz: double);
procedure SetThingForceAtRelPos(T: integer; Fx, Fy, Fz, Px, Py, Pz: double);
procedure SetThingRelForce(T: integer; Fx, Fy, Fz: double);
procedure SetThingRelForceAtRelPos(T: integer; Fx, Fy, Fz, Px, Py, Pz: double);

procedure SetThingSurfaceMu(T: integer; mu, mu2: double);

function GetThingSpeed(T: integer): TPoint3D;
procedure SetThingSpeed(T: integer; vx, vy, vz: double);
function GetThingAngularVel(T: integer): TPoint3D;

procedure ClearThings;


function GetShellPos(R, i: integer): TPoint3D;
procedure SetShellColor(R, i: integer; Red, Green, Blue: byte);
function GetShellColor(R, i: integer): TRGBAColor;

function GetObstacleIndex(ID: string): integer;
procedure SetObstacleColor(I: integer; Red, Green, Blue: byte);
function GetObstacleColor(I: integer): TRGBAColor;


function GetAxisOdo(R, i: integer): integer;

function GetAxisState(R, i: integer): TAxisState;
function GetAxisPos(R, i: integer): double;
function GetAxisSpeed(R, i: integer): double;
function GetAxisTorque(R, i: integer): double;
function GetAxisI(R, i: integer): double;
function GetAxisU(R, i: integer): double;
function GetAxisUIPower(R, i: integer): double;
function GetAxisTWPower(R, i: integer): double;

function GetAxisStateRef(R, i: integer): TAxisState;
function GetAxisPosRef(R, i: integer): double;
function GetAxisSpeedRef(R, i: integer): double;

function GetAxisMotorSpeed(R, i: integer): double;
function GetAxisMotorPos(R, i: integer): double;
function GetAxisMotorPosDeg(R, i: integer): double;

function GetAxisEnergy(R, i: integer): double;
procedure ResetAxisEnergy(R, i: integer);

procedure SetMotorPars(R, i: integer; aMotorPars: TMotorPars);
function GetMotorPars(R, i: integer): TMotorPars;

procedure SetMotorControllerPars(R, i: integer; nKi, nKd, nKp, nKf: double);
function GetMotorControllerPars(R, i: integer): TMotorControllerPars;

procedure SetMotorControllerMode(R, i: integer; newMode: string);
function GetMotorControllerMode(R, i: integer): string;

procedure SetMotorActive(R, i: integer; nState: boolean);
function IsMotorActive(R, i: integer): boolean;

procedure SetFrictionDef(R, i: integer; nBv, nFc, nCoulombLimit: double);
function GetFrictionDef(R, i: integer): TFrictionDef;

procedure SetBeltSpeed(R, i: integer; nSpeed: double);
function GetBeltSpeed(R, i: integer): double;

procedure SetAxisSpring(R, i: integer; k, ZeroPos: double);

procedure SetAxisStateRef(R, i: integer; aState: TAxisState);
procedure SetAxisPosRef(R, i: integer; aPos: double);
procedure SetAxisSpeedRef(R, i: integer; aSpeed: double);
procedure SetAxisVoltageRef(R, i: integer; aVoltage: double);
procedure SetAxisTorqueRef(R, i: integer; aTorque: double);

function GetAxisPosDeg(R, i: integer): double;
function GetAxisSpeedDeg(R, i: integer): double;

function GetAxisPosRefDeg(R, i: integer): double;
function GetAxisSpeedRefDeg(R, i: integer): double;

function Deg(angle: double): double;
function Rad(angle: double): double;

function GetAxisIndex(R: integer; ID: string; i: integer): integer;

procedure LoadJointWayPoints(r: integer; JointPointsFileName: string);
procedure SaveJointWayPoints(r: integer; JointPointsFileName: string);
//function CountAxisWayPoints(R, i: integer): integer;
//function GetAxisWayPoint(R, i, idx: integer): TAxisPoint;
function CountJointWayPoints(R, i: integer): integer;
function GetJointWayPoint(R, i, idx: integer): TAxisPoint;
procedure SetJointWayPoint(R, i, idx: integer; apos, aspeed, atime: double);

function GetAxisTrajPoint(R, i, idx: integer): TAxisPoint;

procedure SetAxisTrajPoint(R, i, idx: integer; LP: TAxisPoint);
procedure AddAxisTrajPoint(R, i: integer; LP: TAxisPoint);
procedure DelAxisTrajPoint(R, i, idx: integer);
function CountAxisTrajPoints(R, i: integer): integer;
procedure ClearAxisTrajPoints(R, i: integer);

procedure SetTrailColor(T: integer; Red, Green, Blue: byte);
procedure AddTrailNode(T: integer; x, y, z: double);
procedure DelTrailNode(T: integer);
procedure ClearTrail(T: integer);




{

getsolidindex
getsolidmass

function GetLinkWayPointPos(R, i, j: integer): double;
function GetLinkWayPointTime(R, i, j: integer): double;
function GetLinkWayPointW(R, i, j: integer): double;

function GetLinkTrajPointPos(R, i, j: integer): double;
function GetLinkTrajPointTime(R, i, j: integer): double;
function GetLinkTrajPointW(R, i, j: integer): double;
}

{  TAxis = class
    Friction: TFriction;
    Motor: TMotor;
    TrajectPoints, WayPoints: TAxisTrajList;
    Odo: TOdoState;
    ref: TAxisInputs;
    torque: double;
}


{  TRobot = class
    Solids: TSolidList;
    Links: TSolidLinkList;

    MainBody: TSolid;
    Shells: TSolidList;
    Wheels: TWheelList;
    IRSensors: TSensorList;
    Kind: TRobotKind;
    SamplesCount, DecPeriodSamples: integer;
    Name: string;
    ForceMoved: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetXYZTeta(new_x, new_y, new_z, new_teta: double);}

function KeyPressed(k: integer): Boolean;

const
  // Virtual Keys, Standard Set
  VK_LBUTTON = 1;
  VK_RBUTTON = 2;
  VK_CANCEL = 3;
  VK_MBUTTON = 4;  // NOT contiguous with L & RBUTTON
  VK_BACK = 8;
  VK_TAB = 9;
  VK_CLEAR = 12;
  VK_RETURN = 13;
  VK_SHIFT = $10;
  VK_CONTROL = 17;
  VK_MENU = 18;
  VK_PAUSE = 19;
  VK_CAPITAL = 20;
  VK_KANA = 21;
  VK_HANGUL = 21;
  VK_JUNJA = 23;
  VK_FINAL = 24;
  VK_HANJA = 25;
  VK_KANJI = 25;
  VK_CONVERT = 28;
  VK_NONCONVERT = 29;
  VK_ACCEPT = 30;
  VK_MODECHANGE = 31;
  VK_ESCAPE = 27;
  VK_SPACE = $20;
  VK_PRIOR = 33;
  VK_NEXT = 34;
  VK_END = 35;
  VK_HOME = 36;
  VK_LEFT = 37;
  VK_UP = 38;
  VK_RIGHT = 39;
  VK_DOWN = 40;
  VK_SELECT = 41;
  VK_PRINT = 42;
  VK_EXECUTE = 43;
  VK_SNAPSHOT = 44;
  VK_INSERT = 45;
  VK_DELETE = 46;
  VK_HELP = 47;
  //VK_0 thru VK_9 are the same as ASCII '0' thru '9' ($30 - $39)
  //VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' ($41 - $5A)
  VK_LWIN = 91;
  VK_RWIN = 92;
  VK_APPS = 93;
  VK_NUMPAD0 = 96;
  VK_NUMPAD1 = 97;
  VK_NUMPAD2 = 98;
  VK_NUMPAD3 = 99;
  VK_NUMPAD4 = 100;
  VK_NUMPAD5 = 101;
  VK_NUMPAD6 = 102;
  VK_NUMPAD7 = 103;
  VK_NUMPAD8 = 104;
  VK_NUMPAD9 = 105;
  VK_MULTIPLY = 106;
  VK_ADD = 107;
  VK_SEPARATOR = 108;
  VK_SUBTRACT = 109;
  VK_DECIMAL = 110;
  VK_DIVIDE = 111;
  VK_F1 = 112;
  VK_F2 = 113;
  VK_F3 = 114;
  VK_F4 = 115;
  VK_F5 = 116;
  VK_F6 = 117;
  VK_F7 = 118;
  VK_F8 = 119;
  VK_F9 = 120;
  VK_F10 = 121;
  VK_F11 = 122;
  VK_F12 = 123;
  VK_F13 = 124;
  VK_F14 = 125;
  VK_F15 = 126;
  VK_F16 = 127;
  VK_F17 = 128;
  VK_F18 = 129;
  VK_F19 = 130;
  VK_F20 = 131;
  VK_F21 = 132;
  VK_F22 = 133;
  VK_F23 = 134;
  VK_F24 = 135;
  VK_NUMLOCK = 144;
  VK_SCROLL = 145;
  //VK_L & VK_R - left and right Alt, Ctrl and Shift virtual keys.
  //Used only as parameters to GetAsyncKeyState() and GetKeyState().
  //No other API or message will distinguish left and right keys in this way.
  VK_LSHIFT = 160;
  VK_RSHIFT = 161;
  VK_LCONTROL = 162;
  VK_RCONTROL = 163;
  VK_LMENU = 164;
  VK_RMENU = 165;
  VK_PROCESSKEY = 229;
  VK_ATTN = 246;
  VK_CRSEL = 247;
  VK_EXSEL = 248;
  VK_EREOF = 249;
  VK_PLAY = 250;
  VK_ZOOM = 251;
  VK_NONAME = 252;
  VK_PA1 = 253;
  VK_OEM_CLEAR = 254;


implementation

uses Math, Viewer, odeimport, utils, Keyboard, GLObjects, SysUtils,
  Classes;

function Deg(angle: double): double;
begin
  Result := RadToDeg(angle);
end;

function Rad(angle: double): double;
begin
  Result := DegToRad(angle);
end;


function GetSceneConstant(constantName: string; defaultValue: double): double;
var idx: integer;
begin
  idx := WorldODE.Parser.VarsList.IndexOf(uppercase(constantName));
  if idx >= 0 then begin
    result:= pdouble(WorldODE.Parser.VarsList.Objects[idx])^;
  end else begin
    result := defaultValue;
  end;
end;


procedure SetRobotPos(R: integer; x, y, z, teta: double);
begin
  WorldODE.Robots[R].SetXYZTeta(x, y, z, teta);
end;


function GetRobotPos2D(R: integer): TState2D;
var v1, v2: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.angle := 0;

  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetPosition(MainBody.Body)^;
    Result.x := v1[0];
    Result.y := v1[1];
    dBodyGetRelPointPos(MainBody.Body, 1,0,0, v2);
    Result.angle := atan2(v2[1]-v1[1], v2[0]-v1[0]);
  end;
end;



function GetRobotVel2D(R: integer): TState2D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.angle := 0;

  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetLinearVel(MainBody.Body)^;
    Result.x := v1[0];
    Result.y := v1[1];
    v1 := dBodyGetAngularVel(MainBody.Body)^;
    Result.angle := v1[2];
  end;
end;


function GetRobotCenterOfMass(R: integer): TPoint3D;
var v0: TdVector3;
begin
  v0 := WorldODE.Robots[R].CalcCenterOfMass;
  result.x := v0[0];
  result.y := v0[1];
  result.z := v0[2];
end;

function TdVector3ToTPoint3D(v1: TdVector3): TPoint3D;
begin
  Result.x := v1[0];
  Result.y := v1[1];
  Result.z := v1[2];
end;


function GetThingPos(T: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  with WorldODE.Things[T] do begin
    if Body = nil then exit;
    v1 := dBodyGetPosition(Body)^;
    Result.x := v1[0];
    Result.y := v1[1];
    Result.z := v1[2];
  end;
end;

procedure SetThingPos(T: integer; x, y, z: double);
begin
  WorldODE.Things[T].SetLinSpeed(0, 0, 0);
  WorldODE.Things[T].SetPosition(x, y, z);
end;

function GetThingSize(T: integer): TPoint3D;
begin
  WorldODE.Things[T].GetSize(result.x, result.y, result.z);
end;

procedure SetThingSize(T: integer; x, y, z: double);
begin
  WorldODE.Things[T].SetSize(x, y, z);
end;

function GetThingRotMat(T: integer): Matrix;
var pR: PdMatrix3;
    row, col: integer;
begin
  result := Mzeros(3,3);

  with WorldODE.Things[T] do begin
    if Body = nil then exit;
    pR := dBodyGetRotation(Body);
    for row := 0 to 2 do begin
      for col := 0 to 2 do begin
        Msetv(Result, row, col, pR^[4*row+col]);
      end;
    end;
  end;
end;

procedure SetThingRotationMat(T: integer; Rot: Matrix);
var RM: TdMatrix3;
    row, col: integer;
begin
  with WorldODE.Things[T] do begin
    if Body = nil then exit;

    for row := 0 to 2 do begin
      for col := 0 to 2 do begin
        RM[4*row+col] := MGetv(Rot, row, col);
      end;
    end;

    dBodySetRotation(Body, RM);
  end;
end;


function GetThingAgularVelMat(T: integer): Matrix;
var v: PdVector3;
begin
  result := Mzeros(3,1);

  with WorldODE.Things[T] do begin
    if Body = nil then exit;
    v := dBodyGetAngularVel(Body);
    Msetv(Result, 0, 0, v^[0]);
    Msetv(Result, 1, 0, v^[1]);
    Msetv(Result, 2, 0, v^[2]);
  end;
end;



function GetThingIndex(ID: string): integer;
begin
  result := WorldODE.Things.IndexFromID(ID);
end;

function GetThingsCount: integer;
begin
  result := WorldODE.Things.Count;
end;

function GetSolidIndex(R: integer; ID: string): integer;
begin
  result := WorldODE.Robots[R].Solids.IndexFromID(ID);
end;


procedure SetSolidMass(R, i: integer; nmass: double);
var Bmass: TdMass;
begin
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    dBodygetMass(Body, Bmass);
    Bmass.mass := nmass;
    dBodySetMass(Body, @Bmass);
  end;
end;


function GetSolidMass(R, i: integer): double;
var Bmass: TdMass;
begin
  result := 0;

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    dBodyGetMass(Body, Bmass);
    Result := Bmass.mass;
  end;
end;

function GetSolidCenterOfMass(R, i: integer): TPoint3D;
var mass: TdMass;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    dBodyGetMass(Body, mass);

    Result.x := mass.c[0];
    Result.y := mass.c[1];
    Result.z := mass.c[2];
  end;
end;


procedure SetSolidPos(R, i: integer; x, y, z: double);
begin
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    dBodySetPosition(Body, x, y, z);
  end;
end;

procedure SetSolidPosMat(R, i: integer; P: Matrix);
begin
  SetSolidPos(R, i, Mgetv(P, 0, 0), Mgetv(P, 1, 0), Mgetv(P, 2, 0));
end;

procedure SetSolidRotationMat(R, i: integer; Rot: Matrix);
var RM: TdMatrix3;
    row, col: integer;
begin
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;

    for row := 0 to 2 do begin
      for col := 0 to 2 do begin
        RM[4*row+col] := MGetv(Rot, row, col);
      end;
    end;

    dBodySetRotation(Body, RM);
  end;
end;


function GetSolidPos(R, i: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetPosition(Body)^;
    Result.x := v1[0];
    Result.y := v1[1];
    Result.z := v1[2];
  end;
end;


function GetSolidLinearVel(R, i: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetLinearVel(Body)^;
    Result.x := v1[0];
    Result.y := v1[1];
    Result.z := v1[2];
  end;
end;

function GetSolidPosMat(R, i: integer): Matrix;
var v: PdVector3;
begin
  result := Mzeros(3,1);

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v := dBodyGetPosition(Body);
    Msetv(Result, 0, 0, v^[0]);
    Msetv(Result, 1, 0, v^[1]);
    Msetv(Result, 2, 0, v^[2]);
  end;
end;


function GetSolidLinearVelMat(R, i: integer): Matrix;
var v: PdVector3;
begin
  result := Mzeros(3,1);

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v := dBodyGetLinearVel(Body);
    Msetv(Result, 0, 0, v^[0]);
    Msetv(Result, 1, 0, v^[1]);
    Msetv(Result, 2, 0, v^[2]);
  end;
end;


function GetSolidAgularVelMat(R, i: integer): Matrix;
var v: PdVector3;
begin
  result := Mzeros(3,1);

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v := dBodyGetAngularVel(Body);
    Msetv(Result, 0, 0, v^[0]);
    Msetv(Result, 1, 0, v^[1]);
    Msetv(Result, 2, 0, v^[2]);
  end;
end;


function GetSolidRotMat(R, i: integer): Matrix;
var pR: PdMatrix3;
    row, col: integer;
begin
  result := Mzeros(3,3);

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    pR := dBodyGetRotation(Body);
    for row := 0 to 2 do begin
      for col := 0 to 2 do begin
        Msetv(Result, row, col, pR^[4*row+col]);
      end;
    end;
  end;
end;



function GetRobotX(R: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetPosition(MainBody.Body)^;
    Result := v1[0];
  end;
end;

function GetRobotY(R: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetPosition(MainBody.Body)^;
    Result := v1[1];
  end;
end;


function GetRobotTheta(R: integer): double;
var v1, v2: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetPosition(MainBody.Body)^;
    dBodyGetRelPointPos(MainBody.Body, 1,0,0, v2);
    Result := atan2(v2[1]-v1[1], v2[0]-v1[0]);
  end;
end;


function GetRobotVx(R: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetLinearVel(MainBody.Body)^;
    Result := v1[0];
  end;
end;

function GetRobotVy(R: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetLinearVel(MainBody.Body)^;
    Result := v1[1];
  end;
end;


function GetRobotW(R: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R] do begin
    if MainBody = nil then exit;
    if MainBody.Body = nil then exit;
    v1 := dBodyGetAngularVel(MainBody.Body)^;
    Result := v1[2];
  end;
end;


function GetSolidX(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetPosition(Body)^;
    Result := v1[0];
  end;
end;

function GetSolidY(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetPosition(Body)^;
    Result := v1[1];
  end;
end;

function GetSolidZ(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetPosition(Body)^;
    Result := v1[2];
  end;
end;

function GetSolidTheta(R, i: integer): double;
var v1, v2: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetPosition(Body)^;
    dBodyGetRelPointPos(Body, 1,0,0, v2);
    Result := atan2(v2[1]-v1[1], v2[0]-v1[0]);
  end;
end;

function GetSolidVx(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetLinearVel(Body)^;
    Result := v1[0];
  end;
end;


function GetSolidVy(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetLinearVel(Body)^;
    Result := v1[1];
  end;
end;

function GetSolidVz(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    v1 := dBodyGetLinearVel(Body)^;
    Result := v1[2];
  end;
end;


function GetGlobalSensorIndex(ID: string): integer;
begin
  result := WorldODE.Sensors.IndexFromID(ID);
end;

function GetGlobalSensorVal(i: integer): double;
begin
  result := WorldODE.Sensors[i].measures[0].value;
end;

function GetSensorIndex(R: integer;ID: string): integer;
begin
  result := WorldODE.Robots[R].Sensors.IndexFromID(ID);
end;

function GetSensorVal(R, i: integer): double;
begin
  result := -1;
  if WorldODE.Robots[r].Sensors[i].measures[0].has_measure then
    result := WorldODE.Robots[r].Sensors[i].measures[0].value;
end;

procedure SetSensorColor(R, i: integer; Red, Green, Blue: byte);
begin
  WorldODE.Robots[R].Sensors[i].SetColor(Red/255, Green/255, Blue/255);
end;


function GetThingColor(T, c: integer): TRGBAColor;
begin
  with result do WorldODE.Things[T].GetColor(Red, Green, Blue, Alpha);
end;

procedure SetThingColor(T, c: integer; Red, Green, Blue: byte);
begin
  WorldODE.Things[T].SetColor(Red/255, Green/255, Blue/255);
end;


function GetSolidColor(R, i: integer): TRGBAColor;
//var R, G, B, A: byte;
begin
  with result do WorldODE.Robots[R].Solids[i].GetColor(Red, Green, Blue, Alpha);
end;

procedure SetSolidColor(R, I: integer; Red, Green, Blue: byte);
begin
  WorldODE.Robots[R].Solids[i].SetColor(Red/255, Green/255, Blue/255);
end;

function GetSolidCanvas(R, i: integer): TCanvas;
begin
  result := WorldODE.Robots[R].Solids[i].PaintBitmap.Canvas;
end;

{function GetSolidBitmap(R, i: integer): TBitmap;
begin
  result := WorldODE.Robots[R].Solids[i].PaintBitmap;
end;


procedure SolidCanvasDrawText(R, i: integer; x, y: integer; txt: string);
begin
  WorldODE.Robots[R].Solids[i].PaintBitmap.Canvas.TextOut(x, y, txt);
end;}

procedure SolidCanvasClear(R, i: integer);
begin
  with WorldODE.Robots[R].Solids[i].PaintBitmap do begin
    Canvas.FillRect(Rect(0,0, Width, Height) );
  end;
end;

procedure SetSolidForce(R, i: integer; Fx, Fy, Fz: double);
begin
  WorldODE.Robots[r].Solids[i].SetForce(Fx, Fy, Fz);
end;

function GetAxisOdo(R, i: integer): integer;
begin
  result := WorldODE.Robots[r].Axes[i].Odo.Value;
end;


function GetAxisState(R, i: integer): TAxisState;
begin
  with WorldODE.Robots[r].Axes[i] do begin
    result.pos := GetPos();
    //result.vel := GetSpeed();
    result.vel := filt_speed;
    result.Im := Motor.Im;
    result.Vm := Motor.voltage;
    result.Torque := torque;
  end;
end;

function GetAxisPos(R, i: integer): double;
begin
  result := GetAxisState(R, i).pos;
end;

function GetAxisSpeed(R, i: integer): double;
begin
  result := GetAxisState(R, i).vel;
end;

function GetAxisTorque(R, i: integer): double;
begin
  result := GetAxisState(R, i).Torque;
end;

function GetAxisI(R, i: integer): double;
begin
  result := GetAxisState(R, i).Im;
end;

function GetAxisU(R, i: integer): double;
begin
  result := GetAxisState(R, i).Vm;
end;

function GetAxisStateRef(R, i: integer): TAxisState;
begin
  with WorldODE.Robots[r].Axes[i] do begin
    result.pos := ref.theta;
    result.vel := ref.w;
  end;
end;


function GetAxisPosRef(R, i: integer): double;
begin
  result := GetAxisStateRef(R, i).pos;
end;


function GetAxisSpeedRef(R, i: integer): double;
begin
  result := GetAxisStateRef(R, i).vel;
end;


function GetAxisMotorSpeed(R, i: integer): double;
begin
  with WorldODE.Robots[r].Axes[i].Motor do begin
    result := w;
  end;
end;

function GetAxisMotorPos(R, i: integer): double;
begin
  with WorldODE.Robots[r].Axes[i].Motor do begin
    result := teta;
  end;
end;

function GetAxisMotorPosDeg(R, i: integer): double;
begin
  with WorldODE.Robots[r].Axes[i].Motor do begin
    result := deg(teta);
  end;
end;

function GetAxisSpring(R, i: integer): TSpringDef;
begin
  with WorldODE.Robots[r].Axes[i] do begin
    result.k := Spring.K;
    result.ZeroPos := Spring.ZeroPos;
  end;
end;


function GetAxisEnergy(R, i: integer): double;
begin
  result := WorldODE.Robots[r].Axes[i].Motor.EnergyDrain;
end;

function GetFrictionDef(R, i: integer): TFrictionDef;
begin
  with WorldODE.Robots[r].Axes[i] do begin
    result.Bv := Friction.Bv;
    result.Fc := Friction.Fc;
//    result.CoulombLimit := Friction.CoulombLimit;
  end;
end;

function GetBeltSpeed(R, i: integer): double;
begin
  result := WorldODE.Robots[r].Solids[i].BeltSpeed;
end;


function IsMotorActive(R, i: integer): boolean;
begin
  result := WorldODE.Robots[R].Axes[i].Motor.active;
end;

function GetMotorControllerPars(R, i: integer): TMotorControllerPars;
begin
  with WorldODE.Robots[r].Axes[i].Motor.Controller do begin
    result.Ki := Ki;
    result.Kd := Kd;
    result.Kp := Kp;
    result.Kf := Kf;
  end;
end;

function GetMotorControllerMode(R, i: integer): string;
begin
  with WorldODE.Robots[r].Axes[i].Motor.Controller do begin
    // cmPIDPosition, cmPIDSpeed, cmState
    result :=  ControlModeNames[ControlMode];
  end;
end;


procedure SetAxisSpring(R, i: integer; k, ZeroPos: double);
begin
  with WorldODE.Robots[r].Axes[i] do begin
    Spring.K := k;
    Spring.ZeroPos := ZeroPos;
  end;
end;


procedure ResetAxisEnergy(R, i: integer);
begin
  WorldODE.Robots[r].Axes[i].Motor.EnergyDrain := 0;
end;


procedure SetFrictionDef(R, i: integer; nBv, nFc, nCoulombLimit: double);
begin
  with WorldODE.Robots[r].Axes[i].Friction do begin
    Bv := nBv;
    Fc := nFc;
//    CoulombLimit := nCoulombLimit;
  end;
end;


procedure SetBeltSpeed(R, i: integer; nSpeed: double);
begin
  WorldODE.Robots[r].Solids[i].BeltSpeed := nSpeed;
end;


procedure SetMotorPars(R, i: integer; aMotorPars: TMotorPars);
begin
  with WorldODE.Robots[r].Axes[i].Motor do begin
    Ri := aMotorPars.Ri;
    Li := aMotorPars.Li;
    Ki := aMotorPars.Ki;
    Vmax := aMotorPars.Vmax;
    Imax := aMotorPars.Imax;
    GearRatio := aMotorPars.GearRatio;
    simple := aMotorPars.simple;
  end;
end;

function GetMotorPars(R, i: integer): TMotorPars;
begin
  with WorldODE.Robots[r].Axes[i].Motor do begin
    result.Ri := Ri;
    result.Li := Li;
    result.Ki := Ki;
    result.Vmax := Vmax;
    result.Imax := Imax;
    result.GearRatio := GearRatio;
    result.simple := simple;
  end;
end;


procedure SetMotorActive(R, i: integer; nState: boolean);
begin
  WorldODE.Robots[r].Axes[i].Motor.active := nState;
end;


procedure SetMotorControllerPars(R, i: integer; nKi, nKd, nKp, nKf: double);
begin
  with WorldODE.Robots[r].Axes[i].Motor.Controller do begin
    Ki := nKi;
    Kd := nKd;
    Kp := nKp;
    Kf := nKf;
    Sek := 0;
  end;
end;

procedure SetMotorControllerMode(R, i: integer; newMode: string);
var s: string;
begin
  s := LowerCase(newMode);
  with WorldODE.Robots[r].Axes[i].Motor.Controller do begin
    // cmPIDPosition, cmPIDSpeed, cmState
    if s = 'pidposition' then begin
      ControlMode := cmPIDPosition;
    end else if s = 'pidspeed' then begin
      ControlMode := cmPIDSpeed;
    end else if (s = 'state') or (s = 'statefeedback') then begin
      ControlMode := cmState;
    end else raise Exception.Create('Invalid SetMotorControllerMode parameter: ' + newMode);
    Sek := 0;
  end;
end;


procedure SetAxisStateRef(R, i: integer; aState: TAxisState);
begin
  with WorldODE.Robots[r].Axes[i] do begin
    ref.theta := aState.pos;
    ref.w := aState.vel;
  end;
end;


procedure SetAxisPosRef(R, i: integer; aPos: double);
begin
  WorldODE.Robots[r].Axes[i].ref.theta := aPos;
end;


procedure SetAxisSpeedRef(R, i: integer; aSpeed: double);
begin
  WorldODE.Robots[r].Axes[i].ref.w := aSpeed;
end;

procedure SetAxisVoltageRef(R, i: integer; aVoltage: double);
begin
  WorldODE.Robots[r].Axes[i].ref.volts := aVoltage;
end;

procedure SetAxisTorqueRef(R, i: integer; aTorque: double);
begin
  WorldODE.Robots[r].Axes[i].ref.Torque := aTorque;
end;


function GetAxisPosDeg(R, i: integer): double;
begin
  result := RadToDeg(GetAxisPos(R,i));
end;

function GetAxisSpeedDeg(R, i: integer): double;
begin
  result := RadToDeg(GetAxisSpeed(R,i));
end;

function GetAxisPosRefDeg(R, i: integer): double;
begin
  result := RadToDeg(GetAxisPosRef(R,i));
end;

function GetAxisSpeedRefDeg(R, i: integer): double;
begin
  result := RadToDeg(GetAxisSpeedRef(R,i));
end;

function GetAxisUIPower(R, i: integer): double;
begin
  result := WorldODE.Robots[r].Axes[i].Motor.PowerDrain;
end;

function GetAxisTWPower(R, i: integer): double;
begin
  result := GetAxisTorque(R,i) * GetAxisSpeed(R,i);
end;


function GetAxisIndex(R: integer; ID: string; i: integer): integer;
begin
  result := WorldODE.Robots[R].Axes.IndexFromAxisID(ID, i);
end;

procedure LoadJointWayPoints(r: integer; JointPointsFileName: string);
var i: integer;
begin
  // clear actual waypoints
  WorldODE.Robots[r].AxesWayPointsIDs.Clear;
  for i := 0 to WorldODE.Robots[r].Axes.Count-1 do begin
    WorldODE.Robots[r].Axes[i].WayPoints.ClearAll
  end;
  // Load new ones
  WorldODE.LoadJointWayPointsFromXML(JointPointsFileName, r);
end;


procedure SaveJointWayPoints(r: integer; JointPointsFileName: string);
begin
  WorldODE.SaveJointWayPointsToXML(JointPointsFileName, r);
end;


function CountJointWayPoints(R, i: integer): integer;
begin
  result := WorldODE.Robots[r].Axes[i].WayPoints.Count;
end;


function GetJointWayPoint(R, i, idx: integer): TAxisPoint;
begin
  with WorldODE.Robots[r].Axes[i].WayPoints[idx] do begin
    result.pos := pos;
    result.speed := speed;
    result.final_time := t;
  end;
end;

procedure SetJointWayPoint(R, i, idx: integer; apos, aspeed, atime: double);
begin
  with WorldODE.Robots[r].Axes[i].WayPoints[idx] do begin
    pos := apos;
    speed := aspeed;
    t := atime;
  end;
end;



function GetAxisTrajPoint(R, i, idx: integer): TAxisPoint;
begin
  with WorldODE.Robots[r].Axes[i].TrajectPoints[idx] do begin
    result.pos := pos;
    result.speed := speed;
    result.final_time := t;
  end;
end;

procedure SetAxisTrajPoint(R, i, idx: integer; LP: TAxisPoint);
begin
  with WorldODE.Robots[r].Axes[i].TrajectPoints[idx] do begin
    pos := LP.pos;
    speed := LP.speed;
    t := LP.final_time;
  end;
end;


procedure AddAxisTrajPoint(R, i: integer; LP: TAxisPoint);
var AxisTraj: TAxisTraj;
begin
  with WorldODE.Robots[r].Axes[i] do begin
    AxisTraj := TAxisTraj.Create;
    with AxisTraj do begin
      pos := LP.pos;
      speed := LP.speed;
      t := LP.final_time;
    end;
    TrajectPoints.Add(AxisTraj);
  end;
end;


procedure DelAxisTrajPoint(R, i, idx: integer);
begin
  with WorldODE.Robots[r].Axes[i] do begin
    if (idx < 0) or (idx >= TrajectPoints.Count) then exit;
    TrajectPoints.Remove(TrajectPoints[idx]);
  end;
end;


function CountAxisTrajPoints(R, i: integer): integer;
begin
  result := WorldODE.Robots[r].Axes[i].TrajectPoints.Count;
end;

procedure ClearAxisTrajPoints(R, i: integer);
begin
  WorldODE.Robots[r].Axes[i].TrajectPoints.ClearAll;
end;

function KeyPressed(k: integer): Boolean;
begin
  if IsKeyDown(VK_SHIFT) or IsKeyDown(VK_CONTROL) or IsKeyDown(VK_MENU) then begin
    result := False;
  end else
    result := IsKeyDown(TVirtualKeyCode(k));
end;

procedure SetFireScale(x, y, z: double);
begin
  FViewer.GLDummyCFire.Scale.SetVector(x, y, z);
end;


procedure SetFirePosition(x, y, z: double);
begin
  FViewer.GLDummyCFire.Position.SetPoint(x, y, z);
end;


procedure StartFire;
begin
  FViewer.GLFireFXManager.Disabled := false;
end;


procedure StopFire;
begin
  FViewer.GLFireFXManager.Disabled := true;
end;


procedure StartSolidFire(R, I: integer);
begin
  FViewer.GLFireFXManager.Reference := WorldODE.Robots[R].Solids[i].GLObj;
  FViewer.GLFireFXManager.Disabled := false;
end;


procedure StopSolidFire(R, I: integer);
begin
  FViewer.GLFireFXManager.Disabled := true;
  FViewer.GLFireFXManager.Reference := nil;
end;



procedure SetTrailColor(T: integer; Red, Green, Blue: byte);
begin
  (FViewer.GLDTrails.Children[T] as TGLLines).linecolor.SetColor(red/255, green/255, blue/255);
end;


procedure AddTrailNode(T: integer; x, y, z: double);
begin
  FViewer.AddTrailNode(T, x, y, z);
end;


procedure DelTrailNode(T: integer);
begin
  FViewer.DelTrailNode(T);
end;


procedure ClearTrail(T: integer);
begin
  (FViewer.GLDTrails.Children[T] as TGLLines).Nodes.Clear;
end;

type
  TThingType = (ttBox, ttSphere, ttCylinder);

function AddThing(ThingType: TThingType; ID: string; mass, posx, posY, posZ, sx, sy, sz: double; rgb24: integer): integer;
var newThing: TSolid;
begin
  with WorldODE do begin
    if Things.IndexFromID(ID) >= 0 then
      raise Exception.Create('AddThing Error - Dulpicate ID: ' + ID);
    newThing := TSolid.Create;
    result := Things.Add(newThing);
    newThing.ID := ID;

    newThing.BuoyantMass := 0;
    newThing.Drag := 0;
    newThing.StokesDrag := 1e-5;
    newThing.RollDrag := 1e-3;

    if ThingType = ttBox then begin
      CreateSolidBox(newThing, mass, posX, posY, posZ, sx, sy, sz);
    end else if ThingType = ttSphere then begin
      CreateSolidSphere(newThing, mass, posX, posY, posZ, sx{:radius});
    end else if ThingType = ttCylinder then begin
      CreateSolidCylinder(newThing, mass, posX, posY, posZ, sx, sz);
    end;

    newThing.SetZeroState();
    newThing.SetColorRGB(rgb24);
  end;
end;

procedure MeshThing(ID, MeshFile, MeshShadowFile: string; MeshScale: double; MeshCastsShadows: boolean);
var idx: integer;
begin
  with WorldODE do begin
    idx := Things.IndexFromID(ID);
    if idx < 0 then exit;
    LoadSolidMesh(Things[idx], MeshFile, MeshShadowFile, MeshScale, MeshCastsShadows);
    Things[idx].GLObj.Visible := false;
  end;
end;


function AddThingSphere(ID: string; mass, posx, posY, posZ, radius: double; rgb24: integer): integer;
begin
  result := AddThing(ttSphere, ID, mass, posx, posY, posZ, radius, 0, 0, rgb24);
end;


function AddThingBox(ID: string; mass, posx, posY, posZ, sizeX, sizeY, sizeZ: double; rgb24: integer): integer;
begin
  result := AddThing(ttBox, ID, mass, posx, posY, posZ, sizeX, sizeY, sizeZ, rgb24);
end;

function AddThingCylinder(ID: string; mass, posx, posY, posZ, radius, len: double; rgb24: integer): integer;
begin
  result := AddThing(ttCylinder, ID, mass, posx, posY, posZ, radius, 0, len, rgb24);
end;


function RemoveAndFreeThing(killThing: TSolid): integer;
//var killThing: TSolid;
begin
  with WorldODE do begin
    result := Things.remove(killThing);
    if result >= 0 then
      killThing.Free;
  end;
end;


function DeleteThing(ID: string): integer;
var killThing: TSolid;
begin
  with WorldODE do begin
    result := Things.IndexFromID(ID);
    if result < 0 then exit;
    killThing := Things[result];
    result := Things.remove(killThing);
    if result >= 0 then begin
      DeleteSolid(killThing);
      killThing.Free;
    end;
  end;
end;


procedure SetThingForce(T: integer; Fx, Fy, Fz: double);
begin
  dBodyAddForce(WorldODE.Things[T].Body, Fx, Fy, Fz);
end;

procedure SetThingForceAtRelPos(T: integer; Fx, Fy, Fz, Px, Py, Pz: double);
begin
  dBodyAddRelForceAtRelPos(WorldODE.Things[T].Body, Fx, Fy, Fz, Px, Py, Pz);
end;


procedure SetThingRelForce(T: integer; Fx, Fy, Fz: double);
begin
  dBodyAddRelForce(WorldODE.Things[T].Body, Fx, Fy, Fz);
end;

procedure SetThingRelForceAtRelPos(T: integer; Fx, Fy, Fz, Px, Py, Pz: double);
begin
  dBodyAddRelForceAtRelPos(WorldODE.Things[T].Body, Fx, Fy, Fz, Px, Py, Pz);
end;


procedure SetThingSurfaceMu(T: integer; mu, mu2: double);
begin
  WorldODE.Things[T].SetSurfacePars(mu, mu2, 0, 0, 0);
//  if mu2 <> 0 then WorldODE.Things[T].kind := skOmniSurface;
end;


function GetThingSpeed(T: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  v1 := WorldODE.Things[T].GetLinSpeed;

  Result.x := v1[0];
  Result.y := v1[1];
  Result.z := v1[2];
end;

function GetThingAngularVel(T: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  v1 := WorldODE.Things[T].GetAngularVel;

  Result.x := v1[0];
  Result.y := v1[1];
  Result.z := v1[2];
end;


procedure SetThingSpeed(T: integer; vx, vy, vz: double);
begin
  WorldODE.Things[T].SetLinSpeed(vx, vy, vz);
end;

procedure ClearThings;
var i: integer;
begin
  for i := 0 to WorldODE.Things.Count - 1 do begin
    WorldODE.DeleteSolid(WorldODE.Things[i]);
  end;
  WorldODE.Things.ClearAll;
end;


function GetShellPos(R, i: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  with WorldODE.Robots[R].Shells[i] do begin
    if geom = nil then exit;
    v1 := dGeomGetPosition(geom)^;
    Result.x := v1[0];
    Result.y := v1[1];
    Result.z := v1[2];
  end;
end;


procedure SetShellColor(R, i: integer; Red, Green, Blue: byte);
begin
  WorldODE.Robots[R].Shells[i].SetColor(Red/255, Green/255, Blue/255);
end;


function GetShellColor(R, I: integer): TRGBAColor;
begin
  with result do WorldODE.Robots[R].Shells[I].GetColor(Red, Green, Blue, Alpha);
end;


function GetObstacleIndex(ID: string): integer;
begin
  result := WorldODE.Obstacles.IndexFromID(ID);
end;


procedure SetObstacleColor(I: integer; Red, Green, Blue: byte);
begin
  WorldODE.Obstacles[I].SetColor(Red/255, Green/255, Blue/255);
end;

function GetObstacleColor(I: integer): TRGBAColor;
begin
  with result do WorldODE.Obstacles[I].GetColor(Red, Green, Blue, Alpha);
end;


{
function GetLinkWayPointPos(R, i, j: integer): double;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Links.Count) then exit;
    with WorldODE.Robots[r].Links[i] do begin
      if (j < 0) or (j >= Axis.WayPoints.Count) then exit;
      result := Axis.WayPoints[j].pos;
    end;
  end;
end;


function GetLinkWayPointW(R, i, j: integer): double;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Links.Count) then exit;
    with WorldODE.Robots[r].Links[i] do begin
      if (j < 0) or (j >= Axis.WayPoints.Count) then exit;
      result := Axis.WayPoints[j].speed;
    end;
  end;
end;

function GetLinkWayPointTime(R, i, j: integer): double;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Links.Count) then exit;
    with WorldODE.Robots[r].Links[i] do begin
      if (j < 0) or (j >= Axis.WayPoints.Count) then exit;
      result := Axis.WayPoints[j].t;
    end;
  end;
end;
}

end.
