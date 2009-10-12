unit ODERobotsPublished;

interface

uses ODERobots, PathFinder;

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

procedure SetFirePosition(x, y, z: double);
procedure StartFire;
procedure StopFire;
procedure StopSolidFire(R, I: integer);
procedure StartSolidFire(R, I: integer);


procedure SetRobotPos(R: integer; x, y, z, teta: double);

function GetRobotPos2D(R: integer): TState2D;
function GetRobotVel2D(R: integer): TState2D;

function GetSolidIndex(R: integer; ID: string): integer;

function GetSolidMass(R, i: integer): double;
function GetSolidCenterOfMass(R, i: integer): TPoint3D;

function GetSolidPos(R, i: integer): TPoint3D;
function GetSolidLinearVel(R, i: integer): TPoint3D;

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

function GetSensorVal(R, i: integer): double;

function GetThingIndex(ID: string): integer;

function GetThingColor(T, c: integer): TRGBAColor;
procedure SetThingColor(T, c: integer; Red, Green, Blue: byte);

function GetThingPos(T: integer): TPoint3D;
procedure SetThingPos(T: integer; x, y, z: double);

function GetThingSize(T: integer): TPoint3D;
procedure SetThingSize(T: integer; x, y, z: double);

function GetSolidColor(R, i: integer): TRGBAColor;
procedure SetSolidColor(R, I: integer; Red, Green, Blue: byte);

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

function GetAxisEnergy(R, i: integer): double;
procedure ResetAxisEnergy(R, i: integer);

procedure SetMotorControllerPars(R, i: integer; nKi, nKd, nKp, nKf: double);
function GetMotorControllerPars(R, i: integer): TMotorControllerPars;

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
function CountAxisWayPoints(R, i: integer): integer;
function GetAxisWayPoint(R, i, idx: integer): TAxisPoint;

function GetAxisTrajPoint(R, i, idx: integer): TAxisPoint;

procedure SetAxisTrajPoint(R, i, idx: integer; LP: TAxisPoint);
procedure AddAxisTrajPoint(R, i: integer; LP: TAxisPoint);
procedure DelAxisTrajPoint(R, i, idx: integer);
function CountAxisTrajPoints(R, i: integer): integer;
procedure ClearAxisTrajPoints(R, i: integer; LP: TAxisPoint);

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

uses Math, Viewer, odeimport, utils, Keyboard, GLObjects;

function Deg(angle: double): double;
begin
  Result := RadToDeg(angle);
end;

function Rad(angle: double): double;
begin
  Result := DegToRad(angle);
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


function GetThingIndex(ID: string): integer;
begin
  result := WorldODE.Things.IndexFromID(ID);
end;


function GetSolidIndex(R: integer; ID: string): integer;
begin
  result := WorldODE.Robots[R].Solids.IndexFromID(ID);
end;


function GetSolidMass(R, i: integer): double;
var mass: TdMass;
begin
  result := 0;

  with WorldODE.Robots[R].Solids[i] do begin
    if Body = nil then exit;
    dBodyGetMass(Body, mass);
    Result := mass.mass;
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

function GetSensorVal(R, i: integer): double;
begin
  result := WorldODE.Robots[r].IRSensors[i].measure;
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
    result.CoulombLimit := Friction.CoulombLimit;
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
    CoulombLimit := nCoulombLimit;
  end;
end;


procedure SetBeltSpeed(R, i: integer; nSpeed: double);
begin
  WorldODE.Robots[r].Solids[i].BeltSpeed := nSpeed;
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


function CountAxisWayPoints(R, i: integer): integer;
begin
  result := WorldODE.Robots[r].Axes[i].WayPoints.Count;
end;


function GetAxisWayPoint(R, i, idx: integer): TAxisPoint;
begin
  with WorldODE.Robots[r].Axes[i].WayPoints[idx] do begin
    result.pos := pos;
    result.speed := speed;
    result.final_time := t;
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

procedure ClearAxisTrajPoints(R, i: integer; LP: TAxisPoint);
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
