unit ODERobotsPublished;

interface

uses ODERobots;

type
  TAxisPoint = record
    pos: double;
    speed: double;
    final_time: double;
  end;

  TAxisState = record
    pos: double;
    vel: double;
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
function GetSolidTheta(R, i: integer): double;

function GetAxisOdo(R, i: integer): integer;

function GetAxisState(R, i: integer): TAxisState;
function GetAxisPos(R, i: integer): double;
function GetAxisSpeed(R, i: integer): double;
function GetAxisI(R, i: integer): double;

function GetAxisStateRef(R, i: integer): TAxisState;
function GetAxisPosRef(R, i: integer): double;
function GetAxisSpeedRef(R, i: integer): double;

procedure SetAxisStateRef(R, i: integer; aState: TAxisState);
procedure SetAxisPosRef(R, i: integer; aPos: double);
procedure SetAxisSpeedRef(R, i: integer; aSpeed: double);

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

implementation

uses Math, Viewer, odeimport, utils;

function Deg(angle: double): double;
begin
  Result := RadToDeg(angle);
end;

function Rad(angle: double): double;
begin
  Result := DegToRad(angle);
end;


function GetRobotPos2D(R: integer): TState2D;
var v1, v2: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.angle := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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

function GetSolidIndex(R: integer; ID: string): integer;
begin
  result := -1;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  result := WorldODE.Robots[R].Solids.IndexFromID(ID);
end;


function GetSolidMass(R, i: integer): double;
var mass: TdMass;
begin
  result := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      dBodyGetMass(MainBody.Body, mass);
      Result := mass.mass;
    end;
  end;
end;

function GetSolidCenterOfMass(R, i: integer): TPoint3D;
var mass: TdMass;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      dBodyGetMass(MainBody.Body, mass);

      Result.x := mass.c[0];
      Result.y := mass.c[1];
      Result.z := mass.c[2];
    end;
  end;
end;


function GetSolidPos(R, i: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      v1 := dBodyGetPosition(MainBody.Body)^;
      Result.x := v1[0];
      Result.y := v1[1];
      Result.z := v1[2];
    end;
  end;
end;

function GetSolidLinearVel(R, i: integer): TPoint3D;
var v1: TdVector3;
begin
  result.x := 0;
  result.y := 0;
  result.z := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      v1 := dBodyGetLinearVel(MainBody.Body)^;
      Result.x := v1[0];
      Result.y := v1[1];
      Result.z := v1[2];
    end;
  end;
end;

function GetRobotX(R: integer): double;
var v1: TdVector3;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
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
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      v1 := dBodyGetPosition(Body)^;
      Result := v1[0];
    end;
  end;
end;

function GetSolidY(R, i: integer): double;
var v1: TdVector3;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      v1 := dBodyGetPosition(Body)^;
      Result := v1[1];
    end;
  end;
end;

function GetSolidTheta(R, i: integer): double;
var v1, v2: TdVector3;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Solids.Count) then exit;
    with Solids[i] do begin
      if Body = nil then exit;
      v1 := dBodyGetPosition(Body)^;
      dBodyGetRelPointPos(Body, 1,0,0, v2);
      Result := atan2(v2[1]-v1[1], v2[0]-v1[0]);
    end;
  end;
end;


function GetAxisOdo(R, i: integer): integer;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      result := Odo.Value;
    end;
  end;
end;


function GetAxisState(R, i: integer): TAxisState;
begin
  result.pos := 0;
  result.vel := 0;
  result.Im := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      result.pos := GetPos();
      //result.vel := GetSpeed();
      result.vel := filt_speed;
      result.Im := Motor.Im;
    end;
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

function GetAxisI(R, i: integer): double;
begin
  result := GetAxisState(R, i).Im;
end;


function GetAxisStateRef(R, i: integer): TAxisState;
begin
  result.pos := 0;
  result.vel := 0;

  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      result.pos := Axes[i].ref.theta;
      result.vel := Axes[i].ref.w;
    end;
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


procedure SetAxisStateRef(R, i: integer; aState: TAxisState);
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      ref.theta := aState.pos;
      ref.w := aState.vel;
    end;
  end;
end;


procedure SetAxisPosRef(R, i: integer; aPos: double);
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      ref.theta := aPos;
    end;
  end;
end;


procedure SetAxisSpeedRef(R, i: integer; aSpeed: double);
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      ref.w := aSpeed;
    end;
  end;
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


function GetAxisIndex(R: integer; ID: string; i: integer): integer;
begin
  result := -1;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  result := WorldODE.Robots[R].Axes.IndexFromAxisID(ID, i);
end;


procedure LoadJointWayPoints(r: integer; JointPointsFileName: string);
var i: integer;
begin
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;
  // clear actual waypoints
  for i := 0 to WorldODE.Robots[r].Axes.Count-1 do begin
    WorldODE.Robots[r].Axes[i].WayPoints.ClearAll
  end;
  // Load new ones
  WorldODE.LoadJointWayPointsFromXML(JointPointsFileName, r);
end;


procedure SaveJointWayPoints(r: integer; JointPointsFileName: string);
var i: integer;
begin
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  WorldODE.SaveJointWayPointsToXML(JointPointsFileName, r);
end;


function CountAxisWayPoints(R, i: integer): integer;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      result := Axes[i].WayPoints.Count;
    end;
  end;
end;


function GetAxisWayPoint(R, i, idx: integer): TAxisPoint;
begin
  with result do begin
    pos := 0;
    speed := 0;
    final_time := 0;
  end;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      if (idx < 0) or (idx >= Axes[i].WayPoints.Count) then exit;
      with result do begin
        pos := Axes[i].WayPoints[idx].pos;
        speed := Axes[i].WayPoints[idx].speed;
        final_time := Axes[i].WayPoints[idx].t;
      end;
    end;
  end;
end;

function GetAxisTrajPoint(R, i, idx: integer): TAxisPoint;
begin
  with result do begin
    pos := 0;
    speed := 0;
    final_time := 0;
  end;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      if (idx < 0) or (idx >= Axes[i].TrajectPoints.Count) then exit;
      with result do begin
        pos := Axes[i].TrajectPoints[idx].pos;
        speed := Axes[i].TrajectPoints[idx].speed;
        final_time := Axes[i].TrajectPoints[idx].t;
      end;
    end;
  end;
end;

procedure SetAxisTrajPoint(R, i, idx: integer; LP: TAxisPoint);
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      if (idx < 0) or (idx >= Axes[i].TrajectPoints.Count) then exit;
      with Axes[i].TrajectPoints[idx] do begin
        pos := LP.pos;
        speed := LP.speed;
        t := LP.final_time;
      end;
    end;
  end;
end;


procedure AddAxisTrajPoint(R, i: integer; LP: TAxisPoint);
var AxisTraj: TAxisTraj;
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      //if (idx < 0) or (idx >= Axes[i].TrajectPoints.Count) then exit;
      AxisTraj := TAxisTraj.Create;
      with AxisTraj do begin
        pos := LP.pos;
        speed := LP.speed;
        t := LP.final_time;
      end;
      Axes[i].TrajectPoints.Add(AxisTraj);
    end;
  end;
end;


procedure DelAxisTrajPoint(R, i, idx: integer);
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      if (idx < 0) or (idx >= Axes[i].TrajectPoints.Count) then exit;
      Axes[i].TrajectPoints.Remove(Axes[i].TrajectPoints[idx]);
    end;
  end;
end;


function CountAxisTrajPoints(R, i: integer): integer;
begin
  result := 0;
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      result := Axes[i].TrajectPoints.Count;
    end;
  end;
end;

procedure ClearAxisTrajPoints(R, i: integer; LP: TAxisPoint);
begin
  if (R < 0) or (R >= WorldODE.Robots.Count) then exit;
  with WorldODE.Robots[R] do begin
    if (i < 0) or (i >= Axes.Count) then exit;
    with WorldODE.Robots[r].Axes[i] do begin
      Axes[i].TrajectPoints.ClearAll;
    end;
  end;
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
