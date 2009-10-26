unit ODERobots;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLWin32Viewer, ODEImport,
  VectorGeometry, GLGeomObjects, ExtCtrls, ComCtrls, GLTexture,
  keyboard, math, remote, Contnrs;

const
  //ODE world constants
  MAX_CONTACTS = 8;

  MaxJointSamples = 256;
  MaxKeyVals = 8;
  MaxAxis = 3;

const MaxDim = 8;

{const
  skDefault   = 0;
  skOmniWheel = 1;
  skMotorBelt = 2;}
type
  TSolidKind = (skDefault, skOmniWheel, skMotorBelt, skPropeller);

type
  TControlMode = (cmPIDPosition, cmPIDSpeed, cmState);

  TFriction = record
    Bv, Fc: double;
    CoulombLimit: double;
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
    Encoder: TEncoder;
    Controller: TMotController;
    voltage, PowerDrain, EnergyDrain: double;
    active, simple: boolean;
  end;

  TMatterPropertie = (smMetallic, smFerroMagnetic);
  TMatterProperties = set of TMatterPropertie;

  TSolid = class
    Body: PdxBody;
    Geom : PdxGeom;
    GLObj, AltGLObj, ShadowGlObj: TGLSceneObject;
    kind: TSolidKind;
    MatterProperties: TMatterProperties;
    BeltSpeed: double;
    ParSurface{, MaxParSurface} : TdSurfaceParameters;
    ID: string;
    description: string;
    BuoyantMass, Volume, Drag, StokesDrag, RollDrag: double;
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
    procedure SetTexture(TextureName: string; TextureScale: double);
    function GetPosition: TdVector3;
    function GetRotation: TdMatrix3;
    procedure SetSize(sizeX, sizeY, sizeZ: double);
    procedure GetSize(out sizeX, sizeY, sizeZ: double);
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
    procedure GLCreate(Parent: TGLBaseSceneObject; aRadius, aHeight: double);
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

  TSensorNoise = record
    var_k, var_d, offset, gain: double;
    active: boolean;
  end;

  TSensorKind = (skGeneric, skIR, skIRSharp, skSonar);

  TSensor = class
    //Geom : PdxGeom;
    Geoms: TGeomList;
    GLObj: TGLSceneObject;
    measure: double;
    pos, normal: TdVector3;
    kind: TSensorKind;
    Noise: TSensorNoise;
    has_measure: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetColor(R, G, B: single; A: single = 1);
  end;
  //pTIRSensor = ^TIRSensor;

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
    IRSensors: TSensorList;
    Kind: TRobotKind;
    //SamplesCount, DecPeriodSamples: integer;
    //SecondsCount, DecPeriod: double;
    Name: string;
    ForceMoved: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetXYZTeta(new_x, new_y, new_z, new_teta: double);
    procedure GetXYZTeta(out x, y, z, teta: double);
  end;

  TRobotList = class(TList)
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
  end;

implementation

uses Utils;

{ TRobot }

constructor TRobot.Create;
begin
  Solids := TSolidList.Create;
  Shells := TSolidList.Create;
  Axes := TAxisList.Create;
  AxesWayPointsIDs := TStringList.Create;
  Links := TSolidLinkList.Create;
  Wheels := TWheelList.Create;
  IRSensors := TSensorList.Create;
end;

destructor TRobot.Destroy;
begin
  IRSensors.ClearAll;
  IRSensors.Free;
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
var i, j: integer;
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


{ TRobotList }

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
end;

destructor TSolid.Destroy;
begin

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
  GLObj.Material.FrontProperties.Diffuse.SetColor(R, G, B, A);
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
  if GLObj = nil then exit;
  LM := GLObj.Material.MaterialLibrary.Materials.GetLibMaterialByName(TextureName);
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

{ TAxis }


constructor TAxis.Create;
begin
  TrajectPoints := TAxisTrajList.Create;
  WayPoints := TAxisTrajList.Create;
  speed_lambda := 0.95;
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
  end;
  //TODO more Joint types
end;


procedure TAxis.GetDir(var result: TdVector3);
begin
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

procedure TAxis.GLCreate(Parent: TGLBaseSceneObject; aRadius, aHeight: double);
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

procedure TAxis.GLSetPosition;
var vtmp: TdVector3;
begin
  if GLObj = nil then exit;
  with TGLCylinder(GLObj) do begin
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

constructor TSensor.Create;
begin
  Geoms := TGeomList.Create;
end;

destructor TSensor.Destroy;
begin
  Geoms.DeleteAllGeoms();
  Geoms.Free;
  inherited;
end;

procedure TSensor.SetColor(R, G, B: single; A: single = 1);
begin
  if GLObj = nil then exit;
//  with GLObj as TGLCylinder do begin
    GLObj.Material.FrontProperties.Diffuse.SetColor(R, G, B, A);
//  end;
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

end.
