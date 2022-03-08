const
 NumJoints = 6;

// Global Variables
var
  irobot, iB5, iB6, iHead: integer;
  d1, d2, d3: double;
  
  R01, R12, R23: Matrix;
  R03: Matrix;

  JointPos: array[0..NumJoints - 1] of double;

  state: string;
  ReqThetas: matrix;
  Tol, tis: double;


function DHMat(a, alpha, d, theta: double): Matrix;
var ct, st, ca, sa: double;
    R: Matrix;
begin
  ct := cos(theta);
  st := sin(theta);
  ca := cos(alpha);
  sa := sin(alpha);
  
  R := Meye(4);
  MSetV(R, 0, 0, ct); MSetV(R, 0, 1,-st * ca);  MSetV(R, 0, 2, st * sa);  MSetV(R, 0, 3, a * ct);
  MSetV(R, 1, 0, st); MSetV(R, 1, 1, ct * ca);  MSetV(R, 1, 2,-ct * sa);  MSetV(R, 1, 3, a * st);
  MSetV(R, 2, 0,  0); MSetV(R, 2, 1, sa     );  MSetV(R, 2, 2, ca     );  MSetV(R, 2, 3, d     );
  result := R;
end;


function RotZMat(theta: double): Matrix;
var ct, st: double;
    R: Matrix;
begin
  ct := cos(theta);
  st := sin(theta);

  R := Meye(3);
  MSetV(R, 0, 0, ct); MSetV(R, 0, 1,-st);  MSetV(R, 0, 2, 0);
  MSetV(R, 1, 0, st); MSetV(R, 1, 1, ct);  MSetV(R, 1, 2, 0);
  MSetV(R, 2, 0,  0); MSetV(R, 2, 1, 0 );  MSetV(R, 2, 2, 1);

  result := R;
end;


function RotXMat(theta: double): Matrix;
var ct, st: double;
    R: Matrix;
begin
  ct := cos(theta);
  st := sin(theta);

  R := Meye(3);
  MSetV(R, 0, 0, 1 ); MSetV(R, 0, 1, 0 );  MSetV(R, 0, 2, 0  );
  MSetV(R, 1, 0, 0 ); MSetV(R, 1, 1, ct);  MSetV(R, 1, 2, -st);
  MSetV(R, 2, 0,  0); MSetV(R, 2, 1, st);  MSetV(R, 2, 2, ct );

  result := R;
end;



// Place here the Inverse Kinematics calculations
function IK(Xtool, Rtool: matrix; Ltool: double): matrix;
begin
  result := Mzeros(6, 1);
end;



procedure SetThetas(Thetas: matrix);
var i: integer;
begin
  for i := 0 to 5 do begin
    SetAxisPosRef(iRobot, i, Mgetv(Thetas, i, 0));
  end;
end;


function JointError(ReqThethas: matrix): double;
var err: double;
    i: integer;
begin
  err := 0;
  for i := 0 to 5 do begin
    err := err + abs(GetAxisPos(iRobot, i) - Mgetv(ReqThethas, i, 0));
  end;
  result := err;
end;


procedure SetNewState(newState: string);
begin
  State := newState;
  tis := 0;
end;


procedure Control;
var i: integer;
    B5Pos, B6Pos: Matrix;
    B6Rot, B6RotCalc: Matrix;
    xw, yw, zw: double;
    RTool, XTool, XWrist, LTool: matrix;

    HeadPos, HeadRot: matrix;
begin

  B5Pos := GetSolidPosMat(iRobot, iB5);
  MatrixToRange(11, 2, B5Pos);

  B6Pos := GetSolidPosMat(iRobot, iB6);
  MatrixToRange(16, 2, B6Pos);

  B6rot := GetSolidRotMat(iRobot, iB6);
  MatrixToRangeF(16, 4, B6Rot, '%.3f');

  // Read joint positions
  for i := 0 to NumJoints -1 do begin
    JointPos[i] := GetAxisPos(irobot, i);
  end;

  // and show
  for i := 0 to NumJoints -1 do begin
    SetRCValue(3 + i, 2, format('%.3g',[Deg(JointPos[i])]));
  end;

  // control equations
  // ...

  if (state = 'idle') and RCButtonPressed(10,4) then begin
    SetNewState('above');
  end else if (state = 'above') and (JointError(ReqThetas) < Tol) and (tis > 5.0) then begin
    SetNewState('lock');
  end;

  if state = 'idle' then begin
    ReqThetas := Mzeros(6, 1);
    SetThetas(ReqThetas);
  end else if state = 'above' then begin
    ReqThetas := Ik(HeadPos, HeadRot, 0.15);
    SetThetas(ReqThetas);
  end else if state = 'lock' then begin
    ReqThetas := Ik(HeadPos, HeadRot, 0.8);
    SetThetas(ReqThetas);
  end;


  tis := tis + 0.04;

  SetRCValue(3, 7, state);
end;


procedure Initialize;
var i: integer;
begin
  irobot := 0;

  iB5 := GetSolidIndex(irobot, 'B5');
  iB6 := GetSolidIndex(irobot, 'B6');

  //iHead := GetSolidIndex(iScrew, 'screw_head');

  SetRCValue(2, 1, 'Joint');
  SetRCValue(2, 2, 'Pos (deg)');
  for i := 0 to NumJoints -1 do begin
    SetRCValue(3 + i, 1, format('%d',[i]));
  end;

  d1 := 0.55;
  d2 := 0.4;
  d3 := 0.37;

  state := 'idle';
  Tol := 0.2;
end;
