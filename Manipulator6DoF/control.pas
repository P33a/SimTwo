const
 NumJoints = 6;

// Global Variables
var
  irobot, iB5, iB6: integer;
  d1, d2, d3: double;
  
  R01, R12, R23: Matrix;
  R03: Matrix;

  JointVoltages: array[0..NumJoints - 1] of double;
  JointPos: array[0..NumJoints - 1] of double;


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

procedure Control;
var i: integer;
    B5Pos: Matrix;
begin
  B5Pos := GetSolidPosMat(iRobot, iB5);
  MatrixToRange(11, 2, B5Pos);

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
  for i := 0 to NumJoints -1 do begin
    JointVoltages[i] := 0;
  end;

  // send control to motor
  for i := 0 to NumJoints -1 do begin
    SetAxisVoltageRef(irobot, i, JointVoltages[i]);
  end;
end;

procedure Initialize;
var i: integer;
begin
  irobot := 0;
  iB5 := GetSolidIndex(irobot, 'B5');
  iB6 := GetSolidIndex(irobot, 'B6');
  SetRCValue(2, 1, 'Joint');
  SetRCValue(2, 2, 'Pos (deg)');
  for i := 0 to NumJoints -1 do begin
    SetRCValue(3 + i, 1, format('%d',[i]));
  end;

  

  d1 := 0.55;
  d2 := 0.4;
  d3 := 0.37;
end;
