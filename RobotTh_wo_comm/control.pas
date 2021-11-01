type
  TGains = array[0..4] of double;

  TJoint = record
    dx: integer; //index
    K: array[0..4] of double;
    theta_abs, theta_rel, w_abs, w_rel: double;
    theta_ref, u: double;
    phi_a, gama_a, x_a: double;
  end;

  TLeg = record
    v_x, v_y, w_wheel: double;
    theta_1, theta_2, theta_wheel: double;
  end;

  TRobot = record
    a, b, wheel_rad: double;
    x, y, theta: double;
    v, vn, w: double;
  end;

  TTrajControlMode = (cmManual, cmScript);

  { Trajectory control }
  TTrajShape = (tsLine, tsCurve);

  TStateFollow = (sfStop, sfFollow);

  TLine = record
    x_i, y_i, x_f, y_f, alpha, length: double;
    u_x, u_y: double; //line direction (unit vector)
  end;

  TCurve = record
    x1, y1, x2, y2, x3, y3: double;
    x_c, y_c, r: double;
    rotation: integer;
  end;

// Global Variables
const
  { Trajectory control }
  // Constants (K - gain; Ti - integral time; Td - derivative time)
  K_LINE_V = 7;
  K_LINE_W = 0.8;
  K_CURVE_V = 2.4;
  Td_CURVE_V = 1.7;
  K_CURVE_W = 3.5;
  Td_CURVE_W = 1.15;
  // Tolerances
  TOL_DIST = 0.03;

var
  t: double;
  msg: string;
  robot: TRobot;
  TrajControlMode: TTrajControlMode;
  TrajShape: TTrajShape;
  go: boolean;
  gains_matrix: TGains;
  joints: array[0..7] of TJoint;
  legs: array[0..3] of TLeg;
  data_SS_controller: TStringList;
  StateFollow: TStateFollow;
  traj_control_on: boolean;
  dist2curve_prev, e_theta_prev: double;
  line: TLine;
  curve: TCurve;

function normalise_full_rotation(ang: double): double;
begin
  result := ang;
  if result < 0 then result := ang + 2 * Pi();
end;

procedure set_k(var joint: TJoint; K: TGains);
var i: integer;
begin
  for i := 0 to 4 do begin
    joint.K[i] := K[i];
  end;
end;

procedure joint_control(var J: TJoint);
var maxV: double;
begin
  J.theta_rel := GetAxisPos(0, J.dx);
  J.theta_abs := GetAxisPos(0, J.dx + 1) + J.theta_rel;
  J.w_rel := GetAxisSpeed(0, J.dx);
  J.w_abs := GetAxisSpeed(0, J.dx + 1) + J.w_rel;

  J.x_a := J.phi_a * J.x_a + J.gama_a * (J.theta_ref - J.theta_abs);

  maxV := 24;
  if J.x_a > maxV / J.K[4] then begin
    J.x_a := maxV / J.K[4];
  end else if J.x_a < -maxV / J.K[4] then begin
    J.x_a := -maxV / J.K[4]
  end;

  J.u := J.K[0] * J.theta_rel + J.K[1] * J.w_rel + J.K[2] * J.theta_abs + J.K[3] * J.w_abs;
  J.u := -J.u + J.x_a * J.K[4];

  if t >= 10 then SetAxisVoltageRef(0, J.dx, 0)
  else SetAxisVoltageRef(0, J.dx, J.u * 1);
end;

procedure kinematics(const i: integer; var L: TLeg; const r: TRobot);
begin
  if (i = 0) or (i = 3) then L.v_x := r.v + r.w * r.b
  else L.v_x := r.v - r.w * r.b;

  if (i = 0) or (i = 1) then L.v_y := r.vn + r.w * r.a
  else L.v_y := r.vn - r.w * r. a;

  L.w_wheel := Sqrt(L.v_x * L.v_x + L.v_y * L.v_y) / r.wheel_rad;
  L.theta_wheel := ATan2(L.v_y, L.v_x);
end;

procedure manual_traj_control(var r: TRobot);
var
  v_nom, vn_nom, w_nom: double;
begin
  v_nom := 0.5;
  vn_nom := 0.5;
  w_nom := 0.5;

  r.v := 0;
  r.vn := 0;
  r.w := 0

  if KeyPressed(vk_down) then r.v := -1
  else if KeyPressed(vk_up) then r.v := 1;

  if KeyPressed(vk_left) then r.vn := 1
  else if KeyPressed(vk_right) then r.vn := -1;

  if KeyPressed(vk_tab) then r.w := 1;

  r.v := r.v * v_nom;
  r.vn := r.vn * vn_nom;
  r.w := r.w * w_nom;
end;

procedure charact_line(var L: TLine);
begin
  L.length := Sqrt((L.x_f - L.x_i) * (L.x_f - L.x_i) + (L.y_f - L.y_i) * (L.y_f - L.y_i));

  L.alpha := ATan2(L.y_f - L.y_i, L.x_f - L.x_i);
  L.u_x := (L.x_f - L.x_i) / L.length;
  L.u_y := (L.y_f - L.y_i) / L.length;
end;

procedure charact_curve(var C: TCurve);
var p1, p2, p3, p4: double;
    vx_c1, vy_c1, vx_c2, vy_c2, vx_c3, vy_c3, beta_c1, beta_c2, beta_c3, beta_1, beta_2, beta_3: double;
begin
  p1 := C.x1 * (C.y2 - C.y3) - C.y1 * (C.x2 - C.x3) + C.x2 * C.y3 - C.x3 * C.y2;
  p2 := (C.x1*C.x1 + C.y1*C.y1) * (C.y3 - C.y2) + (C.x2*C.x2 + C.y2*C.y2) * (C.y1 - C.y3) + (C.x3*C.x3 + C.y3*C.y3) * (C.y2 - C.y1);
  p3 := (C.x1*C.x1 + C.y1*C.y1) * (C.x2 - C.x3) + (C.x2*C.x2 + C.y2*C.y2) * (C.x3 - C.x1) + (C.x3*C.x3 + C.y3*C.y3) * (C.x1 - C.x2);
  p4 := (C.x1*C.x1 + C.y1*C.y1) * (C.x3 * C.y2 - C.x2 * C.y3) + (C.x2*C.x2 + C.y2*C.y2) * (C.x1 * C.y3 - C.x3 * C.y1) + (C.x3*C.x3 + C.y3*C.y3) * (C.x2 * C.y1 - C.x1 * C.y2);

  C.x_c := -p2 / (2 * p1);
  C.y_c := -p3 / (2 * p1);
  C.r := Sqrt((p2*p2 + p3*p3 - 4 * p1 * p4) / (4 * p1*p1));

  { Determine direction of rotation }
  vx_c1 := C.x1 - C.x_c;
  vy_c1 := C.y1 - C.y_c;
  vx_c2 := C.x2 - C.x_c;
  vy_c2 := C.y2 - C.y_c;
  vx_c3 := C.x3 - C.x_c;
  vy_c3 := C.y3 - C.y_c;

  beta_c1 := ATan2(vy_c1, vx_c1);
  beta_c2 := ATan2(vy_c2, vx_c2);
  beta_c3 := ATan2(vy_c3, vx_c3);

  beta_1 := normalise_full_rotation(DiffAngle(beta_c1, beta_c1));
  beta_2 := normalise_full_rotation(DiffAngle(beta_c2, beta_c1));
  beta_3 := normalise_full_rotation(DiffAngle(beta_c3, beta_c1));

  if beta_3 > beta_2 then C.rotation := 1
  else C.rotation := -1;
end;

procedure TrajControl(const traj: TTrajShape; var R: TRobot);
var v_nom, w_nom, v_deaccel: double;
    dist2line, e_theta, e_dist: double;
    u_x, u_y, dist2centre: double;
    P_x, P_y, tangent_angle: double;
    dist2curve, D_dist2curve, D_e_theta: double;  
    v_f_deaccel: double;
begin
  v_nom := 0.7;
  w_nom := 0.7;   

  v_f_deaccel := 0.20;

  // Characterise trajectory and robot in relation to it
  case traj of
    tsLine: begin
      e_dist := Sqrt((R.x - line.x_f) * (R.x - line.x_f) + (R.y - line.y_f) * (R.y - line.y_f));

      // Variables for the controller
      dist2line := ((line.y_i - R.y) * line.u_x - (line.x_i - R.x) * line.u_y) / (line.u_x * line.u_x + line.u_y * line.u_y); //perpendicular between robot and line
      e_theta := DiffAngle(line.alpha, R.theta);

      v_deaccel := (v_nom - v_f_deaccel) / 0.20 * e_dist + v_f_deaccel;
    end;

    tsCurve: begin
      { Nearest point to curve -> P }
      // Unit vector passing through curve centre and robot current position
      dist2centre := Sqrt((R.x - curve.x_c) * (R.x - curve.x_c) + (R.y - curve.y_c) * (R.y - curve.y_c));
      u_x := (R.x - curve.x_c) / dist2centre;
      u_y := (R.y - curve.y_c) / dist2centre;

      P_x := curve.x_c + curve.r * u_x;
      P_y := curve.y_c + curve.r * u_y;

      { For controller : (P_x,P_y) = (R.x, R.y) + dist2curve * (u_x,u_y) }
      if u_x = 0 then dist2curve := (P_y - R.y) / u_y
      else dist2curve := (P_x - R.x) / u_x;

      e_dist := Sqrt((R.x - curve.x3) * (R.x - curve.x3) + (R.y - curve.y3) * (R.y - curve.y3));

      // Orientation of the tangent to the curve
      if curve.rotation = 1 then tangent_angle := ATan2(u_x, -u_y)
      else tangent_angle := ATan2(-u_x, u_y);

      e_theta := DiffAngle(tangent_angle, R.theta);

      v_deaccel := (v_nom - v_f_deaccel) / 0.45 * e_dist + v_f_deaccel;
    end;
  end;

  D_dist2curve := (dist2curve - dist2curve_prev) / ScriptPeriod();
  dist2curve_prev := dist2curve;
  D_e_theta := (e_theta - e_theta_prev) / ScriptPeriod();
  e_theta_prev := e_theta;

  // Transitions
  case StateFollow of
    sfFollow: begin
      if e_dist <= TOL_DIST then StateFollow := sfStop;
    end;
  end;

  // Outputs
  case StateFollow of
    sfStop: begin
      R.v := 0;
      R.vn := 0;
      R.w := 0;
    end;

    sfFollow: begin    
      if v_nom <= v_deaccel then R.v := v_nom
      else R.v := v_deaccel;

      case traj of
        tsLine: begin
          R.vn := K_LINE_V * dist2line;
          R.w := K_LINE_W * e_theta;
        end;

        tsCurve: begin
          w_nom := curve.rotation * R.v / curve.r;

          R.vn := -curve.rotation * K_CURVE_V * (dist2curve + Td_CURVE_V * D_dist2curve);
          R.w := w_nom + K_CURVE_W * (e_theta + Td_CURVE_W * D_e_theta);
        end;
      end;
    end;
  end;

  SetRCValue(26, 2, format('%d', [StateFollow]));
  SetRCValue(26, 4, format('%g', [robot.x]));
  SetRCValue(27, 4, format('%g', [robot.y]));
  SetRCValue(28, 4, format('%g', [Deg(robot.theta)]));
  SetRCValue(29, 4, format('%g', [Deg(e_theta)]));
  SetRCValue(31, 4, format('%g', [e_dist]));
  SetRCValue(32, 4, format('%g', [dist2line]));
  SetRCValue(34, 4, format('%g', [dist2curve]));

  SetRCValue(20, 6, format('%g', [robot.v]));
  SetRCValue(20, 7, format('%g', [robot.vn]));
  SetRCValue(20, 8, format('%g', [robot.w]));
end;

procedure Control;
var i: integer;
begin
  t := t + ScriptPeriod;

  robot.x := GetRobotX(0);
  robot.y := GetRobotY(0);
  Robot.theta := GetRobotTheta(0);

  // Control
  if RCButtonPressed(23, 1) then begin
    TrajControlMode := cmManual;
    SetRCValue(23, 3, 'Manual');
    StateFollow := sfStop;
  end else if RCButtonPressed(23, 2) then begin
    TrajControlMode := cmScript;
    SetRCValue(23, 3, 'Script');
    SetRCValue(23, 4, format('%g', [t]));
  end;

  if RCButtonPressed(23, 6) then begin
    TrajShape := tsLine;
    SetRCValue(23, 9, 'Line');
    line.x_i := 0;
    line.y_i := 0;
    line.x_f := 2;
    line.y_f := 0;
    charact_line(line);
  end else if RCButtonPressed(23, 7) then begin
    TrajShape := tsCurve;
    SetRCValue(23, 9, 'Curve');
    curve.x1 := 0;
    curve.y1 := 0;
    curve.x2 := 1;
    curve.y2 := -1;
    curve.x3 := -1;
    curve.y3 := -1;
    {curve.x1 := 0;
    curve.y1 := 0;
    curve.x2 := 1;
    curve.y2 := 1;
    curve.x3 := -1;
    curve.y3 := 1; }
    {curve.x1 := 2.635;
    curve.y1 := 9.855;
    curve.x2 := 1.515;
    curve.y2 := 9.210;
    curve.x3 := 0.395;
    curve.y3 := 9.138;}
    charact_curve(curve);
  end;

  if RCButtonPressed(23, 8) then begin
    SetRCValue(29, 6, format('%g', [t]));
    go := True;
    StateFollow := sfFollow;
  end;

  // Controller non-rigid joints
  for i := 0 to 7 do begin
    if i mod 2 = 0 then joints[i].theta_ref := Rad(GetRCValue(16, 2))
    else joints[i].theta_ref := Rad(GetRCValue(16, 3));

    joint_control(joints[i]);

    {SetRCValue(2, i+2, format('%g', [Deg(joints[i].theta_abs)]));
    SetRCValue(3, i+2, format('%g', [Deg(joints[i].theta_rel)]));
    SetRCValue(4, i+2, format('%g', [joints[i].w_abs]));
    SetRCValue(5, i+2, format('%g', [joints[i].w_rel]));
    SetRCValue(6, i+2, format('%g', [joints[i].u]));
    SetRCValue(7, i+2, format('%g', [Deg(joints[i].theta_ref)]));}
  end;

  case TrajControlMode of
    cmManual: manual_traj_control(robot);
    cmScript: begin
      if go then TrajControl(TrajShape, robot)
      else begin
        robot.v := GetRCValue(19, 2);
        robot.vn := GetRCValue(19, 3);
        robot.w := GetRCValue(19, 4);
      end;
    end;
  end;

  // Set speeds
  for i := 0 to 3 do begin
    kinematics(i, legs[i], robot);

    // Restrict theta_wheel and w_wheel (due to mechanical constraints)
    if legs[i].theta_wheel > Pi()/2 then begin
      legs[i].theta_wheel := legs[i].theta_wheel - Pi();
      legs[i].w_wheel := -legs[i].w_wheel;
    end else if legs[i].theta_wheel < -Pi()/2 then begin
      legs[i].theta_wheel := legs[i].theta_wheel + Pi();
      legs[i].w_wheel := -legs[i].w_wheel;
    end;

    SetAxisPosRef(0, i * 7, legs[i].theta_wheel);
    SetAxisSpeedRef(0, 5 + i * 7, legs[i].w_wheel);

    {SetRCValue(10, i+2, format('%g', [legs[i].v_x]));
    SetRCValue(11, i+2, format('%g', [legs[i].v_y]));
    SetRCValue(12, i+2, format('%g', [legs[i].w_wheel]));
    SetRCValue(13, i+2, format('%g', [Deg(legs[i].theta_wheel)]));}
  end;

  SetRCValue(19, 6, format('%g', [robot.v]));
  SetRCValue(19, 7, format('%g', [robot.vn]));
  SetRCValue(19, 8, format('%g', [robot.w]));

end;

procedure Initialize;
var
  i: integer;
begin
  t := 0;

  robot.a := 0.305;
  robot.b := 0.2025;
  robot.wheel_rad := 0.100;
  robot.a := GetSceneConstant('length_a', robot.a);
  robot.b := GetSceneConstant('length_b', robot.b);
  robot.wheel_rad := GetSceneConstant('wheel_diameter', robot.wheel_rad) / 2;

  robot.v := 0;
  robot.vn := 0;
  robot.w := 0;

  gains_matrix[0] := 10.7110;
  gains_matrix[1] := 0.5160;
  gains_matrix[2] := 2.3263;
  gains_matrix[3] := 0.4410;
  gains_matrix[4] := 0.3118;

  joints[0].dx := 1;
  joints[1].dx := 3;
  joints[2].dx := 8;
  joints[3].dx := 10;
  joints[4].dx := 15;
  joints[5].dx := 17;
  joints[6].dx := 22;
  joints[7].dx := 24;

  for i := 0 to 7 do begin
    set_k(joints[i], gains_matrix);

    joints[i].phi_a := 1;
    joints[i].gama_a := 1;
    joints[i].theta_ref := 0;

    SetRCValue(1, i+2, format('%d', [joints[i].dx]));
  end;

  // Initial position
  SetRobotPos(0, -0.5, 0.25, 0.534, Rad(-20));
  //SetRobotPos(0, -1, 0, 0.534, 0);

  // Set null speed
  for i := 0 to 2 do SetRCValue(19, i+2, '0');

  TrajControlMode := cmManual;
  SetRCValue(23, 3, 'Manual');

  StateFollow := sfStop;

  dist2curve_prev := 0;
  e_theta_prev := 0;
end;

