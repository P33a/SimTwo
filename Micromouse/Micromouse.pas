// Global Variables Here
var
  go: boolean;


procedure ManualControl(var V1, V2: double);
var V, W: double;
    Vmax, Wmax: double;
begin
  Vmax := 0.6;
  Wmax := 0.3;

  V := 0;
  W := 0;

  if KeyPressed(vk_down) then begin
    V := -1;
  end else if KeyPressed(vk_left) then begin
    W := 1;
  end else if KeyPressed(vk_right) then begin
    W := -1
  end else if KeyPressed(vk_up) then begin
    V := 1;
  end;

  V1 := V * Vmax - W * Wmax;
  V2 := V * Vmax + W * Wmax;
end;


// this procedure is called periodicaly (default: 40 ms)
procedure Control;
var V1, V2: double;
    Rx, Ry, Rtheta: double;
begin
  if RCButtonPressed(2, 3) then begin
    Rx := GetRCValue(3, 3);
    Ry := GetRCValue(4, 3);
    Rtheta := Rad(GetRCValue(5, 3));
    SetRobotPos(0, Rx, Ry, 0, RTheta);
    SetRCBackColor(1, 1, RGBToColor(200, 100, 0));
  end;

  // Read the robot position
  Rx := GetRobotX(0);
  Ry := GetRobotY(0);
  Rtheta := GetRobotTheta(0);

  // Show the robot position
  SetRCValue(3, 2, format('%.3g', [Rx]));
  SetRCValue(4, 2, format('%.3g', [Ry]));
  SetRCValue(5, 2, format('%.3g', [deg(Rtheta)]));

  // Control the robot here (replace ManualControl by your procedure)
  ManualControl(V1, V2);

  // Set the desired speed
  SetAxisSpeedRef(0, 0, V1/(0.065/2));
  SetAxisSpeedRef(0, 1, V2/(0.065/2));

  // Show the robot path in a Chart
  if RCButtonPressed(3, 5) then go := true;
  if RCButtonPressed(4, 5) then go := false;

  if go then begin
    ChartSeriesAddXY(0, 0, Rx, Ry);
    //ChartSeriesAddXY(0, 1, time, RTheta);
  end;
end;

// this procedure is called once when the script is started
procedure Initialize;
begin
  go := false;
  ChartSeriesSetCount(0, 2);
  ChartSeriesSetColor(0, 0, clGreen);
  ChartSeriesSetColor(0, 1, clRed);
  ChartSeriesClear(0, 0);
  ChartSeriesClear(0, 1);

  ChartSetAxisMinMax(0, 0, 1, 0, 2);
end;

