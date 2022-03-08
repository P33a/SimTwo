// 
//

const
 NumMachines = 4;
 NumParts = 4;

type
  TControlMode = (cmManual, cmSerial);
  TRobotControls = record
    SolenoidActive: integer;
    V, W: double;
    PWM1, PWM2: integer;
  end;

  TMachine = record
    Xi, Yi, Xo: double;
  end;

// Global Variables
var
  irobot, NumRobots: integer;
  t: double;
  sens_line: array[0..4] of integer;
  touch_sensor: integer;
  enc1, enc2: integer;
  rfid_tag: integer;

  ControlMode: TControlMode;
  RobotControls: TRobotControls;

  iSolenoid, iMicroSwitch, iRFID: integer;
  ArduinoState: integer;

  FrameDigits: integer;

  channel: char;
  frame, frameSource: integer;
  frameData: string;

  part_pos_z: double;
  Machines: array[0..NumMachines - 1] of TMachine;
  machine_pos_x, machine_pos_y: double;

//Procedure, functions and Routines

procedure processFrame(channel: char; value: integer; source: integer);
var i: integer;
    s: string;
begin
  //writeln(channel + ' ' + inttohex(value, 8));
  if channel = 'i' then begin
  //end else if channel = 'r' then begin
    // if the arduino was reset ...
  end else if channel = 'M' then begin
    RobotControls.PWM1 := value;
  end else if channel = 'Q' then begin
    RobotControls.PWM2 := value;
  end else if channel = 'V' then begin
    RobotControls.V := value / 1000;
  end else if channel = 'W' then begin
    RobotControls.W := value / 1000;
  end else if channel = 'L' then begin
    RobotControls.SolenoidActive := value;
  end else if channel = 's' then begin
    ArduinoState := value;
  end else if channel = 'X' then begin
    SetRCValue(16, 3, inttostr(value));
  end else if channel = 'Y' then begin
    SetRCValue(17, 3, inttostr(value));
  end else if channel = 'Z' then begin
    SetRCValue(18, 3, inttostr(value));
  end else if channel in ['s', 't'] then begin
    i := 1 + ord(channel) - ord('r');

  end;
end;



function isHexDigit(c: char): boolean;
begin
  //result := ord(c) in ['0'..'9', 'A'..'F'];
  result := false;
  if ((ord(c) >= ord('0')) and (ord(c) <= ord('9'))) or
     ((ord(c) >= ord('A')) and (ord(c) <= ord('F'))) then result := true;
end;

procedure ReceiveData(s: string);
var c: char;
    value: integer;
    serialData: string;
begin
  if s = '' then exit;
  serialData := s;

  while Length(serialData) > 0 do begin
    c := serialData[1];
    serialData := copy(serialData, 2, 1024);
    if frame = -1 then begin

      //if (c in ['G'..'Z']) or (c in ['g'..'z']) then begin
      if ((ord(c) >= ord('G')) and (ord(c) <= ord('Z'))) or
         ((ord(c) >= ord('g')) and (ord(c) <= ord('z'))) then begin
        frame := 0;
        channel := c;
        frameData := '';
      end;
    end else begin
      if isHexDigit(c) then begin
        frameData := frameData + c;
        inc(frame);
        if frame = FrameDigits then begin
          value := StrToIntDef('$' + frameData, -1);
          processFrame(channel, value, frameSource);
          frame := -1;
        end;
      end else begin
        frame := -1;
      end;
    end;
  end;
end;

procedure SendMessage(c: char; val: Longword);
begin
  WriteComPort(c + IntToHex(Val, 8));
end;

function BuildMessage(c: char; val: Longword): string;
begin
  result := c + IntToHex(Val, 8);
end;

procedure SetPartPos(PartIdx: integer; newX, newY, newTheta: double);
begin
  SetRobotPos(1 + PartIdx, newX, newY, part_pos_z, newTheta);
end;


function GetPartType(PartIdx: integer): string;
begin
  result := GetShellTag(1 + PartIdx, 5);
end;

procedure SetPartType(PartIdx: integer; PartType: integer);
begin
  if PartType = 1 then begin
    SetShellTag(1 + PartIdx, 5, '1');
    SetSolidColor(1 + PartIdx, 0, 200, 0, 0);
  end else if PartType = 2 then begin
    SetShellTag(1 + PartIdx, 5, '2');
    SetSolidColor(1 + PartIdx, 0, 0, 200, 0);
  end else if PartType = 3 then begin
    SetShellTag(1 + PartIdx, 5, '3');
    SetSolidColor(1 + PartIdx, 0, 0, 100, 200);
  end;
end;


procedure ManualControl(var RC: TRobotControls);
var V, W: double;
    Vmax, Wmax: double;
begin
  Vmax := 255;
  Wmax := 125;

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
  end else if KeyPressed(vk_Prior) then begin
    RC.SolenoidActive := 1;
  end else if KeyPressed(vk_Next) then begin
    RC.SolenoidActive := 0;
  end;

  RC.V := V * Vmax;
  RC.W := W * Wmax;

  RC.PWM1 := round(RC.V + RC.W);
  RC.PWM2 := round(RC.V - RC.W);

end;



procedure SerialControl(var RC: TRobotControls);
var i, data: integer;
    s: string;
    tmp: Longword;
begin
  //RC.V := 0;
  //RC.W := 0;
  //RC.SolenoidActive := 0;

  // Read Sensor data
  data := 0;
  for i := 0 to 4 do begin
    data := data + (sens_line[i] shl (i * 6));
  end;
  data := data + (touch_sensor shl 31);

  s := '';
  if RCButtonPressed(4, 5) then begin
    s := s + BuildMessage('s', round(GetRCValue(4, 6)));
  end;
  if RCButtonPressed(5, 5) then begin
    s := s + BuildMessage('s', round(GetRCValue(5, 6)));
  end;

  // SendData
  tmp := enc1;
  tmp := ((tmp and $FFFF) shl 16) or (enc2 and $FFFF);
  s := s + BuildMessage('R', tmp);
  s := s + BuildMessage('I', data);
  s := s + BuildMessage('G', 0);
  WriteComPort(s);
  ///writeln(s);

  s := ReadComPort();
  if s = '' then exit;

  ReceiveData(s);
  //writeln(s);
end;

function PartMachineInDist(M: TMachine; Pi: integer): double;
var P: TPoint3D;
begin
  P := GetSolidPos(1 + Pi, 0);
  result := dist(M.Xi - P.x, M.Yi - P.y);
end;

function RobotMachineInDist(M: TMachine): double;
var P: TPoint3D;
begin
  P := GetSolidPos(0, 0);
  result := dist(M.Xi - P.x, M.Yi - P.y);
end;

function PartMachineOutDist(M: TMachine; Pi: integer): double;
var P: TPoint3D; // Part Position
    i: integer;
    d: double;
begin
  result := 1000;
  for i := 0 to numParts - 1 do begin
    //if i = Pi then continue;
    P := GetSolidPos(1 + i, 0);
    d := dist(M.Xo - P.x, M.Yi - P.y);
    if d < result then result := d;
  end;
end;


procedure MachineParts;
var pi, mi, ci: integer;
    c: string;
begin
  for mi := 0 to NumMachines - 1 do begin
    for Pi := 0 to NumParts - 1 do begin
      if (PartMachineInDist(Machines[mi], Pi) < 0.03) and
         (RobotMachineInDist(Machines[mi]) > 0.17) and
         (PartMachineOutDist(Machines[mi], Pi) > 0.07) then begin
        SetPartPos(Pi, Machines[mi].Xo, Machines[mi].Yi, rad(90));
        c := GetPartType(Pi);
        if c = '1' then ci := 2;
        if c = '2' then ci := 3;
        SetPartType(Pi, ci);
      end;
    end;
  end;
end;


procedure Control;
var StrPacket: TStringList;
    txt: string;
    i, c: integer;
    v: double;

begin
  t := t + ScriptPeriod();

  //if RCButtonPressed(15, 1) then begin
    MachineParts();
  //end;

  if RCButtonPressed(11, 3) then begin
    SetRobotPos(0, GetRCValue(12, 3), GetRCValue(13, 3), 0, rad(GetRCValue(14, 3)));
  end;

  for i:= 0 to 3 do begin
    if RCButtonPressed(11, 4 + i) then begin
      SetRobotPos(1 + i, GetRCValue(12, 4 + i), GetRCValue(13, 4 + i), 0, rad(GetRCValue(14, 4 + i)));
    end;
    if RCButtonPressed(15, 4 + i) then begin
      c := StrToIntDef(GetPartType(i), 0) + 1;
      if c > 3 then c := 1;
      SetPartType(i, c);
    end;
  end;

  for i := 0 to 4 do begin
    sens_line[i] := round(63 * GetSensorVal(0, i));
    SetRCValue(5 + i, 2, format('%d',[sens_line[i]]));
  end;

  touch_sensor := 0;
  if GetSensorVal(0, iMicroSwitch) > 0 then touch_sensor := 1;

  //rfid_tag := round(GetSensorVal(0, iRFID));
  //SetRCValue(10, 2, format('%d', [rfid_tag]));

  enc1 := GetAxisOdo(0, 0);
  enc2 := GetAxisOdo(0, 1);

  SetRCValue(10, 2, format('%d', [enc1]));
  SetRCValue(11, 2, format('%d', [enc2]));

  if RCButtonPressed(2, 3) then begin
    ControlMode := cmManual;
    SetRCValue(2, 2, 'Manual');
  end else if RCButtonPressed(2, 4) then begin
    ControlMode := cmSerial;
    SetRCValue(2, 2, 'Serial');
  end;

  case ControlMode of
    cmManual: ManualControl(RobotControls);
    cmSerial: SerialControl(RobotControls);
  end;

  SetAxisVoltageRef(irobot, 0, RobotControls.PWM1/255.0 * 7.4);
  SetAxisVoltageRef(irobot, 1, RobotControls.PWM2/255.0 * 7.4);

  SetSensorVin(irobot, iSolenoid, RobotControls.SolenoidActive);

  SetRCValue(4, 4, format('%d',[ArduinoState]));
  SetRCValue(5, 4, format('%d',[round(RobotControls.PWM1)]));
  SetRCValue(6, 4, format('%d',[round(RobotControls.PWM2)]));

  SetRCValue(8, 4, format('%5.2f',[sqrt(sqr(GetRobotVx(0)) + sqr(GetRobotVy(0)))]));
end;




procedure Initialize;
begin
  FrameDigits := 8;
  frame := -1;

  ClearButtons();
  iSolenoid := GetSensorIndex(0, 'solenoid1');
  iMicroSwitch := GetSensorIndex(0, 'MicroSwitch');
  iRFID := GetSensorIndex(0, 'RFID');

  ArduinoState := -1;
  t := 0;
  ControlMode := cmManual;
  //SetRCValue(2, 2, 'Manual');

  ControlMode := cmSerial;
  SetRCValue(2, 2, 'Serial');

  //SetRCValue(11, 2, inttostr(ord(isHexDigit('A'))));
  SetPartType(0, 1);
  SetPartType(1, 1);
  SetPartType(2, 2);
  SetPartType(3, 3);

  machine_pos_x := 0.720 / 2;
  machine_pos_y := 0.075;
  part_pos_z := 0.0021;

  with Machines[0] do begin
    Xi := -machine_pos_x - 0.045;
    Yi := 0;
    //SetPartPos(0, Xi, Yi, rad(-90));
    Xo := -machine_pos_x + 0.045;
    //SetPartPos(1, Xo, Yi, rad(90));
  end;

  with Machines[1] do begin
    Xi := -machine_pos_x - 0.045;
    Yi := -2*machine_pos_y;
    //SetPartPos(1, Xi, Yi, rad(-90));
    Xo := -machine_pos_x + 0.045;
    //SetPartPos(1, Xo, Yi, rad(90));
  end;

  with Machines[2] do begin
    Xi := machine_pos_x - 0.045;
    Yi := 2*machine_pos_y;
    //SetPartPos(2, Xi, Yi, rad(-90));
    Xo := machine_pos_x + 0.045;
    //SetPartPos(2, Xo, Yi, rad(90));
  end;

  with Machines[3] do begin
    Xi := machine_pos_x - 0.045;
    Yi := 0;
    //SetPartPos(3, Xi, Yi, rad(-90));
    Xo := machine_pos_x + 0.045;
    //SetPartPos(3, Xo, Yi, rad(90));
  end;

end;


