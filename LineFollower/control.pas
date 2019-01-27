// 
// 
type
  TControlMode = (cmManual, cmScript, cmRemote1, cmRemote2, cmRemote3);
  TRobotControls = record
    Fh: double;
    V1, V2: double;
  end;
  
// Global Variables
var
  irobot, NumRobots: integer;
  t: double;
  sens_line: array[1..6] of double;
  ControlMode: TControlMode;
  RobotControls: TRobotControls;
  NetOutBuf: TUDPBuffer;
  ODOWord1, ODOWord2: word;

procedure EncodeInteger(var StrPacket: TStringList; name: string; data: integer);
begin
  StrPacket.add(name);
  StrPacket.add(format('%d',[data]));
  StrPacket.add('');
end;

procedure EncodeDouble(var StrPacket: TStringList; name: string; data: double);
begin
  StrPacket.add(name);
  StrPacket.add(format('%.6g',[data]));
  StrPacket.add('');
end;

procedure EncodeDoubleFmt(var StrPacket: TStringList; name: string; data: double; fmt: string);
begin
  StrPacket.add(name);
  StrPacket.add(format(fmt,[data]));
  StrPacket.add('');
end;


function DecodeDoubleDef(var StrPacket: TStringList; name: string; defval: double): double;
var i: integer;
begin
  result := defval;
  i := StrPacket.indexof(name);
  if (i < 0) or (i + 1 >= StrPacket.count) then exit;
  result := strtofloat(StrPacket[i+1]);
end;


procedure ManualControl(var RC: TRobotControls);
var V, W: double;
    Vmax, Wmax: double;
begin
  Vmax := 0.2;
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

  RC.V1 := V * Vmax - W * Wmax;
  RC.V2 := V * Vmax + W * Wmax;
end;


procedure ScriptControl(var RC: TRobotControls);
var Vk, Wk, V, W: double;
    i: integer;
    ir_mean, total, mean: double;
begin
  RC.V1 := 0;
  RC.V2 := 0;
  RC.Fh := 0;
  
  Vk := 0.0016;
  Wk := 0.001;

  mean := 0;
  total := 0;
  for i:= 1 to 6 do begin
    mean := mean + sens_line[i] * (10 * (i - 2)  - 5);
    total := total + sens_line[i];
  end;
  
  if total <> 0 then begin
    ir_mean := mean / total;
  end else begin
    ir_mean := -5;
  end;
  
  V := 150;
  W := -15 * ir_mean;

  RC.V1 := V * Vk - W * Wk;
  RC.V2 := V * Vk + W * Wk;
end;


procedure RemoteControlText(var RC: TRobotControls);
var StrPacket: TStringList;
    txt: string;
    i, WhiteLineCount, IRCount: integer;
begin
  StrPacket := TStringList.create;
  try
    EncodeInteger(StrPacket,'Enc1', GetAxisOdo(0, 0));
    EncodeInteger(StrPacket,'Enc2', GetAxisOdo(0, 1));


    WhiteLineCount := 4;
    EncodeInteger(StrPacket,'WhiteLineCount', WhiteLineCount);

    for i := 0 to WhiteLineCount-1 do begin
      EncodeDouble(StrPacket, 'W' + inttostr(i), GetSensorVal(0, i));
    end;

    WriteUDPData(GetRCText(3,2), 9810, StrPacket.text);

    while true do begin
      StrPacket.text := ReadUDPData();
      txt := StrPacket.text;
      if txt = '' then break;
      // Read Motor Speed Reference
      RC.V1 := DecodeDoubleDef(StrPacket, 'V1', 0);
      RC.V2 := DecodeDoubleDef(StrPacket, 'V2', 0);
      RC.Fh := DecodeDoubleDef(StrPacket, 'Fh', 0);
    end;
  finally
    StrPacket.free;
  end;
end;


procedure processFrame(s: string; var RC: TRobotControls);
var value, pwm: integer;
   volts: double;
   curChannel: char;
   mot: TMotorPars;
begin
  while s <> '' do begin
    curChannel := s[1];
    value := strtointdef('$' + copy(s, 2, 4), 0);
    //SetRCValue(1, 2, copy(s, 2, 4));
    //writeln(format('%.2g',[U]));

    if (curChannel = 'V') then begin
      pwm := value;
      RC.v1 := 12.0 * pwm / 128.0;
    end;

    if (curChannel = 'W') then begin
      pwm := value;
      RC.v2 := 12.0 * pwm / 128.0;
    end;

    s := copy(s, 6, $FFFF);
  end;
end;

procedure processFrameBin(s: string; var RC: TRobotControls);
var value, pwm: integer;
   volts: double;
   curChannel: char;
   mot: TMotorPars;
begin
  if length(s) < 8 then exit;
  pwm := ord(s[1]) + ord(s[2]) * 256;
  RC.v1 := 12.0 * pwm / 128.0;

  pwm := ord(s[3]) + ord(s[4]) * 256;
  RC.v2 := 12.0 * pwm / 128.0;
end;


procedure RemoteControlRLAN(var RC: TRobotControls);
var StrPacket: TStringList;
    U, w: double;
    txt: string;
    i, Im, Odo: integer;
begin
  StrPacket := TStringList.create;
  try
    Im := round(1000 * abs(GetAxisI(0, 0)));
    StrPacket.add('I' + IntToHex(Im, 4));
    Im := round(1000 * abs(GetAxisI(0, 1)));
    StrPacket.add('J' + IntToHex(Im, 4));

    ODOWord1 := ODOWord1 + GetAxisOdo(0, 0);
    StrPacket.add('O' + IntToHex(ODOWord1, 4));
    ODOWord2 := ODOWord2 + GetAxisOdo(0, 1);
    StrPacket.add('P' + IntToHex(ODOWord2, 4));

    //SetRCValue(5, 2, StrPacket.text);
    WriteUDPData('127.0.0.1', 9810, StrPacket.text);
    

    StrPacket.text := ReadUDPData();
    for i := 0 to StrPacket.count - 1 do begin
      txt := StrPacket[i];
      SetRCValue(1, 1, txt);
      processFrame(txt, RC);
    end;

    //SetAxisPosRef(irobot, 2, RobotControls.Fh);

  finally
    StrPacket.free;
  end;
end;


procedure RemoteControlBIN(var RC: TRobotControls);
var StrPacket: TStringList;
    U, w: double;
    txt: string;
    i, Im, Odo: integer;
begin
  StrPacket := TStringList.create;
  try
    {Im := round(1000 * abs(GetAxisI(0, 0)));
    StrPacket.add('I' + IntToHex(Im, 4));
    Im := round(1000 * abs(GetAxisI(0, 1)));
    StrPacket.add('J' + IntToHex(Im, 4));

    ODOWord1 := ODOWord1 + GetAxisOdo(0, 0);
    StrPacket.add('O' + IntToHex(ODOWord1, 4));
    ODOWord2 := ODOWord2 + GetAxisOdo(0, 1);
    StrPacket.add('P' + IntToHex(ODOWord2, 4));}

    //SetRCValue(5, 2, StrPacket.text);
    //WriteUDPData('127.0.0.1', 9820, StrPacket.text);
   SetRCValue(13, 2, '1');

    while true do begin
      txt := ReadUDPData();
      if txt = '' then break;
      SetRCValue(13, 2, '2');
      processFrameBin(txt, RC);
      SetRCValue(12, 1, format('%d', [RC.V1]));
      SetRCValue(13, 1, format('%d', [RC.V2]));
    end;
    
{    StrPacket.text := ReadUDPData();
    for i := 0 to StrPacket.count - 1 do begin
      txt := StrPacket[i];
      SetRCValue(1, 1, txt);
      processFrame(txt, RC);
    end;
}
    //SetAxisPosRef(irobot, 2, RobotControls.Fh);

  finally
    StrPacket.free;
  end;
end;



procedure Control;
var StrPacket: TStringList;
    txt: string;
    i: integer;
    rgbacolor: trgbacolor;

    posx:double;
begin
  t := t + ScriptPeriod();
  posx := getRobotX(0);
  SetRCValue(10, 1 ,format('%.3g',[posx]));

  {sens_esq_fr:=GetSensorVal(0,0);
  sens_esq_tr:=GetSensorVal(0,1);
  sens_dir_fr:=GetSensorVal(0,2);
  sens_dir_tr:=GetSensorVal(0,3);
  
  SetRCValue(7, 1 ,format('%.3g',[sens_esq_fr]));
  SetRCValue(8, 1 ,format('%.3g',[sens_esq_tr]));
  SetRCValue(7, 4 ,format('%.3g',[sens_dir_fr]));
  SetRCValue(8, 4 ,format('%.3g',[sens_dir_tr]));
  }
  for i:= 1 to 6 do begin
    sens_line[i] := GetSensorVal(0, i - 1);
    SetRCValue(5, i,format('%.1g',[sens_line[i]]));
  end;
  
  if RCButtonPressed(7, 3) then begin
    SetRobotPos(0, GetRCValue(8,3), GetRCValue(9,3), 0, 0);
  end;

  if RCButtonPressed(2, 3) then begin
    ControlMode := cmManual;
    SetRCValue(2, 2, 'Manual');
  end else if RCButtonPressed(2, 4) then begin
    ControlMode := cmScript;
    SetRCValue(2, 2, 'Script');
  end else if RCButtonPressed(2, 5) then begin
    ControlMode := cmRemote1;
    SetRCValue(2, 2, 'Remote1');
  end else if RCButtonPressed(2, 6) then begin
    ControlMode := cmRemote2;
    SetRCValue(2, 2, 'Remote2');
  end else if RCButtonPressed(2, 7) then begin
    ControlMode := cmRemote3;
    SetRCValue(2, 2, 'Remote3');
  end;

  case ControlMode of
    cmManual: ManualControl(RobotControls);
    cmScript: ScriptControl(RobotControls);
    cmRemote1: RemoteControlText(RobotControls);
    cmRemote2: RemoteControlRLAN(RobotControls);
    cmRemote3: RemoteControlBIN(RobotControls);
  end;

  if (ControlMode = cmManual) or
     (ControlMode = cmScript) or
     (ControlMode = cmRemote1) then begin
    SetMotorControllerState(irobot, 0, true);
    SetMotorControllerState(irobot, 1, true);

    SetAxisSpeedRef(irobot, 0, RobotControls.V1/(0.065/2));
    SetAxisSpeedRef(irobot, 1, RobotControls.V2/(0.065/2));
  end else begin
    SetMotorControllerState(irobot, 0, false);
    SetMotorControllerState(irobot, 1, false);
    SetAxisVoltageRef(irobot, 0, RobotControls.v1);
    SetAxisVoltageRef(irobot, 1, RobotControls.v2);
  end;
end;

procedure Initialize;
begin
  t := 0;
  ControlMode := cmManual;
  SetRCValue(2, 2, 'Manual');
end;
