// 
// 
type
  TControlMode = (cmManual, cmScript, cmRemote, cmSerial);
  TRobotControls = record
    SolenoidActive: integer;
    V, W: double;
  end;
  
// Global Variables
var
  irobot, NumRobots: integer;
  t: double;
  sens_line: array[0..4] of integer;
  touch_sensor: integer;
  rfid_tag: integer;

  ControlMode: TControlMode;
  RobotControls: TRobotControls;

  iSolenoid, iMicroSwitch, iRFID: integer;
  ArduinoState: integer;


  FrameDigits: integer;

  channel: char;
  frame, frameSource: integer;
  frameData: string;

//Procedure, functions and Routines

procedure processFrame(channel: char; value: integer; source: integer);
var i: integer;
    s: string;
begin
  //writeln(channel + ' ' + inttohex(value, 8));
  if channel = 'i' then begin
  //end else if channel = 'r' then begin
    // if the arduino was reset ...
  end else if channel = 'V' then begin
    RobotControls.V := value / 1000;
  end else if channel = 'W' then begin
    RobotControls.W := value / 1000;
  end else if channel = 'M' then begin
    RobotControls.SolenoidActive := value;
  end else if channel = 'S' then begin
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

procedure SendMessage(c: char; val: integer);
begin
  WriteComPort(c + IntToHex(Val, 8));
end;



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
  Vmax := 4 / 7.2 * 255;
  Wmax := 8 / 7.2 * 255;

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

end;


procedure ScriptControl(var RC: TRobotControls);
begin
  RC.V := 0;
  RC.W := 0;
  RC.SolenoidActive := 0;
end;


procedure RemoteControl(var RC: TRobotControls);
var StrPacket: TStringList;
    txt: string;
    i, WhiteLineCount, IRCount: integer;
begin
  StrPacket := TStringList.create;
  try
    EncodeInteger(StrPacket,'Enc1', GetAxisOdo(0, 0));
    EncodeInteger(StrPacket,'Enc2', GetAxisOdo(0, 1));

    IRCount := 4;
    EncodeInteger(StrPacket,'IRCount', IRCount);

    WhiteLineCount := 4;
    EncodeInteger(StrPacket,'WhiteLineCount', WhiteLineCount);

    for i := 0 to IRCount-1 do begin
      EncodeDouble(StrPacket, 'I' + inttostr(i), GetSensorVal(0, i));
    end;

    for i := 0 to WhiteLineCount-1 do begin
      EncodeDouble(StrPacket, 'W' + inttostr(i), GetSensorVal(0, IRCount + i));
    end;

    WriteUDPData(GetRCText(2,7), 9810, StrPacket.text);

    while true do begin
      StrPacket.text := ReadUDPData();
      txt := StrPacket.text;
      if txt <> '' then break;
    end;

    // Read Motor Speed Reference
    RC.V := DecodeDoubleDef(StrPacket, 'V', 0);
    RC.W := DecodeDoubleDef(StrPacket, 'W', 0);
    RC.SolenoidActive := round(DecodeDoubleDef(StrPacket, 'S', 0));
  finally
    StrPacket.free;
  end;
end;

procedure SerialControl(var RC: TRobotControls);
var i, data: integer;
    s: string;
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

  if RCButtonPressed(4, 5) then begin
    SendMessage('s', round(GetRCValue(4, 6)));
  end;
  if RCButtonPressed(5, 5) then begin
    SendMessage('s', round(GetRCValue(5, 6)));
  end;

  // SendData
  SendMessage('t', rfid_tag);
  SendMessage('i', data);
  SendMessage('g', 0);

  s := ReadComPort();
  if s = '' then exit;

  ReceiveData(s);
  //writeln(s);
end;


procedure Control;
var StrPacket: TStringList;
    txt: string;
    i: integer;
    v: double;

begin
  t := t + ScriptPeriod();

  if RCButtonPressed(11, 3) then begin
    SetRobotPos(0, GetRCValue(12, 3), GetRCValue(13, 3), 0, rad(GetRCValue(14, 3)));
  end;

  if RCButtonPressed(11, 4) then begin
    SetRobotPos(1, GetRCValue(12, 4), GetRCValue(13, 4), 0, rad(GetRCValue(14, 4)));
    SetRobotPos(2, GetRCValue(12, 5), GetRCValue(13, 5), 0, rad(GetRCValue(14, 5)));
    SetRobotPos(3, GetRCValue(12, 6), GetRCValue(13, 6), 0, rad(GetRCValue(14, 6)));
    SetRobotPos(4, GetRCValue(12, 7), GetRCValue(13, 7), 0, rad(GetRCValue(14, 7)));
  end;

  for i := 0 to 4 do begin
    sens_line[i] := round(63 * GetSensorVal(0, i));
    SetRCValue(5 + i, 2, format('%d',[sens_line[i]]));
  end;

  touch_sensor := 0;
  if GetSensorVal(0, iMicroSwitch) > 0 then touch_sensor := 1;

  rfid_tag := round(GetSensorVal(0, iRFID));
  SetRCValue(10, 2, format('%d', [rfid_tag]));

  if RCButtonPressed(2, 3) then begin
    ControlMode := cmManual;
    SetRCValue(2, 2, 'Manual');
  end else if RCButtonPressed(2, 4) then begin
    ControlMode := cmScript;
    SetRCValue(2, 2, 'Script');
  end else if RCButtonPressed(2, 5) then begin
    ControlMode := cmRemote;
    SetRCValue(2, 2, 'Remote');
  end else if RCButtonPressed(2, 6) then begin
    ControlMode := cmSerial;
    SetRCValue(2, 2, 'Serial');
  end;

  case ControlMode of
    cmManual: ManualControl(RobotControls);
    cmScript: ScriptControl(RobotControls);
    cmRemote: RemoteControl(RobotControls);
    cmSerial: SerialControl(RobotControls);
  end;

  SetAxisVoltageRef(irobot, 0, (RobotControls.V - RobotControls.W)/255 * 7.4);
  SetAxisVoltageRef(irobot, 1, (RobotControls.V + RobotControls.W)/255 * 7.4);
  SetSensorVin(irobot, iSolenoid, RobotControls.SolenoidActive);

  SetRCValue(4, 4, format('%d',[ArduinoState]));
  SetRCValue(5, 4, format('%d',[round(RobotControls.V)]));
  SetRCValue(6, 4, format('%d',[round(RobotControls.W)]));

  SetRCValue(8, 4, format('%.1g',[sqrt(sqr(GetRobotVx(0)) + sqr(GetRobotVy(0)))]));
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

  //SetRCValue(11, 2, inttostr(ord(isHexDigit('A'))));

end;
