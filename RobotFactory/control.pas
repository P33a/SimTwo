// 
// 
type
  TControlMode = (cmManual, cmScript, cmRemote1, cmRemote2);
  TRobotControls = record
    Fh: double;
    V1, V2: double;
  end;
  
  TPartState = (psRed, psGreen, psBlue, psYellow, psBlank);
  TCellState = (msEmpty, msBusy, msFull);
  
// Global Variables
var
  irobot, NumRobots: integer;
  t,sens_esq_fr,sens_esq_tr,sens_dir_fr,sens_dir_tr,sens_line_cent,sens_line_dir: double;
  sens_line: array[1..4] of double;
  ControlMode: TControlMode;
  RobotControls: TRobotControls;
  iLaser: integer;
  NetOutBuf: TUDPBuffer;
  ODOWord1, ODOWord2: word;
  firstRay, lastRay: integer;
  Log: TStringList;
  LogOn: boolean;

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
  Vmax := 1;
  Wmax := 0.5;

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

  if KeyPressed(ord('1')) then begin
    RC.Fh := 0.0;
  end else if KeyPressed(ord('2')) then begin
    RC.Fh := 0.0152;
  end else if KeyPressed(ord('3')) then begin
    RC.Fh := 0.04;
  end;

end;


procedure ScriptControl(var RC: TRobotControls);
begin
  RC.V1 := 0;
  RC.V2 := 0;
  RC.Fh := 0;
end;


procedure RemoteControlText(var RC: TRobotControls);
var StrPacket: TStringList;
    txt: string;
    i, WhiteLineCount, IRCount: integer;
begin
  StrPacket := TStringList.create;
  try
    {PEncodeInteger(StrPacket,'Enc1', GetAxisOdo(0, 0));
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

    WriteUDPData(GetRCText(3,2), 9810, StrPacket.text);
     }

    while true do begin
      StrPacket.text := ReadUDPPacket();
      txt := StrPacket.text;
      if txt = '' then break;
      WriteLn(txt);
      // Read Motor Speed Reference
      //RC.V1 := DecodeDoubleDef(StrPacket, 'V1', 0);
      //RC.V2 := DecodeDoubleDef(StrPacket, 'V2', 0);
      //RC.Fh := DecodeDoubleDef(StrPacket, 'Fh', 0);
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

procedure SendLaserMessage(LaserScanRays: Matrix; SendToIP: string);
var i, start: integer;
    ld: word;
    d: double;
begin
  ClearUDPBuffer(NetOutBuf);
  // tag this packet as Hokuyo -> Anyone
  NetPutByte(NetOutBuf, ord('H'));
  NetPutByte(NetOutBuf, ord('A'));
  NetPutByte(NetOutBuf, ord('1'));
  for i := firstRay to lastRay do begin
    d := mgetv(LaserScanRays, i, 0);
    if d < 0 then d := 0;
    ld := round(d * 1000);
    NetPutWord(NetOutBuf, ld);
  end;
  WriteUDPData(SendToIP, 9876, NetStringBuffer(NetOutBuf));
end;

{  A := GetGlobalSensorValues(3);
  //A := GetSensorValues(0, 1);
  SendLaserMessage(A, '127.0.0.1');
  n := MNumRows(A);
  SetRCValue(1, 10, format('%d',[n]));
  //for i := 0 to n - 1 do begin
  //  SetRCValue( 2 + i, 10, format('%g',[mgetv(A, i, 0)]));
  //end;
}


function LaserModel(Robot, Laser:Integer; a1, a2, a3, b1, b2 : double): Matrix;
var
  D, N, sigma, Dl : double;
  Laser_scan, Ds : Matrix;
  i : integer;
begin
  Laser_scan := GetSensorValues(Robot, Laser);

  Ds := Mzeros(MNumRows(Laser_scan),
               MNumCols(Laser_scan));

  for i:= 0 to MNumRows(Laser_scan)-1 do begin
    D := Mgetv(Laser_scan,i,0);
    Dl := a1 * D * D + a2 * D + a3;
    sigma := b1 * power(2.71828182846, b2 * D);
    N := RandG(0, sigma);
    Msetv(Ds, i, 0, Dl + N);
  end;

  Result := Ds;
end;


procedure Control;
var StrPacket: TStringList;
    txt, s: string;
    i: integer;
    rgbacolor: trgbacolor;
    LaserValues: Matrix;
    B, Noise, Filter: Matrix;

    posx, posy, posang: double;
    pix: TRGBAColor;
    //pix: integer;
begin
  t := t + ScriptPeriod();
  posx := getRobotX(0);
  SetRCValue(10, 1 ,format('%.3g',[posx]));
  posy := GetRobotY(0);
  posang := GetRobotTheta(0);

  pix := GetCameraPixel(120, 100);
  SetRCValue(11, 1 ,format('%d',[pix.red]));
  SetRCValue(12, 1 ,format('%d',[pix.green]));
  SetRCValue(13, 1 ,format('%d',[pix.blue]));

  SetRCValue(1, 1, inttostr(GetObstacleIndex('Machines_B_LED1')));

  rgbacolor := GetObstacleColor(GetObstacleIndex('Machines_B_LED1'));

  {sens_esq_fr:=GetSensorVal(0,0);
  sens_esq_tr:=GetSensorVal(0,1);
  sens_dir_fr:=GetSensorVal(0,2);
  sens_dir_tr:=GetSensorVal(0,3);
  
  SetRCValue(7, 1 ,format('%.3g',[sens_esq_fr]));
  SetRCValue(8, 1 ,format('%.3g',[sens_esq_tr]));
  SetRCValue(7, 4 ,format('%.3g',[sens_dir_fr]));
  SetRCValue(8, 4 ,format('%.3g',[sens_dir_tr]));
  }
  for i:= 1 to 4 do begin
    sens_line[i]:=GetSensorVal(0, i);
    SetRCValue(5, i,format('%.1g',[sens_line[i]]));
  end;

  //A := GetGlobalSensorValues(3);
  if iLaser >= 0 then begin
    //LaserValues := GetSensorValues(0, iLaser);
//    LaserValues := LaserModel(0, iLaser, 0, 1, 0, 0.001523985, 1.3102842636);

    //SendLaserMessage(LaserValues, '127.0.0.1');
  end;
  
  if RCButtonPressed(2, 9) then begin // Start Log
    if log <> nil then Log.free;
    Log := TStringList.create;
    LogOn := true;
  end;
  
  if RCButtonPressed(2, 10) then begin // Stop Log
    LogOn := false;
  end;
  
  if RCButtonPressed(2, 11) then begin // Stop Log
    if log <> nil then Log.savetofile(GetRCText(2, 12));
  end;
  
  //if LogOn then begin
  //  // x y theta enc1 enc2 Laser
  //  txt := format('%g %g %g  %d %d ', [posx, posy, posang, GetAxisOdo(0, 0), GetAxisOdo(0, 1)]);
  //  for i:= firstRay to LastRay do begin
  //    txt := txt + format('%g ', [Mgetv(LaserValues, i, 0)]);
  //  end;
  //  log.add(txt);
  //end;
  if LogOn then begin
    // 0 ticks loc_age x y theta enc_age enc1 enc2 laser_age Laser
    s := '0; ' + inttostr(0) + '; ';
    s := s + format('%d; %g; %g; %g; ', [0, posx, posy, posang]);

    s := s + format('%d; %d; %d; ', [0, GetAxisOdo(0, 0), GetAxisOdo(0, 1)]);

    s := s + format('%d; ', [0]);
    for i := firstRay to LastRay do begin
      s := s + format('%g; ', [Mgetv(LaserValues, i, 0)]);
    end;
    log.add(s);
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
  end;

  case ControlMode of
    cmManual: ManualControl(RobotControls);
    cmScript: ScriptControl(RobotControls);
    cmRemote1: RemoteControlText(RobotControls);
    cmRemote2: RemoteControlRLAN(RobotControls);
  end;

  if (ControlMode = cmManual) or
     (ControlMode = cmScript) or
     (ControlMode = cmRemote1) then begin
    SetMotorControllerState(irobot, 0, true);
    SetMotorControllerState(irobot, 1, true);

    SetAxisSpeedRef(irobot, 0, 10*RobotControls.V1);
    SetAxisSpeedRef(irobot, 1, 10*RobotControls.V2);
    SetAxisPosRef(irobot, 2, RobotControls.Fh);
  end else begin
    SetMotorControllerState(irobot, 0, false);
    SetMotorControllerState(irobot, 1, false);
    SetAxisVoltageRef(irobot, 0, RobotControls.v1);
    SetAxisVoltageRef(irobot, 1, RobotControls.v2);
  end;
end;

procedure Initialize;
begin
  iLaser := GetSensorIndex(0, 'ranger2d');
  firstRay := 0;
  lastRay := 359;
  t := 0;
  ControlMode := cmManual;
  SetRCValue(2, 2, 'Manual');
  Log := nil;
  LogOn := false;
end;
