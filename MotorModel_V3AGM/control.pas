//
// 

// Global Variables
var
  irobot, eixo, NumRobots: integer;
  Vnom: double;
  Udef: double;
  ODOByte: byte;
  IerroVel,Tamost: double;
  PosImp: integer;

procedure EncodeInteger(var StrPacket: TStringList; name: string; data: integer);
begin
  StrPacket.add(name);
  StrPacket.add(format('%d',[data]));
  StrPacket.add('');
end;

procedure processFrame(s: string);
var value, pwm: integer;
//  value = (HexNibbleToByte(frameHexData[0]) << 12) +
//          (HexNibbleToByte(frameHexData[1]) << 8)  +
//          (HexNibbleToByte(frameHexData[2]) << 4)  +
//          HexNibbleToByte(frameHexData[3]);
   curChannel: char;
   mot: TMotorPars;
begin
  while s <> '' do begin
    curChannel := s[1];
    value := strtointdef('$' + copy(s, 2, 4), 0);
    //SetRCValue(1, 2, copy(s, 2, 4));
    //writeln(format('%.2g',[U]));

    if (curChannel = 'P') then begin
      pwm := value - 1023;
      //motor(pwm);
      //SetAxisVoltageRef(irobot, 0, 12 * pwm / 1023);
      Udef := 12.0 * pwm / 1023.0;
    end;

    if (curChannel = 'O') then begin
       //digitalWrite(enable, LOW);
       //mot := GetMotorPars(0, 0);
       //mot.active := false;
       //SetMotorPars(0, 0, mot);
       SetMotorActive(0, 0, false);
     end;

    if (curChannel = 'R') then begin
      // digitalWrite(enable, HIGH);
       //SetMotorActive(0, 0, true);
       mot := GetMotorPars(0, 0);
       mot.active := true;
       SetMotorPars(0, 0, mot);
    end;
    
    s := copy(s, 6, $FFFF);
  end;
end;


procedure Control;
var StrPacket: TStringList;
    U, w: double;
    txt: string;
    Impulsos,i, Im, Odo: integer;
    Td,Tiv,Ref,Volts,Kp,Kpv,Tdv,Erro,RefVel,RefPos,ErroVel,FeedF: double;
begin
  Impulsos := GetAxisOdo(0, 0);
  PosImp:=PosImp+Impulsos;
  SetRCValue(2,2,format('%d',[Impulsos]));
  SetRCValue(3,2,format('%d',[PosImp]));
  
  // Controlo Manual
  //Volts := GetRCValue(1, 2);



  // Controlo PD Posição
  RefPos := GetRCValue(1, 4);
  Erro := RefPos - PosImp;
  Kp := GetRCValue(2, 10);
  Td := GetRCValue(3, 10);
  //Volts := Kp*(Erro - Td*Impulsos/Tamost);

  // Controlo PD Posição - cascata
  RefVel := Kp*(Erro - Td*Impulsos/Tamost);
  SetRCValue(5,10,format('%.3g',[RefVel]));

  // Controlo PI Velocidade
  //RefVel := GetRCValue(1, 4);
  ErroVel := RefVel - Impulsos;
  Kpv := GetRCValue(2, 7);
  Tiv := GetRCValue(3, 7);
  IerroVel := IerroVel + ErroVel*Tamost;
  Volts := Kpv*(ErroVel + (1/Tiv)*IerroVel);
  



  SetRCValue(1,2,format('%.3g',[Volts]));
  SetAxisVoltageRef(irobot, eixo, Volts);
end;

procedure Initialize;
begin
  Vnom := 12;
  irobot := 0;
  eixo :=0;
  Udef := 0;
  writeln(format('%d %s',[123, 'Init']));
  HUDStrings.clear;
  HUDStrings.add('Test');
  SetMotorActive(0, 0, true);
  IerroVel :=0;
  Tamost := 0.05;
end;
