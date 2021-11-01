// Global Variables
var
  iRobot, iAxis, NumRobots, state_Herm, Ts_ms, t_cicle: integer;
  Vnom: double;
  tH, Ts, THerm, Serror, Error_ant, SpeedRefFilt, PosRefFilt, Pos: double;
  Vel_PosRef, Ac_PosRef: double;
  pid_on, Perturb_on, Hermite: boolean;
  p0, p1, PulsesToPos, t, GainFF: double;
  ChartOn: boolean;
  count: integer;

const
  Kp =  0.067;
  Tau = 0.122;

procedure Control;
var U, w: double;
    txt: string;
    Pulses: integer;
    Kc, Kc_pos, Td, Ti, Kf: double;
    Speed, Vm, Error, SpeedRef, PosRef: double;
    e: double;

    S: TstringList;
begin

  if RCButtonPressed(1, 3) then begin
    s := TstringList.create;
    s.savetofile('c:\tmp\out.txt');
    s.free;
  end;

  if count >= 1500 then exit;
  Vm := count / 1000.0;
  w :=  GetAxisSpeed(iRobot, iAxis);
  ChartSeriesAddXY(0, 0, Vm, w)

  inc(count);
  Vm := count / 1000.0;
  SetAxisVoltageRef(iRobot, iAxis, Vm);
  exit;

  if RCButtonPressed(6, 3) then begin
    pid_on := true;
    Serror := 0;
  end;
  if RCButtonPressed(7, 3) then pid_on := false;
  if RCButtonPressed(9, 3) then Perturb_on := true;
  if RCButtonPressed(10, 3) then Perturb_on := false;
  if RCButtonPressed(14, 3) then begin
    if Hermite then begin
      Hermite := false;
      SetRCValue(14,4,'Off');
    end else begin
      t_cicle := 0;
      Hermite := true;
      state_Herm := 0;
      SetRCValue(14,4,'On');
    end;
  end;


  t := t + Ts;
  Pulses := GetAxisOdo(iRobot, iAxis);
  Speed := PulsesToPos*Pulses/Ts;
  Pos := Pos + PulsesToPos*Pulses;

  SetRCValue(2, 2, format('%d', [Pulses]));
  SetRCValue(3, 2, format('%.2g', [Speed]));
  SetRCValue(3, 3, format('%.2g', [GetAxisSpeed(iRobot, iAxis)]));
  SetRCValue(18, 2, format('%.5g', [Pos]));

  // Controlo Manual
  Vm := GetRCValue(1, 2);

  if pid_on then begin
    SetRCValue(5, 4, 'On');

    // Position Control
    if Hermite then begin
      case state_Herm of
        0: begin
          tH := t_cicle/THerm;
          PosRef := (2*power(tH,3) - 3*power(tH,2) +1)*p0 + (-2*power(tH,3) + 3*power(tH,2))*p1;
          Vel_PosRef := (6*power(tH,2) - 6*tH)*p0 + (-6*power(tH,2) + 6*tH)*p1;
          Ac_PosRef :=  (12*tH - 6)*p0 + (-12*tH + 6)*p1;
          if tH >= 1 then state_Herm := 1;
        end;
        1: begin
          PosRef := p1;
          Vel_PosRef := 0;
          Ac_PosRef := 0;
          if t_cicle >= 2*THerm then state_Herm := 2;
        end;
        2: begin
          tH := (t_cicle - 2*THerm)/THerm;
          PosRef := (2*power(tH,3) -3*power(tH,2) +1)*p1 + (-2*power(tH,3) + 3*power(tH,2))*p0;
          Vel_PosRef := (6*power(tH,2) - 6*tH)*p1 + (-6*power(tH,2) + 6*tH)*p0;
          Ac_PosRef :=  (12*tH - 6)*p1 + (-12*tH + 6)*p0;
          if t_cicle >= 3*THerm then state_Herm := 3;
        end;
        3: begin
          PosRef := p0;
          Vel_PosRef := 0;
          Ac_PosRef := 0;
          if t_cicle >= 4*THerm then begin
            state_Herm := 0;
            t_cicle:= -Ts_ms;
          end;
        end;
      end;

      t_cicle := t_cicle + Ts_ms;
      SetRCValue(14, 2, format('%.2g', [PosRef]));
    end else begin
      PosRef := GetRCValue(14, 2);
    end;

    // Controller

    // Read controller parameters
    SpeedRef := GetRCValue(5, 2);
    Kc :=  GetRCValue(6, 2);
    Ti :=  GetRCValue(7, 2);
    if Ti = 0 then Ti := 1e6;

    //SpeedRefFilt := SpeedRefFilt * 0.8844 + SpeedRef * 0.1156;
    // SpeedRefFilt := SpeedRefFilt * 0.701 + SpeedRef * 0.299;     // Bessel 0.4 s
    SpeedRefFilt := SpeedRefFilt * 0.829 + SpeedRef * 0.171;     // Bessel 0.8 s


    // Calculate Controller output
    Error:= SpeedRef-Speed;
    //Error:= SpeedRef - Speed;
    Serror:=Serror+error*Ts;
    Vm := kc*(Error+(1/Ti)*Serror);


    // Position Control
    PosRef := GetRCValue(14, 2);
    Kc_pos := GetRCValue(15, 2);
    Td := GetRCValue(16, 2);
    Error:= PosRef-Pos;
    Vm := Kc_pos*(Error + Td*(Error - Error_Ant)/Ts);
    Error_ant :=  Error;

  end else begin
    SetRCValue(5, 4, 'Off');
  end;


  if Vm > 15 then begin
    Vm := 15;
    Serror:=Serror - error*Ts;
  end else begin
    if Vm < -15 then begin
      Vm := -15;
      Serror:=Serror - error*Ts;
    end;
  end;
  SetAxisVoltageRef(iRobot, iAxis, Vm);

  SetRCValue(1, 2, format('%.2g', [Vm]));
  SetRCValue(12, 2, format('%.2g', [Serror]));

  if RCButtonPressed(5, 9) then begin
    if ChartOn = TRUE then begin
      ChartOn := FALSE;
      SetRCValue(4,9,'Chart OFF');
    end else begin
      ChartOn := TRUE;
      SetRCValue(4,9,'Chart ON');
    end;
  end;
  if RCButtonPressed(5, 10) then begin
    ChartSeriesClear(0, 0);
    ChartSeriesClear(0, 1);
  end;
  if chartON then begin
    ChartSeriesAddXY(0, 0, t, PosRef);
    ChartSeriesAddXY(0, 1, t, Pos);
    //ChartSeriesAddXY(0, 0, t, SpeedRef);
    //ChartSeriesAddXY(0, 1, t, speed);
  end;


end;

procedure Initialize;
begin
  Vnom := 12;
  iRobot := 0;
  iAxis := 0;
  Ts := 0.05
  TS_ms := 50;
  Pos := GetAxisPos(iRobot, iAxis);
  PulsesToPos := 2 * PI / 10044;
  SetMotorActive(0, 0, true);
  Hermite := false;
  SetRCValue(14,4,'Off');
  THerm := 3000;
  state_Herm := 0;
  p0:=0;
  p1:=1.57;
  t := 0;
  Serror := 0;

  // Charts
  ChartSeriesSetCount(0, 2);
  ChartSeriesSetColor(0, 0, clGreen);
  ChartSeriesSetColor(0, 1, clRed);
  ChartSeriesClear(0, 0);
  ChartSeriesClear(0, 1);
  //ChartSetAxisMinMax(0, -0.05, 1.05, -0.05, 1.05);
  ChartON := False;
  SetRCValue(4,9,'Chart OFF');
end;
