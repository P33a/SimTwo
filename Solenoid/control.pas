// Global Variables Here
var is1, iball: integer;

// this procedure is called periodicaly (default: 40 ms)
procedure Control;
var t: double;
    ir: double;
begin
  ir := GetSensorVal(0, 0);
  SetRCValue(9, 2, format('%f', [ir]));

  SetBeltSpeed(0, 0, 0.1);

  if RCButtonPressed(3, 2) then begin
    SetGlobalSensorVin(is1, 1);
  end else if RCButtonPressed(3, 3) then begin
    SetGlobalSensorVin(is1, 0);
  end;

  if RCButtonPressed(7, 2) then begin
    t := GetRCValue(7, 3);
    t := arccos(t/0);
    t := t/t;
    WriteLn(FloatToStr(t));
  end;

  {SetRCValue(1,1, floattostr(GetSensorVal(0,0)));
  SetGlobalSensorVin(is1, ord(getModbusCoil(1)));

  if RCButtonPressed(5, 2)  then begin
    setModbusInput(1, true);
  end else if RCButtonPressed(5, 3) then begin
    setModbusInput(1, false);
  end;
  }
end;

// this procedure is called once when the script is started
procedure Initialize;
begin
  ClearButtons();
  is1 := GetGlobalSensorIndex('solenoid1');
  iball := GetThingIndex('Ball');
end;
