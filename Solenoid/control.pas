// Global Variables Here
var is1, iball: integer;

// this procedure is called periodicaly (default: 40 ms)
procedure Control;
begin
  if RCButtonPressed(3, 2)  then begin
    SetGlobalSensorVin(is1, 1);
  end else if RCButtonPressed(3, 3) then begin
    SetGlobalSensorVin(is1, 0);
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
