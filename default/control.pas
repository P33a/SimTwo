// Global Variables Here
var
  NetOutBuf: TUDPBuffer;

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
  //NetPutByte(NetOutBuf, ord('0') + mtype); // Type = 1,2,3
  for i := 44 to 725 do begin
    d := mgetv(LaserScanRays, i, 0);
    if d < 0 then d := 0;
    ld := round(d * 1000);
    NetPutWord(NetOutBuf, ld);
  end;
  WriteUDPData(SendToIP, 9876, NetStringBuffer(NetOutBuf));
  //UDP.Send(NetOutBuf.data, NetOutBuf.MessSize, EditSendToIP.Text + ':9876');
end;

// this procedure is called periodicaly (default: 40 ms)
procedure Control;
var vs: TPoint3d;
    A, B, C, R: Matrix;
    i, n: integer;
    clr: TRGBAColor;
begin
  //StartSolidFire(0,1);
  {if KeyPressed(vk_down) then begin
    vs := GetThingSize(1);
    SetThingSize(1, 0.9*vs.x, 0.9*vs.y, 0.9*vs.z);
  end else if KeyPressed(vk_up) then begin
    vs := GetThingSize(1);
    SetThingSize(1, vs.x/0.9, vs.y/0.9, vs.z/0.9);
  end;}
  
  A := RangeToMatrix(4, 2, 3, 3);
  B := RangeToMatrix(4, 6, 3, 3);
  C := Mzeros(3, 3);
  MSetv(C, 1, 1, 3.5);
  
  clr := GetThingColor(2,0);
  SetRCValue( 23, 1, format('%d',[clr.red]));
  SetRCValue( 23, 2, format('%d',[clr.green]));
  SetRCValue( 23, 3, format('%d',[clr.blue]));

  if RCButtonPressed(9, 1) then begin
    R := Minv(A);
    MatrixToRange(9, 2, R);
  end;

  if RCButtonPressed(9, 5) then begin
    MatrixToRange(9, 6, C);
  end;

  if RCButtonPressed(14, 1) then begin
    R := MAdd(A,B);
    MatrixToRange(14, 2, R);
  end;

  if RCButtonPressed(19, 1) then begin
    R := MMult(A,B);
    MatrixToRange(19, 2, R);
  end;

  A := GetGlobalSensorValues(3);
  //A := GetSensorValues(0, 1);
  SendLaserMessage(A, '127.0.0.1');
  n := MNumRows(A);
  SetRCValue(1, 10, format('%d',[n]));
  //for i := 0 to n - 1 do begin
  //  SetRCValue( 2 + i, 10, format('%g',[mgetv(A, i, 0)]));
  //end;
end;

// this procedure is called once when the script is started
procedure Initialize;
begin
  ClearButtons();
  SetThingPos(2, 0, 0, 1);
  WriteLn('Test');
end;

