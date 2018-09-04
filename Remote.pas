unit Remote;

{$MODE Delphi}

interface

const
  MaxRemWheels = 4;
  MaxRemIrSensors = 8;

type

  TRemRobot = packed record
    x,y,teta: single;
    vt,vn,w: single;

    Odos: packed array[0..MaxRemWheels-1] of integer;
    IRSensors: packed array[0..MaxRemIrSensors-1] of single;

    //Scanner: packed array[1..64] of byte;
    //ScannerAngle: double;
    //has_positions, has_speeds, has_odos: byte;
  end;

  TRemBall = packed record
    x,y,vx,vy: single;
    //bcolor: integer;
    //goal: boolean;
  end;

  TRemState = packed record
    id,Number: integer;
    Robot: TRemRobot;
    Ball: TRemBall;
  end;

  TRemControl = packed record
    U: packed array[0..MaxRemWheels-1] of single;
    Wref: packed array[0..MaxRemWheels-1] of single;
    num, kick1, kick2, flags: byte;
    x,y,z,teta: single;
  end;



implementation

end.
