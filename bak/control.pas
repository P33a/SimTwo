// 
// 
type
  TTrajControlMode = (cmManual, cmScript);

  TTimeMeasures = record
    StartTime, intermediate, FinalTime: double;
  end;

  TCellKind = (ckpath, ckobstacle);
  TCell = record
    CellKind: TCellKind;
    //x, y: integer;           // This cell array coordinates
    xw, yw: double;          // Cell center world coordinates
  end;

  TMap = array[0..7] of array [0..7] of TCell;

  arr8x8 = array[0..7] of array [0..7] of integer;     //Acrescentamos estas variáveis

  TCellPath = record
    x, y: integer;           // This cell array coordinates
    prev_x, prev_y: integer; // The previous cell in the path
  end;

  TPath = record
    Cells: array[0..8 * 8 - 1] of TCellPath;  // To store the path
    numCells: integer;
  end;
  
// Global Variables
var
  irobot: integer;
  t: double;
  TimeMeasures: TTimeMeasures;
  go: boolean;
  side: double;

  TrajControlMode: TTrajControlMode;
  Vr, Wr: double;

  Map: TMap;
  distances: arr8x8;  //Acrescentamos estas variaveis

  BestPath: TPath;
  visited: TPath;     //Acrescentamos estas variaveis
  notVisited: TPath;   //Acrescentamos estas variaveis
  BestPathBackawrds: TPath;   //Acrescentamos estas variaveis

  state: integer;
  CurCellIdx: integer;

//Procedure, functions and Routines

procedure UpdateTimes(var TM: TTimeMeasures);
var i: integer;
    x, y: double;
begin
  if RCButtonPressed(24, 2) then begin
    SetRobotPos(0, 0, 0, 0.08, rad(90));
    TM.StartTime := t;
    TM.intermediate := 0;
    TM.FinalTime := 0;
    go := true;
  end else begin
    go := false;
  end;

  x := GetRobotX(0);
  y := GetRobotY(0);

  if (sqrt(sqr(x - 7 * side) +sqr(y - 7 * side)) < 0.05) and (TM.intermediate = 0) then begin
    TM.intermediate := t;
  end;

  if (sqrt(sqr(x) +sqr(y)) < 0.05) and (TM.intermediate <> 0) and (TM.FinalTime = 0)  then begin
    TM.FinalTime := t;
  end;

  SetRCValue(25, 2, format('%3.5g',[t - TM.StartTime]));

  if TM.intermediate <> 0 then begin
    SetRCValue(26, 2, format('%3.5g',[TM.intermediate - TM.StartTime]));
  end else begin
    SetRCValue(26, 2, '');
  end;

  if TM.FinalTime <> 0 then begin
    SetRCValue(27, 2, format('%3.5g',[TM.FinalTime - TM.StartTime]));
  end else begin
    SetRCValue(27, 2, '');
  end;

end;

procedure ManualTrajControl(var V, W: double);
var Vmax, Wmax: double;
begin
  Vmax := 0.5;
  Wmax := 4;

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

  V := V * Vmax;
  W := W * Wmax;
end;


procedure stop(var v, w: double);
begin
  v := 0;
  w := 0;
end;

procedure gotoXY(x, y, angle, xp, yp: double; var v, w: double);
var e_theta, theta_p: double;
begin
  theta_p := atan2(yp - y, xp - x);
  e_theta := DiffAngle(theta_p, angle);
  v := 0.4;
  w := 10 * e_theta;
end;


procedure rotate(angle, target_angle: double; var v, w: double);
var e_theta: double;
begin
  e_theta := DiffAngle(target_angle, angle);
  v := 0;
  w := 10 * e_theta;
end;


procedure ScriptTrajControl(var V, W: double);
var x_r, y_r, theta_r, e_theta: double;
    x_t, y_t: double;
    dp_tresh: double;
    ix, iy: integer;
begin
  V := 0;
  W := 0;

  dp_tresh := 0.05;

  x_r := GetRobotX(0);
  y_r := GetRobotY(0);
  theta_r := GetRobotTheta(0);

  SetRCValue(12, 2, format('%3.1g',[x_r]));
  SetRCValue(13, 2, format('%3.1g',[y_r]));
  SetRCValue(14, 2, format('%3.1g',[deg(theta_r)]));

  ix := BestPath.cells[CurCellIdx].x;
  iy := BestPath.cells[CurCellIdx].y;
  x_t := Map[ix][iy].xw;
  y_t := Map[ix][iy].yw;

  SetRCValue(7, 2, format('%d',[CurCellIdx]));
  SetRCValue(8, 2, format('%3.1g',[x_t]));
  SetRCValue(9, 2, format('%3.1g',[y_t]));

  if (state = 0) and go then begin
    state := 1;
    //go := false;
  end else if (state = 1) and (abs(sqrt(sqr(x_r - x_t) + sqr(y_r - y_t))) < dp_tresh) then begin
    inc(CurCellIdx);
    if CurCellIdx >= BestPath.numCells then begin
      state := 2;
      CurCellIdx := BestPath.numCells - 1;
      dec(CurCellIdx);
    end;
  end else if (state = 2) and (abs(sqrt(sqr(x_r - x_t) + sqr(y_r - y_t))) < dp_tresh) then begin
    dec(CurCellIdx);
    if CurCellIdx < 0 then begin
      state := 0;
      CurCellIdx := 0;
    end;
  end;

  if state = 0 then begin
    stop(V, W);
    CurCellIdx := 1;
  end else if state = 1 then begin
    gotoXY(x_r, y_r, theta_r, x_t, y_t, V, W);
  end else if state = 2 then begin
    gotoXY(x_r, y_r, theta_r, x_t, y_t, V, W);
  end;

  SetRCValue(4, 4, format('%d',[state]));
end;

procedure VWToV1V2(var v1, v2: double; V, W: double);
var kw, b: double;
begin
  b := 12.5;
  Kw := 10;
  V1 := (V - W/b) * kw;
  V2 := (V + W/b) * kw;
end;


procedure PathWayFromFile(Fname: string);
var Text: TStringList;
    s: string;
    i, r, c, rgb: integer;
    x, y, z, h: double;
begin
  h := 0.15;
  Text := 3.create;
  try
    Text.loadFromFile(Fname);
    if Text.count < 8 then exit;
    ClearObstacles();
    for r := 0 to 7 do begin
      s := Text[r];
      if length(s) < 8 then exit;
      for c := 0 to 7 do begin
        y := (7 - r) * side;
        x := c * side;
        z := -1;
        if s[c + 1] = '1' then begin
          z := 0;
          rgb := $0A8CFA;

        end else if s[c + 1] in ['0', '2'] then begin
          z := h/2;
          rgb := $555555;
        end;
        AddOBstacleBox('path' + IntToStr(c) + IntToStr(7 - r), x, y, z, side, side, h, rgb);
      end;
    end;

  finally
    Text.free;
  end;
end;


procedure BuildMap;
var i, x, y, z: integer;
    P: TPoint3D;
begin
  for i := 0 to 63 do begin
    P := GetObstaclePos(i);
    x := round(P.x / side);
    y := round(P.y / side);
    if P.z > 0 then begin
      Map[x][y].CellKind := ckobstacle;
    end else begin
      Map[x][y].CellKind := ckpath;
    end;
     Map[x][y].xw := P.x;
     Map[x][y].yw := P.y;
  end;

  for x := 0 to 7 do begin
    for y := 0 to 7 do begin
      SetRCValue(8 + 7 - y, 8 + x, IntToStr(ord(Map[x][y].CellKind)));
    end;
  end;
end;


procedure AddToPath(var Path: TPath; X, Y: integer; perv_X, prev_Y: integer);
begin
  if Path.numCells >= 64 then exit;

  Path.cells[Path.numCells].x := X;
  Path.cells[Path.numCells].y := Y;
  Path.cells[Path.numCells].prev_x := prev_X;
  Path.cells[Path.numCells].prev_y := prev_Y;
  inc(Path.numCells);
end;

procedure RemoveFromPath(var Path: TPath);
var i := 0
begin
  for index := 0 to Path.numCells - 1 do begin
      Path.cells[index].x = Path.cells[index + 1].x;
      Path.cells[index].y = Path.cells[index + 1].y;
  end;
  dec(Path.numCells);
end;

function min(var x1: integer; var x2:integer): integer;   //Função de minimo
begin
  if (x1 < x2) then begin
    result := x1;
    end;
  if (x1 > x2) then begin
    result := x2;
     end;
end;

function max(var x1: integer; var x2:integer): integer;   //Função de maximo
begin
  if (x1 > x2) then begin
    result := x1;
    end;
  if (x1 < x2) then begin
    result := x2;
     end;
end;

function checkIfExists(var path: TPath; var X: integer; var Y: integer): boolean;   //Verificar se está presente no Path
var i: integer;
begin
   for index := 0 to Path.numCells - 1 do begin
	if ((Path.cells[index].x = X) and (Path.cells[index].y = Y) then begin
		result := true;
		Exit;
	end;
   end;
   result := false;
end;

function getPrevX(var path: TPath; var X: integer; var Y: integer): integer;
var i: integer;
begin
   for index := 0 to Path.numCells - 1 do begin
	if ((Path.cells[index].x = X) and (Path.cells[index].y = Y) then begin
		result := Path.cells[index].prev_x;
		Exit;
	end;
   end;
end;

function getPrevY(var path: TPath; var X: integer; var Y: integer): integer;
var i: integer;
begin
   for index := 0 to Path.numCells - 1 do begin
	if ((Path.cells[index].x = X) and (Path.cells[index].y = Y) then begin
		result := Path.cells[index].prev_y;
		Exit;
	end;
   end;
end;

procedure FindBestPath(var Map: TMap; var Path: TPath);
var x, y, i: integer;
begin
   Path.numCells := 0;
   AddToPath(notVisited, 0, 0, 0, 0);
   distances[0][0]:= 0;

   while ((notVisited.count <> 0) and (checkIfExists(visited, 7, 7) = false) do begin
     for i :=0 to notVisited.count do begin
        if (Map[min(notVisited[i].x -1 , 0)][notVisited[i].y].TCellKind = ckpath) then begin
        	  if(checkIfExists(visited, min(notVisited[i].x -1 , 0), notVisited[i].y) = false or checkIfExists(visited, min(notVisited[i].x -1 , 0), notVisited[I].y) = false)
	     AddToPath(notVisited, min(notVisited[i].x -1 , 0), notVisited[I].y, notVisited[I].x, notVisited[i].y);
        end;

        if (Map[max(notVisited[i].x +1 , 7)][notVisited[i].y].TCellKind = ckpath) then begin
           if(checkIfExists(visited, max(notVisited[i].x +1 , 7), notVisited[I].y) = false or checkIfExists(visited, max(notVisited[i].x +1 , 7), notVisited[I].y) = false)
	     AddToPath(notVisited, max(notVisited[i].x +1 , 7), notVisited[I].y, notVisited[I].x, notVisited[i].y);
        end;

        if (Map[notVisited[i].x] [min(notVisited[i].y -1 , 0)].TCellKind = ckpath) then begin
           if(checkIfExists(visited, notVisited[i].x, min(notVisited[i].y -1 , 0)) = false or checkIfExists(visited, notVisited[i].x, min(notVisited[i].y -1 , 0)) = false)
	     AddToPath(notVisited, notVisited[i].x, min(notVisited[i].y -1 , 0), notVisited[I].x, notVisited[i].y);
        end;

        if (Map[notVisited[i].x] [max(notVisited[i].y +1 , 7)].TCellKind = ckpath) then begin
           if(checkIfExists(visited, notVisited[i].x, max(notVisited[i].y +1 , 7)) = false or checkIfExists(visited, notVisited[i].x, max(notVisited[i].y +1 , 7)) = false)
	     AddToPath(notVisited, notVisited[i].x, max(notVisited[i].y +1 , 7), notVisited[I].x, notVisited[i].y);
        end;

	AddToPath(visited, notVisited[i].x, notVisited[i].y);
	RemoveFromPath(notVisited);
      end;

    end;

  if (checkIfExists(visited, 7, 7) = false) then begin
     raiseException(erCustomError, 'The map is impossible');
  end;

  if (checkIfExists(visited, 7, 7) = true) then begin     
     currentCell := visited.numCells;
     currentX := visited.cells[currentCell].x;
     currentY := visited.cells[currentCell].y;
     AddToPath(BestPathBackawrds, 7, 7, visited.cells[currentCell].prev_x, visited.cells[currentCell].prev_y);
     while ((visited.cells[currentCell].prev_x <> 0) and (visited.cells[currentCell].prev_y <> 0)) do begin
	   AddToPath(BestPathBackawrds, getPrevX(visited, currentX, currentY), getPrevY(visited, currentX, currentY), 0, 0);
     end;
     AddToPath(BestPathBackawrds, 0, 0, 0, 0);

     pathSize := BestPathBackwards.numCells;
     for i := 0 to BestPathBackwards.numCells do begin
	AddToPath(Path, BestPathBackawrds.cells[pathSize].x, BestPathBackawrds[pathSize].y, 0, 0);
	dec(pathSize);
  end;
  
  for r := 0 to Path.numCells - 1 do begin
    SetRCValue(8 + 7 - Path.Cells[i].y, 8 + Path.Cells[i].x, '.');
  end;

end;

procedure Control;
var txt: string;
    i: integer;
    v, w, v1, v2: double;
begin
  t := t + ScriptPeriod();
  UpdateTimes(TimeMeasures);

  if RCButtonPressed(11, 3) then begin
    SetRobotPos(0, GetRCValue(12, 3), GetRCValue(13, 3), 0.15, rad(GetRCValue(14, 3)));
  end;

  if RCButtonPressed(2, 3) then begin
    TrajControlMode := cmManual;
    SetRCValue(2, 2, 'Manual');
  end else if RCButtonPressed(2, 4) then begin
    TrajControlMode := cmScript;
    SetRCValue(2, 2, 'Script');
  end;

  case TrajControlMode of
    cmManual: ManualTrajControl(Vr, Wr);
    cmScript: ScriptTrajControl(Vr, Wr);
  end;


  VWToV1V2(v1, v2, Vr, Wr);
  SetAxisVoltageRef(irobot, 0, v1);
  SetAxisVoltageRef(irobot, 1, v2);

  SetRCValue(5, 4, format('%d',[round(Vr)]));
  SetRCValue(6, 4, format('%d',[round(Wr)]));

  v := sqrt(sqr(GetRobotVx(0)) + sqr(GetRobotVy(0)));
  SetRCValue(5, 2, format('%3.1g',[v]));

  w := GetRobotW(0);
  SetRCValue(6, 2, format('%3.1g',[w]));

  if RCButtonPressed(4, 5) then begin
    state := round(GetRCValue(4, 6));
  end;

  if RCButtonPressed(5, 5) then begin
    state := round(GetRCValue(5, 6));
  end;

  if RCButtonPressed(4, 8) then begin
    SetRobotPos(0, 0, 0, 0.10, rad(90));
    PathWayFromFile('map.txt');
    BuildMap();
    FindBestPath(Map, BestPath);
  end;

  if RCButtonPressed(5, 8) then begin
    ClearObstacles();
  end;
end;


procedure Initialize;
begin
  ClearButtons();

  Vr := 0;
  Wr := 0;

  t := 0;
  TrajControlMode := cmManual;

  state := 0;

  side := 0.25;
  PathWayFromFile('map.txt');
  BuildMap();
  FindBestPath(Map, BestPath);
  SetRobotPos(0, 0, 0, 0.15, rad(90));
end;
