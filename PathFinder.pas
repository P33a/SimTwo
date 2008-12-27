unit PathFinder;

interface

uses windows, extctrls, sysutils, classes, Graphics, Math, AStar;

procedure ClearAStarMap;
procedure SetupAStarMap(xi, yi, xt, yt: integer; nEucliDistK: double);
procedure AddAStarObstacleCircle(xc, yc, r: double);
procedure AddAStarObstacleRect(xi, yi, xf, yf: double);
procedure CalcAStarPath;
function GetAStarPathPoint(i: integer): TPoint;

var AStarMap: TAStarMap;
    AStarActPath: TAStarPath;

implementation


procedure InitAStarMap;
begin
  RecalcSqrtCache(AStarMap);
end;

procedure ClearAStarMap;
begin
  AStarClear(AStarMap);
end;

procedure SetupAStarMap(xi, yi, xt, yt: integer; nEucliDistK: double);
begin

  with AStarMap.InitialPoint do begin
    x := xi;
    y := yi;
  end;

  with AStarMap.TargetPoint do begin
    x := xt;
    y := yt;
  end;

  AStarMap.EucliDistK:= nEucliDistK;
end;

procedure AddAStarObstacleCircle(xc, yc, r: double);
var i, j: integer;
    dx: double;
begin
  for i:=0 to round(r) do begin
    dx:=sqrt(r*r - i*i);
    for j:= -round(dx)+1 to round(dx)-1 do begin
      AStarMap.GridState[round(xc+j),round(yc+i)]:= AStarObstacle;
      AStarMap.GridState[round(xc+j),round(yc-i)]:= AStarObstacle;
    end;
  end;
end;


procedure AddAStarObstacleRect(xi, yi, xf, yf: double);
var x, y: integer;
begin
  for y := round(yi) to round(yf) do begin
    for x := round(xi) to round(xf) do begin
      AStarMap.GridState[x,y]:= AStarObstacle;
    end;
  end;
end;


procedure CalcAStarPath;
begin
  AStarGo(AStarMap);
  AStarActPath := AStarPath(AStarMap);
end;


function GetAStarPathPoint(i: integer): TPoint;
begin
  result.x := AStarActPath.Points[i].x;
  result.y := AStarActPath.Points[i].y;
end;

initialization

RecalcSqrtCache(AStarMap);

end.


