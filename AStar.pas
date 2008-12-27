unit AStar;

interface

uses windows, extctrls, sysutils, classes, Graphics, Math;


const
  AStarGridXSize = 128;
  AStarGridYSize = 128;

  AStarVirgin = 0;
  AStarObstacle = 1;
  AStarClosed = 2;
  AStarOpen = 3;

  FixedPointConst = 65536;

  sqrt2 = round(1.4142135623730950488016887242097 * FixedPointConst);
  //sqrt2 = 92682;


type
  TGridCoord = record
    x,y: Smallint;
  end;

  TNeighbour = record
    x,y: Smallint;
    dist: integer;
  end;

const
  nilCoord: TGridCoord = (x:0; y:0);

  EightWayNeighbours: array[0..7] of TNeighbour =
    ((x:1;  y:0; dist:FixedPointConst), (x:1;  y:-1; dist:sqrt2), (x:0; y:-1; dist:FixedPointConst), (x:-1; y:-1; dist:sqrt2),
     (x:-1; y:0; dist:FixedPointConst), (x:-1; y:1; dist:sqrt2),  (x:0; y:1; dist:FixedPointConst),  (x:1;  y:1; dist:sqrt2));

type
  PAStarCell = ^TAStarCell;
  TAStarCell = record
    G,H: integer;
    ParentDir: smallint;
    HeapIdx: smallint;
    MyCoord: TGridCoord;
  end;

  TAStarGridState = array[0..AStarGridXSize-1, 0..AStarGridYSize-1] of byte;
  TAStarGrid = array[0..AStarGridXSize-1, 0..AStarGridYSize-1] of TAStarCell;

  TAStarHeapArray = record
    count: integer;
    data: array[0 .. 2047] of PAStarCell;
  end;


  TAStarProfile = record
    RemovePointFromAStarList_count: integer;
    RemoveBestFromAStarList_count: integer;
    AddToAStarList_count: integer;
    HeapArrayTotal: integer;
    comparesTotal: integer;
    iter: integer;
  end;

  TAStarMap = record
    InitialPoint, TargetPoint: TGridCoord;
    Grid: TAStarGrid;
    GridState: TAStarGridState;
    HeapArray: TAStarHeapArray;
    Profile: TAStarProfile;
    EucliDistK: double;
  end;

  TRGB32= record
    b,g,r,a: byte;
  end;
  pTRGB32 = ^TRGB32;

  TAStarPath = record
    points: array of TGridCoord;
    count: integer;
    dist: double;
  end;

procedure AStarClear(var Map: TAStarMap);
procedure AStarInit(var Map: TAStarMap);
procedure AStarStep(var Map: TAStarMap);
procedure AStarGo(var Map: TAStarMap);

function AStarPath(var Map: TAStarMap): TAStarPath;

procedure ShowAStarOpenList(var Map: TAStarMap; strs: TStrings);
procedure ShowAStarProfileData(var Map: TAStarMap; strs: TStrings);

procedure RecalcSqrtCache(var Map: TAStarMap);

var
  CalcHCache: array[0..AStarGridXSize-1, 0..AStarGridYSize-1] of integer;

implementation


procedure RecalcSqrtCache(var Map: TAStarMap);
var x, y: integer;
begin
  for x := 0 to AStarGridXSize - 1 do begin
    for y := 0 to AStarGridYSize - 1 do begin
      CalcHCache[x,y] := round(sqrt(x * x * 1.0 + y * y) * Map.EucliDistK * FixedPointConst);
      Map.Grid[x,y].MyCoord.x := x;
      Map.Grid[x,y].MyCoord.y := y;
    end;
  end;
end;

procedure AStarClear(var Map: TAStarMap);
var i: integer;
begin
  // clear the grid, set all nodes to state "virgin"
  FillChar(Map.GridState,sizeof(TAStarGridState), ord(AStarVirgin));
  //ZeroMemory(@Map.GridState[0,0],sizeof(TAStarGridState));

  // Build the wall around the grid,
  // this way, we don't have to worry about the frontier conditions
  for i:=0 to AStarGridXSize-1 do begin
    Map.GridState[i,0]:= AStarObstacle;
    Map.GridState[i,AStarGridYSize-1]:= AStarObstacle;
  end;

  for i:=0 to AStarGridYSize-1 do begin
    Map.GridState[0,i]:= AStarObstacle;
    Map.GridState[AStarGridXSize-1,i]:= AStarObstacle;
  end;

  // clear the heap
  Map.HeapArray.Count := 0;
end;


function GridCoordIsEqual( G1,G2: TGridCoord): boolean;
begin
  if (g1.x = g2.x) and (g1.y = g2.y) then result := true
  else result := false;
end;

function GridCoordIsNil( G1: TGridCoord): boolean;
begin
  if (g1.x = 0) and (g1.y = 0) then result := true
  else result := false;
end;


// ---------------------------------------------------------------
//     Heap Operations

function CalcHeapCost(var Map: TAStarMap; idx: integer): integer;
begin
  with Map.HeapArray.data[idx]^ do begin
    result := G + H;
  end;
end;

procedure SwapHeapElements(var Map: TAStarMap; idx1, idx2: integer);
var ptr1, ptr2: PAStarCell;
begin
  ptr1 := Map.HeapArray.data[idx1];
  ptr2 := Map.HeapArray.data[idx2];
  ptr1^.HeapIdx := idx2;
  ptr2^.HeapIdx := idx1;
  Map.HeapArray.data[idx1] := ptr2;
  Map.HeapArray.data[idx2] := ptr1;
end;

procedure UpdateHeapPositionByPromotion(var Map: TAStarMap; idx: integer);
var parent_idx: integer;
    node_cost: integer;
begin
  // if we are on the first node, there is no way to promote
  if idx = 0 then exit;
  // calc node cost
  node_cost := CalcHeapCost(Map, idx);
  // repeat until we can promote no longer
  while true do begin
    // if we are on the first node, there is no way to promote
    if idx = 0 then exit;

    parent_idx := (idx - 1) div 2;
    // if the parent is better than we are, there will be no promotion
    if CalcHeapCost(Map, parent_idx) < node_cost then exit;
    // if not, just promote it
    SwapHeapElements(Map, idx, parent_idx);
    idx:= parent_idx;
  end;
end;

// update one node after increasing its cost
procedure UpdateHeapPositionByDemotion(var Map: TAStarMap; idx: integer);
var c1, c2, cost: integer;
    idx_child, new_idx: integer;
begin

  cost := CalcHeapCost(Map, idx);

  while true do begin

    idx_child := idx * 2 + 1;
    // if the node has no childs, there is no way to demote
    if idx_child >= Map.HeapArray.count then exit;

    // calc our cost and the first node cost
    c1 := CalcHeapCost(Map, idx_child);
    // if there is only one child, just compare with this one
    if idx_child + 1 >= Map.HeapArray.count then begin
      // if we are better than this child, then no demotion
      if cost < c1 then exit;
      // if not, then do the demotion
      SwapHeapElements(Map, idx, idx_child);
      exit;
    end;

    // calc the second node cost
    c2 := CalcHeapCost(Map, idx_child + 1);

    // select the best node to demote to
    new_idx := idx;
    if c2 < cost then begin
      if c1 < c2 then begin
        new_idx := idx_child;
      end else begin
        new_idx := idx_child + 1;
      end;
    end else if c1 < cost then begin
      new_idx := idx_child;
    end;

    // if there is no better child, just return
    if new_idx = idx then exit;

    // if we want to demote, then swap the elements
    SwapHeapElements(Map, idx, new_idx);
    idx := new_idx;
  end;
end;


procedure RemoveBestFromAStarList(var Map: TAStarMap; out Pnt: TGridCoord);
begin
  inc(Map.Profile.RemoveBestFromAStarList_count);

  with Map.HeapArray do begin
    // return the first node
    Pnt := data[0]^.MyCoord;
    // move the last node into the first position
    data[count - 1]^.HeapIdx := 0;
    data[0] := data[count - 1];
    // update the array size
    Dec(count);
  end;
  // re-sort that "first" node
  UpdateHeapPositionByDemotion(Map,0);
end;


procedure AddToAStarList( var Map: TAStarMap; Pnt: TGridCoord);
var idx: integer;
begin
  inc(Map.Profile.AddToAStarList_count);

  // update the grid state
  Map.GridState[Pnt.x, Pnt.y]:=AStarOpen;

  // insert at the bottom of the heap
  idx := Map.HeapArray.count;
  Map.HeapArray.data[idx] := @(Map.Grid[Pnt.x,Pnt.y]);
  Map.Grid[Pnt.x,Pnt.y].HeapIdx := idx;
  Inc(Map.HeapArray.count);
  
  // update by promotion up to the right place
  UpdateHeapPositionByPromotion(Map, idx);
end;


function CalcH(var Map: TAStarMap; Pi, Pf: TGridCoord): integer;
begin
  Result:= CalcHCache[Abs(Pi.x-Pf.x), Abs(Pi.y-Pf.y)];
end;


procedure AStarStep(var Map: TAStarMap);
var curPnt, NewPnt: TGridCoord;
    ith, NeighboursCount: integer;
    newG: integer;
begin
  inc(Map.Profile.iter);
  inc(Map.Profile.HeapArrayTotal, Map.HeapArray.count);

  RemoveBestFromAStarList(Map,curPnt);
  Map.GridState[curPnt.x,curPnt.Y]:=AStarClosed;

  NeighboursCount := 8;
  for ith:=0 to NeighboursCount-1 do begin
    NewPnt.x := CurPnt.x + EightWayNeighbours[ith].x;
    NewPnt.y := CurPnt.y + EightWayNeighbours[ith].y;

    case Map.GridState[NewPnt.x, NewPnt.y] of
      AStarClosed: continue;

      AStarVirgin: begin
        with Map.Grid[NewPnt.x, NewPnt.y] do begin
          ParentDir := ith;
          G := Map.Grid[CurPnt.x, CurPnt.y].G + EightWayNeighbours[ith].dist;
          H := CalcH(Map, NewPnt, Map.TargetPoint);
        end;
        AddToAStarList(Map,NewPnt);
      end;

      AStarOpen: begin
        newG := Map.Grid[CurPnt.x, CurPnt.y].G + EightWayNeighbours[ith].dist;
        if newG < Map.Grid[NewPnt.x, NewPnt.y].G then begin
          Map.Grid[NewPnt.x, NewPnt.y].G := newG;
          Map.Grid[NewPnt.x, NewPnt.y].ParentDir := ith;
          UpdateHeapPositionByPromotion(Map, Map.Grid[NewPnt.x, NewPnt.y].HeapIdx);
        end;
      end;
    end;
  end;

end;


procedure AStarInit(var Map: TAStarMap);
begin
  zeromemory(@(Map.Profile),sizeof(Map.Profile));
  AddToAStarList(Map, Map.InitialPoint);
  Map.Grid[Map.InitialPoint.x, Map.InitialPoint.y].H:= CalcH( Map, Map.InitialPoint, Map.TargetPoint);
end;

// check if the given point is inside an obstacle and move it to the closest open space
procedure AStarCheckBoundary(var pnt: TGridCoord; var Map: TAStarMap);
var i, j, x, y: integer;
begin
  // if we are not inside an obstacle, just return
  if Map.GridState[pnt.x,pnt.y] <> AStarObstacle then exit;

  for i := 1 to AStarGridXSize - 1 do begin
    for j := 0 to 7 do begin
      x := pnt.x + EightWayNeighbours[j].x * i;
      y := pnt.y + EightWayNeighbours[j].y * i;
      if (x >= 0) and (y >= 0) and (x < AStarGridXSize) and (y < AStarGridYSize) then begin
        if Map.GridState[x,y] <> AStarObstacle then begin
          pnt.x := x;
          pnt.y := y;
          exit;
        end;
      end;
    end;
  end;
end;

procedure AStarCheckBoundaries(var Map: TAStarMap);
begin
  AStarCheckBoundary(Map.InitialPoint, Map);
  AStarCheckBoundary(Map.TargetPoint, Map);
end;

procedure AStarGo(var Map: TAStarMap);
begin
  AStarCheckBoundaries(Map);
  AStarInit(Map);
  while true do begin
    AStarStep(Map);

    //if Map.iter >= 400 then exit;
    // found the way
    if Map.GridState[Map.TargetPoint.x, Map.TargetPoint.y] = AStarClosed then break;
    // there is no path
    if Map.HeapArray.count = 0 then break;
  end;
end;

procedure ShowAStarOpenList(var Map: TAStarMap; strs: TStrings);
//var Pnt, NextPnt, PrevPnt: TGridCoord;
begin
  //TODO
{  strs.beginUpdate;
  with Map do begin
    Pnt:=OpenList.head;
    if GridCoordIsNil(Pnt) then exit;
    while true do begin
      PrevPnt:=Grid[Pnt.x,pnt.y].ListPrev;
      NextPnt:=Grid[Pnt.x,pnt.y].ListNext;
      strs.Add(format('%.2f - %d,%d n:%d,%d p:%d,%d',[Grid[Pnt.x,pnt.y].G+Grid[Pnt.x,pnt.y].H, Pnt.x, Pnt.y, NextPnt.x, NextPnt.y, PrevPnt.x, PrevPnt.y]));
      if GridCoordIsNil(NextPnt) then break;
      Pnt:=NextPnt;
    end;
  end;
  strs.EndUpdate;}
end;

procedure ShowAStarProfileData(var Map: TAStarMap; strs: TStrings);
begin
  strs.beginUpdate;
  with Map do begin
    strs.Add(format('Iters: %d',[Profile.iter]));
    strs.Add(format('Removes: %d',[Profile.RemovePointFromAStarList_count]));
    strs.Add(format('Best Removes: %d',[Profile.RemoveBestFromAStarList_count]));
    strs.Add(format('Adds: %d',[Profile.AddToAStarList_count]));
    strs.Add(format('Mean List Count: %.2f',[Profile.HeapArrayTotal/Profile.iter]));
    strs.Add(format('Mean Compares: %.2f',[Profile.comparesTotal/Profile.iter]));

  end;
  strs.EndUpdate;
end;

function AStarPath(var Map: TAStarMap): TAStarPath;
var i, xm, ym, dir: integer;
begin
  result.count := 1;
  result.dist := 0;
  // Find path lenght
  with map do begin
    xm:=TargetPoint.x;
    ym:=TargetPoint.y;
    while true do begin
      if (xm = InitialPoint.x) and (ym=InitialPoint.y) then break;
      dir:=Grid[xm,ym].ParentDir;
      if (dir < 0) or (dir > high(EightWayNeighbours)) then begin
        result.count := 0;
        exit;
      end;
      xm:=xm-EightWayNeighbours[dir].x;
      ym:=ym-EightWayNeighbours[dir].y;
      inc(result.count);
      result.dist := result.dist + EightWayNeighbours[dir].dist;
    end;
  end;
  SetLength(result.points, result.count);
  // Fill path
  i := result.count - 1;
  with map do begin
    xm:=TargetPoint.x;
    ym:=TargetPoint.y;
    while true do begin
      result.points[i].x := xm;
      result.points[i].y := ym;
      if (xm = InitialPoint.x) and (ym=InitialPoint.y) then break;

      dir:=Grid[xm,ym].ParentDir;
      xm:=xm-EightWayNeighbours[dir].x;
      ym:=ym-EightWayNeighbours[dir].y;
      dec(i);
    end;
  end;
end;


end.
