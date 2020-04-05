unit octree;

{$mode delphi}

interface

uses
  Classes, SysUtils{, fgl};


type
  TOctreeNode = class
    level: integer;
    parent: TOctreeNode;
    nodes: array[0..7] of TOctreeNode;
    x, y, z, d: double;
  end;

  //TOctreeNodeList = specialize TFPGObjectList<TOctreeNode>;


  TOctree = class
    root: TOctreeNode;
    max_level: integer;
  end;



implementation

end.

