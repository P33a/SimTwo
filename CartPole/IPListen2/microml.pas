unit microml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type TDoubleArray = array of double;

procedure EncodeInteger(var StrPacket: TStringList; name: string; data: integer);
procedure EncodeDouble(var StrPacket: TStringList; name: string; data: double);
procedure EncodeDoubleArray(var StrPacket: TStringList; name: string; data: array of double);

function DecodeDoubleDef(var StrPacket: TStringList; name: string; defval: double): double;
function DecodeDoubleArray(var StrPacket: TStringList; name: string): TDoubleArray;


implementation

procedure EncodeInteger(var StrPacket: TStringList; name: string; data: integer);
begin
  StrPacket.add(name);
  StrPacket.add(format('%d',[data]));
  StrPacket.add('');
end;

procedure EncodeDouble(var StrPacket: TStringList; name: string; data: double);
begin
  StrPacket.add(name);
  StrPacket.add(format('%.6g',[data]));
  StrPacket.add('');
end;

procedure EncodeDoubleArray(var StrPacket: TStringList; name: string; data: array of double);
var i: integer;
begin
  StrPacket.add(name);
  for i := 0 to Length(data) - 1 do begin
    StrPacket.add(format('%.6g',[data[i]]));
  end;
  StrPacket.add('');
end;

function DecodeDoubleDef(var StrPacket: TStringList; name: string; defval: double): double;
var i: integer;
begin
  result := defval;
  i := StrPacket.indexof(name);
  if (i < 0) or (i + 1 >= StrPacket.count) then exit;
  result := strtofloat(StrPacket[i+1]);
end;

function DecodeDoubleArray(var StrPacket: TStringList; name: string): TDoubleArray;
var offset, i, size: integer;
begin
  setLength(result, 0);
  offset := StrPacket.indexof(name);  // Find the array name
  if (offset < 0)  then exit;
  size := 0;
  for i := offset + 1 to StrPacket.count - 1 do begin  // loop to finf its size
    if StrPacket[i] = '' then break;
    size := size + 1;
  end;

  if size = 0 then exit;

  setLength(result, size);

  for i := 0 to size - 1 do begin  // Loop to fill the array
    result[i] := strtofloat(StrPacket[offset + 1 + i]);
  end;
end;


end.





