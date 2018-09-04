unit RLan;

{$MODE Delphi}

interface

//uses
//  SysUtils, Classes, Graphics, Controls,
//  StdCtrls, ExtCtrls, math;
uses windows, extctrls, sysutils, classes, Math;

const
  UDPBufSize=1500;

type

  TUDPBuffer= record
    data: array[0..UDPBufSize-1] of byte;
    MessSize, ReadDisp: integer;
  end;

procedure ClearUDPBuffer(var Buf: TUDPBuffer);

procedure NetPutBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
procedure NetBufferSeek(var Buf: TUDPBuffer; disp: integer);
procedure NetGetBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
function NetStringBuffer(var Buf: TUDPBuffer): string;

procedure NetPutByte(var Buf: TUDPBuffer; value: byte);
procedure NetPutWord(var Buf: TUDPBuffer; value: word);
procedure NetPutShort(var Buf: TUDPBuffer; value: SmallInt);
procedure NetPutInt(var Buf: TUDPBuffer; value: integer);
procedure NetPutFloat(var Buf: TUDPBuffer; value: single);
procedure NetPutString(var Buf: TUDPBuffer; str: string);
procedure NetPutAngle(var Buf: TUDPBuffer; value: double);

function NetPeekByte(var Buf: TUDPBuffer): byte;
function NetGetByte(var Buf: TUDPBuffer): byte;
function NetGetWord(var Buf: TUDPBuffer): word;
function NetGetShort(var Buf: TUDPBuffer): SmallInt;
function NetGetInt(var Buf: TUDPBuffer): integer;
function NetGetFloat(var Buf: TUDPBuffer): single;
function NetGetString(var Buf: TUDPBuffer): string;
function NetGetAngle(var Buf: TUDPBuffer): double;
procedure Test(var B: double);

implementation

uses Utils;

procedure NetPutBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
begin
  if Buf.MessSize+size_it >= UDPBufSize then
    raise Exception.Create('Write: UDP Message size too big');
  //copyMemory(@(Buf.data[Buf.MessSize]),@data,size_it);
  move(data, Buf.data[Buf.MessSize], size_it);
  Buf.MessSize:=Buf.MessSize+size_it;
end;

procedure NetBufferSeek(var Buf: TUDPBuffer; disp: integer);
begin
  if (disp>Buf.MessSize) or (disp=-1) then disp:=Buf.MessSize;
  Buf.ReadDisp:=disp;
end;

procedure NetGetBuffer(var Buf: TUDPBuffer; var data; size_it: integer);
begin
  if Buf.ReadDisp+size_it>Buf.MessSize then
    raise Exception.Create('Read: UDP Message size too big');
  //copyMemory(@data,@(Buf.data[Buf.ReadDisp]),size_it);
  move(Buf.data[Buf.ReadDisp], data, size_it);
  Buf.ReadDisp:=Buf.ReadDisp+size_it;
end;

function NetStringBuffer(var Buf: TUDPBuffer): string;
begin
  result := stringofchar(chr(0), Buf.MessSize);
  move(Buf.data[0], result[1], Buf.MessSize);
end;

procedure NetPutByte(var Buf: TUDPBuffer; value: byte);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutWord(var Buf: TUDPBuffer; value: word);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutShort(var Buf: TUDPBuffer; value: SmallInt);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutInt(var Buf: TUDPBuffer; value: integer);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutFloat(var Buf: TUDPBuffer; value: single);
begin
  NetPutBuffer(Buf,value,sizeof(value));
end;

procedure NetPutString(var Buf: TUDPBuffer; str: string);
var len: word;
begin
  len:=length(str);
  //NetPutBuffer(Buf,len,sizeof(len));
  NetPutWord(Buf,len);
  if len>0 then
    NetPutBuffer(Buf,str[1],len);
end;

procedure NetPutAngle(var Buf: TUDPBuffer; value: double);
var tmp: word;
begin
  tmp:=round((NormalizeAngle(value)+pi)*10000);
  NetPutWord(Buf,tmp);
end;

function NetPeekByte(var Buf: TUDPBuffer): byte;
begin
  result:=0;
  if Buf.ReadDisp>Buf.MessSize then exit;
  result:=Buf.data[Buf.ReadDisp];
end;

function NetGetByte(var Buf: TUDPBuffer): byte;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetWord(var Buf: TUDPBuffer): word;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetShort(var Buf: TUDPBuffer): SmallInt;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetInt(var Buf: TUDPBuffer): integer;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetFloat(var Buf: TUDPBuffer): single;
begin
  NetGetBuffer(Buf,result,sizeof(result));
end;

function NetGetString(var Buf: TUDPBuffer): string;
var size: word;
begin
  result:='';
  size:=NetGetWord(Buf);
  if size=0 then exit;
  result:=stringofchar(chr(0),size);
  NetGetBuffer(Buf,result[1],size);
end;

function NetGetAngle(var Buf: TUDPBuffer): double;
begin
  result:=NormalizeAngle(NetGetWord(buf)/10000-pi);
end;


procedure ClearUDPBuffer(var Buf:TUDPBuffer);
begin
  zeroMemory(@(Buf.data[0]),UDPBufSize);
  //fillbyte(Buf.data, sizeof(Buf.data), 0);
  Buf.MessSize:=0;
end;

procedure Test(var B: double);
begin

end;

end.
