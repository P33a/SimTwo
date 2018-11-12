unit OBufferedStreams;

{

  LICENSE

  Author: Ondrej Pokorny, http://www.kluug.net

  License: MPL / GPL / LGPL

}

{$I OBufferedStreams.inc}

{$IFDEF O_DELPHI_XE4}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  SysUtils, Classes;

type

  {$IFNDEF O_DELPHI_2009}
    TBytes = Array of Byte;
  {$ENDIF}

  TOBufferedWriteStream = class(TStream)
  private
    fStream: TStream;
    fStreamPosition: Int64;
    fStreamSize: Int64;

    fTempBuffer: TBytes;
    fBufferSize: Cardinal;
  protected
    function GetSize: Int64; {$IFNDEF O_DELPHI_6}override;{$ENDIF}
  public
    constructor Create(const aStream: TStream; const aBufferSize: Cardinal = 10*1024 {10 KB});
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF O_DELPHI_XE3}
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    {$ENDIF}

    function Read(var {%H-}Buffer; {%H-}Count: Longint): Longint; override;
    {$IFDEF O_DELPHI_XE3}
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    {$ENDIF}

    function Seek(Offset: Longint; Origin: Word): Longint; override;
  public
    procedure EnsureEverythingWritten;
  public
    property BufferSize: Cardinal read fBufferSize write fBufferSize;
  end;

  TOBufferedReadStream = class(TStream)
  private
    fStream: TStream;
    fStreamPosition: Int64;
    fStreamSize: Int64;
    fTempBuffer: TBytes;
    fTempBufferPosition: Cardinal;
    fBlockFlushTempStream: Integer;

    fBufferSize: Cardinal;

    procedure ClearTempBuffer;
    procedure CheckTempBuffer(const aReadBytes: Int64);
  protected
    function GetSize: Int64; {$IFNDEF O_DELPHI_6}override;{$ENDIF}
  public
    constructor Create(const aStream: TStream; const aBufferSize: Cardinal = 10*1024 {10 KB});
    destructor Destroy; override;

    function Write(const {%H-}Buffer; {%H-}Count: Longint): Longint; override;
    {$IFDEF O_DELPHI_XE3}
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    {$ENDIF}

    function Read(var Buffer; Count: Longint): Longint; override;
    {$IFDEF O_DELPHI_XE3}
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    {$ENDIF}

    function Seek(Offset: Longint; Origin: Word): Longint; override;
  public
    procedure BlockFlushTempStream;
    procedure UnblockFlushTempStream;
  public
    property BufferSize: Cardinal read fBufferSize write fBufferSize;
  end;

implementation

{ TOBufferedWriteStream }

constructor TOBufferedWriteStream.Create(const aStream: TStream;
  const aBufferSize: Cardinal);
begin
  inherited Create;

  if not Assigned(aStream) then
    raise Exception.Create('TOBufferedWriteStream.Create: nil aStream parameter!');

  fStream := aStream;
  fStreamPosition := fStream.Position;
  fStreamSize := fStream.Size;

  fBufferSize := aBufferSize;

  SetLength(fTempBuffer, 0);
end;

destructor TOBufferedWriteStream.Destroy;
begin
  EnsureEverythingWritten;

  SetLength(fTempBuffer, 0);

  inherited;
end;

procedure TOBufferedWriteStream.EnsureEverythingWritten;
var
  xInc: Integer;
begin
  if Length(fTempBuffer) > 0 then begin
    xInc := fStream.Write(fTempBuffer[0], Length(fTempBuffer));
    fStreamSize := fStreamSize + xInc;
    fStreamPosition := fStreamPosition + xInc;
    SetLength(fTempBuffer, 0);
  end;
end;

function TOBufferedWriteStream.GetSize: Int64;
begin
  Result := fStreamSize + Length(fTempBuffer);
end;

function TOBufferedWriteStream.Read(var Buffer; Count: Longint): Longint;
begin
  {$IFDEF FPC}
  Result := 0;//JUST TO OMIT WARNING MESSAGES
  {$ENDIF}
  raise Exception.Create('TOBufferedWriteStream.Read: You can''t read from TOBufferedWriteStream!');
end;

{$IFDEF O_DELPHI_XE3}
function TOBufferedWriteStream.Read(Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  Result := Self.Read(Buffer[Offset], Count);
end;
{$ENDIF}

function TOBufferedWriteStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if (Origin = soFromCurrent) and (Offset = 0) then begin
    Result := fStreamPosition + Length(fTempBuffer);
  {$IFDEF DELPHI6_DOWN}
  //BECAUSE OF GetSize function!!!
  end else if (Origin = soFromEnd) and (Offset = 0) then begin
    Result := GetSize;
  end else if (Origin = soFromBeginning) and (Offset = fStreamPosition + Length(fTempBuffer)) then begin//CURRENT POSITION
    Result := fStreamPosition + Length(fTempBuffer);
  {$ENDIF}
  end else begin
    raise Exception.Create('TOBufferedWriteStream.Seek: You can''t use seek in TOBufferedWriteStream');
  end;
end;

function TOBufferedWriteStream.Write(const Buffer; Count: Longint): Longint;
var
  xOldTempLength: Int64;
begin
  if Int64(Length(fTempBuffer)) > Int64(fBufferSize) then
    EnsureEverythingWritten;//WRITE TEMP BUFFER

  if Int64(Count) > Int64(fBufferSize) then begin
    //count to write bigger then buffer -> write directly
    fStream.Write(Buffer, Count);
  end else if Count > 0 then begin
    //store to temp!
    xOldTempLength := Length(fTempBuffer);
    SetLength(fTempBuffer, xOldTempLength + Count);
    Move(Buffer, fTempBuffer[xOldTempLength], Count);
  end;
  Result := Count;
end;

{$IFDEF O_DELPHI_XE3}
function TOBufferedWriteStream.Write(const Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  Result := Self.Write(Buffer[Offset], Count);
end;
{$ENDIF}

{ TOBufferedReadStream }

procedure TOBufferedReadStream.BlockFlushTempStream;
begin
  Inc(fBlockFlushTempStream);
end;

procedure TOBufferedReadStream.CheckTempBuffer(const aReadBytes: Int64);
var
  xLastLength, xReadBytes: Cardinal;
  xInc: Integer;
begin
  if fTempBufferPosition+aReadBytes > Length(fTempBuffer) then begin
    //LOAD NEXT BUFFER INTO TEMP STREAM, LEAVE UNREAD TAIL
    if fBlockFlushTempStream = 0 then
      ClearTempBuffer;

    if fStreamPosition < fStreamSize then begin
      xLastLength := Length(fTempBuffer);
      xReadBytes := fBufferSize;

      //CHECK THAT WE HAVE ALL NECESSARY BYTES IN TEMP BUFFER
      if Length(fTempBuffer)-Int64(fTempBufferPosition)+xReadBytes < Int64(aReadBytes) then
        xReadBytes := aReadBytes - (Length(fTempBuffer)-Int64(fTempBufferPosition));

      //CHECK THAT WE READ ONLY TO STREAM SIZE
      if xReadBytes > fStreamSize-fStreamPosition then
        xReadBytes := fStreamSize-fStreamPosition;

      SetLength(fTempBuffer, xLastLength+xReadBytes);
      xInc := fStream.Read(fTempBuffer[xLastLength], xReadBytes);
      fStreamPosition := fStreamPosition + xInc;
    end;
  end;
end;

procedure TOBufferedReadStream.ClearTempBuffer;
var
  xBuffer: TBytes;
  xBufferLength: Int64;
begin
  if fTempBufferPosition > fBufferSize then begin
    xBufferLength := Length(fTempBuffer) - Int64(fTempBufferPosition);
    if xBufferLength < 0 then
      xBufferLength := 0;

    SetLength(xBuffer, xBufferLength);

    if xBufferLength > 0 then
      Move(fTempBuffer[fTempBufferPosition], xBuffer[0], xBufferLength);

    SetLength(fTempBuffer, xBufferLength);

    if xBufferLength > 0 then
      Move(xBuffer[0], fTempBuffer[0], xBufferLength);

    fTempBufferPosition := 0;
  end;
end;

constructor TOBufferedReadStream.Create(const aStream: TStream;
  const aBufferSize: Cardinal);
begin
  inherited Create;

  if not Assigned(aStream) then
    raise Exception.Create('TOBufferedReadStream.Create: nil aStream parameter!');

  fStream := aStream;
  fStreamPosition := fStream.Position;
  fStreamSize := fStream.Size;

  fBufferSize := aBufferSize;

  SetLength(fTempBuffer, 0);
end;

destructor TOBufferedReadStream.Destroy;
begin
  SetLength(fTempBuffer, 0);

  inherited;
end;

function TOBufferedReadStream.GetSize: Int64;
begin
  Result := fStreamSize;
end;

{$IFDEF O_DELPHI_XE3}
function TOBufferedReadStream.Read(Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  Result := Self.Read(Buffer[Offset], Count);
end;
{$ENDIF}

function TOBufferedReadStream.Read(var Buffer; Count: Longint): Longint;
begin
  CheckTempBuffer(Count);

  if Count > Length(fTempBuffer) - Int64(fTempBufferPosition) then
    Count := Length(fTempBuffer) - Int64(fTempBufferPosition);

  if Count > 0 then begin
    Move(fTempBuffer[fTempBufferPosition], Buffer, Count);
    fTempBufferPosition := fTempBufferPosition + Cardinal(Count);
  end;

  Result := Count;
end;

function TOBufferedReadStream.Seek(Offset: Integer; Origin: Word): Longint;
var
  xAbsolutePosition: Int64;
begin
  if (Origin = soFromCurrent) and (Offset = 0) then begin
    //CURRENT POSITION
    Result := fStreamPosition - Length(fTempBuffer) + fTempBufferPosition;
  end else begin
    //SEEK TO POSITION AND CLEAR TEMP STREAM

    case Origin of
      soFromCurrent: xAbsolutePosition := fStreamPosition - Length(fTempBuffer) + fTempBufferPosition + Offset;
      soFromEnd: xAbsolutePosition := fStreamSize + Offset;
    else
      //soFromBeginning
      xAbsolutePosition := Offset;
    end;

    if (xAbsolutePosition > (fStreamPosition - Length(fTempBuffer))) and
      (xAbsolutePosition > (fStreamPosition - Length(fTempBuffer)))
    then begin
      //WITHIN TEMP RANGE
      fTempBufferPosition := xAbsolutePosition - (fStreamPosition - Length(fTempBuffer));
      Result := fStreamPosition - Length(fTempBuffer) + fTempBufferPosition;
    end else begin
      //OUTSIDE TEMP RANGE, CLEAR TEMP STREAM
      Result := fStream.Seek(soFromBeginning, xAbsolutePosition);
      fStreamPosition := Result;
      SetLength(fTempBuffer, 0);
      fTempBufferPosition := 0;
    end;
  end;
end;

procedure TOBufferedReadStream.UnblockFlushTempStream;
begin
  if fBlockFlushTempStream > 0 then
    Dec(fBlockFlushTempStream);
end;

{$IFDEF O_DELPHI_XE3}
function TOBufferedReadStream.Write(const Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  Result := Self.Write(Buffer[Offset], Count);
end;
{$ENDIF}

function TOBufferedReadStream.Write(const Buffer; Count: Longint): Longint;
begin
  {$IFDEF FPC}
  Result := 0;//JUST TO OMIT WARNING MESSAGES
  {$ENDIF}
  raise Exception.Create('TOBufferedReadStream.Write: You can''t write to TOBufferedReadStream!');
end;

end.
