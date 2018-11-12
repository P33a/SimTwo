unit OTextReadWrite;

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
  SysUtils, Classes, OBufferedStreams, OEncoding;

type

  {$IF DEFINED(O_UNICODE)}
  OTextWideString = String;//UNICODE for Delphi, UTF8 for FPC!!!
  OTextWideChar = Char;//UNICODE for Delphi, UTF8 for FPC!!!
  POTextWideChar = PChar;
  {$ELSE}
  OTextWideString = WideString;//UNICODE!!!
  OTextWideChar = WideChar;//UNICODE!!!
  POTextWideChar = PWideChar;
  {$IFEND}

  TOTextReader = class(TObject)
  private
    fTempString: OTextWideString;
    fTempStringPosition: Integer;
    fTempStringLength: Integer;
    fBlockFlushTempString: Integer;
    fBufferSize: Integer;

    fStream: TOBufferedReadStream;
    fStreamSize: Int64;
    fStreamPosition: Int64;
    fStreamStartPosition: Int64;

    fEncoding: TEncoding;
    fOwnsEncoding: Boolean;

    // undo support
    fPreviousChar: OTextWideChar;
    fReadFromUndo: Boolean;
    fCanUndo: Boolean;

    //custom buffer support
    fCustomBuffer: POTextWideChar;
    fCustomBufferPosition,
    fCustomBufferLength: Integer;

    procedure SetEncoding(const Value: TEncoding);

    function GetPreviousChar: OTextWideChar;
    function GetApproxPosition: Int64;
  public
    constructor Create(aStream: TStream; const aDefaultSingleByteEncoding: TEncoding = nil;
      const aBufferSize: Integer = 10*1024 {10 KB});
    destructor Destroy; override;
  public
    function ReadNextChar(var outChar: OTextWideChar): Boolean;
    procedure UndoRead;

    procedure WriteLastCharToBuffer;
    procedure WriteCharToBuffer(const aChar: OTextWideChar);
    function GetCustomBuffer: OTextWideString;
    function CustomBufferLength: Integer;
    procedure ClearCustomBuffer;
    procedure RemoveLastCharFromBuffer;

    procedure BlockFlushTempString;
    procedure UnblockFlushTempString;
  public
    property Encoding: TEncoding read fEncoding write SetEncoding;

    property ApproxPosition: Int64 read GetApproxPosition;//Approximate position in stream
    property StreamSize: Int64 read fStreamSize;
  end;

  TOTextWriter = class(TObject)
  private
    fTempString: OTextWideString;
    fTempStringPosition: Integer;
    fTempStringLength: Integer;
    fBufferSize: Integer;

    fStream: TStream;

    fEncoding: TEncoding;
    fOwnsEncoding: Boolean;

    fWriteBOM: Boolean;
    fBOMWritten: Boolean;

    procedure _WriteToStream(const aString: OTextWideString);
    procedure SetEncoding(const Value: TEncoding);
  public
    constructor Create(aStream: TStream; const aBufferSize: Integer = 10*1024 {10*1024 Chars = 20 KB});
    destructor Destroy; override;
  public
    procedure WriteString(const aString: OTextWideString);
    procedure EnsureEverythingWritten;
  public
    property Encoding: TEncoding read fEncoding write SetEncoding;
    property OwnsEncoding: Boolean read fOwnsEncoding write fOwnsEncoding;
    property WriteBOM: Boolean read fWriteBOM write fWriteBOM;
  end;


function GetEncodingFromStream(const aStream: TStream;
  var {%H-}aTempStringPosition: Int64;
  const aLastPosition: Int64;
  const {%H-}aDefaultSingleByteEncoding: TEncoding): TEncoding;

implementation

{$IFDEF FPC}
uses LazUTF8;
{$ENDIF}

function GetEncodingFromStream(const aStream: TStream;
  var aTempStringPosition: Int64;
  const aLastPosition: Int64;
  const aDefaultSingleByteEncoding: TEncoding): TEncoding;
var
  xSize: Integer;
  xBuffer: TEncodingBuffer;
  xEncoding: TEncoding;
begin
  //MULTI BYTE ENCODINGS MUST HAVE A BOM DEFINED!!!
  if Assigned(aDefaultSingleByteEncoding) and aDefaultSingleByteEncoding.IsSingleByte then
    Result := aDefaultSingleByteEncoding
  else
    Result := OEncoding_Ansi;

  xSize := aLastPosition - aStream.Position;
  if xSize <= 2 then
    Exit;//BOM may be 3 characters

  if xSize > 32 then
    xSize := 32;

  SetLength(xBuffer, xSize);
  aStream.Read(xBuffer[TEncodingBuffer_FirstElement], xSize);
  xEncoding := nil;
  aTempStringPosition := aTempStringPosition +
    TEncoding.GetBufferEncoding(xBuffer, xEncoding {$IFDEF O_DELPHI_XE}, Result{$ENDIF});

  if Assigned(xEncoding) then
    Result := xEncoding;
  if not Assigned(Result) then
    Result := TEncoding.{$IFDEF O_DELPHI_XE2}ANSI{$ELSE}ASCII{$ENDIF};

  aStream.Position := aTempStringPosition;
end;

function LoadString(const aReadStream: TStream; const aByteCount: Int64;
  var aTempString: OTextWideString;
  const aEncoding: TEncoding): Int64;
var
  xBuffer: TEncodingBuffer;
  xUTF8Inc: Integer;
const
  BS = TEncodingBuffer_FirstElement;
begin
  if aByteCount = 0 then begin
    Result := 0;
    Exit;
  end;
  SetLength(xBuffer, aByteCount);
  aReadStream.Read(xBuffer[BS], aByteCount);
  Result := aByteCount;
  if aEncoding is TUTF8Encoding then begin
    if
     ((Ord(xBuffer[BS+aByteCount-1]) and $80) = $00)
    then//last byte is 0xxxxxxx
      xUTF8Inc := 0
    else if
     ((aByteCount > 1) and ((Ord(xBuffer[BS+aByteCount-1]) and $E0) = $C0)) or//110xxxxx -> double char
     ((aByteCount > 2) and ((Ord(xBuffer[BS+aByteCount-2]) and $F0) = $E0)) or//1110xxxx -> triple char
     ((aByteCount > 3) and ((Ord(xBuffer[BS+aByteCount-3]) and $F8) = $F0)) or//11110xxx -> 4 char
     ((aByteCount > 4) and ((Ord(xBuffer[BS+aByteCount-4]) and $FC) = $F8)) or//111110xx -> 5 char
     ((aByteCount > 5) and ((Ord(xBuffer[BS+aByteCount-5]) and $FE) = $FC))   //1111110x -> 6 char
    then
      xUTF8Inc := 1
    else if
     ((aByteCount > 1) and ((Ord(xBuffer[BS+aByteCount-1]) and $F0) = $E0)) or//1110xxxx -> triple char
     ((aByteCount > 2) and ((Ord(xBuffer[BS+aByteCount-2]) and $F8) = $F0)) or//11110xxx -> 4 char
     ((aByteCount > 3) and ((Ord(xBuffer[BS+aByteCount-3]) and $FC) = $F8)) or//111110xx -> 5 char
     ((aByteCount > 4) and ((Ord(xBuffer[BS+aByteCount-4]) and $FE) = $FC))   //1111110x -> 6 char
    then
      xUTF8Inc := 2
    else if
     ((aByteCount > 1) and ((Ord(xBuffer[BS+aByteCount-1]) and $F8) = $F0)) or//11110xxx -> 4 char
     ((aByteCount > 2) and ((Ord(xBuffer[BS+aByteCount-2]) and $FC) = $F8)) or//111110xx -> 5 char
     ((aByteCount > 3) and ((Ord(xBuffer[BS+aByteCount-3]) and $FE) = $FC))   //1111110x -> 6 char
    then
      xUTF8Inc := 3
    else if
     ((aByteCount > 1) and ((Ord(xBuffer[BS+aByteCount-1]) and $FC) = $F8)) or//111110xx -> 5 char
     ((aByteCount > 2) and ((Ord(xBuffer[BS+aByteCount-2]) and $FE) = $FC))   //1111110x -> 6 char
    then
      xUTF8Inc := 4
    else if
     ((aByteCount > 1) and ((Ord(xBuffer[BS+aByteCount-1]) and $FE) = $FC))   //1111110x -> 6 char
    then
      xUTF8Inc := 5
    else
      xUTF8Inc := 0;//ERROR ?

    if xUTF8Inc > 0 then begin
      SetLength(xBuffer, aByteCount + xUTF8Inc);
      aReadStream.Read(xBuffer[BS+aByteCount], xUTF8Inc);
      Result := Result + xUTF8Inc;
    end;
  end;
  aTempString := aTempString + aEncoding.GetString(xBuffer);
end;

procedure ClearTempString(var aTempString: OTextWideString;
  var aTempStringPosition: Integer;
  const aTempStringLength: Integer;
  const aBufferSize: Integer);
begin
  if
    (aTempStringPosition > (aBufferSize div SizeOf(OTextWideChar)))
  then begin
    if aTempStringLength >= aTempStringPosition-1 then begin
      aTempString := ''
    end else begin
      Delete(aTempString, 1, aTempStringPosition-1);
    end;
    aTempStringPosition := 1;
  end;
end;

procedure CheckTempString(const aReadStream: TStream;
  var aCurrentPosition: Int64;
  const aLastPosition: Int64;
  var aTempString: OTextWideString; var aTempStringPosition, aTempStringLength: Integer;
  const aReadChars: Integer;
  const aEncoding: TEncoding;
  const aBufferSize: Integer;
  const aAllowedClearTempString: Boolean);
var
  xReadBytes: Int64;
  xInc: Int64;
begin
  if
    (aTempStringPosition+aReadChars-1 > aTempStringLength)
  then begin
    //LOAD NEXT BUFFER INTO TEMP STREAM, LEAVE UNREAD TAIL
    if aAllowedClearTempString then
      ClearTempString(aTempString, aTempStringPosition, aTempStringLength, aBufferSize);

    xReadBytes := aLastPosition-aCurrentPosition;
    if xReadBytes > aBufferSize then
      xReadBytes := aBufferSize;
    if xReadBytes > 0 then begin
      xInc := LoadString(aReadStream, xReadBytes, aTempString, aEncoding);
      aTempStringLength := Length(aTempString);
      aCurrentPosition := aCurrentPosition + xInc;
    end;
  end;
end;

{ TOTextReader }

procedure TOTextReader.BlockFlushTempString;
begin
  Inc(fBlockFlushTempString);
  fStream.BlockFlushTempStream;
end;

procedure TOTextReader.ClearCustomBuffer;
begin
  fCustomBufferPosition := 1;
end;

constructor TOTextReader.Create(aStream: TStream; const aDefaultSingleByteEncoding: TEncoding;
  const aBufferSize: Integer);
begin
  inherited Create;

  fStream := TOBufferedReadStream.Create(aStream, aBufferSize);//MUST BE HERE IF aStream DOES NOT...
  fStreamPosition := fStream.Position;
    //...SUPPORT SEEKING (BECAUSE OF GETTING THE ENCODING IN THE BEGINNING) !!!
    //E.G. A ZIP STREAM DOES NOT SUPPORT SEEKING!!!

  fStreamSize := fStream.Size;
  fBufferSize := aBufferSize;

  fEncoding := GetEncodingFromStream(fStream, fStreamPosition, fStreamSize, aDefaultSingleByteEncoding);
  fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);//False;

  fTempString := '';
  fTempStringPosition := 1;

  fCustomBufferLength := 256;
  fCustomBufferPosition := 1;
  GetMem(fCustomBuffer, fCustomBufferLength * SizeOf(OTextWideChar));
end;

function TOTextReader.CustomBufferLength: Integer;
begin
  Result := fCustomBufferPosition - 1;
end;

destructor TOTextReader.Destroy;
begin
  fStream.Free;

  if fOwnsEncoding then
    fEncoding.Free;

  FreeMem(fCustomBuffer);

  inherited;
end;

function TOTextReader.GetCustomBuffer: OTextWideString;
begin
  SetString(Result, fCustomBuffer, fCustomBufferPosition - 1);
  ClearCustomBuffer;
end;

function TOTextReader.GetApproxPosition: Int64;
begin
  //YOU CAN'T KNOW IT EXACTLY!!! (due to Lazarus Unicode->UTF8 or Delphi UTF8->Unicode conversion etc.)
  Result := fStreamPosition - fStreamStartPosition + fTempStringPosition;
end;

function TOTextReader.GetPreviousChar: OTextWideChar;
begin
  if FCanUndo then
    Result := FPreviousChar
  else
    raise Exception.Create('You can''t read previous char at this position.');

  FCanUndo := False;
end;

function TOTextReader.ReadNextChar(var outChar: OTextWideChar): Boolean;
begin
  if fReadFromUndo then begin
    outChar := GetPreviousChar;
    fReadFromUndo := False;
    Result := True;
    Exit;
  end;

  Result := (fStreamPosition < fStreamSize) or
    (fTempStringPosition <= fTempStringLength);
  fCanUndo := Result;

  if not Result then begin
    outChar := #0;
  end else begin
    CheckTempString(fStream, fStreamPosition, fStreamSize, fTempString,
      fTempStringPosition, fTempStringLength, 1, fEncoding, fBufferSize,
      (fBlockFlushTempString = 0));
    if fTempStringPosition <= fTempStringLength then begin
      outChar := fTempString[fTempStringPosition];
      fPreviousChar := outChar;
      Inc(fTempStringPosition);
    end else begin
      Result := False;
      outChar := #0;
    end;
  end;
end;

procedure TOTextReader.RemoveLastCharFromBuffer;
begin
  if fCustomBufferPosition > 1 then
    Dec(fCustomBufferPosition);
end;

procedure TOTextReader.SetEncoding(const Value: TEncoding);
begin
  if fEncoding <> Value then begin//the condition fEncoding <> Value must be here!!!
    fEncoding := Value;
    fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);

    //CLEAR ALREADY READ STRING AND GO BACK
    //fStreamPosition := fStreamPosition - fTempStringLength + fTempStringPosition-1;//ooo
    fStreamPosition := fStreamStartPosition;
    fStream.Position := fStreamPosition;

    fTempString := '';
    fTempStringLength := 0;
    fTempStringPosition := 1;
  end;
end;

procedure TOTextReader.UnblockFlushTempString;
begin
  if fBlockFlushTempString > 0 then begin
    Dec(fBlockFlushTempString);

    fStream.UnblockFlushTempStream;
  end;
end;

procedure TOTextReader.UndoRead;
begin
  fReadFromUndo := True;
end;

procedure TOTextReader.WriteCharToBuffer(const aChar: OTextWideChar);
begin
  if fCustomBufferPosition-1 = fCustomBufferLength then
  begin
    fCustomBufferLength := 2 * fCustomBufferLength;
    ReallocMem(fCustomBuffer, fCustomBufferLength * SizeOf(OTextWideChar));
  end;
  fCustomBuffer[fCustomBufferPosition-1] := aChar;
  Inc(fCustomBufferPosition);
end;

procedure TOTextReader.WriteLastCharToBuffer;
begin
  if FCanUndo then
    WriteCharToBuffer(FPreviousChar)
  else
    raise Exception.Create('WriteLastCharToBuffer: can''t read previous char.');
end;

{ TOTextWriter }

constructor TOTextWriter.Create(aStream: TStream; const aBufferSize: Integer);
begin
  inherited Create;

  fStream := aStream;

  fBufferSize := aBufferSize;

  fEncoding := TEncoding.Default;
  fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);

  fWriteBOM := False;

  fTempStringLength := fBufferSize;
  SetLength(fTempString, fTempStringLength);
  fTempStringPosition := 1;
end;

destructor TOTextWriter.Destroy;
begin
  EnsureEverythingWritten;

  if fOwnsEncoding then
    fEncoding.Free;

  inherited;
end;

procedure TOTextWriter.EnsureEverythingWritten;
begin
  if fTempStringPosition > 1 then begin
    if fTempStringLength = fTempStringPosition-1 then begin
      _WriteToStream(fTempString);
    end else begin
      _WriteToStream(Copy(fTempString, 1, fTempStringPosition-1));
    end;
    fTempStringPosition := 1;
  end;
end;

procedure TOTextWriter.SetEncoding(const Value: TEncoding);
begin
  if fEncoding <> Value then begin
    fEncoding := Value;
    fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);
  end;
end;

procedure TOTextWriter.WriteString(const aString: OTextWideString);
var
  xStringLength: Integer;
begin
  xStringLength := Length(aString);
  if xStringLength = 0 then
    Exit;

  if fTempStringPosition > fBufferSize then begin
    EnsureEverythingWritten;//WRITE TEMP BUFFER
  end;

  if xStringLength > fBufferSize then begin
    EnsureEverythingWritten;//MUST BE HERE -> WRITE TEMP BUFFER
    _WriteToStream(aString);
  end else begin
    if fTempStringPosition-1 + xStringLength > fTempStringLength then begin
      fTempStringLength := fTempStringPosition-1 + xStringLength;
      SetLength(fTempString, fTempStringLength);
    end;
    Move(aString[1], fTempString[fTempStringPosition], xStringLength*SizeOf(OTextWideChar));
    fTempStringPosition := fTempStringPosition + xStringLength;
  end;
end;

procedure TOTextWriter._WriteToStream(const aString: OTextWideString);
var
  xBytes: TEncodingBuffer;
  xBytesLength: Integer;
  xBOM: TEncodingBuffer;
begin
  if fWriteBOM and not fBOMWritten then begin
    //WRITE BOM
    xBOM := fEncoding.GetPreamble;
    if Length(xBOM) > 0 then
      fStream.WriteBuffer(xBOM[TEncodingBuffer_FirstElement], Length(xBOM));
  end;
  fBOMWritten := True;

  xBytes := fEncoding.GetBytes(aString);
  xBytesLength := Length(xBytes);
  if xBytesLength > 0 then
    fStream.WriteBuffer(xBytes[TEncodingBuffer_FirstElement], xBytesLength);
end;

end.
