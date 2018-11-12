unit OEncoding;

interface

{$I OBufferedStreams.inc}

{

  LICENSE

  Author: Ondrej Pokorny, http://www.kluug.net

  License: MPL / GPL / LGPL

}

{$IFDEF O_DELPHI_XE4}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}


uses SysUtils, OmniXML_Types;

const
  //see http://msdn.microsoft.com/en-us/library/windows/desktop/dd317756%28v=vs.85%29.aspx
  CP_UNICODE =  1200; // Unicode pseudo-codepage,
  CP_UNICODEBE =  1201; // Big-Endian Unicode pseudo-codepage,
  WIN_1250 = 1250; // Central European Alphabet (Windows)
  WIN_1251 = 1251; // Cyrillic Alphabet (Windows)
  WIN_1252 = 1252; // Western Alphabet
  WIN_1253 = 1253; // Greek Alphabet (Windows)
  WIN_1254 = 1254; // Turkish Alphabet
  WIN_1255 = 1255; // Hebrew Alphabet (Windows)
  WIN_1256 = 1256; // Arabic Alphabet (Windows)
  WIN_1257 = 1257; // Baltic Alphabet (Windows)
  WIN_1258 = 1258; // Vietnamese Alphabet (Windows)
  ISO_8859_1 = 28591; // Western Alphabet (ISO)
  ISO_8859_2 = 28592; // Central European Alphabet (ISO)
  ISO_8859_3 = 28593; // Latin 3 Alphabet (ISO)
  ISO_8859_4 = 28594; // Baltic Alphabet (ISO)
  ISO_8859_5 = 28595; // Cyrillic Alphabet (ISO)
  ISO_8859_6 = 28596; // Arabic Alphabet (ISO)
  ISO_8859_7 = 28597; // Greek Alphabet (ISO)
  ISO_8859_8 = 28598; // Hebrew Alphabet (ISO)
  ISO_8859_9 = 28599; // Turkish Alphabet (ISO)
  KOI8_R = 20866; // Cyrillic Alphabet (Russian)
  KOI8_U = 21866; // Cyrillic Alphabet (Ukrainian)
  CP_UTF16 = CP_UNICODE;
  CP_UTF16BE = CP_UNICODEBE;
  CP_UTF7 = 65000;
  CP_UTF8 = 65001;// = 65001;  // UTF-8 translation

  HEADER_UTF16: WideChar = WideChar($FEFF);  // don't change!

type
  TCodePage = record
    CodePage: Word;
    Alias: XmlString;
  end;
  TCodePages = array[1..26] of TCodePage;

const
  CodePages: TCodePages = (
    (CodePage:   932; Alias: 'shift-jis'),  // Japanese (Shift-JIS)
    (CodePage: CP_UTF16; Alias: 'utf-16'),  //
    (CodePage: CP_UTF16BE; Alias: 'utf-16be'),  //
    (CodePage: WIN_1250; Alias: 'windows-1250'),  // Central European Alphabet (Windows)
    (CodePage: WIN_1251; Alias: 'windows-1251'),  // Cyrillic Alphabet (Windows)
    (CodePage: WIN_1252; Alias: 'windows-1252'),  // Western Alphabet
    (CodePage: WIN_1253; Alias: 'windows-1253'),  // Greek Alphabet (Windows)
    (CodePage: WIN_1254; Alias: 'windows-1254'),  // Turkish Alphabet
    (CodePage: WIN_1255; Alias: 'windows-1255'),  // Hebrew Alphabet (Windows)
    (CodePage: WIN_1256; Alias: 'windows-1256'),  // Arabic Alphabet (Windows)
    (CodePage: WIN_1257; Alias: 'windows-1257'),  // Baltic Alphabet (Windows)
    (CodePage: WIN_1258; Alias: 'windows-1258'),  // Vietnamese Alphabet (Windows)
    (CodePage: ISO_8859_1; Alias: 'iso-8859-1'),  // Western Alphabet (ISO)
    (CodePage: ISO_8859_2; Alias: 'iso-8859-2'),  // Central European Alphabet (ISO)
    (CodePage: ISO_8859_3; Alias: 'iso-8859-3'),  // Latin 3 Alphabet (ISO)
    (CodePage: ISO_8859_4; Alias: 'iso-8859-4'),  // Baltic Alphabet (ISO)
    (CodePage: ISO_8859_5; Alias: 'iso-8859-5'),  // Cyrillic Alphabet (ISO)
    (CodePage: ISO_8859_6; Alias: 'iso-8859-6'),  // Arabic Alphabet (ISO)
    (CodePage: ISO_8859_7; Alias: 'iso-8859-7'),  // Greek Alphabet (ISO)
    (CodePage: ISO_8859_8; Alias: 'iso-8859-8'),  // Hebrew Alphabet (ISO)
    (CodePage: ISO_8859_9; Alias: 'iso-8859-9'),  // Turkish Alphabet (ISO)
    (CodePage: KOI8_R; Alias: 'koi8-r'),  // Cyrillic Alphabet (Russian)
    (CodePage: KOI8_U; Alias: 'koi8-u'),  // Cyrillic Alphabet (Ukrainian)
    (CodePage: 50220; Alias: 'iso-2022-jp'),  // Japanese (JIS)
    (CodePage: 51932; Alias: 'euc-jp'),  // Japanese (EUC)
    (CodePage: CP_UTF8; Alias: 'utf-8')  // Universal Alphabet (UTF-8)
  );

{$IFDEF O_DELPHI_2009}
type
  TEncodingBuffer = TBytes;
const
  TEncodingBuffer_FirstElement = 0;
{$ELSE}
type
  TEncodingBuffer = AnsiString;
const
  TEncodingBuffer_FirstElement = 1;

type

  TEncoding = class(TObject)
  public
    class function GetBufferEncoding(const Buffer: TEncodingBuffer; var AEncoding: TEncoding): Integer; overload;
    class function GetBufferEncoding(const Buffer: TEncodingBuffer; var AEncoding: TEncoding;
      ADefaultEncoding: TEncoding): Integer; overload;
    function GetPreamble: TEncodingBuffer; virtual; abstract;

    function GetString(const Bytes: TEncodingBuffer): XmlString; virtual; abstract;
    function GetBytes(const S: XmlString): TEncodingBuffer; virtual; abstract;

    class function GetEncoding(CodePage: Integer): TEncoding;
  public
    class function IsSingleByte: Boolean; virtual; abstract;
    class function IsStandardEncoding(AEncoding: TEncoding): Boolean;

    function EncodingName: XmlString; virtual; abstract;
    function EncodingAlias: XmlString;
  public
    class function Default: TEncoding;
    class function Unicode: TEncoding;
    class function UTF8: TEncoding;
    class function ANSI: TEncoding;
    class function ASCII: TEncoding;
  public
    destructor Destroy; override;
  end;


  TUnicodeEncoding = class(TEncoding)
  public
    function GetPreamble: TEncodingBuffer; override;
    function GetString(const Bytes: TEncodingBuffer): XmlString; override;
    function GetBytes(const S: XmlString): TEncodingBuffer; override;
    class function IsSingleByte: Boolean; override;
    function EncodingName: XmlString; override;
  end;

  TUTF8Encoding = class(TEncoding)
  public
    function GetPreamble: TEncodingBuffer; override;
    function GetString(const Bytes: TEncodingBuffer): XmlString; override;
    function GetBytes(const S: XmlString): TEncodingBuffer; override;
    class function IsSingleByte: Boolean; override;
    function EncodingName: XmlString; override;
  end;

  TMBCSEncoding = class(TEncoding)
  private
    fCodePage: Cardinal;
  public
    constructor Create(aCodePage: Cardinal);
  public
    function GetPreamble: TEncodingBuffer; override;
    function GetString(const Bytes: TEncodingBuffer): XmlString; override;
    function GetBytes(const S: XmlString): TEncodingBuffer; override;
    class function IsSingleByte: Boolean; override;
    function EncodingName: XmlString; override;
  public
    property CodePage: Cardinal read fCodePage;
  end;
{$ENDIF O_DELPHI_2009}

{$IF NOT DEFINED(FPC) AND (DEFINED(O_DELPHI_2009))}
//Delphi 2009 to 2010
type
  TEncodingHelper = class helper for TEncoding
  public
    {$IF NOT DEFINED(O_DELPHI_XE)}
    function EncodingName: String;
    {$IFEND}
    function EncodingAlias: String;
  end;
  TMBCSEncodingHelper = class helper for TMBCSEncoding
  public
    function GetCodePage: Integer;
  end;
{$IFEND}


function GetCreateCodePage(const aCodePage: Word): TEncoding; overload;
function GetCreateCodePage(const Alias: string; var aEncoding: TEncoding): Boolean; overload;

function OEncoding_Unicode: TEncoding;
function OEncoding_UTF8: TEncoding;
function OEncoding_Ansi: TEncoding;

implementation

{$IFNDEF O_DELPHI_2009}

{$IF DEFINED(MSWINDOWS)}
uses Windows;
{$ELSEIF DEFINED(FPC)}
uses LConvEncoding;
{$IFEND}

var
  fxANSIEncoding: TEncoding = nil;
  fxUTF8Encoding: TEncoding = nil;
  fxUnicodeEncoding: TEncoding = nil;
  fxASCIIEncoding: TEncoding = nil;

{$IF DEFINED(MSWINDOWS) AND NOT DEFINED(O_DELPHI_2009)}
type
  _cpinfoExW = record
    MaxCharSize: UINT;                       { max length (bytes) of a char }
    DefaultChar: array[0..MAX_DEFAULTCHAR - 1] of Byte; { default character }
    LeadByte: array[0..MAX_LEADBYTES - 1] of Byte;      { lead byte ranges }
    UnicodeDefaultChar: WideChar;
    Codepage: UINT;
    CodePageName: array[0..MAX_PATH -1] of WideChar;
  end;
  TCPInfoExW = _cpinfoExW;

  function GetCPInfoExW(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: TCPInfoExW): BOOL; stdcall; external kernel32 name 'GetCPInfoExW';
{$IFEND}

{$IF DEFINED(FPC) AND NOT DEFINED(MSWINDOWS)}
function UTF8ToCodePage(const S: XmlString; aCodePage: Cardinal): AnsiString;
begin
  case aCodePage of
    932: Result := UTF8ToCP932(S);
    1250: Result := UTF8ToCP1250(S);
    1251: Result := UTF8ToCP1251(S);
    1252: Result := UTF8ToCP1252(S);
    1253: Result := UTF8ToCP1253(S);
    1254: Result := UTF8ToCP1254(S);
    1255: Result := UTF8ToCP1255(S);
    1256: Result := UTF8ToCP1256(S);
    1257: Result := UTF8ToCP1257(S);
    1258: Result := UTF8ToCP1258(S);
    ISO_8859_1: Result := UTF8ToISO_8859_1(S);
    ISO_8859_2: Result := UTF8ToISO_8859_2(S);
    KOI8_R, KOI8_U: Result := UTF8ToKOI8(S);
  else
    Result := S;//Encoding not supported by lazarus
  end;
end;
function CodePageToUTF8(const S: AnsiString; aCodePage: Cardinal): XmlString;
begin
  case aCodePage of
    932: Result := CP932ToUTF8(S);
    1250: Result := CP1250ToUTF8(S);
    1251: Result := CP1251ToUTF8(S);
    1252: Result := CP1252ToUTF8(S);
    1253: Result := CP1253ToUTF8(S);
    1254: Result := CP1254ToUTF8(S);
    1255: Result := CP1255ToUTF8(S);
    1256: Result := CP1256ToUTF8(S);
    1257: Result := CP1257ToUTF8(S);
    1258: Result := CP1258ToUTF8(S);
    ISO_8859_1: Result := ISO_8859_1ToUTF8(S);
    ISO_8859_2: Result := ISO_8859_2ToUTF8(S);
    KOI8_R, KOI8_U: Result := KOI8ToUTF8(S);
  else
    Result := S;//Encoding not supported by lazarus
  end;
end;
{$IFEND}

{ TEncoding }

class function TEncoding.ANSI: TEncoding;
begin
  if not Assigned(fxANSIEncoding) then begin
    {$IFDEF MSWINDOWS}
    fxANSIEncoding := TMBCSEncoding.Create(GetACP);
    {$ELSE}
    fxANSIEncoding := TMBCSEncoding.Create(ISO_8859_1);
    {$ENDIF}
  end;
  Result := fxANSIEncoding;
end;

class function TEncoding.ASCII: TEncoding;
begin
  if not Assigned(fxASCIIEncoding) then
    fxASCIIEncoding := TMBCSEncoding.Create(1252);
  Result := fxASCIIEncoding;
end;

class function TEncoding.Default: TEncoding;
begin
  {$IFDEF MSWINDOWS}
  Result := ANSI;
  {$ELSE}
  Result := UTF8;
  {$ENDIF}
end;

class function TEncoding.GetBufferEncoding(const Buffer: TEncodingBuffer;
  var AEncoding: TEncoding): Integer;
begin
  Result := GetBufferEncoding(Buffer, AEncoding, Default);
end;

class function TEncoding.GetBufferEncoding(const Buffer: TEncodingBuffer;
  var AEncoding: TEncoding; ADefaultEncoding: TEncoding): Integer;
begin
  if (Length(Buffer) >= 3) and (Buffer[1] = #$EF) and (Buffer[2] = #$BB) and (Buffer[3] = #$BF) then begin
    AEncoding := UTF8;
    Result := 3;
  end else if (Length(Buffer) >= 2) and (Buffer[1] = #$FF) and (Buffer[2] = #$FE) then begin
    AEncoding := Unicode;
    Result := 2;
  end else begin
    AEncoding := ADefaultEncoding;
    Result := 0;
  end;
end;

class function TEncoding.Unicode: TEncoding;
begin
  if not Assigned(fxUnicodeEncoding) then
    fxUnicodeEncoding := TUnicodeEncoding.Create;
  Result := fxUnicodeEncoding;
end;

class function TEncoding.UTF8: TEncoding;
begin
  if not Assigned(fxUTF8Encoding) then
    fxUTF8Encoding := TUTF8Encoding.Create;
  Result := fxUTF8Encoding;
end;

class function TEncoding.IsStandardEncoding(AEncoding: TEncoding): Boolean;
begin
  Result :=
    (AEncoding <> nil) and
    ((AEncoding = fxANSIEncoding) or
    (AEncoding = fxUTF8Encoding) or
    (AEncoding = fxUnicodeEncoding) or
    (AEncoding = fxASCIIEncoding));
end;

function TEncoding.EncodingAlias: XmlString;
var
  xCodePage, I: Integer;
begin
  if Self is TMBCSEncoding then
    xCodePage := TMBCSEncoding(Self).CodePage
  else if Self is TUnicodeEncoding then
    xCodePage := CP_UNICODE
  else if Self is TUTF8Encoding then
    xCodePage := CP_UTF8
  else
    xCodePage := 0;

  for I := Low(CodePages) to High(CodePages) do
  if CodePages[I].CodePage = xCodePage then begin
    Result := CodePages[I].Alias;
    Exit;
  end;

  Result := IntToStr(xCodePage);
end;

class function TEncoding.GetEncoding(CodePage: Integer): TEncoding;
begin
  case CodePage of
    CP_UNICODE: Result := TUnicodeEncoding.Create;
    //CP_UNICODEBE: Result := TBigEndianUnicodeEncoding.Create;
    //CP_UTF7: Result := TUTF7Encoding.Create;
    CP_UTF8: Result := TUTF8Encoding.Create;
  else
    Result := TMBCSEncoding.Create(CodePage);
  end;
end;

destructor TEncoding.Destroy;
begin
  if (Self = fxANSIEncoding) then
    fxANSIEncoding := nil
  else if (Self = fxUTF8Encoding) then
    fxUTF8Encoding := nil
  else if (Self = fxUnicodeEncoding) then
    fxUnicodeEncoding := nil
  else if (Self = fxASCIIEncoding) then
    fxASCIIEncoding := nil;

  inherited;
end;

{ TMBCSEncoding }

constructor TMBCSEncoding.Create(aCodePage: Cardinal);
begin
  inherited Create;

  fCodePage := aCodePage;
end;

function TMBCSEncoding.EncodingName: XmlString;
{$IFDEF MSWINDOWS}
var
  LCPInfo: TCPInfoExW;
begin
  if GetCPInfoExW(FCodePage, 0, LCPInfo{%H-}) then
    Result := LCPInfo.CodePageName
  else
    Result := IntToStr(fCodePage);
end;
{$ELSE}
begin
  Result := IntToStr(fCodePage);
end;
{$ENDIF}

function TMBCSEncoding.GetBytes(const S: XmlString): TEncodingBuffer;
{$IFDEF MSWINDOWS}
var
  xLength: integer;
  {$IFDEF FPC}
  xUS: UnicodeString;
  {$ENDIF}
{$ENDIF}
begin
  if S = '' then begin
    Result := '';
    Exit;
  end;

  {$IFDEF MSWINDOWS}
    {$IFDEF FPC}
    xUS := UTF8Decode(S);
    xLength := WideCharToMultiByte(fCodePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      PWideChar(@xUS[1]), -1, nil, 0, nil, nil);

    SetLength(Result, xLength-1);
    if xLength > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        PWideChar(@xUS[1]), -1, @Result[1], xLength-1, nil, nil);
    {$ELSE}
    xLength := WideCharToMultiByte(fCodePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      PWideChar(@S[1]), -1, nil, 0, nil, nil);

    SetLength(Result, xLength-1);
    if xLength > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        PWideChar(@S[1]), -1, @Result[1], xLength-1, nil, nil);
    {$ENDIF}
  {$ELSE}
  Result := UTF8ToCodePage(S, fCodePage);
  {$ENDIF}
end;

function TMBCSEncoding.GetPreamble: TEncodingBuffer;
begin
  Result := '';
end;

function TMBCSEncoding.GetString(const Bytes: TEncodingBuffer): XmlString;
{$IFDEF MSWINDOWS}
var
  xLength: integer;
  {$IFDEF FPC}
  xUS: UnicodeString;
  {$ENDIF}
{$ENDIF}
begin
  if Bytes = '' then begin
    Result := '';
    Exit;
  end;

  {$IFDEF MSWINDOWS}
    {$IFDEF FPC}
    xLength := MultiByteToWideChar(fCodePage, MB_PRECOMPOSED, PAnsiChar(@Bytes[1]), -1, nil, 0);
    SetLength(xUS, xLength-1);
    if xLength > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(@Bytes[1]), -1, PWideChar(@xUS[1]), xLength-1);
    Result := UTF8Encode(xUS);
    {$ELSE}
    xLength := MultiByteToWideChar(fCodePage, MB_PRECOMPOSED, PAnsiChar(@Bytes[1]), -1, nil, 0);
    SetLength(Result, xLength-1);
    if xLength > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(@Bytes[1]), -1, PWideChar(@Result[1]), xLength-1);
    {$ENDIF}
  {$ELSE}
  Result := CodePageToUTF8(Bytes, fCodePage);
  {$ENDIF}
end;

class function TMBCSEncoding.IsSingleByte: Boolean;
begin
  Result := True;
end;

{ TUnicodeEncoding }

function TUnicodeEncoding.EncodingName: XmlString;
begin
{$IFDEF MSWINDOWS}
  Result := '1200  (Unicode)';
{$ELSE}
  Result := 'Unicode (UTF-16LE)';
{$ENDIF}
end;

function TUnicodeEncoding.GetBytes(const S: XmlString): TEncodingBuffer;
var xCharCount: Integer;
  {$IFDEF FPC}
  xUS: UnicodeString;
  {$ENDIF}
begin
  {$IFDEF FPC}
  //FPC
  xUS := UTF8Decode(S);
  xCharCount := Length(xUS);
  SetLength(Result, xCharCount*2);
  if xCharCount > 0 then begin
    Move(xUS[1], Result[1], xCharCount*2);
  end;
  {$ELSE}
  //DELPHI
  xCharCount := Length(S);
  SetLength(Result, xCharCount*2);
  if xCharCount > 0 then begin
    Move(S[1], Result[1], xCharCount*2);
  end;
  {$ENDIF}
end;

function TUnicodeEncoding.GetPreamble: TEncodingBuffer;
begin
  SetLength(Result, 2);
  Result[1] := #$FF;
  Result[2] := #$FE;
end;

function TUnicodeEncoding.GetString(const Bytes: TEncodingBuffer): XmlString;
var
  xByteCount: Integer;
  {$IFDEF FPC}
  xUS: UnicodeString;
  {$ENDIF}
begin
  xByteCount := Length(Bytes);
  if xByteCount = 0 then begin
    Result := '';
    Exit;
  end;
  {$IFDEF FPC}
  //FPC
  SetLength(xUS, xByteCount div 2);
  Move(Bytes[1], xUS[1], xByteCount);
  Result := UTF8Encode(xUS);
  {$ELSE}
  //DELPHI
  SetLength(Result, xByteCount div 2);
  Move(Bytes[1], Result[1], xByteCount);
  {$ENDIF}
end;

class function TUnicodeEncoding.IsSingleByte: Boolean;
begin
  Result := False;
end;

{ TUTF8Encoding }

function TUTF8Encoding.EncodingName: XmlString;
{$IFDEF MSWINDOWS}
var
  LCPInfo: TCPInfoExW;
begin
  if GetCPInfoExW(CP_UTF8, 0, LCPInfo{%H-}) then
    Result := LCPInfo.CodePageName
  else
    Result := '65001  (UTF-8)';
end;
{$ELSE}
begin
  Result := 'UTF-8';
end;
{$ENDIF}

function TUTF8Encoding.GetBytes(const S: XmlString): TEncodingBuffer;
{$IFNDEF FPC}
var
  xCharCount: Integer;
  xBuffer: AnsiString;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := S;
  {$ELSE}
  //DELPHI
  xBuffer := UTF8Encode(S);
  xCharCount := Length(xBuffer);
  SetLength(Result, xCharCount);
  if xCharCount > 0 then begin
    Move(xBuffer[1], Result[TEncodingBuffer_FirstElement], xCharCount);
  end;
  {$ENDIF}
end;

function TUTF8Encoding.GetPreamble: TEncodingBuffer;
begin
  SetLength(Result, 3);
  Result[1] := #$EF;
  Result[2] := #$BB;
  Result[3] := #$BF;
end;

function TUTF8Encoding.GetString(const Bytes: TEncodingBuffer): XmlString;
{$IFNDEF FPC}
var
  xBuffer: AnsiString;
  xByteCount: Integer;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := Bytes;
  {$ELSE}
  //DELPHI
  xByteCount := Length(Bytes);
  if xByteCount > 0 then begin
    SetLength(xBuffer, xByteCount);
    Move(Bytes[1], xBuffer[1], xByteCount);
    Result := UTF8Decode(xBuffer);
  end else begin
    Result := '';
  end;
  {$ENDIF}
end;

class function TUTF8Encoding.IsSingleByte: Boolean;
begin
  Result := False;
end;
{$ENDIF O_DELPHI_2009}

{$IF NOT DEFINED(FPC) AND (DEFINED(O_DELPHI_2009))}
{ TEncodingHelper }

{$IF NOT DEFINED(O_DELPHI_XE)}
function TEncodingHelper.EncodingName: String;
begin
  if Self is TMBCSEncoding then
    Result := IntToStr(TMBCSEncoding(Self).GetCodePage)
  else if Self is TUnicodeEncoding then
    Result := IntToStr(CP_UNICODE)
  else if Self is TBigEndianUnicodeEncoding then
    Result := IntToStr(CP_UNICODEBE)
  else
    Result := '';
end;
{$IFEND}

function TEncodingHelper.EncodingAlias: String;
var
  xCodePage, I: Integer;
begin
  if Self is TMBCSEncoding then
    xCodePage := TMBCSEncoding(Self).GetCodePage
  else if Self is TUnicodeEncoding then
    xCodePage := CP_UNICODE
  else if Self is TBigEndianUnicodeEncoding then
    xCodePage := CP_UNICODEBE
  else
    xCodePage := 0;

  for I := Low(CodePages) to High(CodePages) do
  if CodePages[I].CodePage = xCodePage then begin
    Result := CodePages[I].Alias;
    Exit;
  end;

  Result := IntToStr(xCodePage);
end;

{ TMBCSEncodingHelper }

function TMBCSEncodingHelper.GetCodePage: Integer;
begin
  Result := Self.FCodePage;
end;
{$IFEND}

function OEncoding_Unicode: TEncoding;
begin
  Result := TEncoding.Unicode;
end;

function OEncoding_UTF8: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

function OEncoding_Ansi: TEncoding;
begin
  Result := TEncoding.{$IF DEFINED(O_DELPHI_XE2) OR DEFINED(FPC)}ANSI{$ELSE}ASCII{$IFEND};
end;

function GetCreateCodePage(const aCodePage: Word): TEncoding;
begin
  case aCodePage of
    CP_UTF8: Result := OEncoding_UTF8;
    CP_UTF16: Result := OEncoding_Unicode;
  else
    Result := TEncoding.GetEncoding(aCodePage);//TMBCSEncoding.Create(aCodePage);
  end;
end;

function GetCreateCodePage(const Alias: string; var aEncoding: TEncoding): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := Low(TCodePages);
  while (not Result) and (i <= High(TCodePages)) do
  begin
    Result := CompareText(Alias, CodePages[i].Alias) = 0;
    if Result then
      aEncoding := GetCreateCodePage(CodePages[i].CodePage)
    else
      Inc(i);
  end;
end;

{$IFNDEF O_DELPHI_2009}
initialization

finalization
  fxANSIEncoding.Free;
  fxUTF8Encoding.Free;
  fxUnicodeEncoding.Free;
  fxASCIIEncoding.Free;


{$ENDIF O_DELPHI_2009}

end.