unit modbusTCP;

{$MODE Delphi}

interface

uses math;

const
  ModBusBits = $10000;
  ModBusBytes = $10000 div 8;
  inBufSize = 256;
  outBufSize = 256;

type
  TModbusState = (mbsIdle, mbsHeader, msbLen, msbUnitId, msbFunctionCode,
                  mbsHighAddress, mbsLowAddress, mbsHighNum, mbsLowNum,
                  msbBytesNum, msbBytes, msbError);

  TModbusEvent = procedure(command: byte) of object;

  TModbusFrame = record
    TransactionIdentifier: integer;  // 2 bytes - For synchronization between messages of server & client
    ProtocolIdentifier: integer;     // 2 bytes - Zero for Modbus/TCP
    LengthField: word;               // 2 bytes - Number of remaining bytes in this frame
    UnitIdentifier: byte;            // 1 byte - Slave Address (255 if not used)
    FunctionCode: byte;              // 1 byte - Function codes as in other variants
    Address: word;
    Num: word;
    BytesNum: byte;
  end;

  { TModbusData }

  TModbusData = class
    Inputs: array[0..ModBusBytes - 1] of byte;
    Coils: array[0..ModBusBytes - 1] of byte;

    function getCoil(coilAddr: integer): boolean;
    function getInput(Addr: integer): boolean;
    procedure setInput(Addr: integer; newValue: boolean);
    procedure setCoil(Addr: integer; newValue: boolean);
  end;

  { TModbusServer }

  TModbusServer = class
    Data: TModbusData;
    msg: RawByteString;
    Frame: TModbusFrame;
    InBuf: array[0..InBufSize - 1] of byte;
    OutBuf: array[0..OutBufSize - 1] of byte;
    InBufCount, OutBufCount: integer;

    State: TModbusState;
    byteCount: integer;

    curTransactionIdentifier: word;
    reqStartAddress, reqCount: word;

    ReceivedMessagesCount: integer;
    OnReceiveEvent: TModbusEvent;
  private
    procedure clearOutBuf();
    procedure clearInBuf();
    procedure addInBuf(data: byte);
    procedure addOutBuf(data: byte);
    procedure add16OutBuf(data: word);
    procedure ProcessInputMessage();

  public
    response: RawByteString;

    function BigEndianWord(bytes: word): RawByteString;

    procedure MessageStateMachine(mess: RawByteString);

    constructor Create(useData: TModbusData; newReceiveEvent: TModbusEvent = nil);
    destructor Destroy; override;

    function WriteMultipleCoilsAnswer(): RawByteString;
    function ReadMultipleCoils(UnitId: byte; StartAddress, Count: word): RawByteString;

    function ReadMultipleInputsAnswer(): RawByteString;

    function getByteWith8Coils(coilAddr: integer): byte;
    procedure write8Coils(coilAddr: integer; eightCoils: byte);
  end;


implementation

uses SysUtils;

{ TModbusNEW }

// Receive State Machine
//  State = (mbsIdle, mbsHeader, msbLen, msbUnitId, msbFunctionCode,
//           msbBytesNum, msbBytes, msbError);

procedure TModbusServer.MessageStateMachine(mess: RawByteString);
var b: integer;
    i, len, ibit, bitIdx, value: integer;
begin
  if mess = '' then exit;

  msg := msg + mess;
  len := length(msg);
  //while not (msg = '') do begin

  for i := 1 to len do begin
    b := ord(msg[i]);
    case State of
      mbsIdle: begin
        if b = $B1 then begin
          State := mbsHeader;
          byteCount := 1;
          Frame.TransactionIdentifier := $B1 shl 8;
        end;
      end;

      mbsHeader: begin     // B1 6E 00 00
        if (byteCount = 1) and (b = $6E) then begin
          Frame.TransactionIdentifier := Frame.TransactionIdentifier or $6E;
          inc(byteCount);
        end else if (byteCount = 2) and (b = 0) then begin
          Frame.ProtocolIdentifier := 0;
          inc(byteCount);
        end else if (byteCount = 3) and (b = 0) then begin
          Frame.ProtocolIdentifier := 0;
          State := msbLen;
          byteCount := 0;
        end else begin   // There was an error: resync
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;
        end;
      end;

      msbLen: begin
        if (byteCount = 0) then begin
          Frame.LengthField := b shl 8;
          inc(byteCount);
        end else if (byteCount = 1) then begin
          Frame.LengthField := Frame.LengthField or b;
          State := msbUnitId;
        end;
      end;

      msbUnitId: begin
        Frame.UnitIdentifier := b;
        State := msbFunctionCode;
      end;

      msbFunctionCode: begin
        Frame.FunctionCode := b;
        State := mbsHighAddress;
      end;

      mbsHighAddress: begin
        Frame.Address := b shl 8;
        State := mbsLowAddress;
      end;

      mbsLowAddress: begin
        Frame.Address := Frame.Address or b;
        State := mbsHighNum;
      end;

      mbsHighNum: begin
        Frame.Num := b shl 8;
        State := mbsLowNum;
      end;

      mbsLowNum: begin
        Frame.Num := Frame.Num or b;
        clearInBuf();

        if Frame.FunctionCode in [$01, $02, $03, $04, $05, $06] then begin // These commands have no extra bytes
          // The frame is complete and can be processed
          inc(ReceivedMessagesCount);
          if assigned(OnReceiveEvent) then OnReceiveEvent(Frame.FunctionCode);
          ProcessInputMessage();
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;

        end else if Frame.FunctionCode in [$10, $0F] then begin
          // Must read the number of extra bytes
          State := msbBytesNum;

        end else begin  // Unrecognized Function Code: Resync
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;
        end;
      end;

      msbBytesNum: begin
        Frame.BytesNum := b;
        State := msbBytes;
      end;

      msbBytes: begin
        addInBuf(b);

        if byteCount + 1 >= Frame.BytesNum  then begin  // All bytes read: Resync
          // The frame is complete
          inc(ReceivedMessagesCount);
          if assigned(OnReceiveEvent) then OnReceiveEvent(Frame.FunctionCode);
          ProcessInputMessage();
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;
        end;
        inc(byteCount);
      end;

    end;
  end;

  msg := '';
end;


constructor TModbusServer.Create(useData: TModbusData; newReceiveEvent: TModbusEvent);
begin
  msg := '';
  curTransactionIdentifier := $B16E;
  Data := useData;

  State := mbsIdle;
  OnReceiveEvent := newReceiveEvent;

  Frame.ProtocolIdentifier := -1; //Bad Frame, for now
  Frame.LengthField := 0;
end;

destructor TModbusServer.Destroy;
begin

  inherited;
end;

procedure TModbusServer.clearOutBuf();
var i: integer;
begin
  for i := 0 to outBufSize - 1 do OutBuf[i] := 0;
  OutBufCount := 0;
end;

procedure TModbusServer.clearInBuf();
var i: integer;
begin
  for  i := 0 to inBufSize - 1 do InBuf[i] := 0;
  InBufCount := 0;
end;

procedure TModbusServer.addInBuf(data: byte);
begin
  if (InBufCount >= InBufSize) then exit;
  InBuf[InBufCount] := data;
  inc(InBufCount);
end;


procedure TModbusServer.addOutBuf(data: byte);
begin
  if (outBufCount >= outBufSize) then exit;
  OutBuf[OutBufCount] := data;
  inc(OutBufCount);
end;

procedure TModbusServer.add16OutBuf(data: word);
begin
  if (OutBufCount >= outBufSize - 1) then exit;
  OutBuf[OutBufCount] := (data shr 8) and $FF;
  inc(OutBufCount);
  OutBuf[OutBufCount] := data and $FF;
  inc(OutBufCount);
end;

{
TransactionIdentifier: integer;  // 2 bytes - For synchronization between messages of server & client
ProtocolIdentifier: integer;     // 2 bytes - Zero for Modbus/TCP
LengthField: word;               // 2 bytes - Number of remaining bytes in this frame
UnitIdentifier: byte;            // 1 byte - Slave Address (255 if not used)
FunctionCode: byte;              // 1 byte - Function codes as in other variants
Address: word;
Num: word;
BytesNum: byte;
}

procedure TModbusServer.ProcessInputMessage();
var i: integer;
begin
  case Frame.FunctionCode of

    $02: begin
      response := ReadMultipleInputsAnswer();
    end;

    $0F: begin // write multiple coils
      //for i := 0 to Frame.Num div 8 - 1 do begin
      for i := 0 to InBufCount - 1 do begin
        Data.Coils[Frame.Address div 8 + i] := InBuf[i];
      end;
      response := WriteMultipleCoilsAnswer();
    end;
  end;
end;

function TModbusServer.BigEndianWord(bytes: word): RawByteString;
begin
  result := chr(bytes div 256) + chr(bytes mod 256);
end;


function TModbusServer.ReadMultipleInputsAnswer(): RawByteString;
var i, ByteCount, ByteAddress: integer;
    s: RawByteString;
begin
  ByteCount := ceil(Frame.Num / 8);
  ByteAddress := ceil(Frame.Address / 8);

  result := BigEndianWord(curTransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 1 + ByteCount) +
            chr(Frame.UnitIdentifier) +
            chr(02) +
            chr(ByteCount);
  s := '';
  for i := 0 to ByteCount - 1 do begin
    s := s + chr(Data.Inputs[ByteAddress + i]);
  end;
  result := result + s;
end;

function TModbusServer.WriteMultipleCoilsAnswer(): RawByteString;
begin
  result := BigEndianWord(curTransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(6) +
            chr(Frame.UnitIdentifier) +
            chr($0F) +
            BigEndianWord(Frame.Address) +
            BigEndianWord(Frame.num);
end;
{
function TModbusServer.WriteMultipleCoils(UnitId: byte; StartAddress, Count: word): RawByteString;
var payloadCount, i, ibyte, ibit: integer;
begin
  payloadCount := (1 + count div 8);

  result := BigEndianWord(curTransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 2 + 2 + 1 + payloadCount) +
            chr(UnitId) +
            chr(15) +
            BigEndianWord(StartAddress) +
            BigEndianWord(Count) +
            chr(payloadCount);

  SetLength(coilbits, payloadCount);
  for i := 0 to payloadCount - 1 do begin
    coilbits[i] := 0;
  end;

  for i := 0 to Count - 1 do begin
    ibyte := i div 8;
    ibit := i mod 8;
    if Data.Coils[StartAddress + i] then begin
      coilbits[ibyte] := coilbits[ibyte] or (1 shl ibit);
    end;
  end;

  for i := 0 to payloadCount - 1 do begin
    result := result + chr(coilbits[i]);
  end;

end;
}

function TModbusServer.ReadMultipleCoils(UnitId: byte; StartAddress, Count: word): RawByteString;
begin
  reqStartAddress := StartAddress;
  reqCount := count;
  result := BigEndianWord(curTransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 2 + 2) +
            chr(UnitId) +
            chr(01) +
            BigEndianWord(reqStartAddress) +
            BigEndianWord(reqCount);
end;


function TModbusData.getCoil(coilAddr: integer): boolean;
begin
  result := Coils[coilAddr div 8] and (1 shl (coilAddr mod 8)) <> 0;
end;

function TModbusData.getInput(Addr: integer): boolean;
begin
  result := Inputs[Addr div 8] and (1 shl (Addr mod 8)) <> 0;
end;

procedure TModbusData.setInput(Addr: integer; newValue: boolean);
var idx: integer;
begin
  idx := Addr div 8;
  if idx > high(Inputs) then exit;
  if newValue then begin
    Inputs[idx] := Inputs[idx] or (1 shl (Addr mod 8));
  end else begin
    Inputs[idx] := Inputs[idx] and not (1 shl (Addr mod 8));
  end;
end;

procedure TModbusData.setCoil(Addr: integer; newValue: boolean);
var idx: integer;
begin
  idx := Addr div 8;
  if idx > high(Inputs) then exit;
  if newValue then begin
    Coils[idx] := Coils[idx] or (1 shl (Addr mod 8));
  end else begin
    Coils[idx] := Coils[idx] and not (1 shl (Addr mod 8));
  end;
end;


function TModbusServer.getByteWith8Coils(coilAddr: integer): byte;
begin
  coilAddr := coilAddr div 8;
  result := Data.Coils[coilAddr];
end;


procedure TModbusServer.write8Coils(coilAddr: integer; eightCoils: byte);
begin
  coilAddr := coilAddr div 8;
  Data.Coils[coilAddr] := eightCoils;
end;


end.


