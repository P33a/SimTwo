unit modbusTCP;

{$MODE Delphi}

interface


const
  ModBusBits = $10000;

type

  TModbusHeader = record
    TransactionIdentifier: integer; //2 bytes - For synchronization between messages of server & client
    ProtocolIdentifier: integer; 	  //2 bytes - Zero for Modbus/TCP
    LengthField: integer; //2 bytes - Number of remaining bytes in this frame
    UnitIdentifier: integer; //1 byte - Slave Address (255 if not used)
    FunctionCode: integer; //1 byte - Function codes as in other variants
  end;

  TModbus = class
    Inputs: array[0..ModBusBits - 1] of byte;
    Coils: array[0..ModBusBits - 1] of byte;
    msg: string;
    Header, ResponseHeader: TModbusHeader;
    response: string;
  public
    procedure ProcessModBusMessage(mess: string);
    function BigEndianWord(bytes: word): string;
    function BuildHeader(Head: TModbusHeader): string;

    function getInputBit(bit_addr: integer): integer;

    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses SysUtils;

{ TModbus }

procedure TModbus.ProcessModBusMessage(mess: string);
var len, i: integer;
    refNum, bitCount, byteCount: integer;
    curBitCount: integer;
    tmpByte: byte;
begin
  if mess = '' then exit;

  msg := msg + mess;
  len := length(msg);

  if len < 8 then begin
    Header.ProtocolIdentifier := -1; //Bad Header, for now
    exit;
  end;

  with Header do begin
    if ProtocolIdentifier < 0 then begin
      TransactionIdentifier := $FF * ord(msg[1]) + ord(msg [2]);
      ProtocolIdentifier := $FF * ord(msg[3]) + ord(msg [4]);
      LengthField := $FF * ord(msg[5]) + ord(msg [6]);
      UnitIdentifier := ord(msg [7]);
      FunctionCode := ord(msg [8]);
    end;
  end;

  if len + 6 < Header.LengthField then exit;

  // Now we can process the message


  //Read Discrete Inputs
  if Header.FunctionCode = 2 then begin
    refNum := $FF * ord(msg[8 + 1]) + ord(msg [8 + 2]);
    bitCount := $FF * ord(msg[8 + 3]) + ord(msg [8 + 4]);
    byteCount := (bitCount + 7) div 8;

    ResponseHeader := Header;
    ResponseHeader.LengthField := 2 + byteCount + 1;
    response := BuildHeader(ResponseHeader);
    response := response + chr(byteCount);

    for i := 0 to BitCount - 1 do begin
      curBitCount  := i mod 8;
      // if it is the first bit then clear tmpByte
      if curBitCount = 0 then begin
        tmpByte := 0;
      end;

      if Inputs[(refNum + i) and (ModBusBits - 1)] <> 0 then begin
        tmpByte := tmpByte or (1 shl curBitCount);
      end;
      // if it is the last bit then add tmpByte to the output
      if (curBitCount = 7) or (i = BitCount - 1) then begin
        response := response + chr(tmpByte);
      end;
    end;
  //Write Multiple Coils
  end else if Header.FunctionCode = 15 then begin
    refNum := $FF * ord(msg[8 + 1]) + ord(msg [8 + 2]);
    bitCount := $FF * ord(msg[8 + 3]) + ord(msg [8 + 4]);
    byteCount := (bitCount + 7) div 8;

    ResponseHeader := Header;
    ResponseHeader.LengthField := 1 + byteCount + 1;
    response := BuildHeader(ResponseHeader) + BigEndianWord(refNum) + BigEndianWord(bitCount);

    for i := 0 to BitCount - 1 do begin
      curBitCount  := i mod 8;
      tmpByte := ord(msg[8 + 6 + (i div 8)]);
      if (tmpByte and (1 shl curBitCount)) <> 0 then begin
        Coils[(refNum + i) and (ModBusBits - 1)] := 1;
      end else begin
        Coils[(refNum + i) and (ModBusBits - 1)] := 0;
      end;
    end;

  end;

  delete(msg, 1, Header.LengthField + 6);
  Header.ProtocolIdentifier := -1; //Bad Header, for now

end;

constructor TModbus.Create;
begin
  msg := '';
  response := '';

  Header.ProtocolIdentifier := -1; //Bad Header, for now
  Header.LengthField := 0;
end;

destructor TModbus.Destroy;
begin

  inherited;
end;

function TModbus.BigEndianWord(bytes: word): string;
begin
  result := chr(bytes div 256) + chr(bytes mod 256);
end;

function TModbus.BuildHeader(Head: TModbusHeader): string;
begin
//      TransactionIdentifier := $FF * ord(msg[1]) + ord(msg [2]);
//      ProtocolIdentifier := $FF * ord(msg[3]) + ord(msg [4]);
//      LengthField := $FF * ord(msg[5]) + ord(msg [6]);
//      UnitIdentifier := ord(msg [7]);
//      FunctionCode := ord(msg [8]);
  with Head do
    result := BigEndianWord(TransactionIdentifier) +
              BigEndianWord(ProtocolIdentifier) +
              BigEndianWord(LengthField) +
              chr(UnitIdentifier) +
              chr(FunctionCode);
end;

function TModbus.getInputBit(bit_addr: integer): integer;
begin
  if (Inputs[bit_addr div 8] and (1 shl (bit_addr mod 8))) <> 0 then begin
    result := 1;
  end else begin
    result := 0;
  end;
end;

end.
