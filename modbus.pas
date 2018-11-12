unit modbus;

interface


const
  ModBusBits = 2000;

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
    Header: TModbusHeader;
  public
    procedure ProcessModBusRequest(msg: string);

    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TModbus }

procedure TModbus.ProcessModBusRequest(msg: string);
var len, i: integer;
begin
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

//  if len < Header.LengthField

end;

constructor TModbus.Create;
begin
  Header.ProtocolIdentifier := -1; //Bad Header, for now
  Header.LengthField := 0;
end;

destructor TModbus.Destroy;
begin

  inherited;
end;

end.
