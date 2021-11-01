{ SdpoZMQ v1

  CopyRight (C) 2017 Paulo Costa

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at paco@fe.up.pt
}

unit sdpoZMQ;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF LINUX}
  Classes,
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ELSE}
  Windows, Classes,
{$ENDIF}
  zmq, sysutils, LResources, Forms, Controls, Graphics, Dialogs, math, fgl;


type
  TZMQSocketType = (
  zmqPair := ZMQ_PAIR,
  zmqPub := ZMQ_PUB,
  zmqSub := ZMQ_SUB,
  zmqReq := ZMQ_REQ,
  zmqRep := ZMQ_REP,
  zmqDealer := ZMQ_DEALER,
  zmqRouter := ZMQ_ROUTER,
  zmqPull := ZMQ_PULL,
  zmqPush := ZMQ_PUSH,
  zmqXpub := ZMQ_XPUB,
  zmqXsub := ZMQ_XSUB,
  zmqStream := ZMQ_STREAM);

  { TMsgPart }

  TMsgPart = record
    disp, size: integer;

    class operator = (const A, B: TMsgPart): boolean;
    //class operator < (const A, B: TByteA): Boolean;
    //class operator > (const A, B: TByteA): Boolean;
  end;

  TMsgParts = specialize TFPGList<TMsgPart>;

type
  TSdpoZMQ = class;

  TZMQReadThread = class(TThread)
  public
    MustDie: boolean;
    Owner: TSdpoZMQ;
  protected
    procedure CallEvent;
    procedure Execute; override;
  published
    property Terminated;
  end;

  { TSdpoZMQ }

  TSdpoZMQ = class(TComponent)
  private
    FActive: boolean;

    Fcontext: pointer;
    Fsocket: pointer;
    FSocketType: TZMQSocketType;
    FAddress: string;

    FOnReceiveData: TNotifyEvent;
    ReadThread: TZMQReadThread;
    //message: zmq_msg_t;
    data_size: integer;
    data: TMemoryStream;
    MsgParts: TMsgParts;

    procedure StartThread;
    procedure SocketClose;

    procedure SocketException(str: string);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Bind(SocketType: TZMQSocketType; address: string = 'tcp://*:5556');
    procedure Connect(SocketType: TZMQSocketType; address: string = 'tcp://localhost:5556');
    procedure Subscribe(topic: string);
    procedure Close;

    // read data from socket
    function DataSize: integer;
    function ReadData: string;
    function ReadBuffer(var buf; size: integer): integer;
    procedure ReadStream(MemoryStream: TMemoryStream);

    function PartCount: integer;
    function ReadPartData(idx: integer): string;
    function ReadPartBuffer(idx: integer; var buf; size: integer): integer;
    procedure ReadPartStream(idx: integer; MemoryStream: TMemoryStream);

    // write data
    function WriteData(s: string; multiPart: boolean = false): integer;
    function WriteBuffer(var buf; size: integer; multiPart: boolean = false): integer;
    function WriteStream(MemoryStream: TMemoryStream; multiPart: boolean = false): integer;

  published
    property Active: boolean read FActive;

    property SocketType: TZMQSocketType read FSocketType;
    property Address: string read FAddress;

    property OnReceiveData: TNotifyEvent read FOnReceiveData write FOnReceiveData;
  end;

//procedure Register;

implementation

{ TMsgPart }

class operator TMsgPart. = (const A, B: TMsgPart): boolean;
begin
   Result := (A.disp = B.disp) and (A.size = B.size);
end;

{ TSdpoZMQ }

procedure TSdpoZMQ.Close;
begin
  SocketClose;
end;

procedure TSdpoZMQ.SocketClose;
begin
  // Capture thread stop request
  if assigned(ReadThread) then begin
    ReadThread.FreeOnTerminate := false;
    ReadThread.MustDie := true;
  end;

  FActive := false;

  // Stop capture thread
  if assigned(ReadThread) then begin
    while not ReadThread.Terminated do begin
      Application.ProcessMessages;
    end;
    ReadThread.Free;
    ReadThread := nil;
  end;

  // Close Socket
  if assigned(Fsocket) then begin
    zmq_close(Fsocket);
    Fsocket := nil;
    zmq_ctx_destroy(Fcontext);
    Fcontext := nil;
  end;

end;

constructor TSdpoZMQ.Create(AOwner: TComponent);
begin
  inherited;
  Fsocket := nil;
  ReadThread := nil;
  data := TMemoryStream.Create;
  MsgParts := TMsgParts.Create;
end;

function TSdpoZMQ.DataSize: integer;
begin
  result := data_size;
end;


destructor TSdpoZMQ.Destroy;
begin
  Close;
  MsgParts.Free;
  data.Free;
  inherited;
end;


procedure TSdpoZMQ.StartThread;
begin
  // Launch Thread
  ReadThread := TZMQReadThread.Create(true);
  ReadThread.Owner := Self;
  ReadThread.MustDie := false;
  ReadThread.start;
end;

procedure TSdpoZMQ.Bind(SocketType: TZMQSocketType; address: string);
var rc: integer;
begin
  //  Prepare our context and publisher
  Fcontext := zmq_ctx_new();
  Fsocket := zmq_socket(Fcontext, ord(SocketType));
  rc := zmq_bind(Fsocket, pchar(Address)); //'tcp://*:5556'
  if rc <> 0 then
    raise Exception.Create('Could not Bind Socket '+ FAddress);

  FActive := true;
  FAddress := Address;
  FSocketType := SocketType;

  // Launch listening Thread
  if SocketType in [ zmqPair, zmqSub, zmqReq, zmqRep, zmqDealer, zmqRouter, zmqPull, zmqXpub, zmqXsub, zmqStream] then
    StartThread;
end;
// zmqPair, zmqPub, zmqSub, zmqReq, zmqRep, zmqDealer, zmqRouter, zmqPull, zmqPush, zmqXpub, zmqXsub, zmqStream


procedure TSdpoZMQ.Connect(SocketType: TZMQSocketType; address: string);
var rc: integer;
begin
  //  Prepare our context and publisher
  Fcontext := zmq_ctx_new();
  Fsocket := zmq_socket(Fcontext, ord(SocketType));
  rc := zmq_connect(Fsocket, pchar(Address)); //'tcp://localhost:5556'
  if rc <> 0 then
    raise Exception.Create('Could not Connect Socket '+ FAddress);

  FActive := true;
  FAddress := Address;
  FSocketType := SocketType;

  // Launch Thread
  StartThread;
end;

procedure TSdpoZMQ.Subscribe(topic: string);
var rc: integer;
begin
  rc := zmq_setsockopt(Fsocket, ZMQ_SUBSCRIBE, pchar(topic), Length(topic));
  if rc <> 0 then
    raise Exception.Create('Could not Subscrive Socket '+ FAddress);
end;


function TSdpoZMQ.ReadData: string;
begin
  result := '';
  if not Active then exit;

  result := StringOfChar(#0, data_size);
  data.Position := 0;
  data.Read(result[1], data_size);
  //Move(zmq_msg_data(@message)^, result[1], data_size);
end;

procedure TSdpoZMQ.ReadStream(MemoryStream: TMemoryStream);
begin
  if not Active then exit;

  //MemoryStream.LoadFromStream(data);
  MemoryStream.Write(data.Memory^, data_size);
  //MemoryStream.Write(zmq_msg_data(@message)^, data_size);
end;

function TSdpoZMQ.ReadPartData(idx: integer): string;
begin
  result := '';
  if not Active then exit;
  if (idx < 0) or (idx >= MsgParts.Count) then exit;

  result := StringOfChar(#0, MsgParts.Items[idx].size);

  data.Position := MsgParts.Items[idx].disp;
  data.Read(result[1], MsgParts.Items[idx].size);
end;

function TSdpoZMQ.ReadPartBuffer(idx: integer; var buf; size: integer): integer;
var sz: integer;
begin
  result := 0;
  if not Active then exit;
  if (idx < 0) or (idx >= MsgParts.Count) then exit;

  sz := min(size, MsgParts.Items[idx].size);
  data.Position := MsgParts.Items[idx].disp;
  data.Read(buf, MsgParts.Items[idx].size);
  result := sz;
end;

procedure TSdpoZMQ.ReadPartStream(idx: integer; MemoryStream: TMemoryStream);
begin
  if not Active then exit;
  if (idx < 0) or (idx >= MsgParts.Count) then exit;

  //MemoryStream.LoadFromStream(data);
  MemoryStream.Write(pbyte(data.Memory)[MsgParts.Items[idx].disp], MsgParts.Items[idx].size);
end;

function TSdpoZMQ.PartCount: integer;
begin
  result := MsgParts.Count;
end;

function TSdpoZMQ.ReadBuffer(var buf; size: integer): integer;
var sz: integer;
begin
  result := 0;
  if not Active then exit;

  sz := min(size, data_size);
  data.Position := 0;
  data.Read(buf, data_size);
  //move(zmq_msg_data(@message)^, buf, sz);
  result := sz;
end;


function TSdpoZMQ.WriteBuffer(var buf; size: integer; multiPart: boolean): integer;
var rc: integer;
    msg: zmq_msg_t;
    flags: integer;
begin
  if not Active then
    SocketException('can not write to a closed Socket.');

  flags := ifthen(multiPart, ZMQ_SNDMORE, 0);
  zmq_msg_init_size(@msg, size);
  move(buf, zmq_msg_data(@msg)^, size);
  rc := zmq_msg_send(@msg, Fsocket, flags or ZMQ_DONTWAIT);
  zmq_msg_close(@msg);
  result := rc;
end;

function TSdpoZMQ.WriteStream(MemoryStream: TMemoryStream; multiPart: boolean): integer;
var rc, size: integer;
    msg: zmq_msg_t;
    flags: integer;
begin
  if not Active then
    SocketException('can not write to a closed Socket.');

  flags := ifthen(multiPart, ZMQ_SNDMORE, 0);
  size := MemoryStream.Size;
  zmq_msg_init_size(@msg, size);
  move((MemoryStream.Memory)^, zmq_msg_data(@msg)^, size);
  rc := zmq_msg_send(@msg, Fsocket, flags or ZMQ_DONTWAIT);
  zmq_msg_close(@msg);
  result := rc;
end;


function TSdpoZMQ.WriteData(s: string; multiPart: boolean): integer;
var rc: integer;
    msg: zmq_msg_t;
    flags: integer;
begin
  if not Active then
    SocketException('can not write to a closed Socket.');

  flags := ifthen(multiPart, ZMQ_SNDMORE, 0);
  zmq_msg_init_size(@msg, length(s));
  move(pchar(s)^, zmq_msg_data(@msg)^, length(s));
  rc := zmq_msg_send(@msg, Fsocket, flags or ZMQ_DONTWAIT);
  zmq_msg_close(@msg);
  result := rc;
end;


procedure TSdpoZMQ.SocketException(str: string);
begin
  raise Exception.Create('ZMQ Socket error: ' + str);
end;


{ TZMQReadThread }

procedure TZMQReadThread.CallEvent;
begin
  if Assigned(Owner.FOnReceiveData) then begin
    Owner.FOnReceiveData(Owner);
  end;
end;

procedure TZMQReadThread.Execute;
var pollitem: zmq_pollitem_t;
    rc: integer;
    message: zmq_msg_t;
    more: int64;
    more_size: PtrUInt;
    part_size: integer;
    MsgPart: TMsgPart;
begin
  try
    while not MustDie do begin
      pollitem.socket :=  Owner.Fsocket;
      pollitem.events := ZMQ_POLLIN;
      rc := zmq_poll(@pollitem, 1, 50);
      if (rc <> 0) then begin
        zmq_msg_init(@message);
        owner.data_size := zmq_msg_recv(@message, Owner.Fsocket, 0);
        if(owner.data_size = -1) then begin
          // error
          break;
        end;
        owner.MsgParts.Clear;
        MsgPart.disp := 0;
        MsgPart.size := owner.data_size;
        owner.MsgParts.Add(MsgPart);

        owner.data.Position := 0;
        owner.data.Write(zmq_msg_data(@message)^, owner.data_size);
        zmq_msg_close(@message);

        while true do begin   // Check for a multipart message
          more_size := sizeof(more);
          zmq_getsockopt(Owner.Fsocket, ZMQ_RCVMORE, @more, @more_size);
          if more = 0 then break;  // Not a multipart message

          zmq_msg_init(@message);
          part_size := zmq_msg_recv(@message, Owner.Fsocket, 0);
          if(part_size = -1) then begin
            // error
            break;
          end;

          MsgPart.disp := owner.data.Position;
          MsgPart.size := part_size;
          owner.MsgParts.Add(MsgPart);

          owner.data_size += part_size;  // New total message size
          owner.data.Write(zmq_msg_data(@message)^, part_size);
          zmq_msg_close(@message);
        end;
        if part_size = -1 then break;

        Synchronize(@CallEvent);
      end;
    end;
  finally
    Terminate;
  end;

end;


//procedure Register;
//begin
//  RegisterComponents('5dpo', [TSdpoZMQ]);
//end;

initialization
//{$i TSdpoZMQ.lrs}

end.
