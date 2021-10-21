unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, lNetComponents, lNet, math;

type
  TRemoteImage = packed record
    id, Number, size, NumPackets, ActPacket: integer;
    data: array[0..511] of byte;
  end;

  { TFMain }

  TFMain = class(TForm)
    UDP: TLUDPComponent;
    EditPacketCount: TEdit;
    EditDebug: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure UDPReceive(aSocket: TLSocket);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    RemoteImage: TRemoteImage;
    ImgPacketCount: integer;
    PacketCount, actImageNum: integer;
    Stream: TMemoryStream;
    JpegImage: TJpegImage;
    MTU: integer;
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FormShow(Sender: TObject);
begin
  UDP.Listen(9898);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UDP.Disconnect();
end;

procedure TFMain.UDPReceive(aSocket: TLSocket);
var msg: string;
begin
  UDP.GetMessage(msg);
  inc(PacketCount);
  if msg = '' then exit;
  EditPacketCount.Text := inttostr(PacketCount);
  move(msg[1], RemoteImage, min(length(msg), sizeof(remoteimage)));

  if RemoteImage.ActPacket = 0 then begin
    Stream.SetSize(RemoteImage.size);
    Stream.Seek(0, soFromBeginning);
    Stream.Position := 0;
    ImgPacketCount := 0;
  end;

  //Stream.Seek(MTU * RemoteImage.ActPacket, soFromBeginning);
  stream.writeBuffer(RemoteImage.data[0], MTU);
  inc(ImgPacketCount);

  if (RemoteImage.ActPacket = RemoteImage.NumPackets - 1) and
     (ImgPacketCount =  RemoteImage.NumPackets) then begin
    JpegImage := TJpegImage.Create;
    EditDebug.Text := inttostr(Stream.position);
    try
      Stream.Seek(0, soFromBeginning);
      JpegImage.LoadFromStream(Stream);
      Canvas.Draw(0,0, JpegImage);
    finally
      JpegImage.Free;
    end;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  MTU := 512;
  Stream := TMemoryStream.Create;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  Stream.free;
end;

end.

