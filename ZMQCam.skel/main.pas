unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, sdpoZMQ, BGRABitmap, BGRABitmapTypes, math;

type
  TRemoteImage = packed record
    id, Number, size, NumPackets, ActPacket: integer;
    data: array[0..511] of byte;
  end;

  { TFMain }

  TFMain = class(TForm)
    EditPacketCount: TEdit;
    EditDebug: TEdit;
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ZMQReceive;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ReadSocket(Sender: TObject);
    { private declarations }
  public
    RemoteImage: TRemoteImage;
    ImgPacketCount: integer;
    PacketCount, actImageNum: integer;
    Stream: TMemoryStream;
    JpegImage: TJpegImage;

    ZSub: TSdpoZMQ;
    bmp: TBGRABitmap;
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }


procedure TFMain.ReadSocket(Sender: TObject);
var i, len: integer;
    s, txt: string;
begin
  {if ZSub.PartCount() > 0 then begin
    for i := 0 to ZSub.PartCount - 1 do begin
      s := ZSub.ReadPartData(i);
      Debug(IntToStr(length(s)));
      Debugln(s);
    end;
    exit;
  end;}
  inc(ImgPacketCount);
  EditPacketCount.Text := IntToStr(ImgPacketCount);

  Stream.Clear;
  ZSub.ReadStream(Stream);
  len := Stream.Position;
  if len = 0 then exit;

  EditDebug.Text := IntToStr(len);
  Stream.Seek(0, soBeginning);

  bmp.TextOut(10, 10, IntToStr(len), BGRABlack);
  //for i := 0 to bmp.Height - 1 do begin
  //  stream.Read(bmp.ScanLine[i]^,  bmp.Width * 4);
  //end;
  stream.Read(bmp.DataByte^,  len);
  bmp.InvalidateBitmap;
  refresh();

  //Image.Picture.Bitmap.LoadFromStream(Stream, Stream.Size);
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  //SdpoZMQSub.Connect(zmqSub, 'tcp://192.168.1.144:5556');
  ZSub.Connect(zmqSub, 'tcp://127.0.0.1:9899');
  ZSub.Subscribe('');
end;

procedure TFMain.FormPaint(Sender: TObject);
begin
  bmp.Draw(Canvas, 0, 0, True);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ZSub.Close();
end;

procedure TFMain.ZMQReceive;
var msg: string;
begin

end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Stream := TMemoryStream.Create;

  ZSub := TSdpoZMQ.Create(FMain);
  ZSub.OnReceiveData := @ReadSocket;

  bmp := TBGRABitmap.Create(320, 240, BGRAWhite);
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  bmp.Free;
  Stream.free;
end;

end.

