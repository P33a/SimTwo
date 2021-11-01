unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, JSONPropStorage, ComCtrls, sdpoZMQ, zmq, BGRABitmap,
  BGRABitmapTypes, math;

const
  ImgSize = 964;
  ImgWidth = ImgSize;
  ImgHeight = ImgSize;

  OrgImgSize = 480;
  OrgImgWidth = 640;
  OrgImgHeight = 480;

type
  TDistortionMap = array[0..OrgImgSize - 1, 0..OrgImgSize - 1] of TPoint;

  TBayerImg = array[0..ImgHeight - 1, 0..ImgWidth - 1] of byte;

  TRemoteImage = packed record
    id, Number, size, NumPackets, ActPacket: integer;
    data: array[0..511] of byte;
  end;

  { TFMain }

  TFMain = class(TForm)
    BSetDistortion: TButton;
    CBShow: TCheckBox;
    CBSend: TCheckBox;
    EditPacketCount: TEdit;
    EditOvisIP: TEdit;
    EditDistPol0: TEdit;
    EditDistPol1: TEdit;
    EditDistPol2: TEdit;
    JSONPropStorage: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    StatusBar: TStatusBar;
    Timer: TTimer;
    procedure BSetDistortionClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ReadSocket(Sender: TObject);
    procedure loadBayer(fname: string);
    procedure SetDistortion;
    { private declarations }
  public
    RemoteImage: TRemoteImage;
    ImgPacketCount: integer;
    PacketCount, actImageNum: integer;
    Stream: TMemoryStream;
    JpegImage: TJpegImage;

    ZSub, ZPub: TSdpoZMQ;
    bmp, SquareBmp, DistBmp, SendBmp: TBGRABitmap;
    BayerImg: TBayerImg;

    DistortionMap: TDistortionMap;
    distCoefs: array of double;
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}


function PolyEval(const P: array of double; x: double): double;
var i: integer;
begin
  result := 0;
  for i := high(P) downto 0 do begin
    result := result * X + P[i];
  end;
end;


function sat0(v, limit: integer): integer; inline;
begin
  result := v;
  if v > limit then result := limit;
  if v < 0 then result := 0;
end;

procedure BuildDistortedImage(var DistortedImage: TBGRABitmap; var img: TBGRABitmap; var DistortionMap: TDistortionMap);
var x, y: integer;
    Pt: TPoint;
begin
  for y := 0 to DistortedImage.Height - 1 do begin
    for x := 0 to DistortedImage.Height - 1 do begin
      pt := DistortionMap[y, x];
      if pt.X >= 0 then begin
        (DistortedImage.ScanLine[y] + x)^ := (img.ScanLine[Pt.y] + Pt.X)^;
      end else begin
        (DistortedImage.ScanLine[y] + x)^ := BGRABlack;
      end;
    end;
  end;
end;

procedure BuildDistortionMap(var DistortionMap: TDistortionMap; P: array of double);
var x, y, xc, yc, xd, yd: integer;
    d, f: double;
begin
  for y := 0 to OrgImgSize - 1 do begin
    for x := 0 to OrgImgSize - 1 do begin
      xc := x - OrgImgSize div 2;
      yc := y - OrgImgSize div 2;
      d := sqrt(sqr(xc) + sqr(yc));
      f := PolyEval(P, d);

      if (f > 0) then begin
        xd := round(xc / f + OrgImgSize / 2);
        yd := round(yc / f + OrgImgSize / 2);
      end;
      if (f <= 0) or
         (xd < 0) or
         (yd < 0) or
         (xd > OrgImgSize - 1) or
         (yd > OrgImgSize - 1)  then begin
        DistortionMap[y, x].X := -1;
      end else begin
        DistortionMap[y, x].X := xd;
        DistortionMap[y, x].Y := yd;
      end;
    end;
  end;
end;

procedure BuildBayerImage(var BayerImg: TBayerImg; var img: TBGRABitmap);
var x, y, x2, y2: integer;
begin
  for y := 0 to ImgHeight div 2 - 1 do begin
    y2 := 2 * y;
    for x := 0 to ImgWidth div 2 - 1 do begin
      x2 := 2 * x;
      BayerImg[y2, x2] := TBGRAPixel(img.GetPixel(x2, y2)).red;
      BayerImg[y2, x2 + 1] := TBGRAPixel(img.GetPixel(x2 + 1, y2)).green;
      BayerImg[y2 + 1, x2] := TBGRAPixel(img.GetPixel(x2, y2 + 1)).green;
      BayerImg[y2 + 1, x2 + 1] := TBGRAPixel(img.GetPixel(x2 + 1, y2 + 1)).blue;
    end;
  end;
end;

{ TFMain }

procedure TFmain.loadBayer(fname: string);
var Fdesc: file;
begin
  if FileExists(fname) then begin
    assignFile(FDesc, Fname);
    Reset(Fdesc, sizeof(BayerImg));
    Blockread(FDesc, BayerImg[0,0], 1);
    closefile(Fdesc);
  end;
end;

procedure TFMain.ReadSocket(Sender: TObject);
var y, x, len, disp: integer;
    s, txt: string;
    start: QWord;
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

  start := GetTickCount64();
  // Read the ZMQ buffer
  Stream.Clear;
  ZSub.ReadStream(Stream);
  len := Stream.Position;
  if len = 0 then exit;

  //EditDebug.Text := IntToStr(len);
  Stream.Seek(0, soBeginning);

  bmp.TextOut(10, 10, IntToStr(len), BGRABlack);
  //for i := 0 to bmp.Height - 1 do begin
  //  stream.Read(bmp.ScanLine[i]^,  bmp.Width * 4);
  //end;

  // Read the bitmap
  stream.Read(bmp.DataByte^,  len);
  bmp.InvalidateBitmap;
  txt := format('%2d', [GetTickCount64() - start]);

  // Crop the square in the middle
  disp := (bmp.Width - bmp.Height) div 2;
  for y := 0 to bmp.Height - 1 do begin
    for x := 0 to bmp.Height - 1 do begin
      (SquareBmp.ScanLine[y] + x)^ := (bmp.ScanLine[y] + x + disp)^;
    end;
  end;
  txt := txt + ' Sq ' + inttostr(GetTickCount64() - start);

  BuildDistortedImage(DistBmp, SquareBmp, DistortionMap);

  if Assigned(SendBmp) then SendBmp.Free;
  //SendBmp := SquareBmp.Resample(ImgWidth, ImgWidth, rmSimpleStretch) as TBGRABitmap;
  SendBmp := DistBmp.Resample(ImgWidth, ImgWidth, rmSimpleStretch) as TBGRABitmap;
  txt := txt + ' Rs ' + inttostr(GetTickCount64() - start);

  if CBSend.Checked then begin
    BuildBayerImage(BayerImg, SendBmp);
    txt := txt + ' By ' + inttostr(GetTickCount64() - start);

    ZPub.WriteBuffer(BayerImg, SizeOf(BayerImg));
    txt := txt + ' Snd ' + inttostr(GetTickCount64() - start);
  end;
  refresh();

  //EditDebug.Text := txt;
  StatusBar.SimpleText := txt;
  //Image.Picture.Bitmap.LoadFromStream(Stream, Stream.Size);

end;

procedure TFMain.FormShow(Sender: TObject);
begin
  SetDistortion();

  ZSub.SetSockOpt(zmq.ZMQ_RCVHWM, 2);
  //SdpoZMQSub.Connect(zmqSub, 'tcp://192.168.1.144:5556');
  ZSub.Connect(zmqSub, 'tcp://127.0.0.1:9899');  // SimTwo is in the same machine
  ZSub.Subscribe('');

  //ZPub.Connect(zmqPub, 'tcp://192.168.126.129:6868');
  ZPub.SetSockOpt(zmq.ZMQ_SNDHWM, 2);
  ZPub.Connect(zmqPub, 'tcp://' + EditOvisIP.Text + ':6868');
end;

procedure TFMain.FormPaint(Sender: TObject);
begin
  if CBShow.Checked then begin
    //bmp.Draw(Canvas, 0, 0, True);
    //SquareBmp.Draw(Canvas, 0, 0, True);
    SendBmp.Draw(Canvas, 0, 0, True);
  end;
end;

procedure TFMain.BSetDistortionClick(Sender: TObject);
begin
  SetDistortion();
end;


procedure TFMain.SetDistortion;
begin
  distCoefs[0] := StrToFloatDef(EditDistPol0.Text, distCoefs[0]);
  distCoefs[1] := StrToFloatDef(EditDistPol1.Text, distCoefs[1]);
  distCoefs[2] := StrToFloatDef(EditDistPol2.Text, distCoefs[2]);
  BuildDistortionMap(DistortionMap, distCoefs);
end;


procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ZSub.Close();
end;

procedure TFMain.TimerTimer(Sender: TObject);
begin
  //ZPub.WriteBuffer(BayerImg, SizeOf(BayerImg));
end;


procedure TFMain.FormCreate(Sender: TObject);
begin
  Stream := TMemoryStream.Create;

  ZSub := TSdpoZMQ.Create(FMain);
  ZSub.OnReceiveData := @ReadSocket;

  bmp := TBGRABitmap.Create(OrgImgWidth, OrgImgHeight, BGRAWhite);

  SquareBmp := TBGRABitmap.Create(OrgImgSize, OrgImgSize, BGRAWhite);
  DistBmp := TBGRABitmap.Create(OrgImgSize, OrgImgSize, BGRAWhite);

  SendBmp := TBGRABitmap.Create(ImgWidth, ImgWidth, BGRAWhite);

  ZPub :=  TSdpoZMQ.Create(FMain);
  //loadBayer('bayer1.byr');

  SetLength(distCoefs, 3);
  distCoefs[0] := 2.5;
  distCoefs[1] := -0.015;
  distCoefs[2] := 0; //-0.0002;

end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  SquareBmp.Free;
  SendBmp.Free;
  DistBmp.Free;
  bmp.Free;
  Stream.free;
end;

end.

