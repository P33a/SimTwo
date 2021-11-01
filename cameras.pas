unit cameras;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, IniPropStorage, sdpoZMQ, zmq,
  GLGraphics, GLLCLViewer, math, GLCrossPlatform, ODERobotsPublished;

const
  CameraImageWidth = 320 * 2;
  CameraImageHeight = 240 * 2;

type
  TCameraImage = array[0..CameraImageHeight - 1, 0..CameraImageWidth - 1] of TGLPixel32;

  { TFCameras }

  TFCameras = class(TForm)
    IniPropStorage: TIniPropStorage;
    PageControl: TPageControl;
    RGImageTransport: TRadioGroup;
    TabDefault: TTabSheet;
    GLSceneViewer: TGLSceneViewer;
    ImageCam: TImage;
    SBQuality: TScrollBar;
    EditJPGQuality: TEdit;
    EditJPGSize: TEdit;
    CBShowJpeg: TCheckBox;
    EditDeltaT: TEdit;
    Label1: TLabel;
    CBSendImage: TCheckBox;
    TabJPEG: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBQualityChange(Sender: TObject);
    procedure GLSceneViewerPostRender(Sender: TObject);
  private
    { Private declarations }
  public
    t_delta, t_last: int64;
    Bmp32: TBitmap;

    ZPub: TSdpoZMQ;

    procedure UpdateGLCameras;
    procedure SendImage(GLBmp32: TGLBitmap32);
  end;

var
  FCameras: TFCameras;
  CameraImage: TCameraImage;

function GetCameraPixel(X, Y: integer): TRGBAColor;

implementation

{$R *.lfm}

uses Viewer, ProjConfig, Params;

function GetCameraPixel(X, Y: integer): TRGBAColor;
var Pixel: TGLPixel32;
begin
  Pixel := CameraImage[y, x];
  result.Red := Pixel.r;
  result.Green := Pixel.g;
  result.Blue := Pixel.b;
  result.alpha := Pixel.a;
end;



procedure TFCameras.FormCreate(Sender: TObject);
begin
  GLSceneViewer.Width := CameraImageWidth;
  GLSceneViewer.Height := CameraImageHeight;

  IniPropStorage.IniFileName := GetIniFineName(copy(name, 2, MaxInt));
  QueryPerformanceFrequency(t_delta);
  t_delta := t_delta div 1000;
  Bmp32 := TGLBitmap.Create;
  Bmp32.PixelFormat := pf32bit;
  Bmp32.Width := GLSceneViewer.Width;
  Bmp32.Height := GLSceneViewer.Height;
  Bmp32.Canvas.TextOut(0, 0, 'Init');
  //Bmp32.Canvas.Pixels[0, 0]

  ZPub := TSdpoZMQ.Create(FCameras);
  ZPub.SetSockOpt(zmq.ZMQ_SNDHWM, 2);
end;

procedure TFCameras.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FViewer.GLHUDTextObjName.Visible := true;
  ZPub.Close();
end;

procedure TFCameras.FormDestroy(Sender: TObject);
begin
  Bmp32.Free;
end;

procedure TFCameras.FormShow(Sender: TObject);
begin
  FParams.UDPServer_alt.Listen(9899);
  ZPub.bind(zmqPub, 'tcp://*:9899');
end;

procedure TFCameras.SBQualityChange(Sender: TObject);
begin
  EditJPGQuality.Text := inttostr(SBQuality.Position);
end;


procedure TFCameras.UpdateGLCameras;
var GLBmp32: TGLBitmap32;
    JpegImage: TJpegImage;
    Stream: TMemoryStream;
    i, sz, MTU: integer;
    t_start, t_end: int64;
    x, y: integer;

    Pixel: TGLPixel32;
    ps, pd: PGLPixel32Array;
    tmp: byte;
    invertY: boolean;
begin

  if Visible and assigned(WorldODE) then begin
    FViewer.GLHUDTextObjName.Visible := false;
    with WorldODE do begin
      inc(RemoteImageDecimation);
      if RemoteImageDecimation >= FViewer.GLCameraMem.Tag then begin
        RemoteImageDecimation := 0;
      end else exit;
    end;

    QueryPerformanceCounter(t_start);
    GLBmp32 := GLSceneViewer.Buffer.CreateSnapShot;

    if RGImageTransport.ItemIndex in [0, 1, 2] then begin
      //for i := 0 to Bmp32.Height - 1 do begin
      //  Move(GLBmp32.ScanLine[(Bmp32.Height - 1) - i]^, Bmp32.ScanLine[i]^, Bmp32.Width * 4);
      //end;
      invertY := RGImageTransport.ItemIndex = 1;
      for y := 0 to Bmp32.Height - 1 do begin
        if invertY then ps := GLBmp32.ScanLine[y]
        else ps := GLBmp32.ScanLine[(Bmp32.Height - 1) - y];
        pd := Bmp32.ScanLine[y];
        for x := 0 to Bmp32.Width - 1 do begin
          Pixel := ps[x];
          CameraImage[y, x] := pixel;
          tmp := pixel.b;
          pixel.b := pixel.r;
          pixel.r := tmp;
          pd[x] := pixel;
        end;
      end;
    end;

    if CBShowJpeg.Checked or CBSendImage.Checked then begin

      if RGImageTransport.ItemIndex = 0 then begin
        //FDimensions.ImageCam.Canvas.Draw(0,0, Bmp32);
        Stream := TMemoryStream.Create;
        JpegImage := TJpegImage.Create;
        //JpegImage.Smoothing := true;
        //JpegImage.Performance := jpBestQuality;
        JpegImage.CompressionQuality := SBQuality.Position;

        JpegImage.Assign(Bmp32);
        //TODO  JpegImage.Compress;
        JpegImage.SaveToStream(Stream);
        FCameras.EditJPGSize.Text := inttostr(Stream.Position);

        if CBSendImage.Checked and Fparams.UDPServer_alt.Connected then begin
          MTU := 512;
          with WorldODE do begin
            RemoteImage.id := $5241;
            inc(RemoteImage.Number);
            RemoteImage.size := Stream.Position;
            RemoteImage.NumPackets := (RemoteImage.size + MTU - 1) div MTU;
            RemoteImage.ActPacket := 0;

            Stream.Seek(0, soFromBeginning);

            for i := 0 to RemoteImage.NumPackets - 1 do begin
              RemoteImage.ActPacket := i;
              sz := min(RemoteImage.size - (i * MTU), MTU);
              Stream.readBuffer(RemoteImage.data[0], sz);
              //Fparams.UDPServer.SendBuffer(Fparams.EditRemoteIP.Text, 9898, RemoteImage, sizeof(RemoteImage) - 512 + sz);
              //SetLength(netbuf, sizeof(RemoteImage) - 512 + sz);
              //move(RemoteImage, netbuf[0], sizeof(RemoteImage) - 512 + sz);
              //Fparams.UDPServer.SendBuffer(Fparams.EditRemoteIP.Text, 9898, netbuf);
              Fparams.UDPServer_alt.Send(RemoteImage, sizeof(RemoteImage) - 512 + sz, trim(Fparams.EditRemoteIP.Text) + ':9898');
              //Fparams.UDPServer_alt.Send(RemoteImage, sizeof(RemoteImage) - 512 + sz, '127.0.0.1:9898');
            end;
          end;
        end;
        if CBShowJpeg.Checked then begin
          Stream.Seek(0, soFromBeginning);
          JpegImage.LoadFromStream(Stream);
          ImageCam.Canvas.Draw(0,0, JpegImage);
          //ImageCam.Refresh;
        end;
        Stream.Free;
        JpegImage.Free;

      end else if RGImageTransport.ItemIndex = 1 then begin
        ZPub.WriteBuffer(Bmp32.ScanLine[0]^, GLBmp32.DataSize);
        //ZPub.WriteBuffer(GLBmp32.Data^, GLBmp32.DataSize);
        //for y := 0 to Bmp32.Height - 1 do begin
        //  ZPub.WriteBuffer(Bmp32.ScanLine[(Bmp32.Height - 1) - y]^, Bmp32.Width * 4, y <>  Bmp32.Height - 1);
        //end;
      end;
    //Bmp32.free;
    end;

    GLBmp32.Free;

    QueryPerformanceCounter(t_end);
    EditDeltaT.text := format('%.2f [%.2f]',[(t_end - t_start)/t_delta, (t_start - t_last)/t_delta]);
    t_last := t_start;
  end;
end;


procedure TFCameras.SendImage(GLBmp32: TGLBitmap32);
var hMapFile: THandle;
    pBuf: pointer;
    fname: string;
begin
  fname := 'SimTwo_Camera_0';

  hMapFile := CreateFileMapping(
                 INVALID_HANDLE_VALUE,    // use paging file
                 nil,                     // default security
                 PAGE_READWRITE,          // read/write access
                 0,                       // maximum object size (high-order DWORD)
                 GLBmp32.DataSize,        // maximum object size (low-order DWORD)
                 pchar(fname));                  // name of mapping object

  if hMapFile = 0 then
    raise Exception.Create('Unable to open memory file: ' + fname);

  pBuf := MapViewOfFile(hMapFile,   // handle to map object
                        FILE_MAP_ALL_ACCESS, // read/write permission
                        0,
                        0,
                        GLBmp32.DataSize);
  if pBuf = nil then begin
    FileClose(hMapFile); { *Converted from CloseHandle* }
    raise Exception.Create('Unable to map memory file: ' + fname);
  end;

  CopyMemory(pBuf, GLBmp32.Data, GLBmp32.DataSize);

  UnmapViewOfFile(pBuf);

  FileClose(hMapFile); { *Converted from CloseHandle* }
end;



procedure TFCameras.GLSceneViewerPostRender(Sender: TObject);
begin
  UpdateGLCameras;
end;

end.

{
#define BUF_SIZE 256
TCHAR szName[]=TEXT("Global\\MyFileMappingObject");
TCHAR szMsg[]=TEXT("Message from first process.");

int _tmain()

   HANDLE hMapFile;
   LPCTSTR pBuf;

   hMapFile = CreateFileMapping(
                 INVALID_HANDLE_VALUE,    // use paging file
                 NULL,                    // default security
                 PAGE_READWRITE,          // read/write access
                 0,                       // maximum object size (high-order DWORD)
                 BUF_SIZE,                // maximum object size (low-order DWORD)
                 szName);                 // name of mapping object

   if (hMapFile == NULL)
   {
      _tprintf(TEXT("Could not create file mapping object (%d).\n"),
             GetLastError());
      return 1;
   }
   pBuf = (LPTSTR) MapViewOfFile(hMapFile,   // handle to map object
                        FILE_MAP_ALL_ACCESS, // read/write permission
                        0,
                        0,
                        BUF_SIZE);

   if (pBuf == NULL)
   {
      _tprintf(TEXT("Could not map view of file (%d).\n"),
             GetLastError());

       CloseHandle(hMapFile);

      return 1;
   }


   CopyMemory((PVOID)pBuf, szMsg, (_tcslen(szMsg) * sizeof(TCHAR)));
    _getch();

   UnmapViewOfFile(pBuf);

   CloseHandle(hMapFile);

   return 0;
}

