unit cameras;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GLWin32Viewer, StdCtrls, ComCtrls, IniPropStorage,
  GLGraphics, GLLCLViewer, math, IdGlobal;

type

  { TFCameras }

  TFCameras = class(TForm)
    IniPropStorage: TIniPropStorage;
    PageControl: TPageControl;
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
    procedure FormCreate(Sender: TObject);
    procedure SBQualityChange(Sender: TObject);
    procedure GLSceneViewerPostRender(Sender: TObject);
  private
    { Private declarations }
  public
    t_delta, t_last: int64;

    procedure UpdateGLCameras;
    procedure SendImage(GLBmp32: TGLBitmap32);
  end;

var
  FCameras: TFCameras;

implementation

{$R *.lfm}

uses Viewer, ProjConfig, Params;



procedure TFCameras.FormCreate(Sender: TObject);
begin
  IniPropStorage.IniFileName := GetIniFineName;
  QueryPerformanceFrequency(t_delta);
  t_delta := t_delta div 1000;
end;

procedure TFCameras.SBQualityChange(Sender: TObject);
begin
  EditJPGQuality.Text := inttostr(SBQuality.Position);
end;


procedure TFCameras.UpdateGLCameras;
var GLBmp32: TGLBitmap32;
    Bmp32: TBitmap;
    JpegImage: TJpegImage;
    Stream: TMemoryStream;
    i, sz, MTU: integer;
    t_start, t_end: int64;
    netbuf: TIdBytes;
begin
  if Visible then begin
    with WorldODE do begin
      inc(RemoteImageDecimation);
      if RemoteImageDecimation >= FViewer.GLCameraMem.Tag then begin
        RemoteImageDecimation := 0;
      end else exit;
    end;

    QueryPerformanceCounter(t_start);
    GLBmp32 := GLSceneViewer.Buffer.CreateSnapShot;
    Bmp32 := GLBmp32.Create32BitsBitmap;
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

    if CBSendImage.Checked then begin
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
          SetLength(netbuf, sizeof(RemoteImage) - 512 + sz);
          move(RemoteImage, netbuf[0], sizeof(RemoteImage) - 512 + sz);
          Fparams.UDPServer.SendBuffer(Fparams.EditRemoteIP.Text, 9898, netbuf);
        end;
      end;
    end;
    if CBShowJpeg.Checked then begin
      Stream.Seek(0, soFromBeginning);
      JpegImage.LoadFromStream(Stream);
      ImageCam.Canvas.Draw(0,0, JpegImage);
    end;
    Stream.Free;
    JpegImage.Free;
    Bmp32.free;
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

