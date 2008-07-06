unit RemoteControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, IdSocketHandle, StdCtrls, Remote;

type
  TFRemoteControl = class(TForm)
    UDPServer: TIdUDPServer;
    EditData: TEdit;
    Memo: TMemo;
    EditRemoteIP: TEdit;
    Label8: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UDPServerUDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
                                                                               
var
  FRemoteControl: TFRemoteControl;

implementation

{$R *.dfm}

procedure TFRemoteControl.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  UDPServer.Active := false;
end;

procedure TFRemoteControl.UDPServerUDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var RemControl: TRemControl;
begin
  //AData.
  //EditData.Text
  Memo.Lines.LoadFromStream(Adata);

  ZeroMemory(@RemControl,sizeof(RemControl));
  RemControl.U[0] := -24;
  RemControl.U[1] := 24;

  RemControl.Wref[0] := -24;
  RemControl.Wref[1] := 24;
  UDPServer.SendBuffer(EditRemoteIP.Text, 9800, RemControl, sizeof(RemControl));
end;

end.
