unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, lNetComponents, StdCtrls, lNet,
  IniPropStorage, ExtCtrls;//, microml;

type
  TJointState = record
    Theta, W: double;
  end;

  { TFMain }

  TFMain = class(TForm)
    BTest: TButton;
    CBListen: TCheckBox;
    CBLoopBack: TCheckBox;
    EditCount: TEdit;
    EditListenPort: TEdit;
    EditMess: TEdit;
    EditIP: TEdit;
    IniPropStorage: TIniPropStorage;
    Timer: TTimer;
    UDP: TLUDPComponent;
    Memo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    EditTheta: TEdit;
    RGControl: TRadioGroup;
    Label3: TLabel;
    EditW: TEdit;
    Label4: TLabel;
    EditPosition: TEdit;
    Label5: TLabel;
    EditV: TEdit;
    EditManualControl: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    EditControl: TEdit;
    Label8: TLabel;
    EditK1: TEdit;
    Label9: TLabel;
    EditK2: TEdit;
    Label10: TLabel;
    EditK3: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    EditK4: TEdit;
    BSet: TButton;
    procedure BTestClick(Sender: TObject);
    procedure CBListenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure UDPReceive(aSocket: TLSocket);
    procedure BSetClick(Sender: TObject);
  private
    function Control(msg: string): string;
    procedure UDPListenRefresh;
    function Fuzzy(Theta, W, Pos, V: double): double;
    { private declarations }
  public
    stringlist: TStringList;
    K1, K2, K3, K4: double;
  end;


var
  FMain: TFMain;

implementation


procedure EncodeInteger(var StrPacket: TStringList; name: string; data: integer);
begin
  StrPacket.add(name);
  StrPacket.add(format('%d',[data]));
  StrPacket.add('');
end;

procedure EncodeDouble(var StrPacket: TStringList; name: string; data: double);
begin
  StrPacket.add(name);
  StrPacket.add(format('%.6g',[data]));
  StrPacket.add('');
end;

procedure EncodeDoubleArray(var StrPacket: TStringList; name: string; data: array of double);
var i: integer;
begin
  StrPacket.add(name);
  for i := 0 to Length(data) - 1 do begin
    StrPacket.add(format('%.6g',[data[i]]));
  end;
  StrPacket.add('');
end;

function DecodeDoubleDef(var StrPacket: TStringList; name: string; defval: double): double;
var i: integer;
begin
  result := defval;
  i := StrPacket.indexof(name);
  if (i < 0) or (i + 1 >= StrPacket.count) then exit;
  result := strtofloat(StrPacket[i+1]);
end;


{ TFMain }


procedure TFMain.FormCreate(Sender: TObject);
begin
  if Paramcount >0 then begin
    IniPropStorage.IniFileName:=ExtractFilePath(Application.ExeName) + '\' + ParamStr(1);
  end else begin
    IniPropStorage.IniFileName:=ExtractFilePath(Application.ExeName) + '\config.ini';
  end;
  stringlist := TStringList.Create;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  stringlist.Free;
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UDP.Disconnect;
end;

procedure TFMain.BTestClick(Sender: TObject);
begin
  UDP.SendMessage(EditMess.Text, EditIP.Text);
end;

procedure TFMain.UDPListenRefresh;
begin
  if CBListen.Checked then begin
    UDP.Listen(strtoint(EditListenPort.Text));
  end else begin
    UDP.Disconnect;
  end;
end;

procedure TFMain.CBListenClick(Sender: TObject);
begin
  UDPListenRefresh();
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  BSetClick(Sender);
  UDPListenRefresh();
end;

procedure TFMain.TimerTimer(Sender: TObject);
begin
  EditCount.Text := inttostr(EditCount.Tag);
  EditCount.Tag := 0;
end;

procedure TFMain.UDPReceive(aSocket: TLSocket);
var msg: string;
begin
  UDP.GetMessage(msg); // Read the Packet

  Memo.Clear;
  Memo.lines.Add(msg); // Show it

  msg := Control(msg); // Calc new packet

  if CBLoopBack.Checked and (msg <> '') then begin
    UDP.SendMessage(msg, EditIP.Text); // Send it
  end;
  inc(EditCount.Tag);
end;

procedure TFMain.BSetClick(Sender: TObject);
begin
  K1 := strtofloat(EditK1.Text);
  K2 := strtofloat(EditK2.Text);
  K3 := strtofloat(EditK3.Text);
  K4 := strtofloat(EditK4.Text);
end;



function TFMain.Control(msg: string): string;
var StrPacket: TStringList;
    U: double;
    Theta, W, Pos, V: double;
begin
  result := '';

  StrPacket := TStringList.create;
  try
    // Decode Packet
    StrPacket.text := msg;
    Theta := DecodeDoubleDef(StrPacket, 'theta', 0);
    W := DecodeDoubleDef(StrPacket, 'omega', 0);
    Pos := DecodeDoubleDef(StrPacket, 'position', 0);
    V := DecodeDoubleDef(StrPacket, 'speed', 0);

    EditTheta.Text := format('%.2f',[Theta / pi * 180]);
    EditW.Text := format('%.3f',[W]);
    EditPosition.Text := format('%.3f',[Pos]);
    EditV.Text := format('%.3f',[V]);


    U := 0; // default value

    // control functions
    case RGControl.ItemIndex of
      0: U := strtofloatdef(EditManualControl.Text, 0);
      1: U := K1 * Theta + K2 * W + K3 * Pos + K4 * V;
      2: U := Fuzzy(Theta, W, Pos, V);
    end;

    // Encode Control
    StrPacket.Clear;
    EncodeDouble(StrPacket,'U', U);

    result := StrPacket.text;

  finally
    StrPacket.free;
  end;
end;


function TFMain.Fuzzy(Theta, W, Pos, V: double): double;
begin
  result := 0;
end;


initialization
  {$I main.lrs}

end.

