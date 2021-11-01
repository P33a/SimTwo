unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniPropStorage,
  StdCtrls, lNetComponents, lNet, Math;

const
  N_NRJ = 8;

type
  { Non-rigid joints controller }

  TGains = array[0..4] of double;

  TJoint = record
    dx: integer; //index
    K: array[0..4] of double;
    theta_abs, theta_rel, w_abs, w_rel: double;
    theta_ref, u: double;
    phi_a, gama_a, x_a: double;
  end;

  { TFMain }

  TFMain = class(TForm)
    IniPropStorage: TIniPropStorage;
    MemoDebug: TMemo;
    MemoDebug1: TMemo;
    UDP: TLUDPComponent;
    procedure FormShow(Sender: TObject);
    procedure UDPReceive(aSocket: TLSocket);
  private

  public
    procedure Init;
    procedure SetK(var joint: TJoint; K: TGains);
    procedure JointControl(var J: TJoint);
    procedure SetJointVariables(const SL: TStringList; var J: TJoint);
  end;

var
  FMain: TFMain;

  gain_joint0, gain_joint1: TGains;
  joints: array[0 .. N_NRJ-1] of TJoint;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FormShow(Sender: TObject);
begin
  UDP.Listen(9999);
  Init();
end;

procedure TFMain.UDPReceive(aSocket: TLSocket);
var msg: string;
    SL_msg, SL_joint: TStringList;
    i, j: integer;
begin
  UDP.GetMessage(msg);
  if msg = '' then exit;

  MemoDebug.Clear;
  MemoDebug.Append(msg);
  SL_msg := TStringList.Create;
  SL_joint := TStringList.Create;

  // Read data from non-rigid joints
  try
    SL_msg.Text := msg;
    if SL_msg.Count >= 5 * N_NRJ then begin
      for i := 0 to (N_NRJ - 1) do begin
        for j := 0 to 4 do SL_joint.Add(SL_msg[j + i * 5]);
        SetJointVariables(SL_joint, joints[i]);
        SL_joint.Clear;
      end;
    end;
  finally
    SL_joint.Free;
    SL_msg.Clear;
  end;
  //MemoDebug1.Clear;

  // Compute voltage reference for non-rigid joints
  for i := 0 to (N_NRJ - 1) do begin
    if i mod 2 = 0 then joints[i].theta_ref := DegToRad(-65)
    else joints[i].theta_ref := DegToRad(130);
    JointControl(joints[i]);
    SL_msg.Add(format('%g', [joints[i].u]));
    {MemoDebug1.Append(IntToStr(joints[i].dx));
    MemoDebug1.Append(FloatToStr(joints[i].x_a));
    MemoDebug1.Append(FloatToStr(joints[i].u)); }
  end;

  UDP.SendMessage(SL_msg.Text, aSocket);
end;

procedure TFMain.Init;
var i: integer;
begin
  gain_joint0[0] := 10.7110;
  gain_joint0[1] := 0.5160;
  gain_joint0[2] := 2.3263;
  gain_joint0[3] := 0.4410;
  gain_joint0[4] := 0.3118;

  gain_joint1[0] := 10.7110;
  gain_joint1[1] := 0.5160;
  gain_joint1[2] := 2.3263;
  gain_joint1[3] := 0.4410;
  gain_joint1[4] := 0.3118;

  for i := 0 to (N_NRJ - 1) do begin
    if i mod 2 = 0 then SetK(joints[i], gain_joint1)
    else SetK(joints[i], gain_joint0);

    joints[i].phi_a := 1;
    joints[i].gama_a := 1;
    joints[i].theta_ref := 0;
  end;
end;   

procedure TFMain.SetK(var joint: TJoint; K: TGains);
var
  i: integer;
begin
  for i := 0 to 4 do begin
    joint.K[i] := K[i];
  end;
end;

procedure TFMain.JointControl(var J: TJoint);
var maxV: double;
begin
  J.x_a := J.phi_a * J.x_a + J.gama_a * (J.theta_ref - J.theta_abs);

  maxV := 24;
  if J.x_a > maxV / J.K[4] then begin
    J.x_a := maxV / J.K[4];
  end else if J.x_a < -maxV / J.K[4] then begin
    J.x_a := -maxV / J.K[4]
  end;

  J.u := J.K[0] * J.theta_rel + J.K[1] * J.w_rel + J.K[2] * J.theta_abs + J.K[3] * J.w_abs;
  J.u := -J.u + J.x_a * J.K[4];
end;

procedure TFMain.SetJointVariables(const SL: TStringList; var J: TJoint);
begin
  J.dx := StrToIntDef(SL[0], J.dx);
  J.theta_rel := StrToFloatDef(SL[1], J.theta_rel);
  J.theta_abs := StrToFloatDef(SL[2], J.theta_abs) + J.theta_rel;
  J.w_rel := StrToFloatDef(SL[3], J.w_rel);
  J.w_abs := StrToFloatDef(SL[4], J.w_abs) + J.w_rel;
end;

end.

