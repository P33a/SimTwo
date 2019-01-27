type
 TRobotControls = record
    V,Vr,Vl: double;
    Wr,Wl: double;
    SensorF,SensorR,SensorL:double;
  end;
// Global Variables Here
var
RobotControls : TRobotControls;
IDRobot: integer;
LostPacket: integer;

//Procedure, functions and Routines
function DecodeDouble(var StrPacket: TStringList; name: string; defval: double): double;
var i: integer;
//este procedimento decodifica as funções double para envia-las
begin
  result := defval;
  i := StrPacket.indexof(name);

  if (i < 0) or (i + 1 >= StrPacket.count) then exit;
  result := strtofloat(StrPacket[i+1]);
end;


procedure SendDataCOM(var RC: TRobotControls);
var
StrPacket: string; //cria uma lista estruturada de strings
//ReceivePacket: TStringList;
begin
   //Inicia a String do JSON            format('%.6g',[data])
   StrPacket := '{';
   
   StrPacket:= StrPacket + 'wr' + ':' + format('%.3g',[RC.Wr]) + ',';
   StrPacket:= StrPacket + 'wl' + ':' + format('%.3g',[RC.Wl]) + ',';

   StrPacket:= StrPacket + 'sf' + ':' + format('%.3g',[RC.SensorF]) + ',';
   StrPacket:= StrPacket + 'sr' + ':' + format('%.3g',[RC.SensorR]) + ',';
   StrPacket:= StrPacket + 'sl' + ':' + format('%.3g',[RC.SensorL]);

   //Finaliza a String Json
   StrPacket := StrPacket + '}';
   StrPacket := StrPacket + #10;


   //WriteUDPData('192.168.5.1',9810, StrPacket); //Ip_Uc +  Port.
   
   WriteComPort(StrPacket);
   //Writeln('Send: '+ StrPacket );

end;

procedure ReadDataCom();
var
ReceivePacket: TStringList;

begin
   ReceivePacket:=  TStringList.create;
   ReceivePacket.text := ReadComPort();

   if( ReceivePacket.text<>'')then
   begin
       LostPacket:= 0; //Reinicia a dos pacotes perdidos
       RobotControls.Vr:= DecodeDouble(ReceivePacket, 'Vr',RobotControls.Vr);    //atribui os valores
       RobotControls.Vl := DecodeDouble(ReceivePacket, 'Vl',RobotControls.Vl);    //atribui os valores
       SendDataCOM(RobotControls);
   end
   else
   begin
    LostPacket:= LostPacket +1;
    Writeln('Lost: '+ inttostr(LostPacket));
    if (LostPacket>50)then
    begin
       RobotControls.Vr:= 0;
       RobotControls.Vl := 0;
    end;
   end;
   ReceivePacket.free;
end;


// this procedure is called periodicaly (default: 40 ms)
procedure Control;begin
   ReadDataCom();
   
   //Atualiza as velocidades dos motores.
   SetAxisSpeedRef(IDRobot, 0, RobotControls.Vr);
   SetAxisSpeedRef(IDRobot, 1, RobotControls.Vl);
   AddTrailNode(IDRobot, GetRobotX(IDRobot), GetRobotY(IDRobot), 0.01); //Add a follow line;

   //Lê dados dos sensores
   RobotControls.SensorR:=GetSensorVal(IDRobot,2);
   RobotControls.SensorL:=GetSensorVal(IDRobot,0);
   RobotControls.SensorF:=GetSensorVal(IDRobot,1);
   RobotControls.Wr := GetAxisOdo(IDRobot,0);
   RobotControls.Wl := GetAxisOdo(IDRobot,1);


end;

// this procedure is called once when the script is started
procedure Initialize;
var
  times: integer;
begin
 IDRobot:=0;
 LostPacket:=0;
 SetTrailColor(0, 0, 0,0);   // Trail[0] is black
 SetRobotPos(IDRobot,0.09,0.09,0.001,1.57);
 WriteComPort ('{Reset:1}'+#10);
 ClearTrail(0);
end;
