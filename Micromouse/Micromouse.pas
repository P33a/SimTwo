type
 TRobotControls = record
    V,VR,VL: double;
    W1,W2: double;
    SensorFrontal,SensorDireito,SensorEsquerdo:double;
  end;
// Global Variables Here
var
RobotControls : TRobotControls;
irobot: integer;

//Procedure, functions and Routines





function DecodeJsonDouble(var StrPacket: TStringList; name: string; defval: double): double;
var i: integer;
//este procedimento decodifica as funções doubre para envialas
begin
  result := defval;
  i := StrPacket.indexof(name);

  if (i < 0) or (i + 1 >= StrPacket.count) then exit;
  result := strtofloat(StrPacket[i+1]);
end;


procedure Communication_MicroController(var RC: TRobotControls);
var
StrPacket: string; //cria uma lista estruturada de strings
ReceivePacket: TStringList;

begin

   ReceivePacket:=  TStringList.create;
   
   RC.SensorDireito:=GetSensorVal(0,2);
   RC.SensorEsquerdo:=GetSensorVal(0,0);
   RC.SensorFrontal:=GetSensorVal(0,1);

   
   //Inicia a String do JSON            format('%.6g',[data])
   StrPacket := '{';

   StrPacket:= StrPacket + 'W1' + ':' + format('%.6g',[RC.W1]) + ',';
   StrPacket:= StrPacket + 'W2' + ':' + format('%.6g',[RC.W2]) + ',';

   StrPacket:= StrPacket + 'SensorFrontal' + ':' + format('%.6g',[RC.SensorFrontal]) + ',';
   StrPacket:= StrPacket + 'SensorDireito' + ':' + format('%.6g',[RC.SensorDireito]) + ',';
   StrPacket:= StrPacket + 'SensorEsquerdo' + ':' + format('%.6g',[RC.SensorEsquerdo]) + ',';

   //Finaliza a String Json
   StrPacket := StrPacket + '}';

   WriteUDPData('192.168.43.123',9810, StrPacket);


   ReceivePacket.text:= ReadUDPData();

   RC.VR := DecodeJsonDouble(ReceivePacket, 'VR', 0);    //atribui os valores
   RC.VL := DecodeJsonDouble(ReceivePacket, 'VL', 0);    //atribui os valores
   
   SetRCValue(1,1,'VR');
   SetRCValue(2,1,floattostr(RC.VR));
   SetRCValue(1,2,'VL');
   SetRCValue(2,2,floattostr(RC.VL));
   ReceivePacket.free;
   
end;

// this procedure is called periodicaly (default: 40 ms)
procedure Control;
begin

   Communication_MicroController(RobotControls); //Remote Control
   AddTrailNode(irobot, GetRobotX(irobot), GetRobotY(irobot), 0.01); //Add a follow line;

end;

// this procedure is called once when the script is started
procedure Initialize;
begin
 irobot:=0;
 SetTrailColor(0, 0, 0,0);   // Trail[0] is black
 SetRobotPos(irobot,0.09,0.09,0.01,1.57);

end;
