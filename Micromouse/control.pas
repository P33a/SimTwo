var
  IDrobot : integer;  //contem o ID do robo, como süo tem 1 ID = 0
  V , W : double;     //V - velocidade linar    W - velocidade angular
  SensorR, SensorL, SensorF : double;  //armazena a leitura dos sensores de distancia
  TurnRight, TurnLeft : boolean;  //Armazena quando ocorre a falta de uma parede a direita
  eixo : boolean;      //Se true movimentacao no eixo X/False eixo Y
  posicao: integer;   //armazena a posicao do robo
  contador : integer;  //contador que controla o giro do robo
  measureonce : boolean;  //evita repetição de paradas pra virar
  posicaoX, posicaoY : integer;
  deslocamentoD, deslocamentoE : double;

//Procedimento que realiza a leitura dos sensores de distância
procedure sensors();
begin
  SensorR:=GetSensorVal(0,2);
  SensorL:=GetSensorVal(0,0);
  SensorF:=GetSensorVal(0,1);

  if(SensorF<0)then
    SensorF:= 5;
  if(SensorR<0)then
    SensorR:= 5;
  if(SensorL<0)then
    SensorL:= 5;

  SetRcValue(2,1,FloatToStr(SensorF));
  SetRcValue(4,1,FloatToStr(SensorR));
  SetRcValue(6,1,FloatToStr(SensorL));

end;

//Procedimento que define e aplica a velocidade final em cada roda
procedure SpeedControl();
var
  finalspeedR, finalspeedL : double;
begin
  finalspeedR:= V - W;
  finalspeedL:= V + W;
  SetAxisSpeedRef(IDrobot, 1, finalspeedR);
  SetAxisSpeedRef(IDrobot, 0, finalspeedL);
end;

//Procedimento que realiza o controle da distância entre o robo e as paredes
procedure BorderControl();
var
  kappa : integer;
  DifSensor, Wb : double;
begin
  Wb := W;     //controle para casos de pulsos momentaneos
  kappa := 100;

  if(abs(SensorR-SensorL)<0.01)then
    W:=(SensorR-SensorL)*50
  else
    if((SensorR>0.15)and(SensorL>0.15))then
      W:=0
    else
      if((SensorR<=SensorL)and(SensorF>0.12))then
      begin
        DifSensor:= SensorR - 0.0791;
        W:=kappa*DifSensor;
      end
      else
        if((SensorL<SensorR)and(SensorF>0.12))then
        begin
          DifSensor:= SensorL - 0.0791;
          W:=-kappa*DifSensor;
        end;

  if((W>50)or(W<-50))then
    W:=Wb;

end;

//Procedimento de aquisição do posicionamento do robô nos eixos X e Y
procedure SLAM();
begin
  if (eixo=TRUE)then
  begin
    posicao:= Round(GetRobotX(IDrobot)*1000);
    posicaoX:=posicao;
  end
  else
  begin
    posicao:= Round(GetRobotY(IDrobot)*1000);
    posicaoY:=posicao;
  end;

end;

//
procedure Odometria();
begin
  deslocamentoD:=deslocamentoD+GetAxisOdo(IDrobot, 0)*0.1251545;
  deslocamentoE:=deslocamentoE+GetAxisOdo(IDrobot, 1)*0.1251545;
  SetRcValue(12,1,IntToStr(Round(deslocamentoD)));
  SetRcValue(14,1,IntToStr(Round(deslocamentoE)));
end;

//Identifica caminhos laterais de possível movimentação
procedure TurnVerify();
begin
  if(((posicao MOD 180)<20))then
    if(SensorR>0.15)then
      TurnRight:=TRUE;

  if((SensorF<0.25)and((posicao MOD 180)<20))then
    if(SensorL>0.15)then
      TurnLeft:=TRUE;

end;

//Procedimento que identifica a posição em que a curva será efetuada
procedure PointOFTurn();
begin
  if((measureonce=FALSE)and(TurnRight=TRUE)or(measureonce=FALSE)and(TurnLeft=TRUE))then
    if(((posicao MOD 90)<10)or((posicao MOD 90)>80))then
    begin
      if(((posicao MOD 180)<10)or((posicao MOD 180)>170))then
        writeln('')
      else
      begin
        contador:=1;
        measureonce:=TRUE;
      end;
    end;

  if((measureonce=FALSE)and(SensorF<0.2))then
    if(((posicao MOD 90)<10)or((posicao MOD 90)>80))then
    begin
      if(((posicao MOD 180)<10)or((posicao MOD 180)>170))then
        writeln('')
      else
      begin
        contador:=-40;
        measureonce:=TRUE;
      end;
    end;

  if(measureonce=TRUE)then      //
    if((posicao MOD 160)<20)then
      measureonce:=FALSE;
end;

//Controle do movimento frontal
procedure MoveFoward();
begin
  Sensors();
  SLAM();
  TurnVerify();
  PointOFTurn();
  if((SensorF<0.180)or(TurnRight=TRUE))then
    V:=20
  else
    V:=40;

  BorderControl();
  SpeedControl()
end;

//Controle do giro para a direita
procedure SpinRight();
begin
  if(contador<2)then
    contador:=contador+1
  else
    if(contador<20)then
    begin
      V:=0;
      W:=11.55;
      contador:=contador+1;
      SpeedControl();
    end
    else
      if(contador<22)then
      begin
        contador:=contador+1;
        W:=0;
        V:=0;
      end
      else
      begin
        contador:=0;
        eixo:=not(eixo);
        TurnRight:=FALSE;
        TurnLeft:=FALSE;
      end;
end;

//Controle do giro para a esquerda
procedure SpinLeft();
begin
  if(contador<2)then
    contador:=contador+1
  else
    if(contador<20)then
    begin
      V:=0;
      W:=-11.65;
      contador:=contador+1;
      SpeedControl();
    end
    else
      if(contador<22)then
      begin
        contador:=contador+1;
        W:=0;
        V:=0;
      end
      else
      begin
        contador:=0;
        eixo:=not(eixo);
        TurnLeft:=FALSE;
        TurnRight:=FALSE;
      end;
end;

//Controle da rotação de 180 graus
procedure Rotate();
begin
  if(contador<-38)then
    contador:=contador+1
  else
    if(contador<-2)then
    begin
      V:=0;
      W:=-11.65;
      contador:=contador+1;
      SpeedControl();
    end
    else
      if(contador<=0)then
      begin
        contador:=contador+1;
        W:=0;
        V:=0;
      end;
end;

//Controle central, este procedimento se repete a cada 40 ms
procedure Control;
begin
  Odometria();
  if(contador>0)then
    begin
      if(TurnRight=TRUE)then
        SpinRight()
      else
        SpinLeft();
    end
    else
     if(contador<0)then
       Rotate()
     else
       MoveFoward();
end;

//Configurações iniciais
procedure Initialize;
var
  i, j : integer;
begin
  V:=40;
  TurnRight:=FALSE;
  TurnLeft:=FALSE;
  eixo:=FALSE;
  measureonce:=FALSE;
  SetRcValue(1,1,'Sensor Frontal:');
  SetRcValue(3,1,'Sensor Direita:');
  SetRcValue(5,1,'Sensor Esquerda:');
  SetRcValue(11,1,'Distância percorrida roda esquerda');
   SetRcValue(13,1,'Distância percorrida roda direita');
end;

