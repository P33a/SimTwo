#include <math.h>

String incomingPacket;  // buffer for incoming packets;  
char  replyPacket[255]; 
String strPacket;

//********Strutura para o MCR************.

struct TRobotControl {
  float Vr,Vl ;
  float SensorL,SensorR,SensorF;
  float Wr,Wl ;
};

TRobotControl RC; //Variavel para controlar o MCR

// **** Control Variables ***

float kappa;            //multiplicador do controle de distancia das paredes
float V, W;             //V - velocidade linear     W - velocidade angular
double eixoY;  //posicao Y
double theta;
long int posicao;             //armazena a posicao em X ou Y conforme o eixo
int eixo;                //eixo de movimentação
bool measureonce = false;         //confere apenas uma vez os sensores para virar e aciona as rotações
bool turn_right, turn_left;
int distance;
bool turn, rotate;
bool parede;
int VAWR, VAWL;

// ****RobotSize*******
double WheelSize = 0.1251545;


// **** Comunication Variables ******
const long interval = 10;  // interval to Send Vr and Vl to SimTwo (milliseconds)
unsigned long previousMillis = 0; 
String inputString = "";         // a String to hold incoming data
bool stringComplete = false;  // whether the string is complete
int flagComunica = 0;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(115200);
  inputString.reserve(200); 
  //Serial.setTimeout(1);
  //Initialize control Variable
  theta = 0;
  turn=false;
  rotate=false;
  parede=false;
  eixo = 0;
  V=20;
  W=0;
  turn_right=false;
  turn_left=false;
  VAWR=0;
  VAWL=0;
  eixoY=90;
  RC.Vr=0;
  RC.Vl=0;
  kappa=100;
  distance=0;
}
void SpeedControlFoward(){
  int VelR, VelL;

  //BorderControl(kappa);
  RC.Vr=V-W;
  RC.Vl=V+W;
}

void BorderControl(float kappa){
  unsigned int sub; //arrumar o controle de borda
  double temp;
  double wa;
  wa = W;
  if((RC.SensorL<RC.SensorR))
        temp = RC.SensorL - 0.0791;
  else
  if((RC.SensorR<RC.SensorL))
      temp = (RC.SensorR - 0.0791)*-1;

  W=temp*kappa;
  
  if((RC.SensorR>0.100)&&(RC.SensorL>0.100))
    W=0;

}

void Odometria(){
  eixoY=eixoY+((RC.Wr*WheelSize+RC.Wl*WheelSize)/2);
  posicao=(int)(eixoY);
}
void RotateOdometria(){
  if(turn_right==true){
    theta=theta+(atan2(RC.Wr*0.1251545,39.5)*57.295);
    RC.Wr=0;
  }else
    if(turn_left==true){
      theta=theta+(atan2(RC.Wl*0.1251545,39.5)*57.295);
      RC.Wl=0;
    }else{
      theta=theta+(atan2(RC.Wr*0.1251545,39.5)*57.295);
      RC.Wr=0;
    }
}

void TurnRight(){
  if((int) theta<115){
    V = 0;
    W = -5;
    RotateOdometria();    
  }  
  else{
    theta=0;
    turn_right=false;
    turn_left=false;
    turn=false;
    parede=false;
    measureonce=false;
    V=20;
    W=0;
    distance=posicao;
  }
}

void TurnLeft(){
  if((int) theta<115){
    V = 0;
    W = 5;
    RotateOdometria();
  }
  else{
    theta=0;
    turn_right=false;
    turn_left=false;
    turn=false;
    parede=false;
    measureonce=false;
    V=20;
    W=0;
    distance=posicao;
  }
}

void Rotate(){
  if((int) theta<220){
    V = 0;
    W = -5;
    RotateOdometria();

  }
  else{
    theta=0;
    turn_right=false;
    turn_left=false;
    turn=false;
    parede=false;
    measureonce=false;
    V=20;
    W=0;
    distance=posicao;
    rotate=false;
  }
}




void loop() {
  unsigned long currentMillis = millis();
  if (currentMillis - previousMillis >= interval){  
    unsigned long diff = currentMillis - previousMillis;  
    previousMillis = currentMillis; // save the last time you send data to SimTwo;    
    if(flagComunica==0){
      strPacket =  "Vr\r\n" + String(RC.Vr)+"\r\n\r\n";
      strPacket =  strPacket + "Vl\r\n" + String(RC.Vl)+"\r\n\r\n";   
      strPacket = strPacket + "  Time: \r\n" + String(diff)+"\r\n\r\n"; //Para Debug
      Serial.println(strPacket);
      flagComunica = 1;
    }         
  }
      
   if((stringComplete)and(flagComunica==1)){
    incomingPacket=inputString;
    inputString = "";
    stringComplete = false;  
    flagComunica=0;  
    if ((DecodeJsonDouble(incomingPacket,"Reset:") != 1 )){      
      RC.Wr = DecodeJsonDouble(incomingPacket,"wr:");
      RC.Wl = DecodeJsonDouble(incomingPacket,"wl:");
      RC.SensorL = DecodeJsonDouble(incomingPacket,"sl:");
      RC.SensorR = DecodeJsonDouble(incomingPacket,"sr:");
      RC.SensorF = DecodeJsonDouble(incomingPacket,"sf:");
      
      if (RC.SensorF == -1){
        RC.SensorF = 200;
      }
      if(rotate==false)  
        BorderControl(kappa);
      Odometria();      

      if(measureonce==false){
      if(RC.SensorR>0.2){
        distance=posicao;
        turn_right=true;
        measureonce=true;
        if(RC.SensorF<0.3)
          parede=true;
      }else
        if(RC.SensorL>0.2){
          if((RC.SensorF<0.2)&&(RC.SensorF>0)){
            turn_left=true;
            measureonce=true;
          }
        }
      }

      if(rotate==false){
      
      if(parede==true){
        W=0;
        if((RC.SensorF<0.07)&&(measureonce==true)){
          turn=true;    
        }
        if(turn==true){
          TurnRight(); 
        }
      }
  
      if(parede==false){
        if(turn_right==true)
        if(((posicao-distance)>160)&&(measureonce==true)){
          TurnRight();
          parede=false;
        }
      }

      if((turn_left==true)&&(measureonce==true)){
        if(RC.SensorF<0.07){
          turn=true;
        }
        if(turn==true){
          TurnLeft();
          
        }
      }
      }

      
      if((RC.SensorF<0.07)&&(RC.SensorF>0)){
        if((turn_right==false)&&(turn_left==false)){
          rotate=true;
          turn_right=true;
        }
      }
      if(rotate==true){
          Rotate();
        }

      

      SpeedControlFoward();             
      //strPacket =  "Vr\r\n" + String(RC.Vr)+"\r\n\r\n";
      //strPacket =   strPacket + "Vl\r\n" + String(RC.Vl)+"\r\n\r\n";
      //Serial.print(strPacket);     
      
    }
    else
    {
      ResetData();
    }       
  }  
}

float DecodeJsonDouble( String s, String search)
{
  int startIndex=0;
  String value;
  String auxiliar;

  startIndex = s.lastIndexOf(search);
  if(startIndex != -1 )
  { 
    auxiliar = search.length();
    startIndex = startIndex + auxiliar.toInt();
    
    while ((s[startIndex] != ',') and (s[startIndex] != '}')) 
    {
      value = value + s[startIndex];
      startIndex = startIndex+1;
    }
  }
  else
  {
    value = '0';
  }
  return value.toFloat();  
}

void ResetData()
{
  theta = 0;
  turn=false;
  rotate=false;
  parede=false;
  eixo = 0;
  V=20;
  W=0;
  turn_right=false;
  turn_left=false;
  VAWR=0;
  VAWL=0;
  eixoY=90;
  RC.Vr=0;
  RC.Vl=0;
  distance=0;
  measureonce = false;
  kappa=150;  

  flagComunica = 0;
}

void serialEvent() {
  while (Serial.available()) {
    // get the new byte:
    char inChar = (char)Serial.read();
    // add it to the inputString:
    inputString += inChar;
    // if the incoming character is a newline, set a flag so the main loop can
    // do something about it:
    if (inChar == '\n') {
      stringComplete = true;
    }
  }
}
