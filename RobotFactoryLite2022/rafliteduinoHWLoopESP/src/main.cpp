/* Copyright (c) 2021  Paulo Costa
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.
   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE. */

#include <Arduino.h>
#include "channels.h"
#include "IRLine.h"
#include "robot.h"
#include "PID.h"
#include "proj_types.h"
#include "SPI.h"
#include "ArduinoNvs.h"

#include <WiFi.h>
#include <WiFiUdp.h>
#include <ArduinoOTA.h>

byte UsingSimulator;
byte go;

//char ssid[32];
//char password[32];
String ssid, password;

int ssid_idx, pass_idx;
int ssid_done, pass_done;

//IPAddress SendIP(192, 168, 1, 172);
int udp_on, ip_on;

WiFiUDP Udp;
unsigned int localUdpPort = 4224;  // local port to listen on

#define UDP_MAX_SIZE 512
uint8_t UdpInPacket[UDP_MAX_SIZE];  // buffer for incoming packets
uint8_t UdpOutPacket[UDP_MAX_SIZE];  // buffer for outgoing packets
int UdpBufferSize = UDP_MAX_SIZE;


#include <Wire.h>
#include <Adafruit_MotorShield.h>

// Create the motor shield object with the default I2C address
static Adafruit_MotorShield AFMS = Adafruit_MotorShield(); 

Adafruit_DCMotor* M1;
Adafruit_DCMotor* M2;
Adafruit_DCMotor* M3;

void sim_loop(void);
void real_loop(void);

void process_serial_packet(char channel, uint32_t value, channels_t& obj);


channels_t serial_channels, udp_channels;

IRLine_t IRLine;

hw_timer_t * timer_enc = NULL;

#define ENC1_A 27
#define ENC1_B 14

#define ENC2_A 19
#define ENC2_B 23

robot_t robot;

void setSolenoidPWM(int new_PWM);

#define TOUCHSW_pin 26

void setSolenoidState()
{
  if (robot.solenoid_state) setSolenoidPWM(177);
  else setSolenoidPWM(0);
}

byte readTouchSwitch(void)
{
  return !digitalRead(TOUCHSW_pin);
}

void control(robot_t& robot);

volatile int encoder1_pos = 0;
volatile int encoder2_pos = 0;

int encoder1_state, encoder2_state;
int encoder_table[16] = {0, 1, -1, 0, -1, 0, 0, 1, 1, 0, 0, -1, 0, -1, 1, 0};

void IRAM_ATTR EncodersInterrupt(void)
{
  byte next_state, table_input;

  next_state = digitalRead(ENC1_A) << 1;
  next_state |= digitalRead(ENC1_B);

  table_input = (encoder1_state << 2) | next_state;
  encoder1_pos += encoder_table[table_input];
  encoder1_state = next_state;


  next_state = digitalRead(ENC2_A) << 1;
  next_state |= digitalRead(ENC2_B);

  table_input = (encoder2_state << 2) | next_state;
  encoder2_pos -= encoder_table[table_input];
  encoder2_state = next_state;
}

//uint32_t delta, current, previous, interval = 40000UL;
schedule_t schedule;

void setMotorPWM(Adafruit_DCMotor* M, int new_PWM, int enable = 1)
{
  int PWM_max = 255;
  if (new_PWM >  PWM_max) new_PWM =  PWM_max;
  if (new_PWM < -PWM_max) new_PWM = -PWM_max;
  //PWM_act = new_PWM;

  if (enable) {
    if (new_PWM >= 0) {
      M->setSpeed(new_PWM);
      M->run(BACKWARD); 
    } else {
      M->setSpeed(abs(new_PWM));
      M->run(FORWARD);
    }
  } else {
    M->run(RELEASE);
  }
}

void setMotorsPWM(int PWM1, int PWM2)
{
  setMotorPWM(M1, PWM1);
  setMotorPWM(M2, PWM2);
}


void setSolenoidPWM(int new_PWM)
{
  int PWM_max = 177;
  if (new_PWM >  PWM_max) new_PWM =  PWM_max;
  if (new_PWM < -PWM_max) new_PWM = -PWM_max;

  setMotorPWM(M3, new_PWM);
}


void serial_write(uint8_t b)
{
  Serial.write(b);
}


#define TX_UDP_BUF_SIZE 256
uint8_t TX_buffer[TX_UDP_BUF_SIZE];
int TX_count;

void serial_send_buffer(void)
{
  if (TX_count != 0) Serial.write(TX_buffer, TX_count);
}

void udp_send_buffer(void)
{
  //Serial.write(TX_buffer, TX_count);
  if (TX_count != 0 && udp_on) {
    Udp.beginPacket(Udp.remoteIP(), Udp.remotePort());
    Udp.write(TX_buffer, TX_count);
    //Serial.print("Sent="); Serial.println(Udp.endPacket());
    Udp.endPacket();  
  } 
  TX_count = 0;
  Serial.write('.');
}


void udp_write(uint8_t b)
{
  //Serial.write(b);

  if (TX_count < TX_UDP_BUF_SIZE) {
    TX_buffer[TX_count] = b;
    TX_count++;
  }
  if (TX_count == TX_UDP_BUF_SIZE) {
    udp_send_buffer();
  }
}



// The i2_check uses the return value of
// the Write.endTransmisstion to see if
// a device did acknowledge to the address.
int i2_check(int address)
{
  Wire.beginTransmission(address);
  return Wire.endTransmission();
}


void i2_scan(void)
{
  byte error, address;
  int nDevices;
 
  Serial.println("Scanning...");
 
  nDevices = 0;
  for(address = 1; address < 127; address++ )
  {
    error = i2_check(address);
 
    if (error == 0) {
      Serial.print("I2C device found at address 0x");
      if (address<16)
        Serial.print("0");
      Serial.print(address,HEX);
      Serial.println("  !");
 
      nDevices++;
    } else if (error == 4) {
      Serial.print("Unknown error at address 0x");
      if (address<16)
        Serial.print("0");
      Serial.println(address,HEX);
    }    
  }
  if (nDevices == 0)
    Serial.println("No I2C devices found\n");
  else
    Serial.println("done\n");
 
  delay(100);           // wait 0.1 seconds for next scan
}

void wifi_init(void)
{
  if (!(ssid_done && pass_done)) return;
  WiFi.mode(WIFI_STA);
  WiFi.begin(ssid.c_str(), password.c_str());
  Serial.print("Connecting to WiFi ..");
  //while (WiFi.status() != WL_CONNECTED) {
  //  Serial.print('.');
  //  delay(1000);
  //}
  Serial.println(WiFi.localIP());
}


void WiFiStationConnected(WiFiEvent_t event, WiFiEventInfo_t info)
{
  Serial.println("Connected to AP successfully!");
}

void WiFiGotIP(WiFiEvent_t event, WiFiEventInfo_t info)
{
  Serial.println("WiFi connected");
  Serial.println("IP address: ");
  Serial.println(WiFi.localIP());

  ip_on = Udp.begin(localUdpPort);
  Serial.printf("Now listening at IP %s, UDP port %d\n", WiFi.localIP().toString().c_str(), localUdpPort);

  ArduinoOTA.begin(); 
}

void WiFiStationDisconnected(WiFiEvent_t event, WiFiEventInfo_t info)
{
  Serial.println("Disconnected from WiFi access point");
  Serial.print("WiFi lost connection. Reason: ");
  Serial.println(info.disconnected.reason);
  udp_on = 0;
  ip_on = 0;
  //Serial.println("Trying to Reconnect");
  //WiFi.begin(ssid, password);
}

void setup()
{
  // Set the pins as input or output as needed
  pinMode(ENC1_A, INPUT_PULLUP);
  pinMode(ENC1_B, INPUT_PULLUP);
  pinMode(ENC2_A, INPUT_PULLUP);
  pinMode(ENC2_B, INPUT_PULLUP);

  pinMode(TOUCHSW_pin, INPUT_PULLUP);

  UsingSimulator = 0;

  schedule.interval = 40000UL;
  robot.dt = 1e-6 * schedule.interval; // in seconds

  // Reserve SSID and password buffers
  //ssid.reserve(32);
  //password.reserve(32);
  ssid_idx = 0;
  pass_idx = 0;
  ssid_done = 0;
  pass_done = 0;

  NVS.begin();

  //NVS.setString("SSID", "xxx");
  //NVS.setString("PASS", "xxx");
  
  ssid = NVS.getString("SSID");
  if (ssid.length() > 0) ssid_done = 1;

  password = NVS.getString("PASS");
  if (password.length() > 0) pass_done = 1;

  wifi_init();

  Serial.begin(115200);
  serial_channels.init(process_serial_packet, serial_write);
  Serial.println("Init Begin!");
  
  if (!UsingSimulator) {
    Wire.begin();
    //Wire.setClock(400000UL);
    //while(1) i2_scan();
    while(i2_check(0x60)) {
      Serial.println("Motor Drive not Found!");  
      delay(100);
    }
    Serial.println("Motor Drive Found!");  
  
    // Configure motor Drive
    AFMS.begin();  // create with the default frequency 1.6KHz
  
    M1 = AFMS.getMotor(2);
    M2 = AFMS.getMotor(1);
    
    //M1->setSpeed(100);
    //M1->run(BACKWARD); 
    //M1->run(FORWARD); 
    M2->setSpeed(50);
    M2->run(BACKWARD); 

    M3 = AFMS.getMotor(3);
  }

  // AD configuration
  analogReadResolution(10);
  analogSetAttenuation(ADC_0db);

  // Timer interrupt for the encoder
  timer_enc = timerBegin(0, 80, true);
  timerAttachInterrupt(timer_enc, &EncodersInterrupt, true);
  timerAlarmWrite(timer_enc, 50, true);
  timerAlarmEnable(timer_enc);

  udp_channels.init(process_serial_packet, udp_write);

  WiFi.onEvent(WiFiStationConnected, SYSTEM_EVENT_STA_CONNECTED);
  WiFi.onEvent(WiFiGotIP, SYSTEM_EVENT_STA_GOT_IP);
  WiFi.onEvent(WiFiStationDisconnected, SYSTEM_EVENT_STA_DISCONNECTED);

  //if (WiFi.status() == WL_CONNECTED) {
  //  Udp.begin(localUdpPort);
  //}
  //Serial.printf("Now listening at IP %s, UDP port %d\n", WiFi.localIP().toString().c_str(), localUdpPort);

  
  // Port defaults to 3232
  // ArduinoOTA.setPort(3232);

  // Hostname defaults to esp3232-[MAC]
  // ArduinoOTA.setHostname("myesp32");

  // No authentication by default
  // ArduinoOTA.setPassword("admin");

  // Password can be set with it's md5 value as well
  // MD5(admin) = 21232f297a57a5a743894a0e4a801fc3
  // ArduinoOTA.setPasswordHash("21232f297a57a5a743894a0e4a801fc3");

  ArduinoOTA
    .onStart([]() {
      String type;
      if (ArduinoOTA.getCommand() == U_FLASH)
        type = "sketch";
      else // U_SPIFFS
        type = "filesystem";

      // NOTE: if updating SPIFFS this would be the place to unmount SPIFFS using SPIFFS.end()
      Serial.println("Start updating " + type);
    })
    .onEnd([]() {
      Serial.println("\nEnd");
      //ESP.restart();
    })
    .onProgress([](unsigned int progress, unsigned int total) {
      Serial.printf("Progress: %u%%\r", (progress / (total / 100)));
    })
    .onError([](ota_error_t error) {
      Serial.printf("Error[%u]: ", error);
      if (error == OTA_AUTH_ERROR) Serial.println("Auth Failed");
      else if (error == OTA_BEGIN_ERROR) Serial.println("Begin Failed");
      else if (error == OTA_CONNECT_ERROR) Serial.println("Connect Failed");
      else if (error == OTA_RECEIVE_ERROR) Serial.println("Receive Failed");
      else if (error == OTA_END_ERROR) Serial.println("End Failed");
    });

  //ArduinoOTA.begin();  

  // Robot Parameters
  robot.b = 0.137 / 2;
  robot.r1 = 0.065 / 2;
  robot.r2 = 0.065 / 2;

  robot.dv_max = 5 * robot.dt;  // Linear velocity change per control period
  robot.dw_max = 10 * robot.dt; // Angular velocity change per control period

  robot.state = 0;
  robot.solenoid_state = 1;
}


void readEncoders(void)
{
  cli();  // Must be done with the interrupts disabled
  robot.enc1 = encoder1_pos;
  robot.enc2 = encoder2_pos;

  encoder1_pos = 0;
  encoder2_pos = 0;
  sei();

  robot.Senc1 += robot.enc1;
  robot.Senc2 += robot.enc2;
}


int analog_pins[5] = {33, 32, 39, 36, 34};


void readIRSensors(void)
{
  byte c;  // Read the five IR sensors using the AD converter
  for (c = 0; c < 5; c++) {
    IRLine.IR_values[c] = 1023 - analogRead(analog_pins[4 - c]);
  }
}

uint32_t encodeIRSensors(void)
{
  byte c;  // Encode five IR sensors with 6 bits for each sensor
  uint32_t result = IRLine.IR_values[0] >> 4; // From 10 bits to 6 bits
  for (c = 1; c < 5; c++) {
    result = (result << 6) | (IRLine.IR_values[c] >> 4);
  }
  return result;
}


void process_serial_packet(char channel, uint32_t value, channels_t& obj)
{
  channels_u val;
  val.u = value;

  if (channel == 'o') {  // Clear acumulated encoder
    robot.Senc1 = 0;
    robot.Senc2 = 0;
  
  } else if (channel == 'R')  { 
    robot.enc1 = int16_t((value >> 16) & 0xFFFF);
    robot.enc2 = int16_t((value >> 00) & 0xFFFF);

    robot.Senc1 += robot.enc1;
    robot.Senc2 += robot.enc2;

  } else if (channel == 'I')  {   // IR Sensors + Touch
    uint8_t c;
    for (c = 0; c < 5; c++) {
      IRLine.IR_values[c] = 16 * ((value >> (c * 6)) & 0x3F);
    } 
    robot.TouchSwitch = ((value >> 31) & 1);  
 
  } else if (channel == 'G')  {  // Control
    go = 1;

  } else if (channel == 'v') {  // Set robot linear speed reference
    robot.v_req = val.f;   

  } else if (channel == 'w') {  // Set robot angular speed reference
    robot.w_req = val.f;   

  } else if (channel == 'x') {  // Set robot x position
    robot.x = val.f;   

  } else if (channel == 'y') {  // Set robot y position
    robot.y = val.f;   

  } else if (channel == 't') {  // Set robot theta angle
    robot.theta = val.f;   

  } else if (channel == 's') {  // Set robot state
    robot.setState(value);

  } else if (channel == 'O') {  // Set Requested PWM
    robot.PWM_1_req = int16_t((value >> 16) & 0xFFFF);
    robot.PWM_2_req = int16_t((value >> 0) & 0xFFFF);

  } else if (channel == 'T') {  // Set general parameters T1 and T2
    robot.T1 = (value >> 16) & 0xFFFF;
    robot.T2 = (value >> 0) & 0xFFFF;

  } else if (channel == 'p') { // Set PID parameter
    robot.PID1.Kp = val.f;
    robot.PID2.Kp = val.f;

  } else if (channel == 'i') { // Set PID parameter
    if (fabs(val.f) > 1e-2 ) {
      robot.PID1.Se = robot.PID1.Se * robot.PID1.Ki / val.f;
      robot.PID2.Se = robot.PID2.Se * robot.PID2.Ki / val.f;
    }
    robot.PID1.Ki = val.f;
    robot.PID2.Ki = val.f;

  } else if (channel == 'm') { // Set PID parameter
    robot.PID1.Kd = val.f;
    robot.PID2.Kd = val.f;

  } else if (channel == 'n') { // Set PID parameter
    robot.PID1.Kf = val.f;
    robot.PID2.Kf = val.f;

  } else if (channel == 'z') { // Set Solenoid
    robot.solenoid_state = value;

  }


}

void loop(void)
{
  if (UsingSimulator) {
    sim_loop();
  } else {
    real_loop();
  }

  if (ip_on) {
    ArduinoOTA.handle();

    int packetSize = Udp.parsePacket();
    if (packetSize) {
      int i;
      udp_on = 1;
      // receive incoming UDP packets
      // and forward it to the serial port

      //Serial.printf("Received %d bytes from %s, port %d\n", packetSize, Udp.remoteIP().toString().c_str(), Udp.remotePort());
      int len = Udp.read(UdpInPacket, UdpBufferSize - 1);
      if (len > 0) {
        UdpInPacket[len] = 0;
      }
      //Serial.printf("UDP packet contents (as string): %s\n", UdpInPacket);

      for (i = 0; i < len; i++) {
        udp_channels.StateMachine(UdpInPacket[i]);
        //Serial.write(UdpInPacket[i]);
      }
    }       
  }
}


void serial_print_format(int value, byte space)
{
  byte b, c;
  b = Serial.print(value);
  for (c = 0; c < space - b; c++) {
     Serial.print(" ");
  }
}

void serial_print_format(float value, byte space)
{
  Serial.printf("%4g", value);
}

void send_udp_channels(void)
{
  udp_channels.send('I', encodeIRSensors() | (robot.solenoid_state << 30) | (robot.TouchSwitch << 31));
  udp_channels.send('U', robot.PWM_1, robot.PWM_2);
  udp_channels.send('R', robot.enc1, robot.enc2);
  udp_channels.send('S', robot.Senc1, robot.Senc2);
  udp_channels.send('T', robot.T1, robot.T2);
  
  udp_channels.sendFloat('v', robot.v1e);
  udp_channels.sendFloat('w', robot.we);
  
  udp_channels.sendFloat('x', robot.x);
  udp_channels.sendFloat('y', robot.y);
  udp_channels.sendFloat('t', robot.theta);

  udp_channels.sendFloat('p', robot.PID1.Kp);
  udp_channels.sendFloat('i', robot.PID1.Ki);
  udp_channels.sendFloat('m', robot.PID1.Kd);
  udp_channels.sendFloat('n', robot.PID1.Kf);

  udp_channels.send('P', robot.state, schedule.delta / 100);
  udp_send_buffer();
}


void real_loop(void)
{
  uint32_t t;
  byte b;
  if (Serial.available()) {
    b = Serial.read();
    if (b == '+') robot.solenoid_state = 1;
    if (b == '-') robot.solenoid_state = 0;
    //if (b == '(') {robot.v1_PWM = 50;}
    if (b == '(') {robot.v += 0.1; robot.w = 0;}
    if (b == '/') {robot.v = 0; robot.w = 1.5;}
    if (b == '=') {robot.v = 0; robot.w =-50;}
    //if (b == ')') {robot.v2_PWM = 50;}
    if (b == ')') {robot.v -= -0.1; robot.w = 0;}
    //if (b == '?') {robot.v1_PWM = 0; robot.v2_PWM = 0;} 
    if (b == '?') {robot.v = 0; robot.w = 0;}
    if (b == '\\') robot.state = 0;
    if (b == '*') robot.state = 1;
    serial_channels.StateMachine(b);
  }

  schedule.current = micros();
  schedule.delta = schedule.current - schedule.previous;

  if (schedule.delta >= schedule.interval) {
    schedule.previous = schedule.current;

    readEncoders();

    t = micros();
    readIRSensors();
    t = micros() - t;

    robot.LastTouchSwitch = robot.TouchSwitch;
    robot.TouchSwitch = readTouchSwitch();

    IRLine.calcIRLineEdgeLeft();
    IRLine.calcIRLineEdgeRight();
    IRLine.calcCrosses();

    robot.odometry();
    control(robot);

    setSolenoidState();

    //robot.accelerationLimit();
    robot.v = robot.v_req;
    robot.w = robot.w_req;

    // Auto Control mode selection:
    // States for 0 to 199 are for PID control
    // States for 200 to 255 are for direct PWM control
    robot.VWToPWM();

    setMotorsPWM(robot.PWM_1, robot.PWM_2);
    //setMotorsPWM(50, 150);

    IPAddress ip = WiFi.localIP();

    serial_channels.send('i', ip[0], ip[1], ip [2], ip[3]);
    
    if(udp_on) send_udp_channels();
    
    Serial.print(F(" "));
    Serial.print(ip.toString());

    Serial.print(F(" E1: "));
    serial_print_format(robot.enc1, 4);

    Serial.print(F(" E2: "));
    serial_print_format(robot.enc2, 4);

    byte c;
    for (c = 0; c < 5; c++) {
       Serial.print(" ");
       Serial.print(IRLine.IR_values[c]);
    }

    Serial.print(F(" T: "));
    serial_print_format(robot.TouchSwitch, 0);

    /*
    Serial.print(F(" V: "));
    serial_print_format(robot.v, 4);
    Serial.print(F(" W: "));
    serial_print_format(robot.w, 4);

    Serial.print(F(" Ve: "));
    serial_print_format(robot.ve, 4);
    Serial.print(F(" We: "));
    serial_print_format(robot.we, 4);    
    */

    Serial.println();
  }

}


void sim_loop(void)
{
  byte b;
  if (Serial.available()) {
    b = Serial.read();
    serial_channels.StateMachine(b);
  }

  if (go) {
    schedule.previous = schedule.current;
    schedule.current = micros();
    schedule.delta = schedule.current - schedule.previous;
    go = 0;

    IRLine.calcIRLineEdgeLeft();
    IRLine.calcIRLineEdgeRight();
    IRLine.calcCrosses();

    robot.odometry();
    control(robot);
    
    //robot.accelerationLimit();
    robot.v = robot.v_req;
    robot.w = robot.w_req;
    robot.VWToPWM();

    //Serial.print(WiFi.localIP().toString());    
    //Serial.print(F(" "));
 
    serial_channels.send('s',  robot.state);
    serial_channels.send('M',  round(robot.PWM_1));
    serial_channels.send('Q',  round(robot.PWM_2));
    serial_channels.send('L',  robot.solenoid_state);
    serial_channels.send('X',  IRLine.crosses);
    //serial_channels.send('Y',  IRLine.pos_left);
    //serial_channels.send('Z',  IRLine.pos_right);

    if(udp_on) send_udp_channels();

  }
  
  /*schedule.current = micros();
  schedule.delta = schedule.current - schedule.previous;

  if (schedule.delta >= schedule.interval) {
    schedule.previous = schedule.current;

    if(udp_on) send_udp_channels();
  }*/
}
