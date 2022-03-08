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

#ifndef ROBOT_H
#define ROBOT_H

#include <Arduino.h>
#include "IRLine.h"
#include "PID.h"

typedef enum { 
  cm_pwm,
  cm_pid
} control_mode_t;

class robot_t {
  public:
  int enc1, enc2;
  int Senc1, Senc2;
  float v1e, v2e;
  float ve, we;
  float ds, dtheta;
  float rel_s, rel_theta;
  float x, y, theta;
  
  byte state;
  uint32_t tis, tes;

  float dt;
  float v, w;
  float v_req, w_req;
  float dv_max, dw_max;
  byte solenoid_state;  
  byte TouchSwitch, LastTouchSwitch;

  float r1, r2, b;
  float pulses_to_meters;
  float wheel_dist;
  
  float v1ref, v2ref;
  int PWM_1, PWM_2;
  int PWM_1_req, PWM_2_req;
  control_mode_t control_mode;
  int T1, T2;

  PID_t PID1, PID2;
  
  robot_t();
  void setState(byte new_state);

  void odometry(void);
  void setRobotVW(float Vnom, float Wnom);

  void followLineRight(IRLine_t& IRLine, float Vnom, float K);
  void followLineLeft(IRLine_t& IRLine, float Vnom, float K);
  void followLine(IRLine_t& IRLine, float Vnom, float K);  

  void accelerationLimit(void);
  void VWToPWM(void);
};







#endif // ROBOT_H
