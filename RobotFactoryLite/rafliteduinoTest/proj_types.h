#ifndef PROJ_TYPES_H
#define PROJ_TYPES_H

#include "Arduino.h"

typedef struct{
  byte state;
  float v, w;
  byte solenoid_state;  
} robot_t;



typedef struct{
    float dt, Ki, Kp, Kd, Kf;
    float w, w_ref;
    float e, last_e, Se;
    float m, scale;
    byte active;
} PID_t;

#endif // PROJ_TYPES_H
