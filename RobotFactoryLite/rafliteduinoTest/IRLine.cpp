/* Copyright (c) 2019  Paulo Costa
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

#include "IRLine.h"
#include "Arduino.h"

IRLine_t::IRLine_t()
{
  IR_WaterLevel = 100;
  IR_tresh = 512;
  cross_tresh = 3;
}

void IRLine_t::calibrate(void)
{
  
}


void IRLine_t::calcIRLineEdgeLeft(void)
{
  byte c, found;
  int v, last_v;

  found = 0;
  IR_max = 0;
  pos_left = 2 * 16.0;
  total = 0;
  last_v = 0;
  for (c = 0; c < 5; c++) {
    v = IR_values[c] - IR_WaterLevel;
    if (v < 0) v = 0;
    if (v > IR_max) IR_max = v;
    total = total + v;

    if (!found && last_v < IR_tresh && v > IR_tresh) {
      pos_left = -8 + 16.0 * (c - 2) + 16.0 * (IR_tresh - last_v) / (v - last_v);
      found = 1;
    }
    last_v = v;
  }
}


void IRLine_t::calcIRLineEdgeRight(void)
{
  byte c, found;
  int v, last_v;

  found = 0;
  IR_max = 0;
  pos_right = -2 * 16.0;
  total = 0;
  last_v = 0;
  for (c = 0; c < 5; c++) {
    v = IR_values[4 - c] - IR_WaterLevel;
    if (v < 0) v = 0;
    if (v > IR_max) IR_max = v;
    total = total + v;

    if (!found && last_v < IR_tresh && v > IR_tresh) {
      pos_right = -(-8 + 16.0 * (c - 2) + 16.0 * (IR_tresh - last_v) / (v - last_v));
      found = 1;
    }
    last_v = v;
  }
  
}




void IRLine_t::calcCrosses(void)
{
  //float blacks = 0;
  blacks = 0;
  
  if (IR_max == 0) {
    cross_count = 0;
    return;
  }
  
  last_cross_count = cross_count;
  
  blacks = total / IR_max;
  if (blacks > 2.5) {
    cross_count++;  
    if (last_cross_count < cross_tresh && cross_count >= cross_tresh) {
      crosses++;  
    }
  } else {
    if (cross_count > 0) cross_count--;
  }

}

/*
void IRLine_t::calcIRLineCenter(void)
{
  byte c;
  int v;

  IR_pos = 0;
  IR_total = 0;
  for (c = 0; c < 5; c++) {
    v = IR_values[c] - IR_WaterLevel;
    if (v < 0) v = 0;
 
    IR_total = IR_total + v;
    IR_pos = IR_pos + v * (c - 2) * 16.0;
  }
  if (IR_total > 0) IR_pos = IR_pos / IR_total;
}
*/
