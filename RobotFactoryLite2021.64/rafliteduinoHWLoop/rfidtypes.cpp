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

#include "rfidtypes.h"
#include "Arduino.h"



const PROGMEM  uint8_t tag_colors[]  = { 
  0x26, 0x22, 0x73, 0x12, TAG_BLUE,
  0x50, 0xF5, 0x4A, 0x2B, TAG_RED,
  0x22, 0x22, 0x22, 0x22, TAG_BLUE,
  0x22, 0x22, 0x22, 0x22, TAG_BLUE,
  0x22, 0x22, 0x22, 0x22, TAG_BLUE,
  0x00, 0x00, 0x00, 0x00, TAG_LAST
};


uint8_t tag_color(byte *buffer)
{
  uint8_t i, found;
  int addr = 0;
  uint8_t color = 0;
  while(1) {
    color = pgm_read_byte_near(tag_colors + addr + 4);
    if (color == TAG_LAST) return TAG_UNKNOWN;
    
    if ((buffer[0] == pgm_read_byte_near(tag_colors + addr + 0)) &&
        (buffer[1] == pgm_read_byte_near(tag_colors + addr + 1)) &&
        (buffer[2] == pgm_read_byte_near(tag_colors + addr + 2)) &&
        (buffer[3] == pgm_read_byte_near(tag_colors + addr + 3))) return color;
    
    addr = addr + 5;    
  }
  return TAG_UNKNOWN;
}
