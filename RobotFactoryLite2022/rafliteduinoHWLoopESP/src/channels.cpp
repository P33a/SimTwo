/* Copyright (c) 2016  Paulo Costa
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

#include "channels.h"
#include "Arduino.h"

static byte isHexNibble(char c)
{
  if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')) return 1;
  else return 0;
}

static byte HexNibbleToByte(char c)
{
  if (c >= '0' && c <= '9') return c - '0';
  else if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  else return 0;
}

void channels_t::sendByte(byte b)
{
  (*serial_write)(b);
}


void channels_t::sendHexNibble(byte b)
{
  if (b < 10) {
    (*serial_write)('0' + b);
  } else if (b < 16) {
    (*serial_write)('A' + (b - 10));
  }
}

void channels_t::sendHexByte(byte b)
{
  sendHexNibble(b >> 4);
  sendHexNibble(b & 0x0F);
}

void channels_t::send(char c, uint32_t v)
{
  sendByte(c);
  sendHexByte(v >> 24);
  sendHexByte((v >> 16) & 0xFF);
  sendHexByte((v >> 8)  & 0xFF);
  sendHexByte(v & 0xFF);
}

void channels_t::send(char c, byte b3, byte b2, byte b1, byte b0)
{
  sendByte(c);
  sendHexByte(b3);
  sendHexByte(b2);
  sendHexByte(b1);
  sendHexByte(b0);
}


void channels_t::send(char c, uint16_t high_word, uint16_t low_word)
{
  sendByte(c);
  sendHexByte((high_word >> 8)  & 0xFF);
  sendHexByte(high_word & 0xFF);
  sendHexByte((low_word >> 8)  & 0xFF);
  sendHexByte(low_word & 0xFF);
}


void channels_t::sendFloat(char c, float v)
{
  //send(c, *((int32_t*) &v));
  channels_u fu = {.f = v};
  send(c, fu.u);
}

channels_t::channels_t()
{
  process_frame = NULL;
  serial_write = NULL;
}

void channels_t::init(void (*process_frame_function)(char, uint32_t, channels_t&),
                      void (*serial_write_function)(uint8_t))
{
  process_frame = process_frame_function;
  serial_write = serial_write_function;
  frameState = -1;
}


void channels_t::StateMachine(byte b)
{
  if (frameState == -1) {             // If we are waiting for a command
    if ((b >= 'G' && b <= 'Z') ||
        (b >= 'g' && b <= 'z')) {     // And it is a valid command  [G..Z] or [g..z]
      frameState = 0;                 // Init frame (it will take values from 0 to 7)
      curChannel = b;                 // Store requested channel
      for(byte i = 0; i < 8; i++) {   // Clean Frame Buffer
        frameHexData[i] = 0;
      }
    }
  } else {                               // We are already reading the frame
    if (isHexNibble(b)) {                // It the byte is valid (an Hex char)
      frameHexData[frameState] = b;  // Store it the the frame buffer (Big Endian)
      frameState++;                      // Expect next nibble
    } else {
      if ((curChannel >= 'g' && curChannel <= 'z') &&
          (b == 0x0D || b == 0x0A || b == '+')) {
        for(byte i = 0; i < frameState; i++) {   // Shift Frame Buffer
          frameHexData[7 - i] = frameHexData[(frameState - 1) - i];
          frameHexData[(frameState - 1) - i] = 0;
        }
        frameState = 8;                 // low case command can be short circuited by a CL or a LF or a plus sign
      } else {
        frameState = -1;                // The byte was invalid: trash the frame and start waiting for a new one
      }
    }

    if (frameState == 8) {              // We have the 8 hex nibbles
      // Build the 32 bit value from the 8 nibbles
      uint32_t value = 0;
      for(byte i = 0; i < 8; i++) {
        value = (value << 4) + HexNibbleToByte(frameHexData[i]);
      }

      if (process_frame) {                    // If the callback processing function was defined
        (*process_frame)(curChannel, value, *this);  // Process the channel + data
      }
      frameState = -1;  // The frame was processed: trash it and start waiting for a new one
    }
  }
}
