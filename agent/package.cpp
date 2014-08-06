/**
* Copyright (c) 2014, Pablo Lamela Seijas
*
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*
* 1. Redistributions of source code must retain the above copyright notice,
* this list of conditions and the following disclaimer.
*
* 2. Redistributions in binary form must reproduce the above copyright notice,
* this list of conditions and the following disclaimer in the documentation
* and/or other materials provided with the distribution.
*
* 3. Neither the name of the copyright holder nor the names of its
* contributors may be used to endorse or promote products derived from this
* software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.
*/


#include "package.hpp"

void putSimpleString(tcp::socket *socket_inst, string the_string);

string getString(tcp::socket *socket_inst)
{
  boost::array<char, 1> phase1;
  stringstream result;
  long length = 0;
  size_t read = 0;
  phase1[0] = '0';
  do {
    length *= 10;
    length += phase1[0] - '0';
    socket_inst->read_some(boost::asio::buffer(phase1)); 
  } while (phase1[0] != ':');
  
  for (; length > 0; length -= read)
  {
    char *phase2 = (char *)malloc(length + 1);
    read = socket_inst->read_some(boost::asio::buffer((void *)phase2, length));
    phase2[read] = '\0';
    result << phase2;
    free(phase2);
  } 
  return result.str();
}

void putString(tcp::socket *socket_inst, string the_string)
{
  stringstream ss;
  ss << the_string.length() << ":" << the_string;
  putSimpleString(socket_inst, ss.str());
}

void putSimpleString(tcp::socket *socket_inst, string the_string)
{
  unsigned int length = the_string.length();
  const char *mem = the_string.c_str();;
  int pos = 0;
  size_t written = 0;
  for (; length > 0; length -= written, pos += written)
  {
    written = socket_inst->send(boost::asio::buffer(mem + pos, length));
  }
}

