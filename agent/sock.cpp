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


#include "sock.hpp"
#include "package.hpp"

boost::asio::io_service io_service;
tcp::socket socket_inst(io_service);
jrawMonitorID monitorSocket;
int counter = 0;

void connectSocket(jvmtiEnv *jvmti_env, unsigned short port)
{
  address_v4 address = address_v4::loopback();
  tcp::endpoint endpoint = tcp::endpoint(address, port);
  socket_inst.connect(endpoint);
  jvmti_env->CreateRawMonitor("monitorSocket", &monitorSocket);
}

void sendStrings(jvmtiEnv *jvmti_env, vector<string> stringsToSend)
{
  vector<string>::iterator it;
  jvmti_env->RawMonitorEnter(monitorSocket);
  for (it = stringsToSend.begin(); it < stringsToSend.end(); it++)
    putString(&socket_inst, *it);
  jvmti_env->RawMonitorExit(monitorSocket);
}

vector<string> recvStrings(jvmtiEnv *jvmti_env, int number)
{
  vector<string> vec;
  jvmti_env->RawMonitorEnter(monitorSocket);
  for (int i = 0; i < number; i++)
  {
    vec.push_back(getString(&socket_inst));
  }
  jvmti_env->RawMonitorExit(monitorSocket);
  return vec;
}

bool sockCounter(jvmtiEnv *jvmti_env, int delta)
{
  bool count = false;
  vector<string> vec;
  jvmti_env->RawMonitorEnter(monitorSocket);
  if ((counter += delta) > 1000)
  {
    counter -= 1000;
    count = true;
  }
  jvmti_env->RawMonitorExit(monitorSocket);
  return count;
}

void destroySocket(jvmtiEnv *jvmti_env)
{
  jvmti_env->DestroyRawMonitor(monitorSocket);
}



