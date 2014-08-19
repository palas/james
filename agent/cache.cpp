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

#include "cache.hpp"

using namespace std;

typedef set<string> stringSet;

stringSet accessibleSet;
jrawMonitorID monitorCache;

void createMonitor(jvmtiEnv *jvmti_env)
{
  jvmti_env->CreateRawMonitor("monitorCache", &monitorCache);
}

bool isNotAccessible(jvmtiEnv *jvmti_env, char *nullEndedStr)
{
  bool result;
  jvmti_env->RawMonitorEnter(monitorCache);
  string theString(nullEndedStr, strlen(nullEndedStr));
  result = (accessibleSet.count(nullEndedStr) > 0);
  jvmti_env->RawMonitorExit(monitorCache);
  return result;
}

void setNotAccessible(jvmtiEnv *jvmti_env, char *nullEndedStr)
{
  jvmti_env->RawMonitorEnter(monitorCache);
  string theString(nullEndedStr);
  accessibleSet.insert(theString);
  jvmti_env->RawMonitorExit(monitorCache);
}

void destroyMonitor(jvmtiEnv *jvmti_env)
{
  jvmti_env->DestroyRawMonitor(monitorCache);
}


