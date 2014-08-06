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

#include "refs.hpp"

jrawMonitorID monitorRefs;
long nextRef = -243895605L;

void createMonitorRefs(jvmtiEnv *jvmti_env)
{
  jvmti_env->CreateRawMonitor("monitorRefs", &monitorRefs);
}

bool hasRef(jvmtiEnv *jvmti_env, jobject obj)
{
  return getRef(jvmti_env, obj);
}

long getRef(jvmtiEnv *jvmti_env, jobject obj)
{
  jlong tag_ptr;
  jint result = jvmti_env->GetTag(obj, &tag_ptr);
  if (result != JNI_OK) {
    char *errmsg;
    jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not get reference! %s\n", errmsg);
    exit(JNI_ERR);
  }
  return tag_ptr;
}

long setNewRef(jvmtiEnv *jvmti_env, jobject obj)
{
  long currRef;
  jvmti_env->RawMonitorEnter(monitorRefs);
  currRef = nextRef++;
  jvmti_env->RawMonitorExit(monitorRefs);
  jvmti_env->SetTag(obj, currRef);
  return currRef;
}

void destroyMonitorRefs(jvmtiEnv *jvmti_env)
{
  jvmti_env->DestroyRawMonitor(monitorRefs);
}


