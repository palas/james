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

#include "httpanalysis.hpp"

bool areEqual(const char *constant, const char *variable)
{
  int length = strlen(constant);
  return !strncmp(constant, variable, length);
}

void httpAnalysisPatch(int type, int depth, jvmtiEnv *jvmti_env, JNIEnv* jni_env,
    char *method_name, char *simplified_method_signature, char *class_signature,
    bool is_static, bool is_native, bool is_synthetic, jthread thread,
    jmethodID method, jvalue return_value, junit_sec js, vector<string> *msg)
{
  int methodType = 0; // 0 - no method, 1 - GET, 2 - POST
  if (areEqual("doGet", method_name)) methodType = 1;
  if (areEqual("doGetL", method_name)) methodType = 1;
  if (areEqual("makeRequest", method_name) && (strlen(simplified_method_signature) == 2)) methodType = 1;
  if (areEqual("doPost", method_name)) methodType = 2;
  if (areEqual("makeRequest", method_name) && (strlen(simplified_method_signature) == 3)) methodType = 2;
  if (methodType) {
    const char *utf = "";
    jboolean isCopy;
    jint result;
    jvalue val_val;
    jstring string_val = NULL;
    if (methodType == 1) msg->push_back("GET");
    if (methodType == 2) msg->push_back("POST");
    result = jvmti_env->GetLocalObject(thread, 0, 0, &val_val.l);
    if (result != JNI_OK) {
      char *errmsg;
      jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
      fprintf(stderr, "Could not URL on HTTP request! %s\n", errmsg);
      exit(JNI_ERR);
    }
    jclass clazz = jni_env->GetObjectClass(val_val.l);
    jmethodID toString = jni_env->GetMethodID(clazz, "toString", "()Ljava/lang/String;");
    string_val = (jstring)jni_env->CallObjectMethod(val_val.l, toString);

    utf = jni_env->GetStringUTFChars(string_val, &isCopy);
    msg->push_back(utf);
    jni_env->ReleaseStringUTFChars(string_val, utf);
  } else if (areEqual("startServer", method_name)) {
    msg->push_back("POST");
    msg->push_back("/StartServer");
  } else if (areEqual("stopServer", method_name)) {
    msg->push_back("POST");
    msg->push_back("/StopServer");
  } else if (areEqual("allocateFrequency", method_name)) {
    msg->push_back("POST");
    msg->push_back("/AllocateFrequency");
  } else if (areEqual("deallocateFrequency", method_name)) {
    msg->push_back("POST");
    msg->push_back("/DeallocateFrequency");
  } else {
    msg->push_back("");
    msg->push_back("");
  }
}

