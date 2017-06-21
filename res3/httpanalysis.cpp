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

threadStringMap threadHTTPMethodMap;
threadStringMap threadURLMap;

bool areEqual(const char *constant, const char *variable)
{
  int length = strlen(constant);
  return !strncmp(constant, variable, length);
}

void remove_http_info(jthread thread)
{
  threadHTTPMethodMap.erase(thread);
  threadURLMap.erase(thread);
}

void get_info_if_http_method(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                             char *method_name, char *method_signature, char *class_signature)
{
  if ((areEqual("openConnection", method_name)) and
      (areEqual("()Ljava/net/URLConnection;", method_signature)) and
      (areEqual("Ljava/net/URL;", class_signature)))
  {
    // Open connection
    string URL;
    jboolean isCopy;
    jint result;
    jvalue value_val;
    const char *utf = NULL;
    jstring string_val = NULL;

    // Obtaining URL
    result = jvmti_env->GetLocalInstance(thread, 0, &(value_val.l));
    if (result != JNI_OK) {
      char *errmsg;
      jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
      fprintf(stderr, "Could not read URL from HTTP request! %s\n", errmsg);
      exit(JNI_ERR);
    }
    jclass clazz = jni_env->GetObjectClass(value_val.l);
    jmethodID toString = jni_env->GetMethodID(clazz, "getPath", "()Ljava/lang/String;");
    string_val = (jstring)jni_env->CallObjectMethod(value_val.l, toString);

    utf = jni_env->GetStringUTFChars(string_val, &isCopy);
    URL = utf;

    // Storing URL
    if (areEqual("/vodkatv", utf) || areEqual("vodkatv", utf)) {
      threadURLMap[thread] = URL;
      threadHTTPMethodMap[thread] = "GET";
    jni_env->ReleaseStringUTFChars(string_val, utf);
    } else {
    jni_env->ReleaseStringUTFChars(string_val, utf);
    }
  } else if ((areEqual("setRequestMethod", method_name)) and
      (areEqual("(Ljava/lang/String;)V", method_signature)) and
      (areEqual("Ljava/net/HttpURLConnection;", class_signature)))
  {
    // Set request method
    string methodStr;
    jboolean isCopy;
    jint result;
    jvalue value_val;
    const char *utf = NULL;
    jstring string_val = NULL;

    // Obtaining Method
    result = jvmti_env->GetLocalObject(thread, 0, 1, &value_val.l);
    if (result != JNI_OK) {
      char *errmsg;
      jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
      fprintf(stderr, "Could not read method from HTTP request! %s\n", errmsg);
      exit(JNI_ERR);
    }
    jclass clazz = jni_env->GetObjectClass(value_val.l);
    jmethodID toString = jni_env->GetMethodID(clazz, "toString", "()Ljava/lang/String;");
    string_val = (jstring)jni_env->CallObjectMethod(value_val.l, toString);

    utf = jni_env->GetStringUTFChars(string_val, &isCopy);
    methodStr = utf;
    jni_env->ReleaseStringUTFChars(string_val, utf);

    // Storing Method
    boost::to_upper(methodStr);
    threadHTTPMethodMap[thread] = methodStr;
  }
}

void httpAnalysisPatch(int type, int depth, jvmtiEnv *jvmti_env, JNIEnv* jni_env,
    char *method_name, char *simplified_method_signature, char *class_signature,
    bool is_static, bool is_native, bool is_synthetic, jthread thread,
    jmethodID method, jvalue return_value, junit_sec js, vector<string> *msg)
{
  if (threadURLMap.count(thread)) {
    msg->push_back(threadHTTPMethodMap[thread]);
    msg->push_back(threadURLMap[thread]);
  } else {
    msg->push_back("");
    msg->push_back("");
  }
  remove_http_info(thread);
}

