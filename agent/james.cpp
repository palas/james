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

#include "james.hpp"

JNIEXPORT jint JNICALL Agent_OnLoad(JavaVM *vm, char *options, void*)
{
  jvmtiEnv *jvmti = NULL;
  jint result;
  jvmtiCapabilities capabilities;
  jvmtiEventCallbacks ecall;
  result = vm->GetEnv((void **)&jvmti, JVMTI_VERSION_1);
  if (result != JNI_OK) {
    char *errmsg;
    jvmti->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not get JVMTI environment! %s\n", errmsg);
    return JNI_ERR;
  }
  memset(&capabilities, 0, sizeof(capabilities));
  capabilities.can_generate_method_entry_events = 1;
  capabilities.can_generate_method_exit_events = 1;
  capabilities.can_access_local_variables = 1;
  capabilities.can_tag_objects = 1;
  capabilities.can_generate_object_free_events = 1;
  result = jvmti->AddCapabilities(&capabilities);
  if (result != JNI_OK) {
    char *errmsg;
    jvmti->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not get capabilities! %s\n", errmsg);
    return JNI_ERR;
  }
  memset(&ecall, 0, sizeof(ecall));
  ecall.MethodEntry = &MethodEntry;
  ecall.MethodExit = &MethodExit;
  ecall.ObjectFree = &ObjectFree;
  result = jvmti->SetEventCallbacks(&ecall, (jint)sizeof(ecall));
  if (result != JNI_OK) {
    char *errmsg;
    jvmti->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not set up the callbacks! %s\n", errmsg);
    return JNI_ERR;
  }
  result = jvmti->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_METHOD_ENTRY, NULL);
  if (result != JNI_OK) {
    char *errmsg;
    jvmti->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not turn on the \"method_entry\" notifications! %s\n", errmsg);
    return JNI_ERR;
  }
  result = jvmti->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_METHOD_EXIT, NULL);
  if (result != JNI_OK) {
    char *errmsg;
    jvmti->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not turn on the \"method_exit\" notifications! %s\n", errmsg);
    return JNI_ERR;
  }
  result = jvmti->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_OBJECT_FREE, NULL);
  if (result != JNI_OK) {
    char *errmsg;
    jvmti->GetErrorName((jvmtiError)result, &errmsg);
    fprintf(stderr, "Could not turn on the \"object_free\" notifications! %s\n", errmsg);
    return JNI_ERR;
  }
  createMonitor(jvmti); 
  createMonitorTrap(jvmti); 
  createMonitorRefs(jvmti); 
  connectSocket(jvmti, atoi(options));
  #ifdef DEBUG
    printf("\n*********\n\nAgent loaded!!!\n\n******************\n\n");
  #endif
  return JNI_OK;
}

JNIEXPORT void JNICALL Agent_OnUnload(JavaVM *vm)
{
  //  destroyMonitor(jvmti_env);  
  //  destroyMonitorTrap(jvmti); 
  //  destroyMonitorRefs(jvmti); 
  //  destroySocket(jvmti_env); 
  #ifdef DEBUG
    printf("\n******************\n\nProgram finished!!!\n\n*********\n\n");
  #endif
}

void JNICALL MethodEntry(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method)
{
  char *method_name, *method_signature, *class_signature;
  jclass class_instance;
  jint method_modifiers;
  jvalue fake_ret;
  jboolean is_native;
  jboolean is_synthetic;
  junit_sec js;
  jvmti_env->GetMethodName(method, &method_name, &method_signature, NULL);
  jvmti_env->GetMethodDeclaringClass(method, &class_instance);
  jvmti_env->GetClassSignature(class_instance, &class_signature, NULL);
  if (!trapInstrumentedCallEntry(jvmti_env, jni_env, thread, method,
	method_name, method_signature, class_signature))
  {
    if ((strncmp("Ljava", class_signature, 5)) && (strncmp("Lorg/apache/tools/ant", class_signature, 19))
        && (strncmp("Lorg/apache/maven", class_signature, 17)) && (strncmp("Lsun", class_signature, 4)) && (strncmp("Lorg/codehaus/plexus", class_signature, 20))
        && (strncmp("Lorg/sonatype", class_signature, 13)) && (strncmp("Lcom/google", class_signature, 11)) && (strncmp("Lcom/sun", class_signature, 8) && (strncmp("Lorg/jacoco", class_signature, 11))))
//    if (strncmp("Ljava", class_signature, 5))
    {
      jvmti_env->IsMethodNative(method, &is_native);
      jvmti_env->IsMethodSynthetic(method, &is_synthetic);
//      if ((!is_native) && (!is_synthetic)) {
      js = cacheIsMethodAnnotated(jvmti_env, jni_env, method, class_instance, class_signature);
	if (js.is_annotated)
	{
	  trapThisThread(jvmti_env, thread, js);
	  traceMethod(0, 0, jvmti_env, jni_env, method_name, method_signature, class_signature,
	      isMethodStatic(jvmti_env, jni_env, method), is_native, is_synthetic, thread,
	      method, fake_ret, js);
	} else if (!isNotAccessible(jvmti_env, class_signature)) {
	  traceMethod(0, -1, jvmti_env, jni_env, method_name, method_signature, class_signature,
	      isMethodStatic(jvmti_env, jni_env, method), is_native, is_synthetic, thread,
	      method, fake_ret, js);
	}
//      }
    }
  }
  jvmti_env->Deallocate((unsigned char *)method_name);
  jvmti_env->Deallocate((unsigned char *)method_signature);
  jvmti_env->Deallocate((unsigned char *)class_signature);
}


void JNICALL MethodExit(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method,
    jboolean was_popped_by_exception, jvalue return_value)
{
  char *method_name, *method_signature, *class_signature;
  jclass class_instance;
  jint static_modifier;
  jboolean is_native;
  jboolean is_synthetic;
  junit_sec js;
  jvmti_env->GetMethodName(method, &method_name, &method_signature, NULL);
  jvmti_env->GetMethodDeclaringClass(method, &class_instance);
  jvmti_env->GetClassSignature(class_instance, &class_signature, NULL);
  if (!trapInstrumentedCallExit(jvmti_env, jni_env, thread, method, was_popped_by_exception, return_value,
	method_name, method_signature, class_signature))
  {
    if ((strncmp("Ljava", class_signature, 5)) && (strncmp("Lorg/apache/tools/ant", class_signature, 19))
        && (strncmp("Lorg/apache/maven", class_signature, 17)) && (strncmp("Lsun", class_signature, 4)) && (strncmp("Lorg/codehaus/plexus", class_signature, 20))
        && (strncmp("Lorg/sonatype", class_signature, 13)) && (strncmp("Lcom/google", class_signature, 11)) && (strncmp("Lcom/sun", class_signature, 8) && (strncmp("Lorg/jacoco", class_signature, 11))))
//    if (strncmp("Ljava", class_signature, 5))
    {
      jvmti_env->IsMethodNative(method, &is_native);
      jvmti_env->IsMethodSynthetic(method, &is_synthetic);
//      if ((!is_native) && (!is_synthetic)) {
      js = cacheIsMethodAnnotated(jvmti_env, jni_env, method, class_instance, class_signature);
	if (js.is_annotated)
	{
	  traceMethod(1, 0, jvmti_env, jni_env, method_name, method_signature, class_signature,
	      isMethodStatic(jvmti_env, jni_env, method), is_native, is_synthetic, thread,
	      method, return_value, js);
	} else if (!isNotAccessible(jvmti_env, class_signature)) {
	  traceMethod(1, -1, jvmti_env, jni_env, method_name, method_signature, class_signature,
	      isMethodStatic(jvmti_env, jni_env, method), is_native, is_synthetic, thread,
	      method, return_value, js);
	}
//      }
    }
  }
  jvmti_env->Deallocate((unsigned char *)method_name);
  jvmti_env->Deallocate((unsigned char *)method_signature);
  jvmti_env->Deallocate((unsigned char *)class_signature);
}

void JNICALL ObjectFree(jvmtiEnv *jvmti_env, jlong tag)
{
  vector<string> msg;
  stringstream ssmsg;
  msg.push_back("START_FREE_EVENT");
  msg.push_back("LONG_VALUE");
  ssmsg << tag;
  msg.push_back(ssmsg.str());
  ssmsg.str(std::string());
  msg.push_back("END_FREE_EVENT");
  sendStrings(jvmti_env, msg);
  if (sockCounter(jvmti_env, msg.size()))
  {
    recvStrings(jvmti_env, 1);
  }
}


junit_sec cacheIsMethodAnnotated(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method,
    jclass class_instance, char *class_signature)
{
  if (isNotAccessible(jvmti_env, class_signature)) {
    junit_sec js;
    js.is_test = false;
    js.is_before = false;
    js.is_after = false;
    js.is_annotated = false;
    return js;
  } else {
    return isMethodAnnotated(jvmti_env, jni_env, method, class_instance, class_signature);
  }
}

