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


#ifndef __TRAP_HPP__
#define __TRAP_HPP__

#include <map>
#include <cstring>
#include <cstdlib>
#include <jvmti.h>
#include <vector>
#include <string>
#include <sstream>
#include "sock.hpp"
#include "refs.hpp"
#include "cache.hpp"
#include "httpanalysis.hpp"
#include <boost/format.hpp>

using namespace std;

void createMonitorTrap(jvmtiEnv *jvmti_env);
void trapThisThread(jvmtiEnv *jvmti_env, jthread thread, junit_sec js);
bool trapInstrumentedCallEntry(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method,
                               char *method_name, char *method_signature, char *class_signature);
bool trapInstrumentedCallExit(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method,
                              jboolean was_popped_by_exception, jvalue return_value,
                              char *method_name, char *method_signature, char *class_signature);
void destroyMonitorTrap(jvmtiEnv *jmvti_env);
void showLocalVariables(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method, jthread thread, char *simp_sig,
                        jvalue return_value, int callbackType, bool isStatic, bool isNew, vector<string> msg);
jboolean isMethodStatic(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method);
void traceMethod(int type, int depth, jvmtiEnv *jvmti_env, JNIEnv* jni_env,
    char *method_name, char *method_signature, char *class_signature,
    bool is_static, bool is_native, bool is_synthetic, jthread thread,
    jmethodID method, jvalue return_value, junit_sec js);
char *simplify_signature(char *class_signature);

junit_sec isMethodAnnotated(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method,
    jclass class_instance, char *class_signature);

#endif

