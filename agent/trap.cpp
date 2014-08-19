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


#include "trap.hpp"

using namespace std;

typedef map<jthread,int> threadIntMap;

threadIntMap threadDepthMap;

jrawMonitorID monitorTrap;

junit_sec global_js;

void createMonitorTrap(jvmtiEnv *jvmti_env)
{
  jvmti_env->CreateRawMonitor("monitorTrap", &monitorTrap);
}

void trapThisThread(jvmtiEnv *jvmti_env, jthread thread, junit_sec js)
{
  jvmti_env->RawMonitorEnter(monitorTrap);
  threadDepthMap[thread] = 1;
  global_js = js;
  jvmti_env->RawMonitorExit(monitorTrap);
}

bool trapInstrumentedCallEntry(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method,
    char *method_name, char *method_signature, char *class_signature)
{
  bool result = false;
  jvalue fake_ret;
  jclass class_instance;
  jvmti_env->RawMonitorEnter(monitorTrap);
  if (threadDepthMap.count(thread) != 0)
  {
    jboolean is_native;
    jboolean is_synthetic;
    jvmti_env->IsMethodNative(method, &is_native);
    jvmti_env->IsMethodSynthetic(method, &is_synthetic);
    if (threadDepthMap[thread] < 2) {
      traceMethod(0, threadDepthMap[thread], jvmti_env, jni_env, method_name, method_signature, class_signature,
      isMethodStatic(jvmti_env, jni_env, method), is_native, is_synthetic, thread, method, fake_ret, global_js);
    }
    threadDepthMap[thread]++;
    result = true;
  }
  jvmti_env->RawMonitorExit(monitorTrap);
  return result;
}

bool trapInstrumentedCallExit(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method,
    jboolean was_popped_by_exception, jvalue return_value,
    char *method_name, char *method_signature, char *class_signature)
{
  bool result = false;
  jclass class_instance;
  jvmti_env->RawMonitorEnter(monitorTrap);
  if (threadDepthMap.count(thread) != 0)
  {
    jboolean is_native;
    jboolean is_synthetic;
    jvmti_env->IsMethodNative(method, &is_native);
    jvmti_env->IsMethodSynthetic(method, &is_synthetic);
    threadDepthMap[thread]--;
    if (threadDepthMap[thread] < 2) {
      traceMethod(1, threadDepthMap[thread], jvmti_env, jni_env, method_name, method_signature, class_signature,
                  isMethodStatic(jvmti_env, jni_env, method), is_native, is_synthetic, thread, method, return_value, global_js);
    }
    if (threadDepthMap[thread] >= 2) {
      get_info_if_http_method(jvmti_env, jni_env, thread, method_name, method_signature, class_signature);
    }
    if (threadDepthMap[thread] == 0)
    {
      threadDepthMap.erase(thread);
    }
    result = true;
  }
  jvmti_env->RawMonitorExit(monitorTrap);
  return result;
}

void destroyMonitorTrap(jvmtiEnv *jvmti_env)
{
  jvmti_env->DestroyRawMonitor(monitorTrap);
}

// void showLocalVariables(jvmtiEnv *jvmti_env, jmethodID method)
// {
//   int i;
//   int result;
//   jint entry_count_ptr;
//   jint max_ptr;
//   jvmtiLocalVariableEntry* table_ptr;
//   result = jvmti_env->GetLocalVariableTable(method, &entry_count_ptr, &table_ptr);
//   if (result != JNI_OK) {
//     char *errmsg;
//     jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
//     if (!strcmp(errmsg, "JVMTI_ERROR_ABSENT_INFORMATION")) {
//       /* Module was not compiled with debug info */
//       result = jvmti_env->GetMaxLocals(method, &max_ptr);
//       if (result != JNI_OK) {
//         char *errmsg;
//         jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
//         fprintf(stderr, "Could not obtain the number of parameters! %s\n", errmsg);
//         return;
//       } else {
//         printf("Num params: %d\n", max_ptr);
//       }
//     } else {
//       fprintf(stderr, "Problem getting local variable table! %s\n", errmsg);
//       return;
//     }
//   } else {
//     for (i = 0; i < entry_count_ptr; i++)
//     {
//       printf("- %s\n", table_ptr[i].name);
//     }
//     jvmti_env->Deallocate((unsigned char *)table_ptr);
//   }
// }

jstring objToString(JNIEnv *jni_env, jobject obj)
{
  jclass clazz = jni_env->GetObjectClass(obj);
  jmethodID toString = jni_env->GetMethodID(clazz, "toString", "()Ljava/lang/String;");
  return (jstring)jni_env->CallObjectMethod(obj, toString);
}

int showLocalVariablesAux(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jthread thread, char symbol_char, int slot,
    jvalue custom_value, vector<string> *msg)
{
  jint int_val;
  jlong long_val;
  jfloat float_val;
  jdouble double_val;
  jobject object_val;
  jvalue val_val;
  stringstream ssmsg;
  jint result;
  int cfornum;
  int inc = 0;
  switch (symbol_char) {
    case 'J':
      if (slot >= 0) {
	result = jvmti_env->GetLocalLong(thread, 0, slot, &long_val);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	long_val = custom_value.j;
      }
      msg->push_back("LONG_VALUE");
      ssmsg << long_val;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      inc++;
      break;
    case 'F':
      if (slot >= 0) {
	result = jvmti_env->GetLocalFloat(thread, 0, slot, &float_val);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	float_val = custom_value.f;
      }
      msg->push_back("FLOAT_VALUE");
      ssmsg << boost::format("%10.30e") % float_val;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      break;
    case 'D':
      if (slot >= 0) {
	result = jvmti_env->GetLocalDouble(thread, 0, slot, &double_val);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	double_val = custom_value.d;
      }
      msg->push_back("DOUBLE_VALUE");
      ssmsg << boost::format("%10.30e") % float_val;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      inc++;
      break;
    case '[':
    case 'L':
      if (slot >= 0) {
	result = jvmti_env->GetLocalObject(thread, 0, slot, &val_val.l);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	val_val.l = custom_value.l;
      }
      if (val_val.l == NULL) {
	msg->push_back("OBJECT_NULL");
      } else {
	val_val.l = jni_env->NewLocalRef(val_val.l);
	jclass cls = jni_env->FindClass("java/lang/Class");
	if (jni_env->ExceptionOccurred()) {
	  jni_env->ExceptionClear();
	  cls = NULL;
	}
	jclass cls2 = jni_env->FindClass("java/lang/String");
	if (jni_env->ExceptionOccurred()) {
	  jni_env->ExceptionClear();
	  cls2 = NULL;
	}
	jboolean isCopy;
	jstring string_val = NULL;
	const char *utf = "";
	if ((cls2 != NULL) && (jni_env->IsInstanceOf(val_val.l, cls2) == JNI_TRUE)) {
	  msg->push_back("STRING_VALUE");
	  string_val = (jstring)val_val.l;
	} else if ((cls != NULL) && (jni_env->IsInstanceOf(val_val.l, cls)) == JNI_TRUE) {
	  msg->push_back("CLASS_VALUE");
	  jmethodID method = jni_env->GetMethodID(cls, "getCanonicalName", "()Ljava/lang/String;");
	  jobject obj = jni_env->CallObjectMethod(val_val.l, method);
	  string_val = (jstring)obj;
	} else {
	  if (hasRef(jvmti_env, val_val.l)) {
	    msg->push_back("OBJECT_ID_TOSTR");
	    ssmsg << getRef(jvmti_env, val_val.l);
	    msg->push_back(ssmsg.str());
	    ssmsg.str(std::string());
	  } else {
	    // Exception for StringBuilders and String Buffers (when created)
	    jclass cls3 = jni_env->FindClass("java/lang/StringBuilder");
	    if (jni_env->ExceptionOccurred()) {
	      jni_env->ExceptionClear();
	      cls3 = NULL;
	    }
	    jclass cls4 = jni_env->FindClass("java/lang/StringBuffer");
	    if (jni_env->ExceptionOccurred()) {
	      jni_env->ExceptionClear();
	      cls4 = NULL;
	    }
	    if ((cls3 != NULL) && (jni_env->IsInstanceOf(val_val.l, cls3) == JNI_TRUE)) {
	      msg->push_back("STRING_BUILDER_NEWID_VALUE");
	      string_val = objToString(jni_env, val_val.l);
	    } else if ((cls4 != NULL) && (jni_env->IsInstanceOf(val_val.l, cls4) == JNI_TRUE)) {
	      msg->push_back("STRING_BUFFER_NEWID_VALUE");
	      string_val = objToString(jni_env, val_val.l);
	    } else { // End of exception
	      msg->push_back("OBJECT_NEWID_TOSTR");
	    }
	    ssmsg << setNewRef(jvmti_env, val_val.l);
	    msg->push_back(ssmsg.str());
	    ssmsg.str(std::string());
	  }
	}
	if (string_val != NULL) {
	  utf = jni_env->GetStringUTFChars(string_val, &isCopy);
	  msg->push_back(utf);
	  jni_env->ReleaseStringUTFChars(string_val, utf);
	} else {
	  msg->push_back(utf);
	}

	jni_env->DeleteLocalRef(val_val.l);
      }
      break;
    case 'Z':
      if (slot >= 0) {
	result = jvmti_env->GetLocalInt(thread, 0, slot, &val_val.i);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	val_val.z = custom_value.z;
      }
      msg->push_back("BOOLEAN_VALUE");
      ssmsg << (val_val.z ? "TRUE" : "FALSE");
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      break;
    case 'B':
      if (slot >= 0) {
	result = jvmti_env->GetLocalInt(thread, 0, slot, &val_val.i);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
	    fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	val_val.b = custom_value.b;
      }
      msg->push_back("BYTE_VALUE");
      cfornum = val_val.b;
      ssmsg << cfornum;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      break;
    case 'C':
      if (slot >= 0) {
	result = jvmti_env->GetLocalInt(thread, 0, slot, &val_val.i);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	val_val.c = custom_value.c;
      }
      msg->push_back("CHAR_VALUE");
      cfornum = val_val.c;
      ssmsg << cfornum;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      break;
    case 'S':
      if (slot >= 0) {
	result = jvmti_env->GetLocalInt(thread, 0, slot, &val_val.i);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	val_val.s = custom_value.s;
      }
      msg->push_back("SHORT_VALUE");
      ssmsg << val_val.s;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      break;
    case 'I':
      if (slot >= 0) {
	result = jvmti_env->GetLocalInt(thread, 0, slot, &int_val);
	if (result != JNI_OK) {
	  char *errmsg;
	  jvmti_env->GetErrorName((jvmtiError)result, &errmsg);
          #ifdef DEBUG
            fprintf(stderr, "Could not get local object! %s\n", errmsg);
          #endif
	  return -1; // exit(JNI_ERR);
	}
      } else {
	int_val = custom_value.i;
      }
      msg->push_back("INTEGER_VALUE");
      ssmsg << int_val;
      msg->push_back(ssmsg.str());
      ssmsg.str(std::string());
      break;
  }
  return inc;
}

bool showLocalVariables(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method, jthread thread,
    char *simp_sig, jvalue return_value, int callbackType, bool isStatic,
    bool isNew, vector<string> *msg)
{
  jobject object_val;
  jvmtiError error;
  int size = strlen(simp_sig) - 1;
  int slot = 0;
  int i;
  for (i = 0; i < size; i++, slot++)
  {
    int temp;
    msg->push_back("PARAM");
    temp = showLocalVariablesAux(jvmti_env, jni_env, thread, simp_sig[i],
	slot + (isStatic?0:1), return_value, msg);
    if ((-1) == temp) {
      #ifdef DEBUG
        fprintf(stderr, "a (%d) %s, %s - %s\n", slot, (isStatic?"static":"dynamic"), (isNew?"new":"not_new"), simp_sig);
      #endif
      return false;
    };
    slot += temp;
  }
  i = strlen(simp_sig) - 1;
  if (callbackType == 1)
  {
    if ((isNew)/* && (simp_sig[i] != 'V')*/) {
      msg->push_back("RETURN_VALUE");
      jvalue value_val;
      jvmti_env->GetLocalInstance(thread, 0, &(value_val.l));
      if ((-1) == showLocalVariablesAux(jvmti_env, jni_env, thread, 'L', -1, value_val, msg)) {
        #ifdef DEBUG
          fprintf(stderr, "b %d - %s\n", (isStatic?0:1), simp_sig);
        #endif
	return false;
      };
    } else if (simp_sig[i] == 'V') {
      msg->push_back("RETURN_VOID");
    } else {
      msg->push_back("RETURN_VALUE");
      if ((-1) == showLocalVariablesAux(jvmti_env, jni_env, thread, simp_sig[i], -1, return_value, msg)) {
        #ifdef DEBUG
          fprintf(stderr, "c %d - %s\n", (isStatic?0:1), simp_sig);
        #endif
	return false;
      };
    }
  }
  if (!isStatic) {
    if (!isNew) {
      msg->push_back("THIS_VALUE");
      jvalue value_val;
      jvmti_env->GetLocalInstance(thread, 0, &(value_val.l));
      if ((-1) == showLocalVariablesAux(jvmti_env, jni_env, thread, 'L', -1, value_val, msg)) {
        #ifdef DEBUG
          fprintf(stderr, "d %d - %s\n", (isStatic?0:1), simp_sig);
        #endif
	return false;
      };
    } else {
      msg->push_back("THIS_NEW");
    }
  } else {
    msg->push_back("STATIC");
  }
  return true;
}

jboolean isMethodStaticAux(JNIEnv *jni_env, jint modifier)
{
  jclass modif;
  jmethodID isStatic;
  modif = jni_env->FindClass("java/lang/reflect/Modifier");
  isStatic = jni_env->GetStaticMethodID(modif, "isStatic", "(I)Z");
  return jni_env->CallStaticBooleanMethod(modif, isStatic, modifier);
}

jboolean isMethodStatic(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method)
{
  jint method_modifiers;
  jvmti_env->GetMethodModifiers(method, &method_modifiers);
  return isMethodStaticAux(jni_env, method_modifiers);
}

void traceMethod(int type, int depth, jvmtiEnv *jvmti_env, JNIEnv* jni_env,
    char *method_name, char *method_signature, char *class_signature,
    bool is_static, bool is_native, bool is_synthetic, jthread thread,
    jmethodID method, jvalue return_value, junit_sec js)
{
  #ifdef DEBUG
    for (int i = 0; i < depth; i++) printf("  ");
  #endif
  if ((depth >= 0) && (depth <= 1)) {
    switch (type) {
      case 0:
        #ifdef DEBUG
          printf(">>> Entering method %s %s from class %s.\n", method_name, method_signature, class_signature);
        #endif
	break;
      case 1:
        #ifdef DEBUG
          printf("<<< Exiting method %s %s from class %s.\n", method_name, method_signature, class_signature);
        #endif
        break;
    }
  }
  fflush(stdout);
  vector<string> msg;
  stringstream ss;
  string mns = string(method_name);
  string mss = string(method_signature);
  string css = string(class_signature);
  msg.push_back("START_CALLBACK");
  switch (type) {
    case 0: msg.push_back("ENTER_METHOD");
	    break;
    case 1: msg.push_back("EXIT_METHOD");
	    break;
  }
  ss << depth;
  msg.push_back(ss.str());
  if (is_static) msg.push_back("static");
  else msg.push_back("dynamic");
  msg.push_back(method_name);
  msg.push_back(class_signature);
  msg.push_back((js.is_before)?"TRUE":"FALSE");
  msg.push_back((js.is_test)?"TRUE":"FALSE");
  msg.push_back((js.is_after)?"TRUE":"FALSE");
  char *simp_sig = simplify_signature(method_signature);
  httpAnalysisPatch(type, depth, jvmti_env, jni_env, method_name, simp_sig,
      class_signature, is_static, is_native, is_synthetic, thread, method,
      return_value, js, &msg);
  msg.push_back("START_VARS");
  if (!showLocalVariables(jvmti_env, jni_env, method, thread, simp_sig,
           return_value, type, is_static, !strcmp("<init>", method_name), &msg)) {
    #ifdef DEBUG
      fprintf(stderr, "IS_NATIVE: %s\n", (is_native?"TRUE":"FALSE"));
      fprintf(stderr, "IS_SYNTHETIC: %s\n", (is_synthetic?"TRUE":"FALSE"));
      fprintf(stderr, "EVENT_TYPE: %s\n", (type?"EXIT":"ENTRY"));
      fprintf(stderr, "CLASS_SIGNATURE: %s\n", class_signature);
      fprintf(stderr, "METHOD_NAME: %s\n", method_name);
      fprintf(stderr, "SIG: %s\n", method_signature);
    #endif
    free(simp_sig);
    return;
  }
  free(simp_sig);
  msg.push_back("END_VARS");
  msg.push_back("END_CALLBACK");
  sendStrings(jvmti_env, msg);
  if (sockCounter(jvmti_env, msg.size()))
  {
    recvStrings(jvmti_env, 1);
    jvmti_env->ForceGarbageCollection();
  }
}

char *simplify_signature(char *class_signature)
{
  int i;
  stringstream st;
  for (i = 0; i < strlen(class_signature); i++)
  {
    if ((class_signature[i] != '(') && (class_signature[i] != ')')) {
      st << class_signature[i];
      while (class_signature[i] == '[') i++;
      if (class_signature[i] == 'L') {
	while (class_signature[i] != ';') i++;
      }
    }
  }
  string tmp = st.str();
  const char *cstr = tmp.c_str();
  char *ptr = (char *)malloc(strlen(cstr) + 1);
  return strcpy(ptr, cstr);
}

jobject getReflectedClass(JNIEnv* jni_env, const char *name)
{
  return jni_env->FindClass(name);
}

bool hasAnnotation(JNIEnv *jni_env, jobject reflected_method, jobject reflected_class)
{
  jmethodID getAnnotation;
  jobject result;
  jclass methodClass;
  if ((reflected_method == NULL) || (reflected_class == NULL)) return false;
  methodClass = jni_env->FindClass("java/lang/reflect/Method");
  if (methodClass == NULL) return false;
  getAnnotation = jni_env->GetMethodID(methodClass, "getAnnotation",
      "(Ljava/lang/Class;)Ljava/lang/annotation/Annotation;");
  result = jni_env->CallObjectMethod(reflected_method, getAnnotation, reflected_class);
  return (result != NULL);
}


junit_sec isMethodAnnotated(jvmtiEnv *jvmti_env, JNIEnv *jni_env, jmethodID method,
    jclass class_instance, char *class_signature)
{
  bool result;
  jint method_modifiers;
  junit_sec js;
  jboolean isStatic;
  jobject reflected_method;
  jobject reflected_class;
  isStatic = isMethodStatic(jvmti_env, jni_env, method);
  reflected_method = jni_env->ToReflectedMethod(class_instance, method, isStatic);
  reflected_class = getReflectedClass(jni_env, "org/junit/Test");
  if (reflected_class == NULL) setNotAccessible(jvmti_env, class_signature);
  js.is_test = hasAnnotation(jni_env, reflected_method, reflected_class);
  js.is_annotated = js.is_test;
  jni_env->ExceptionClear();
  reflected_class = getReflectedClass(jni_env, "org/junit/After");
  js.is_after = hasAnnotation(jni_env, reflected_method, reflected_class);
  js.is_annotated |= js.is_after;
  jni_env->ExceptionClear();
  reflected_class = getReflectedClass(jni_env, "org/junit/Before");
  js.is_before = hasAnnotation(jni_env, reflected_method, reflected_class);
  js.is_annotated |= js.is_before;
  jni_env->ExceptionClear();
  return js;
}

