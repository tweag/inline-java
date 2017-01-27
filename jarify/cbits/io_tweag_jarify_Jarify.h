#ifndef _Included_io_tweag_jarify_Jarify
#define _Included_io_tweag_jarify_Jarify

/* C/C++ header file for Java class io_tweag_jarify_Jarify */

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Class:     io_tweag_jarify_Jarify
 * Method:    initializeHaskellRTS
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_io_tweag_jarify_Jarify_initializeHaskellRTS
  (JNIEnv *, jclass);

/*
 * Class:     io_tweag_jarify_Jarify
 * Method:    apply
 * Signature: ([B[Ljava/lang/Object;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL Java_io_tweag_jarify_Jarify_apply
  (JNIEnv *, jclass, jbyteArray, jobjectArray);

#ifdef __cplusplus
}
#endif
#endif
