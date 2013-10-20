#include <jni.h>

#include "indri/QueryEnvironment.hpp"
#include "indri/Repository.hpp"

#include "bs_indri_v1_Query.h"

JNIEXPORT jint JNICALL
Java_bs_indri_v1_Query_query_1begin(JNIEnv *jni, jobject obj, jobjectArray indexPaths, jstring query)
{
  (void)obj;
  return 0;
}

JNIEXPORT jobject JNICALL
Java_bs_indri_v1_Query_query_1next_1resultsII()
{
}
