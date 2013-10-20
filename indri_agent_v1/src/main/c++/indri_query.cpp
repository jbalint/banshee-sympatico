#include "indri/QueryEnvironment.hpp"
#include "indri/Repository.hpp"
#include "indri/SnippetBuilder.hpp"

#include <jni.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "bs_indri_v1_Query.h"

using namespace indri::api;

/**************************
 * Linked list of queries *
 **************************/

typedef struct query {
  QueryEnvironment *qe;
  QueryAnnotation *results;
  struct query *next;
  int id;
  int position;
} query;

static query *queries;

static query *query_new()
{
  // infinitely incrementing query id
  static int next_query_id = 1;

  query *q = (query *) malloc(sizeof(query));
  query *e;

  memset(q, 0, sizeof(query));
  q->id = next_query_id++;
  q->qe = new QueryEnvironment;

  if (queries == NULL)
	queries = q;
  else
  {
	for (e = queries; e->next != NULL; e = e->next);
	e->next = q;
  }
  return q;
}

static query *query_get(int id)
{
  query *e;
  for (e = queries; e != NULL; e = e->next)
  {
	if (e->id == id)
	  return e;
  }
  return NULL;
}

static void query_remove(int id)
{
  query *e;
  query *prev = NULL;
  for (e = queries; e != NULL; e = e->next)
  {
	if (e->id != id)
	{
	  prev = e;
	  continue;
	}
	if (prev)
	  prev->next = e->next;
	if (e == queries)
	  queries = e->next;
	delete e->qe;
	free(e);
  }
}

/*********************
 * JNI helper stuffs *
 *********************/

// note, this is a C++-ified version of my normal macro
#define EXCEPTION_FAIL(JNI)												\
  do {																	\
	if (((JNI))->ExceptionCheck()) {									\
	  /*if(IsDebuggerPresent())											\
		DebugBreak();*/													\
	  fprintf(stderr, "EXCEPTION EXISTS at %s:%d\n", __FILE__, __LINE__); \
	  ((JNI))->ExceptionDescribe();										\
	  ((JNI))->ExceptionClear();										\
	  assert(0);														\
	}																	\
  } while(0)

jclass hashMap_class = NULL;
jclass map_class = NULL;
jmethodID hashMap_ctor = NULL;
jmethodID mapPut_mid = NULL;

jclass arrayList_class = NULL;
jclass list_class = NULL;
jmethodID arrayList_ctor = NULL;
jmethodID listAdd_mid = NULL;

jclass double_class = NULL;
jmethodID double_ctor = NULL;

static void init_jni_helpers(JNIEnv *jni)
{
  if (hashMap_class)
	return;

  hashMap_class = (jclass) jni->NewGlobalRef(jni->FindClass("java/util/HashMap"));
  EXCEPTION_FAIL(jni);
  assert(hashMap_class);
  map_class = (jclass) jni->NewGlobalRef(jni->FindClass("java/util/Map"));
  EXCEPTION_FAIL(jni);
  assert(map_class);
  hashMap_ctor = jni->GetMethodID(hashMap_class, "<init>", "()V");
  EXCEPTION_FAIL(jni);
  assert(hashMap_ctor);
  mapPut_mid = jni->GetMethodID(map_class, "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  EXCEPTION_FAIL(jni);
  assert(mapPut_mid);

  arrayList_class = (jclass) jni->NewGlobalRef(jni->FindClass("java/util/ArrayList"));
  EXCEPTION_FAIL(jni);
  assert(arrayList_class);
  list_class = (jclass) jni->NewGlobalRef(jni->FindClass("java/util/List"));
  EXCEPTION_FAIL(jni);
  assert(list_class);
  arrayList_ctor = jni->GetMethodID(arrayList_class, "<init>", "()V");
  EXCEPTION_FAIL(jni);
  assert(arrayList_ctor);
  listAdd_mid = jni->GetMethodID(list_class, "add", "(Ljava/lang/Object;)Z");
  EXCEPTION_FAIL(jni);
  assert(listAdd_mid);

  double_class = (jclass) jni->NewGlobalRef(jni->FindClass("java/lang/Double"));
  EXCEPTION_FAIL(jni);
  assert(double_class);
  double_ctor = jni->GetMethodID(double_class, "<init>", "(D)V");
  EXCEPTION_FAIL(jni);
  assert(double_ctor);
}

static jobject new_HashMap(JNIEnv *jni)
{
  jobject map;
  init_jni_helpers(jni);
  map = jni->NewObject(hashMap_class, hashMap_ctor);
  EXCEPTION_FAIL(jni);
  return map;
}

static void add_string_to_map(JNIEnv *jni, jobject map, const char *key, const char *value)
{
  jstring k, v;
  k = jni->NewStringUTF(key);
  EXCEPTION_FAIL(jni);
  v = jni->NewStringUTF(value);
  EXCEPTION_FAIL(jni);
  jni->CallObjectMethod(map, mapPut_mid, k, v);
  EXCEPTION_FAIL(jni);
}

static void add_double_to_map(JNIEnv *jni, jobject map, const char *key, double value)
{
  jstring k;
  jobject v;
  k = jni->NewStringUTF(key);
  EXCEPTION_FAIL(jni);
  v = jni->NewObject(double_class, double_ctor, value);
  EXCEPTION_FAIL(jni);
  jni->CallObjectMethod(map, mapPut_mid, k, v);
  EXCEPTION_FAIL(jni);
}

static jobject new_ArrayList(JNIEnv *jni)
{
  jobject list;
  init_jni_helpers(jni);
  list = jni->NewObject(arrayList_class, arrayList_ctor);
  EXCEPTION_FAIL(jni);
  return list;
}

static void add_object_to_list(JNIEnv *jni, jobject list, jobject object)
{
  jni->CallObjectMethod(list, listAdd_mid, object);
  EXCEPTION_FAIL(jni);
}

/*********************
 * Impl of Java APIs *
 *********************/

JNIEXPORT jint JNICALL
Java_bs_indri_v1_Query_query_1begin(JNIEnv *jni, jobject obj, jobjectArray indexPaths, jstring jQueryString)
{
  query *q = query_new();
  const char *query_string = jni->GetStringUTFChars(jQueryString, NULL);
  jsize index_path_size;
  jstring indexPath;
  const char *index_path;

  (void) obj;
  assert(indexPaths);
  assert(query_string);

  index_path_size = jni->GetArrayLength(indexPaths);

  for (int i = 0; i < index_path_size; ++i)
  {
	// if this is taking a stack slot, watch java stack size, DeleteLocalRef
	indexPath = (jstring) jni->GetObjectArrayElement(indexPaths, i);
	assert(indexPath);
	index_path = jni->GetStringUTFChars(indexPath, NULL);
	assert(index_path);
	//printf("Adding path '%s'\n", index_path);
	q->qe->addIndex(index_path);
	jni->ReleaseStringUTFChars(indexPath, index_path);
  }

  q->results = q->qe->runAnnotatedQuery(query_string, 1000 /* TODO arbitrary limit */);
  assert(q->results);

  jni->ReleaseStringUTFChars(jQueryString, query_string);
  query_string = NULL;
  return q->id;
}

JNIEXPORT jobject JNICALL
Java_bs_indri_v1_Query_query_1next_1results(JNIEnv *jni, jobject obj, jint queryId, jint requestedResultCount, jobjectArray fields)
{
  query *q = query_get(queryId);
  std::vector<ScoredExtentResult> resultVec = q->results->getResults();
  jobject result_list = new_ArrayList(jni);
  jsize fieldCount = jni->GetArrayLength(fields);

  (void) obj;

  if (requestedResultCount > (resultVec.size() - q->position))
	requestedResultCount = (resultVec.size() - q->position);

  std::vector<ScoredExtentResult> rv2(&resultVec[q->position],
									  &resultVec[q->position + requestedResultCount]);
  resultVec = rv2;

  q->position += requestedResultCount;

  std::vector<ParsedDocument*> parsedDocs = q->qe->documents(resultVec);

  std::vector<int> documentIDs;
  documentIDs.reserve(requestedResultCount);

  for( size_t i=0; i<requestedResultCount; i++ ) {
    documentIDs.push_back( resultVec[i].document );
  }


  std::vector<DocumentVector*> documentVecs = q->qe->documentVectors(documentIDs);

  for (int i = 0; i < requestedResultCount; ++i)
  {
	jobject row_map = new_HashMap(jni);
	DocumentVector *dv = documentVecs[i];
	ParsedDocument *pdoc = parsedDocs[i];

	add_object_to_list(jni, result_list, row_map);

	// TODO compute this outside the `i' loop and have it ready
	for (int j = 0; j < fieldCount; ++j)
	{
	  jstring jfield = (jstring) jni->GetObjectArrayElement(fields, j);
	  const char *field_name = jni->GetStringUTFChars(jfield, NULL);

	  // first, handle "special" fields
	  if (!strcmp(field_name, "relevance"))
	  {
		add_double_to_map(jni, row_map, "relevance", resultVec[i].score);
	  }
	  else if (!strcmp(field_name, "snippet"))
	  {
		SnippetBuilder builder(true);
		add_string_to_map(jni, row_map, "snippet",
						  builder.build(resultVec[i].document, pdoc, q->results).c_str());
	  }
	  else if (!strcmp(field_name, "content"))
	  {
		add_string_to_map(jni, row_map, "content", pdoc->content);
	  }
	  else
	  {
		// can do this differently if efficiency gain is great enough
		// NOTE this returns stems of terms only
		std::vector<DocumentVector::Field> doc_fields = dv->fields();
		std::vector<int> doc_positions = dv->positions();
		std::vector<std::string> doc_stems = dv->stems();
		for (int k = 0; k < doc_fields.size(); ++k)
		{
		  DocumentVector::Field f = doc_fields[k];
		  if (!strcmp(f.name.c_str(), field_name))
		  {
			char *val = (char *) malloc(100); // TODO completely arbitrary
			sprintf(val, doc_stems[doc_positions[f.begin]].c_str());
			for (int l = f.begin + 1; l < f.end; ++l)
			{
			  if (!strcmp(field_name, "date"))
				strcat(val, "-");
			  else
				strcat(val, " ");
			  strcat(val, doc_stems[doc_positions[l]].c_str());
			}
			add_string_to_map(jni, row_map, field_name, val);
			free(val);
			break;
		  }
		}
	  }

	  jni->ReleaseStringUTFChars(jfield, field_name);
	}
  }

  return result_list;
}

JNIEXPORT void JNICALL
Java_bs_indri_v1_Query_query_1close(JNIEnv *jni, jobject obj, jint queryId)
{
  (void) obj;
  query_remove(queryId);
}
