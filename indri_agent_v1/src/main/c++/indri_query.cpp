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

static void init_jni_helpers(JNIEnv *jni)
{
  if (hashMap_class)
	return;

  hashMap_class = jni->FindClass("java/util/HashMap");
  EXCEPTION_FAIL(jni);
  assert(hashMap_class);
  map_class = jni->FindClass("java/util/Map");
  EXCEPTION_FAIL(jni);
  assert(map_class);
  hashMap_ctor = jni->GetMethodID(hashMap_class, "<init>", "()V");
  EXCEPTION_FAIL(jni);
  assert(hashMap_ctor);
  mapPut_mid = jni->GetMethodID(map_class, "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  EXCEPTION_FAIL(jni);
  assert(mapPut_mid);

  arrayList_class = jni->FindClass("java/util/ArrayList");
  EXCEPTION_FAIL(jni);
  assert(arrayList_class);
  list_class = jni->FindClass("java/util/List");
  EXCEPTION_FAIL(jni);
  assert(list_class);
  arrayList_ctor = jni->GetMethodID(arrayList_class, "<init>", "()V");
  EXCEPTION_FAIL(jni);
  assert(arrayList_ctor);
  listAdd_mid = jni->GetMethodID(list_class, "add", "(Ljava/lang/Object;)Z");
  EXCEPTION_FAIL(jni);
  assert(listAdd_mid);
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
  int resultCount = resultVec.size();
  SnippetBuilder builder(true);
  jobject result_list = new_ArrayList(jni);

  (void) obj;

  std::vector<ParsedDocument*> parsedDocs = q->qe->documents(resultVec);
  // TODO handle positioning
  for (int i = 0; i < resultCount && i < requestedResultCount; ++i)
  {
	jobject row_map = new_HashMap(jni);
	int docId = resultVec[i].document;
	ParsedDocument *d = parsedDocs[i];
	for (int j = 0; j < d->metadata.size(); ++j)
	{
	  printf("MD[%d] %s = %s\n", j, d->metadata[j].key, d->metadata[j].value);
	}

	std::vector<lemur::api::DOCID_T> documentIDs;
	documentIDs.push_back(docId);
	std::vector<indri::api::DocumentVector*> vectors = q->qe->documentVectors(documentIDs);
	DocumentVector *v = vectors[0];
	printf("STEMS\n");
	std::vector<std::string> stems = v->stems();
	for (int j = 0; j < stems.size(); ++j)
	{
	  printf("Stem[%d] = %s\n", j, stems[j].c_str());
	}
	printf("POSITIONS\n");
	std::vector<int> pos = v->positions();
	for (int j = 0; j < pos.size(); ++j)
	{
	  printf("pos[%d] = %d\n", j, pos[j]);
	}
	std::vector<DocumentVector::Field> fields2 = v->fields();
	printf("FIELDS 2\n");
	for (int j = 0; j < fields2.size(); ++j)
	{
	  printf("name = %s , begin = %d , end = %d\n", fields2[j].name.c_str(), fields2[j].begin, fields2[j].end);
	}

	printf("TERMS: %d\n", d->terms.size());
	for (int j = 0; j < d->terms.size(); ++j)
	{
	  printf("%s\n", d->terms[i]);
	}
	// TODO fields retrieval is *not* fast?
	add_string_to_map(jni, row_map, "snippet",
					  builder.build(docId, d, q->results).c_str());
	add_string_to_map(jni, row_map, "content", d->content);
	add_object_to_list(jni, result_list, row_map);
  }

  printf("Fields\n");
  std::vector<std::string> Fields = q->qe->fieldList();
  for (int i = 0; i < Fields.size(); ++i)
  {
	printf("%s\n", Fields[i].c_str());
  }
  return result_list;
}

JNIEXPORT void JNICALL
Java_bs_indri_v1_Query_query_1close(JNIEnv *jni, jobject obj, jint queryId)
{
  (void) obj;
  query_remove(queryId);
}
