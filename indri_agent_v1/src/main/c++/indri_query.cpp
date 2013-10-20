#include "indri/QueryEnvironment.hpp"
#include "indri/Repository.hpp"
#include "indri/SnippetBuilder.hpp"

#include <jni.h>

#include <stdio.h>
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

  (void) obj;

  std::vector<ParsedDocument*> parsedDocs = q->qe->documents(resultVec);
  // TODO handle positioning
  for (int i = 0; i < resultCount && i < requestedResultCount; ++i)
  {
	int docId = resultVec[i].document;
	ParsedDocument *d = parsedDocs[i];
	printf("Doc: text: '%s'\n", d->text);
	printf("Doc: content: '%s'\n", d->content);
	printf("%s\n", builder.build(docId, d, q->results).c_str());
	// TODO fields retrieval is *not* fast?
  }

  return NULL;
}

JNIEXPORT void JNICALL
Java_bs_indri_v1_Query_query_1close(JNIEnv *jni, jobject obj, jint queryId)
{
  (void) obj;
  query_remove(queryId);
}
