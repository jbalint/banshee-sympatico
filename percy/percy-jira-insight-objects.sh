#!/bin/bash
# Script to query the Jira Insight objects for Helm completion source

source $(dirname $0)/curl-params.sh

#set -x
QUERY=$(cat<<EOF
select ?key ?label ?typeName ?id
{
?x a jira:InsightObject ;
jira:insightObjectId ?id ;
jira:insightObjectLabel ?label ;
jira:insightObjectType [
  jira:insightObjectTypeName ?typeName ;
  jira:insightObjectTypeSchema / jira:insightSchemaKey  ?schemaKey ]
BIND(concat(?schemaKey, "-", str(?id)) as ?key)
}
EOF
)

echo '`(' ; \
    curl $CURL_PARAMS -H "Accept: application/sparql-results+json" -G https://jessandmorgan.com/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @html "((key . \"\(.key.value)\") (title . ,(xml-unescape-string \"[\(.key.value)] \(.label.value) - (\(.typeName.value))\")) (url . \"https://jessandmorgan.com/jira/secure/ShowObject.jspa?id=\(.id.value)\"))"' \
    ; echo ")"
