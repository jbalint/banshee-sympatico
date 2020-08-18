#!/bin/bash
# Script to query the Jira Insight objects for Helm completion source

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
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @html "((key . \"\(.key.value)\") (title . ,(xml-unescape-string \"[\(.key.value)] \(.label.value) - (\(.typeName.value))\")) (url . \"https://localhost/jira/secure/ShowObject.jspa?id=\(.id.value)\"))"' \
    ; echo ")"
