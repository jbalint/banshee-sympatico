#!/bin/bash
# Script to query the Jira issues for Helm completion source

# TODO did I want to do something with ?pkey and ?pname? (later, for narrowing)

#set -x
QUERY=$(cat<<EOF
select ?summary ?key ?pkey ?pname {
  graph <virtual://jira> {
    ?x a jira:Issue; jira:summary ?summary ; jira:issueKey ?key ; jira:project ?proj.
    ?proj jira:projectKey ?pkey ; jira:name ?pname.
  }
# TODO : order by removed until Stardog issue #3774 is fixed
#} order by ?key limit 9999
}
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @html "((key . \"\(.key.value)\") (title . ,(xml-unescape-string \"[\(.key.value)] \(.summary.value)\")) (url . \"https://localhost/jira/browse/\(.key.value)\"))"' \
    ; echo ")"

# Outputs (note: escaped characters must be fixed. this must be passed through `eval', also note the backquote action):
# `(
# (,(xml-unescape-string "[KEY-3] Reply to X about Y") . "https://localhost/jira/browse/KEY-3")
# (,(xml-unescape-string "[KEY-4] Get X started on Y") . "https://localhost/jira/browse/KEY-4")
# (,(xml-unescape-string "[KEY-5] Fix Z") . "https://localhost/jira/browse/KEY-5")
# ...)

# Results example:
# {
#   "head" : {
#     "vars" : [
#       "summary",
#       "key",
#       "pkey"
#     ]
#   },
#   "results" : {
#     "bindings" : [
#       {
#         "summary" : {
#           "type" : "literal",
#           "value" : "Increase \"THING\""
#         },
#         "key" : {
#           "type" : "literal",
#           "value" : "KEY-17"
#         },
#         "pkey" : {
#           "type" : "literal",
#           "value" : "KEY"
#         }
#       },
#       {
#         "summary" : {
#           "type" : "literal",
#           "value" : "X show Y instead of Z"
#         },
#         "key" : {
#           "type" : "literal",
#           "value" : "KEY-49"
#         },
#         "pkey" : {
#           "type" : "literal",
#           "value" : "KEY"
#         }
#       },
