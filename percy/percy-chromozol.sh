#!/bin/bash
# Script to query the Chromozol browser tabs for Helm completion source
# https://localhost/mediawiki/index.php/Chromozol_Graph_Representations

# TODO: error handling is bad

#set -x
QUERY=$(cat<<EOF
select ?tabId ?title ?url
from cmzl:
where
{
  # Get the current session (was using max ?start and joining outside this scope, but #4114 prevents that for now)
  {select (max(?ses) as ?ses) {  ?x a cmzl:SessionStartEvent ; cmzl:time ?start ; cmzl:session ?ses }}

  # get open tabs
  {
    select ?ses ?tab (max(?time) as ?latest) where {
      ?ses a cmzl:Session.
      {
        ?ses cmzl:hasTab ?tab.
        ?event a cmzl:TabUpdatedEvent ; cmzl:tab ?tab ; cmzl:time ?time.
      }
      FILTER NOT EXISTS {?closed a cmzl:TabClosedEvent ; cmzl:tab ?tab}
    } group by ?tab ?ses
  }
  ?event cmzl:tab ?tab ; cmzl:time ?latest ; cmzl:content [ cmzl:title ?title ; cmzl:url ?url ].
  ?tab cmzl:tabId ?tabId
} order by desc(?latest) xsd:integer(strafter(?tabId, "."))
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "(\"CMZL: \(.title.value | gsub("\""; "\\\"")) \(.url.value)\" . \"\(.tabId.value)\")"' \
    ; echo ")"

# Output is something like



exit
    #echo "$QUERY"
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "`(\"CMZL: \(.title.value | gsub("\""; "\\\"")) \(.url.value)\" . \"\(.tabId.value)\")"' \
