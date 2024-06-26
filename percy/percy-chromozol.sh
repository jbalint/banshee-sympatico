#!/bin/bash
# Script to query the Chromozol browser tabs for Helm completion source
# https://localhost/mediawiki/index.php/Chromozol_Graph_Representations

# TODO: error handling is bad

source $(dirname $0)/curl-params.sh

#set -x
QUERY=$(cat<<EOF
select ?tabId ?title ?url ?favIconUrl
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
  ?event cmzl:tab ?tab ; cmzl:time ?latest ; cmzl:content ?content .
  ?content cmzl:title ?title ; cmzl:url ?url .
  ?tab cmzl:tabId ?tabId

  # Do the quoting of the JSON value here as we know if it's bound or not
  OPTIONAL { ?content cmzl:favIconUrl ?favIconUrlRaw }
  bind(if(bound(?favIconUrlRaw), concat("\"", str(?favIconUrlRaw), "\""), "nil") as ?favIconUrl)
} order by desc(?latest) xsd:integer(strafter(?tabId, "."))
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl $CURL_PARAMS -H "Accept: application/sparql-results+json" -G https://jessandmorgan.com/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "((title . \"\(.title.value | gsub("\""; "\\\"")) \(.url.value)\") (url . \"\(.url.value)\") (favIconUrl . \(.favIconUrl.value)) (id . \"\(.tabId.value)\"))"' \
    ; echo ")"
