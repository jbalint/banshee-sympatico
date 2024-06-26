#!/bin/bash
# Script to query the bookstore entries for Helm completion source

# TODO: error handling is bad

source $(dirname $0)/curl-params.sh

#set -x
QUERY=$(cat<<EOF
select *
from <http://banshee-sympatico/bookstore>
where {
  ?b a bslib:Book ; nie:isStoredAs [
  nfo:fileName ?fileName ;
  nie:url ?url ;
  nfo:fileCreated ?created
  ]
} order by desc(?created)
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl $CURL_PARAMS -H "Accept: application/sparql-results+json" -G https://jessandmorgan.com/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "((title . \"\(.fileName.value)\") (url . \"\(.url.value)\"))"' \
    ; echo ")"

    
# SEXP output:
#("BOOK: equipment_docs/intel-q87-8-series-chipset-pch-datasheet.pdf" . "https://localhost/bookstore/equipment_docs/intel-q87-8-series-chipset-pch-datasheet.pdf")
#("BOOK: equipment_docs/ThinkCentre-spec-v491-June2016.pdf" . "https://localhost/bookstore/equipment_docs/ThinkCentre-spec-v491-June2016.pdf")
#("BOOK: equipment_docs/Intel_Linux_NVMe_Guide_330602-002.pdf" . "https://localhost/bookstore/equipment_docs/Intel_Linux_NVMe_Guide_330602-002.pdf")

# Output is something like
  #   {
  # "head" : {
  #   "vars" : [
  #     "b",
  #     "fileName",
  #     "url",
  #     "created"
  #   ]
  # },
  # "results" : {
  #   "bindings" : [
  #     {
  #       "b" : {
  #         "type" : "bnode",
  #         "value" : "node1b6knlfc7x528"
  #       },
  #       "fileName" : {
  #         "type" : "literal",
  #         "value" : "File Name.djvu"
  #       },
  #       "url" : {
  #         "type" : "uri",
  #         "value" : "https://localhost/bookstore/File%20Name.djvu"
  #       },
  #       "created" : {
  #         "datatype" : "http://www.w3.org/2001/XMLSchema#dateTime",
  #         "type" : "literal",
  #         "value" : "2017-01-16T10:11:19-06:00"
  #       }
  #     },
