#!/bin/sh

# Get the current tab

source $(dirname $0)/curl-params.sh

QUERY=$(cat $BS_HOME/../chromozol/queries/current_tab.rq)

curl --silent $CURL_PARAMS -H "Accept: application/sparql-results+json" -G https://jessandmorgan.com/stardog/bs/query \
     --data-urlencode query="$QUERY" \
    | jq -r '.results.bindings[] | .url.value' | tail -1
