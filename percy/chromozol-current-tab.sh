#!/bin/sh

# Get the current tab

QUERY=$(cat $BS_HOME/../chromozol/queries/current_tab.rq)

curl --silent -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
     --data-urlencode query="$QUERY" \
    | jq -r '.results.bindings[] | .url.value' | tail -1
