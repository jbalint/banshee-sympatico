#!/bin/bash

#set -x
QUERY=$(cat<<EOF
select *
where {
graph <http://banshee-sympatico/bookmarks> {
[] a nfo:Bookmark ;
  nie:title ?title ;
  nfo:bookmarks / nie:url ?url 
}
}
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "(\"WEB: \(.title.value | gsub("\""; "\\\"")) \(.url.value)\" . \"\(.url.value)\")"' \
    ; echo ")"

