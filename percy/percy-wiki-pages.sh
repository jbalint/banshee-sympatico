#!/bin/bash
# Script to query the wiki pages name for Helm completion source
# swivt:wikiPageModificationDate swivt:wikiPageSortKey

# TODO: error handling is bad

#set -x
QUERY=$(cat<<EOF
select *
where {
      ?page a swivt:Subject.
      ?page rdfs:label ?title.
      ?page swivt:page ?url.
      ?page swivt:wikiPageModificationDate ?d.
      filter(not exists {?page swivt:file ?f })
} order by desc(?d) limit 999
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/semantic-mediawiki/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "(\"WIKI: \(.title.value)\" . \"\(.url.value)\")"' \
    ; echo ")"

# Output is something like
# {
#   "head" : {
#     "vars" : [
#       "page",
#       "title",
#       "url",
#       "f"
#     ]
#   },
#   "results" : {
#     "bindings" : [
#       {
#         "page" : {
#           "type" : "uri",
#           "value" : "http://localhost/mediawiki/index.php/Special:URIResolver/TestSMW"
#         },
#         "title" : {
#           "type" : "literal",
#           "value" : "TestSMW"
#         },
#         "url" : {
#           "type" : "uri",
#           "value" : "https://localhost/mediawiki/index.php/TestSMW"
#         }
#       },
#       {
#         "page" : {
#           "type" : "uri",
#           "value" : "http://localhost/mediawiki/index.php/Special:URIResolver/XXX"
#         },
#         "title" : {
#           "type" : "literal",
#           "value" : "XXX"
#         },
#         "url" : {
#           "type" : "uri",
#           "value" : "https://localhost/mediawiki/index.php/XXX"
#         }
#       }
#     ]
#   }
# }
