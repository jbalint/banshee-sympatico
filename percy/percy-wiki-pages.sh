#!/bin/bash
# Script to query the wiki pages name for Helm completion source
# swivt:wikiPageModificationDate swivt:wikiPageSortKey

# TODO: error handling is bad

#set -x
QUERY=$(cat<<EOF
select *
from <virtual://mediawiki> {
  ?page a mw:Page ;
     mw:pageNamespaceId ?nsid ;
     mw:pageTitle ?title ;
     mw:pageUrl ?url ;
     mw:pageLatestRev ?latestRev ;
     # TODO : improve this, remove magic constants
     # skip 6 (file pages) and 102 (something from SemanticMediaWiki)
     # c.f. namespaces in LocalSettings.php
     FILTER(?nsid IN (0, 2, 14, 3100, 3101, 3102, 3103, 3104, 3105, 3106, 3107))
} order by desc(?latestRev) limit 9999
EOF
)
#echo "$QUERY"
echo '`(' ; \
    curl -u admin:admin  -H "Accept: application/sparql-results+json" -G https://localhost/stardog/bs/query \
         --data-urlencode query="$QUERY" 2> /dev/null \
        | jq -r '.results.bindings[] | @text "((title . \"\(.title.value)\") (url . \"\(.url.value)\"))"' \
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
