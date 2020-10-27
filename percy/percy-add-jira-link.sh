#!/bin/sh

# Add a link to the Jira issue

#set -x

KEY=$1
URL=$2
TITLE="$3"
FAVICON_URL=$4

if [ "$#" -lt 3 ]; then
    echo "Usage: $0 <jira-issue-key> <link-url> <link-title> [<favicon-url>]"
    exit 1
fi

# TODO: [SYS-150] - make this global?
export $(gpg --decrypt ~/sw/private_code/emm.gpg | grep -v "^#" | xargs)

if [ ! -z $FAVICON_URL ] ; then
    FAVICON_JSON="\"icon\":{\"url16x16\":\"$FAVICON_URL\"},"
fi

REQ_BODY="
{
    \"object\": {
        \"url\":\"$URL\",
        $FAVICON_JSON
        \"title\":\"$TITLE\"
    }
}"

curl -X POST -H "Content-Type: application/json" --url "https://localhost/jira/rest/api/latest/issue/$KEY/remotelink" -u "$BS_JIRA_USERNAME:$BS_JIRA_PASSWORD" -d "$REQ_BODY"
