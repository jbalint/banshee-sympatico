#!/bin/sh

# Diagnostic script to make sure percy sources are returning results

for script in percy-bookmarks.sh percy-bookstore.sh percy-chromozol.sh percy-jira-issues.sh percy-jira-insight-objects.sh percy-wiki-pages.sh ; do
    echo -n "=== Executed $script. Results: "
    time $script | wc -l
done
