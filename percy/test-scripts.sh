#!/bin/sh

# Diagnostic script to make sure percy sources are returning results

for script in percy-bookmarks.sh percy-bookstore.sh percy-chromozol.sh percy-jira-issues.sh percy-wiki-pages.sh ; do
    echo -n "$script: "
    $script | wc -l
done
