# Produce a report of files changes in Git repositories

$BS_HOME/git_repo_monitor/target/release/git_repo_monitor print_json | \
    jq -r '@text "\(.repo_path)"' | \
    while read i ; do
        REPO="${i%.git/}"
        (cd "$REPO" ; echo -e "\n\n>>>>>>> Changes in $REPO <<<<<<<<<<" ; git --no-pager diff HEAD --compact-summary)
    done
