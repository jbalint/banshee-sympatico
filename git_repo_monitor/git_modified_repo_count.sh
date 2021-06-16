COUNT=$($BS_HOME/git_repo_monitor/target/release/git_repo_monitor print_json | wc -l)
#COUNT=0

if [[ $COUNT > 0 ]] ; then
    # Colors don't work with Polybar
    #echo -e "Git: \033[31m$COUNT\e[0m"
    echo -e "Git: $COUNT"
fi
