#!/bin/sh

set -e

start_time=$(date +%s)

# Switched to main ledger package (not ledger-git) and no longer need this
#export PYTHONPATH=/home/jbalint/aur/ledger-git/src/ledger
cd $BS_HOME/drachma
python ledger-import.py > main-MONEY-191.ttl

STARDOG_BIN=/home/jbalint/sw/java-sw/stardog-bin/bin

$STARDOG_BIN/stardog query execute bs 'clear silent graph <http://banshee-sympatico/ledger>' && \
    $STARDOG_BIN/stardog data add --named-graph 'http://banshee-sympatico/ledger' bs main-MONEY-191.ttl

$STARDOG_BIN/stardog query execute -f JSON bs 'select ?g (count(*) as ?c) FROM NAMED stardog:context:named where { graph ?g { ?s ?p ?o } } group by ?g ' | jq -r '.results.bindings.[] | @text "named_graph_size{graph=\"\(.g.value)\"} \(.c.value)"' > /tmp/prom/named_graph_sizes.prom

end_time=$(date +%s)
runtime=$(( end_time - start_time ))

cat <<EOF | curl --silent --show-error --data-binary @- "https://jessandmorgan.com/pushgateway/metrics/job/N88-744_ledger_import_to_stardog"
# TYPE batch_job_last_success_timestamp_seconds gauge
batch_job_last_success_timestamp_seconds ${end_time}
# TYPE batch_job_last_duration_seconds gauge
batch_job_last_duration_seconds ${runtime}
EOF
