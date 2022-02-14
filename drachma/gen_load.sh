
# Switched to main ledger package (not ledger-git) and no longer need this
#export PYTHONPATH=/home/jbalint/aur/ledger-git/src/ledger
python ledger-import.py > main-MONEY-191.ttl

STARDOG_BIN=/home/jbalint/sw/java-sw/stardog-bin/bin

$STARDOG_BIN/stardog query execute bs 'clear silent graph <http://banshee-sympatico/ledger>' && \
    $STARDOG_BIN/stardog data add --named-graph 'http://banshee-sympatico/ledger' bs main-MONEY-191.ttl
