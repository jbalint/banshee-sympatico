python2 ledger-import.py > main-MONEY-191.ttl

stardog query execute bs 'clear silent graph <http://banshee-sympatico/ledger>' && \
    stardog data add --named-graph 'http://banshee-sympatico/ledger' bs main-MONEY-191.ttl
