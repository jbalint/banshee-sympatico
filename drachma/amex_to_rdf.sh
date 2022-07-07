#!/bin/bash

# Amex to RDF [BS-120]

INFILE="$1"

JQ_CODE=$(cat <<EOF
 .[] | ["\(.Reference | ltrimstr("'") | rtrimstr("'"))",
"\(.Date | gsub("(?<month>[[:digit:]]+)/(?<day>[[:digit:]]+)/(?<year>[[:digit:]]+)"; "\(.year)/\(.month)/\(.day)")) (#\(.Reference | ltrimstr("'") | rtrimstr("'"))) \(.Description[0:20] | ltrimstr("IC* ") | ltrimstr("TST* ") | gsub("[0-9 \\\-]+$"; ""))  ;
    Expenses:AMEX-\(.Category | gsub(" "; "-"))              $\(.Amount)
    Liabilities:AMEX
"] | @tsv
EOF
)

cat "$INFILE" | \
    python -c 'import csv, json, sys; print(json.dumps([dict(r) for r in csv.DictReader(sys.stdin)]))' | \
    jq -r "$JQ_CODE" | \
    while IFS=$'\t' read -r REFNUM FORMATTED ; do
        #echo "Searching for $REFNUM"
        grep -q "(#$REFNUM)" $LEDGER_FILE || echo -ne "$FORMATTED\n"
    done

exit 0

# Example output:
[] a ledger:AmexTransaction  ;
  ledger:time "2022-06-16T00:00:00Z"^^xsd:dateTime ;
  ledger:description "ACE HARDWARE CENTER MADISON             WI" ;
  ledger:cardMember "JESS MICHAEL BALINT" ;
  ledger:accountNumber "-35003" ;
  ledger:amount "30.56"^^xsd:decimal ;
  ledger:extendedDetails "930588      608-257-1630\nACE HARDWARE CENTER\nMADISON\nWI\nDescription : HARDWARE/TOOLS Price : 0.00\n608-257-1630"
  ledger:appearsOnStatementAs "ACE HARDWARE CENTER MADISON             WI"
  ledger:address "1398 WILLIAMSON ST" ;
  ledger:cityState "MADISON\nWI" ;
  ledger:zipCode "53703-3757" ;
  ledger:country "UNITED STATES" ;
  ledger:reference "'320221680414421609'" ;
  ledger:amexCategory "Merchandise & Supplies-Hardware Supplies" ;
.
