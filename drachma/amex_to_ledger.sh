#!/bin/bash

# Amex to Ledger [BS-120]

INFILE="$1"
LEDGER_FILE="/home/jbalint/tagore_home/Dropbox/important/ledger/main.ledger"

# The first line here is a lot but basically we have three fields:
# 1 the date transformed from mm/dd/yyyy to yyyy-mm-dd
# 2 the reference number with enclosing single-quotes removed
# 3 the description (first 20 bytes the place name, followed by city+state)
JQ_CODE=$(cat <<EOF
reverse | .[] | ["\(.Reference | ltrimstr("'") | rtrimstr("'"))",
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

# Example txn:
{
  "Date": "06/16/2022",
  "Description": "ACE HARDWARE CENTER MADISON             WI",
  "Card Member": "JESS MICHAEL BALINT",
  "Account #": "-35003",
  "Amount": "30.56",
  "Extended Details": "930588      608-257-1630\nACE HARDWARE CENTER\nMADISON\nWI\nDescription : HARDWARE/TOOLS Price : 0.00\n608-257-1630",
  "Appears On Your Statement As": "ACE HARDWARE CENTER MADISON             WI",
  "Address": "1398 WILLIAMSON ST",
  "City/State": "MADISON\nWI",
  "Zip Code": "53703-3757",
  "Country": "UNITED STATES",
  "Reference": "'320221680414421609'",
  "Category": "Merchandise & Supplies-Hardware Supplies"
}

# IC* COSTCO BY INSTAC
