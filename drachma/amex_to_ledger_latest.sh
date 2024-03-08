#!/bin/sh

# Thin wrapper around `amex_to_ledger.sh' to process the
# `activity.csv' (default filename from AMEX)

ACTIVITY_FILE=$HOME/dl/activity.csv

if [[ ! -e "$ACTIVITY_FILE" ]] ; then
    echo "Cannot find $ACTIVITY_FILE"
    exit 1
fi

# We take the last 20 lines because the delimited CSV contains
# embedded newlines and this _seems_ to be a large enough buffer.
# Then find the latest and take the LAST one (file appears to be in
# reverse chronological order).
START_DATE=$(tail -20 $HOME/dl/activity.csv | \
                 perl -lne'if(m#(\d{2})/(\d{2})/(\d{4}),.*#) { print "$3-$1-$2" } else {}' | \
                 tail -1)
END_DATE=$(date +%Y-%m-%d)

NEW_FILENAME=$HOME/dl/amex_activity/AMEX_activity_${START_DATE}_to_${END_DATE}.csv

if [[ -e "$NEW_FILENAME" ]] ; then
    echo "Refusing to overwrite $NEW_FILENAME"
    ls -al "$NEW_FILENAME"
    exit 1
fi

# TODO: don't move file unless the next command succeeds?
mv -i "$ACTIVITY_FILE" "$NEW_FILENAME"

$(dirname $0)/amex_to_ledger.sh "$NEW_FILENAME"
