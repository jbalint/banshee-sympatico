#!/bin/bash

#trap "kill 0" SIGTERM SIGINT SIGHUP EXIT

wmiir read /event | sed -u 's/^/wmii: /' &

rm /tmp/bs_input.fifo
mkfifo /tmp/bs_input.fifo
while [[ 1 ]] ; do
	cat /tmp/bs_input.fifo | while read line ; do
		eval $line &
	done
done
