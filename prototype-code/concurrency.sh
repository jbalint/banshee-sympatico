#!/bin/bash
# a way to funnel multiple streams to one output

trap "kill 0" SIGINT SIGTERM EXIT

# rm /tmp/concurrency.fifo
# mkfifo /tmp/concurrency.fifo

# exec > /tmp/concurrency.fifo

wmiir read /event | sed -u 's/^/1: /' &
wmiir read /event | sed -u 's/^/2: /' &
wmiir read /event | sed -u 's/^/3: /' &
wmiir read /event | sed -u 's/^/4: /' &
wmiir read /event | sed -u 's/^/5: /' &
wmiir read /event | sed -u 's/^/6: /' &
wmiir read /event | sed -u 's/^/7: /' &
wmiir read /event | sed -u 's/^/8: /' &
wmiir read /event | sed -u 's/^/9: /' &
wmiir read /event | sed -u 's/^/a: /' &
wmiir read /event | sed -u 's/^/b: /' &
wmiir read /event | sed -u 's/^/c: /' &
wmiir read /event | sed -u 's/^/d: /' &
wmiir read /event | sed -u 's/^/e: /' &
wmiir read /event | sed -u 's/^/f: /' &

sleep 10
