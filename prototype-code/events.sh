#!/bin/bash

trap "kill 0" SIGTERM SIGINT EXIT

wmiir read /event | sed -u 's/^/wmii: /' &

cat
