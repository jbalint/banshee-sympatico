#!/bin/bash
# Percy client - will start Emacs session if necessary
# TODO - refactor function from h1g1.sh

SOCK=/tmp/emacs$UID/percy

if [ ! -S $SOCK ] ; then
    emacs --eval '(setq server-name "percy")' \
          --eval "(require 'percy)" --daemon
    sleep 1 # necessary?
fi

emacsclient -c -s $SOCK -F '((name . "percy"))' --eval '(percy-anything)'
