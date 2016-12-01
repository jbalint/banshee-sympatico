#!/bin/bash
# Percy client - will start Emacs session if necessary

SOCK=/tmp/emacs$UID/percy

if [ ! -S $SOCK ] ; then
    emacs --eval '(setq server-name "percy")' \
          --eval '(load-file (concat (getenv "BS_HOME") "/percy/percy.el"))' --daemon
    sleep 1 # necessary?
fi

emacsclient -c -s $SOCK -F '((name . "percy"))' --eval '(percy-anything)'
