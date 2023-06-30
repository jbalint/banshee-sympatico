#!/bin/bash
# Percy client - will start Emacs session if necessary
# TODO - refactor function from h1g1.sh

EMACS_SOCKET=/run/user/$UID/emacs/percy

# https://unix.stackexchange.com/a/556790/178680
if ! socat -u OPEN:/dev/null UNIX-CONNECT:$EMACS_SOCKET ; then
    echo "Emacs socket is dead. Removing"
    rm $EMACS_SOCKET
fi

if [[ "$1" == "stop" ]] ; then
    echo "Sending emacs shutdown"
    emacsclient -s $EMACS_SOCKET -e '(kill-emacs)'
else
    if ! [[ -S $EMACS_SOCKET ]] ; then
        emacs --eval '(setq server-name "percy")' \
              --eval "(require 'percy)" --daemon
        sleep 1 # necessary?
    fi

    emacsclient -c -s $EMACS_SOCKET -F '((name . "percy"))' --eval '(percy-anything)'
fi
