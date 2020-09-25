#!/bin/bash

emacsclient -s /run/user/$UID/emacs/percy -e '(kill-emacs)'
