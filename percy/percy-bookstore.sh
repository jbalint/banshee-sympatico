#!/bin/bash
# Script to open bookstore files. Hopefully this script is replaced
# soon by a Stardog-backed version with good schema

BOOKSTORE_ROOT=/home/jbalint/Dropbox/bookstore

echo '`(' ; find ~/Dropbox/bookstore/ -type f | perl -lpe's#/home/jbalint/Dropbox/bookstore/(.*)#("BOOK: $1" . "https://localhost/bookstore/$1")#' ; echo ')'
