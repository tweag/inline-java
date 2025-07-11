#!/bin/sh
( echo "{-# LINE 1 \"$1\" #-}" ; sed 's/####//g' $2) > $3
exit $?

