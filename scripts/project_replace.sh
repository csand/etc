#!/bin/sh
perl -p -i -e 's/oldstring/newstring/g' `grep -ril $1 *`

