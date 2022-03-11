#!/usr/bin/env bash

# This script is designed to identify snowtools requirements.
# It have to be executed in the snowtools folder in a fresh
# virtual environment, with nothing installed inside and no
# access to system site packeges.

for i in $(find . -maxdepth 1 -type d -not -name '__pycache__' -not  -name 'DATA' -not -name 'conf')
do
	echo 'Building python requirements for '$i;
	pushd $i;

    # search through all .py files all import lines
    # sort and remove multiple occurences
    # we remove junk around import syntax
    # sort and remove multiple occurences
    # execute all import and redirect STDERR into STDIN
    # we keep name that failed to be imported
    # sort and remove multiple occurences
    # remove garbage around package name and write list to file
    find . -name '*.py' -exec grep -rhE -e '^import' -e '^\s*from .* import' {} \; \
        | sort -u \
        | sed -e 's/\(.*\)\#.*/\1/' -e 's/^\s*//' -e 's/\s*$//' \
        | sort -u \
        | xargs -P 1 -IIMP python3 -c 'IMP' 2>&1 \
        | grep ModuleNotFoundError \
        | sort -u \
        | sed "s/.*named '\(.*\)'/\1/" ;

	# 2nd method : we somehow install only required package (by die and retry method)
	# pip freeze > requirements.txt
	popd;
done
