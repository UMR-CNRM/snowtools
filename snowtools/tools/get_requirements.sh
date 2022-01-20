#!/usr/bin/env bash
for i in $(find . -maxdepth 1 -type d -not -name '__pycache__' -not  -name 'DATA' -not -name 'conf')
do
	echo 'Building python requirements for '$i;
	pushd $i;
	# 1st method : we look for file with import, try to import them and look at error raised (work in fresh environnement, eg, virtualenv)
	# we look recursively (-r) for filename (-l) of file containing 'import' keyword
	# we only keep 'py' files (we could exclude all not wanted files (pyc, sh, etc...))
	# we keep a single occurence of each filename
	# we transform pathname (path/to/file) into python module path (path.to.file)
	# we then try to import them in python3, and redirected STDERR into STDIN
	# we look for 'ModuleNotFoundError'
	# we exclude 'not a package error', those are local files import
	# we keep the name of module that raised the error
	# we exclude 'ModuleNotFoundError', those left are local import.. supposedly
	# we sort and keep uniq instance
	# WARNIG ONLY FIND THE FIRST IMPORT
	#grep -rl import | grep py | uniq \
	#	| sed 's/\//./g' | xargs -IFILE -P 1 python3 -c 'import FILE' 2>&1 \
	#      	| grep 'ModuleNotFoundError' | grep -v 'not a package' | sed "s/.*'\(\w*\)'.*/\1/" | grep -v 'ModuleNotFoundError' | sort -u > requirements.txt
	# 2nd method : we look for all import (from x import y, import z) and get all x and z.
	grep -rh ' import ' | sort -u | sed 's/\//./g' | sed 's/\(.*\)\#.*/\1/' | sed 's/\s*//' | sed 's/"//g' | xargs -P 1 -IFILE python3 -c 'FILE' 2>&1 | grep ModuleNotFoundError |  sed "s/.*'\(\w*\)'.*/\1/" | sort -u # > requirement_2.txt
	# 3rd method : we somehow install only required package (by die and retry method)
	# pip freeze > requirements.txt
	popd;
done
