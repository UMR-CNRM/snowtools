#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

if [ "$1" = "-h" ]
then
    echo "genscripthelp.sh"
    echo "USAGE:"
    echo ""
    echo "genscripthelp.sh filename srcdir dstdir"
    echo ""
    echo "filename: a file containing two columns separated by a space:"
    echo "           - script path"
    echo "           - destination of rst file"
    echo ""
    exit 0
fi

if [ $@ -ne 3 ]
then
    echo 'Error: Please provide exactly one argument. Use -h for details.'
    exit 1
fi

if [ -f $1 ]
then
    echo "genscripthelp: parse file $1"
else
    echo "genscripthelp: ERROR: File $1 not found"
    exit 2
fi

if [ -d $2 ]
then
    echo " Source directory: $2"
else
    echo "genscripthelp: ERROR: Source directory $2 not found"
fi

if [ -d $3 ]
then
    echo " Destination directory: $3"
else
    echo " Create destination directory: $3"
    mkdir $3
fi


while IFS= read -r line
do
    echo "$line"
    SRC=`echo "$line" | cut -d ' ' -f 1`
    DST=`echo "$line" | cut -d ' ' -f 2`
    echo "  $SRC --> $DST"
    if [ "$DST" = "" ]
    then
        echo "Could not parse line \"$line\" in file $1"
        exit 3
    fi

    if [ ! -f $2/$SRC ]
    then
        echo "  File $2/$SRC not found !"
        continue
    fi

    echo -e "Script \`\`$SRC\`\` help:\n\n" > $3/$DST
    echo -e ".. code-block:: none\n" >> $3/$DST
    python3 $2/$SRC -h | sed -e 's/^/   /' >> $3/$DST
    echo -e "\n\n" >> $3/$DST
done < "$1"
