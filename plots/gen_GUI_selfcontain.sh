#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

# script from Léo Viallon-Galinier
# 2020-09-04
# Generate an archive for standalone use of GUI_Proreader
#
# The generated zip file could be executed directly with python3.
# If Python version < 3.7, the massif names will not be available
# until the unzipping of the file.
#
# If the dependencies are modified, please adapt the list of necessary
# files below (LFILES and LDATAFILES). If they originate from other 
# folders than utils or plots, it may be necessary to adapt the 
# changes in import processes. See function adapt_import below.

SCRIPTNAME='gen_GUI_selfcontain'
VERSION=1
TMP='/tmp'
OUTFILE='GUI_selfcontain.zip'
OUTFILE_DEFAULT='GUI_selfcontain.zip'
PWD_INI="$(pwd)"

if [ -s "$SNOWTOOLS_CEN" ]
then
    LFILESDIR="$SNOWTOOLS_CEN"
else
    LFILESDIR="./"
fi

# List of files to be used
LFILES=("utils/FileException.py"
        "utils/dates.py"
        "utils/prosimu.py"
        "utils/infomassifs.py"
        "utils/S2M_standard_file.py"
        "plots/EvoProfilPlot.py"
        "plots/proReader_mini.py"
        "plots/Dictionnaries.py"
        "plots/GUI_Proreader.py"
        )

# List of data files to be placed under DATA/ folder
LDATAFILES=("DATA/METADATA.xml")

#######################################################################
#                                HELP                                 #
#######################################################################

usage()
{
    echo "***"
    echo "$1:"
    echo "Generate an archive for standalone use of GUI_Proreader"
    echo "the visualisation tool of CROCUS outputs."
    echo ""
    echo "Author : Viallon-Galinier L."
    echo "Thanks to : Mathieu Fructus, Pascal Hagenmuller"
    echo "Version: $VERSION"
    echo "***"
    echo "$1 : USAGE :"
    echo "$1 [OUTPUTZIPFILE] [TMPFOLDER]"
    echo ""
    echo "OUTPUTZIPFILE : Output zip file name (default: $OUTFILE)"
    echo "TMPFOLDER: Temporary folder (default: $TMP)"
    echo "***"

}

# Coloration
if [[ -t 1 ]]
then
    VERT="\033[32m"
    ORANGE="\033[1;33m"
    ROUGE="\033[1;31m"
    STOPC="\033[0m"
else
    VERT=""
    ORANGE=""
    ROUGE=""
    STOPC=""
fi

#######################################################################
#                         ARGS INTERPRETATION                         #
#######################################################################

# Read first argument
if [ $# -ge 1 ]
then
    if [ "$1" = "-h" -o "$1" = "--help" ]
    then
        usage $SCRIPTNAME
        exit 0
    fi
    OUTFILE=$1
fi

# Read second argument
if [ $# -ge 2 ]
then
    TMP=$2
fi

# Test if output file exist
if [ -f "$OUTFILE" ]
then
    read -p "$(echo -e "${ORANGE}WARNING${STOPC}: Output filename $OUTFILE already exists. Overwrite [y/N] ? ")" -n 1 -r CHOICE
    echo ""
    if [[ ! $CHOICE =~ ^[yY]$ ]]
    then
        exit 0
    fi
fi

if [ ! -d "$TMP" ]
then
    echo -e "${ROUGE}ERROR${STOPC}: Temporary folder $TMP does not exist !"
    exit 1
fi

#######################################################################
#                     Generate/Clean temp folder                      #
#######################################################################

TMPFOLDERNAME=`head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13`
TMPFOLDERNAME="$TMP/GUI_selfcontain_temp_gen-$TMPFOLDERNAME"
mkdir "$TMPFOLDERNAME"
echo -e "Working in temporary directory $TMPFOLDERNAME"

cleantmp()
{
    rm -r "$1"
}

#######################################################################
#                            Adapt imports                            #
#######################################################################
adapt_import()
{
    sed -i -e "s/from snowtools.utils\./from /" "$1"
    sed -i -e "s/import snowtools.utils\./import /" "$1"
    sed -i -e "s/from snowtools.plots\. /from /" "$1"
    sed -i -e "s/from \.FileException/from FileException/" "$1"
    sed -i -e "s/^\( *\)main(version='.*')/\1main()/" "$1"
}


#######################################################################
#                        Generation of archive                        #
#######################################################################

# Generic files
for FILEPATH in "${LFILES[@]}"
do
    echo -e "  - File $FILEPATH"
    FILENAME="${FILEPATH##*/}"
    cp "$LFILESDIR/$FILEPATH" "$TMPFOLDERNAME/$FILENAME"
    adapt_import "$TMPFOLDERNAME/$FILENAME"
done

# Data folder
mkdir "$TMPFOLDERNAME/DATA"

for FILEPATH in "${LDATAFILES[@]}"
do
    echo -e "  - File $FILEPATH"
    FILENAME="${FILEPATH##*/}"
    cp "$LFILESDIR/$FILEPATH" "$TMPFOLDERNAME/DATA/$FILENAME"
done

# Main file
echo -e "  - file __main__.py"
DATE=`date --iso-8601=seconds -u` # Date/Time UTC
cat > $TMPFOLDERNAME/__main__.py << EndOfFile
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import GUI_Proreader
import sys

GUI_Proreader.main(version='ZIP $DATE', args=sys.argv[1:])

EndOfFile

echo -e "Generating zip file..."
cd "$TMPFOLDERNAME"
zip $OUTFILE_DEFAULT *.py DATA/*
cd "$PWD_INI"

cp "$TMPFOLDERNAME/$OUTFILE_DEFAULT" "$OUTFILE"

cleantmp $TMPFOLDERNAME

echo -e "${VERT}OK${STOPC}: File generated to $OUTFILE"


