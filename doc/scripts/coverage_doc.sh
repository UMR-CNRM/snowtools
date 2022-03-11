#!/usr/bin/env bash

BASEDIR="$(dirname $( dirname $( dirname "$( readlink -f "${BASH_SOURCE[0]}" )" ) ) )"

# Check executable
if ! command -v docstr-coverage &> /dev/null
then
    echo "docstr-coverage could not be found"
    echo "Could be installed via pip install docstr-coverage"
    exit
fi

usage(){
    echo "$0 [-h] [-v verb_level]"
    echo ""
    echo "verb-level :"
    echo "             1 (default): just print overal stats"
    echo "             2 : Prints stats by file"
    echo "             3 : Details of lacking doctrings"
}

# Read arguments
VERBOSE=1

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -h|--help)
        usage
        exit 0
    ;;
    -v)
        VERBOSE="$2"
        shift # past argument
        shift # past value
    ;;
esac
done

# color
if [[ -t 1 ]]
then
    BOLD="\033[1m"
    STOPC="\033[0m"
else
    BOLD=""
    STOPC=""
fi


cov(){
    echo '**********************************************'
    echo -e " Documentation coverage for folder $BOLD$1$STOPC"
    echo '**********************************************'
    docstr-coverage --verbose=$VERBOSE $BASEDIR/$1
}

# Do doc coverage tests
cov bronx
cov interpolation
cov plots
cov tools
cov utils
cov tasks
