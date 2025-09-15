#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

DOC_TEMPDIR=`mktemp -d`
DIR="$( cd "$( dirname "$( readlink -f "${BASH_SOURCE[0]}" )" )" >/dev/null 2>&1 && pwd )"
URL_GIT="git@github.com:UMR-CNRM/snowtools-doc.git"
URL="https://umr-cnrm.github.io/snowtools-doc/"

echo "========================================================="
echo "  Git clone of current doc"
echo "========================================================="
git clone $URL_GIT $DOC_TEMPDIR

echo "========================================================="
echo "  Building doc..."
echo "========================================================="
pushd $DIR/../
make html
popd
echo ""

echo "========================================================="
echo "  Merging doc and git doc repository"
echo "========================================================="
rsync -av --delete $DIR/../build/html/ $DOC_TEMPDIR/ --exclude .git --exclude objects.inv --exclude .doctrees --exclude .nojekyll > /dev/null

pushd $DOC_TEMPDIR
echo "Creating a commit..."
git add -A
git commit -m "Documentation revision"
echo ""
echo "You can check documentation at $DOC_TEMPDIR/index.html"
echo "> Will push the new version of documentation to $URL_GIT (hence $URL)"
read -p "Is this correct [Y/n] ? " -n 1 -r CHOICE
echo ""
if [[ $CHOICE =~ ^[nN]$ ]]
then
    echo -e "Aborting."
    exit 0
fi

echo "========================================================="
echo "  Pushing..."
echo "========================================================="
git push
popd

echo "Documentation deployed to $URL"

echo "========================================================="
echo "  Cleaning"
echo "========================================================="
if [ -d $DOC_TEMPDIR ]
then
    rm -rf $DOC_TEMPDIR
fi
