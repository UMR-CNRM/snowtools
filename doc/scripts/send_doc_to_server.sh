#!/usr/bin/env bash

HERE="$( cd "$( dirname "$( readlink -f "${BASH_SOURCE[0]}" )" )" >/dev/null 2>&1 && pwd )"
DOCFOLDER="$(dirname $HERE)/build/html/"

read -p "Send to intra.cnrm.meteo.fr ? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    rsync -av --links --delete -e 'ssh' $DOCFOLDER sxcen.cnrm.meteo.fr:/cnrm/cen/users/NO_SAVE/viallonl/snowtools-doc
fi

read -p "Send to snowtools.vln-glr.fr ? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    rsync -av --links --delete -e 'ssh' $DOCFOLDER snowtools@snowtools.vln-glr.fr:snowtools-doc
fi
