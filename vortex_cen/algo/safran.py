#!/usr/bin/env python

"""
Algo Components generating a FORCING file.
"""
from bronx.fancies import loggers
from vortex_cen.algo.components import _CenTaylorRun, _CenTaylorVortexWorker
from bronx.stdtypes.date import Date, Period
from footprints.stdtypes import FPList

import os
import glob
import tarfile

logger = loggers.getLogger(__name__)


class TarGuess(_CenTaylorRun):
    """
    Concatenation of a set of FORCING files into a single forcing.
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several concatenations in parallel.',
        attr = dict(
            kind  = dict(
                values     = ['TarSafranGuess'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['Gridpoint'],
            ),
            domains = dict(
                info     = "List of domains covered by the guess files to tar",
                type     = FPList,
            )
        ),
    )

    def get_subdirs(self, rh, opts):
        """
        """
        avail_members = self.context.sequence.effective_inputs(role=self.role_members)

        print('----------------------------------------------------------------------')
        print('List of Workers :')
        print('-----------------')
        if len(avail_members) > 0:
            subdirs = list()
            # Retrive the subdirectory asociated to each identified RH
            for am in avail_members:
                if am.rh.resource.date.ymdh not in subdirs:
                    subdirs.append(am.rh.resource.date.ymdh)
                    print('* ', am.rh.resource.date.ymdh)
        else:
            subdirs = ['.']
            print('* .')
        print('----------------------------------------------------------------------')
        # logger.info('Workers : \n' + '\n'.join(subdirs))

        return subdirs


class TarGuessWorker(_CenTaylorVortexWorker):
    """
    Concatenation of a set of FORCING files into a single forcing.
    """

    _footprint = dict(
        attr = dict(
            kind    = dict(
                values = ['TarSafranGuess']
            ),
            domains = dict(
                info     = "List of domains covered by the guess files to tar",
                type     = FPList,
            )
        )
    )

    def vortex_task(self, **kwargs):
        """
        """
        for geometry in self.domains:
            tarname = f'ebauches_{geometry}_{self.subdir}.tar'
            with tarfile.open(tarname, mode='w') as tarfic:
                for f in glob.glob(f'{self.subdir}/*/*/P????????*{geometry}*'):
                    # f = 'YYYYMMDD00/mbXXX/ECH/PYYMMDDHH_E_dom_production'
                    ech = int(f.split('/')[2])  # ECH
                    # On veut organiser le tar pour qu'il soit directement exploitable par
                    # l'algo SAFRAN arpès détarrage : toutes les échéances issues d'un même
                    # réseau doivent être regroupées dans le même répertoire et le nom
                    # du fichier guess de la forme PYYMMDDHH correspondant à la date
                    # de validité du guess
                    validity = Date(self.subdir) + Period(hours=ech)
                    arcname = os.path.join(f.split('/')[0], f.split('/')[1], f'P{validity.yymdh}')
                    tarfic.add(f, arcname=arcname)
