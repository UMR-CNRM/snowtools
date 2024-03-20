# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io

from . import prep


def setup(t, **kw):
    return Driver(
        tag='Offline',
        ticket=t,
        nodes=[
            prep.PrepRefill(tag='prep', ticket=t, **kw),
            Offline(tag='offline', ticket=t, **kw),
        ],
        options=kw
    )


class Offline(_VortexTask):
    """
    TODO
    """

    def get_remote_inputs(self):
        """
        TODO
        """
        t = self.ticket

        self.sh.title('FORCING')
        # TODO : imposer 1 seul membre et faire une tâche à part pour un ensemble de FORCINGs ?
        self.forcing = io.get_forcing(*self.common_args, members=self.conf.members,
                                      filename='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc', **self.common_kw)

        self.sh.title('PREP')
        self.prep = io.get_prep(*self.common_args, date=self.conf.datespinup,
                                alternate_xpid=self.ref_reanalysis, **self.common_kw)

        self.sh.title('PGD')
        self.pgd = io.get_pgd(*self.common_args, **self.common_kw)

        self.sh.title('NAMELIST')
        self.namelist = io.get_surfex_namelist(self.conf.uenv)

        self.sh.title('STATIC OFFLINE INPUTS')
        io.get_const_offline(self.conf.geometry, self.conf.uenv)

        self.sh.title('OFFLINE')
        # TODO : pourrait être inclu dans "get_const_offline"
        self.offline = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            genv           = self.conf.uenv,
            gvar           = 'master_surfex_offline_mpi',
        )
        print(t.prompt, 'Executable =', self.offline)
        print()

    def algo(self):
        """
        TODO
        """
        t = self.ticket
        # Algo component to produce to run the SURFEX OFFLINE simulation (MPI parallelization)
        self.sh.title('Toolbox algo OFFLINE')
        algo = toolbox.algo(
            engine         = 'parallel',
            binary         = 'OFFLINE',
            kind           = 'deterministic',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            dateinit       = self.conf.datespinup,
            threshold      = self.conf.threshold,
            drhookprof     = self.conf.drhook,
        )
        print(t.prompt, 'Algo =', algo)
        print()
        self.component_runner(algo, self.offline, mpiopts=dict(nprocs=self.conf.nprocs, ntasks=self.conf.ntasks))

    def put_remote_outputs(self):
        """
        TODO
        """
        self.sh.title('PRO')
        self.pro = io.put_pro(*self.common_args, members=self.conf.members, **self.common_kw)
