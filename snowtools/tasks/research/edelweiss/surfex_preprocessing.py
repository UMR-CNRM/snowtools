# -*- coding: utf-8 -*-
'''
Created on 20 mars 2024
@author: Vernay.M
'''

from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


class Prep(_VortexTask):
    """
    TODO
    """

    def get_remote_inputs(self):
        """
        TODO
        """
        t = self.ticket

        self.sh.title('INIT_TG')
        io.get_init_TG(self.conf.geometry, self.conf.uenv, self.conf.xpid)

        self.sh.title('PREP executable')
        self.prep = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            genv           = self.conf.uenv,
            gvar           = 'master_prep_mpi',
        )
        print(t.prompt, 'Executable =', self.prep)
        print()

    def algo(self):
        """
        TODO
        """
        t = self.ticket
        # Algo component to produce the PREP file
        # Take care : PREP parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
        self.sh.title('Toolbox algo PREP')
        algo = toolbox.algo(
            engine     = 'parallel',
        )
        print(t.prompt, 'PREP algo =', algo)
        print()
        self.component_runner(algo, self.prep, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

    def put_remote_outputs(self):
        """
        TODO
        """
        self.sh.title('PREP')
        io.put_prep(date=self.conf.dateend, filename='PREP_[date:ymdh].nc',
                    **self.common_kw)


class PrepRefill(Prep):
    """
    TODO
    """

    def get_remote_inputs(self):
        """
        TODO
        """
        self.sh.title('PREP')
        prep = io.get_prep(
            vapp           = self.conf.vapp_prep,
            geometry       = self.conf.geometry_prep,
            date           = self.conf.date_prep,
            xpid           = self.conf.xpid_prep,
            alternate_xpid = self.ref_reanalysis,
        )

        if len(prep) == 0:
            # If no valid prep found, make one
            self.make_prep = True
            super().get_remote_inputs()
        else:
            self.make_prep = False

    def algo(self):
        if self.make_prep:
            super().algo()
        else:
            pass

    def put_remote_outputs(self):
        self.get_remote_inputs()
        if self.make_prep:
            super().put_remote_outputs()
        else:
            pass
