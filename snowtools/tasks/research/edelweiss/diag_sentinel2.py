# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


def setup(t, **kw):
    return Driver(
        tag='Diag',
        ticket=t,
        nodes=[
            Diag_sentinel2(tag='diag', ticket=t, **kw),
        ],
        options=kw
    )


class Diag_sentinel2(_VortexTask):
    '''
    Generic task for the computation of Sentinel2-like diagnostics of a SURFEX execution :
    * SMOD (Snow Melt Out Date)
    * SCD (Snow Cover Duration)
    '''

    def get_remote_inputs(self):
        """
        TODO
        """

        self.sh.title('Toolbox input PRO')
        self.pro = io.get_pro(*self.common_args, **self.common_kw, members=self.conf.members)

        # Get a static mask file to remove glacier/forest pixels
        self.sh.title('Toolbox input MASK')
        self.mask = io.get_const(self.conf.uenv, 'mask', self.conf.geometry, **self.common_kw)

    def algo(self):
        """
        TODO
        """
        t = self.ticket
        self.sh.title('Toolbox algo diag')
        tbalgo = toolbox.algo(
            kind         = 'S2diag',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            mask         = self.mask[0],
            engine       = 'algo',  # _CENTaylorRun algo component "family" to execution a piece of python code
            ntasks       = self.conf.ntasks,  # Do not forget to set the number of tasks for parallelisation
            role_members = 'SnowpackSimulation',
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        TODO
        """
        self.sh.title('Toolbox output DIAG')
        self.diag = io.put_diag(*self.common_args, **self.common_kw, members=self.conf.members)
