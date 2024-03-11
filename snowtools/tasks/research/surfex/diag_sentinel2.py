# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Diag',
        ticket=t,
        nodes=[
            Diag_sentinel2(tag='diag', ticket=t, **kw),
        ],
        options=kw
    )


class Diag_sentinel2(Task, S2MTaskMixIn):
    '''
    Generic task for the computation of Sentinel2-like diagnostics of a SURFEX execution :
    * SMOD (Snow Melt Out Date)
    * SCD (Snow Cover Duration)
    '''

    def process(self):

        t = self.ticket

        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input PRO')
            pro = toolbox.input(
                local          = 'mb[member]/PRO.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                date           = self.conf.dateend,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                vapp           = self.conf.vapp,
                vconf          = '[geometry:tag]',
                model          = 'surfex',
                namespace      = self.conf.namespace_in,
                namebuild      = 'flat@cen',
                block          = 'pro',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
                fatal          = True,
            ),
            print(t.prompt, 'PRO input =', pro)
            print()

            # Get a static mask file to remove glacier/forest pixels
            self.sh.title('Toolbox input MASK')
            mask = toolbox.input(
                local          = 'mask.nc',
                nativefmt      = 'netcdf',
                kind           = 'mask',
                genv           = self.conf.cycle,
                intent         = 'in',  # Make a hard link rather than a copy
                fatal          = False,
            ),
            print(t.prompt, 'MASK input =', mask)
            print()

        #######################################################################
        #                            Compute step                             #
        #######################################################################

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo diag')
            tbalgo = toolbox.algo(
                kind         = 'S2diag',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                mask         = mask[0],
                members      = self.conf.members,
            )
            print(t.prompt, 'tbalgo =', tbalgo)
            print()
            tbalgo.run()

        #######################################################################
        #                               Backup                                #
        #######################################################################

        if 'backup' in self.steps or 'late-backup' in self.steps:

            self.sh.title('Toolbox ouput DIAG')
            tbdiag = toolbox.output(
                local          = 'mb[member]/DIAG.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = self.conf.datebegin,  # TODO : changer les dates ?
                dateend        = self.conf.dateend,  # TODO : changer les dates ?
                date           = self.conf.dateend,  # TODO : changer les dates ?
                scope          = 'SesonalSnowCoverDiagnostic',
                nativefmt      = 'netcdf',
                kind           = 'diagnostics',
                vapp           = self.conf.vapp,
                vconf          = '[geometry:tag]',
                model          = 'surfex',
                namespace      = self.conf.namespace_out,
                namebuild      = 'flat@cen',
                block          = 'diag',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
                fatal          = True,
            ),
            print(t.prompt, 'DIAG ouput =', tbdiag)
            print()
