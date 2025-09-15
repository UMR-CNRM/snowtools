# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


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
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(vapp=self.conf.vapp_pro, datebegin=self.conf.datebegin_pro, dateend=self.conf.dateend_pro,
            xpid=self.conf.xpid_pro, geometry=self.conf.geometry_pro, member=self.conf.members))
        self.pro = io.get_pro(**kw)

    def algo(self):
        """
        TODO
        """
        t = self.ticket
        self.sh.title('Toolbox algo diag')
        tbalgo = toolbox.algo(
            kind         = 'scd',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
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
        self.diag = io.put_diag(**self.common_kw, member=self.conf.members)


class ExtractDates(_VortexTask):
    '''
    Generic task for the extraction of a set of dates from a PRO file
    '''

    def get_remote_inputs(self):
        """
        TODO
        """

        self.sh.title('Toolbox input PRO')
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(vapp=self.conf.vapp_pro, datebegin=self.conf.datebegin_pro, dateend=self.conf.dateend_pro,
            xpid=self.conf.xpid_pro, geometry=self.conf.geometry_pro, member=self.conf.members))
        self.pro = io.get_pro(**kw)

    def algo(self):
        """
        TODO
        """
        t = self.ticket
        self.sh.title('Toolbox algo diag')
        tbalgo = toolbox.algo(
            kind          = 'extract_dates',
            datebegin     = self.conf.datebegin,
            dateend       = self.conf.dateend,
            extract_dates = self.conf.extraction_dates,
            engine        = 'algo',  # _CENTaylorRun algo component "family" to execution a piece of python code
            ntasks        = len(self.pro),  # Do not forget to set the number of tasks for parallelisation
            role_members  = 'SnowpackSimulation',
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        TODO
        """
        self.sh.title('Toolbox output PRO')
        # TODO : How to archive that resource ?
        # Actuellement, elle est archivée sous "date=dateend" (car "namebuild=None")
        # --> ca ne semble pas très pertinent
        suffix = '_'.join(self.conf.extraction_dates)
        self.diag = io.put_pro(member=self.conf.members, filename=f'PRO_{suffix}.nc',
                namebuild=None, **self.common_kw,)

        # To put the file on sxcen only :
        # WARNING : the default cache on sxcen is under */home* (defined by the *storeroot* footprint with a
        # default value set in "vortex/conf/store-vortex-free-sxcnrm.ini" configuration file).
        # --> It seems more relevant to use the "NO_SAVE" space for the Vortex cache
        # TODO : find a better way to set this at a higher level
        # TODO : In any case, this must match the user-defined "MTOOLDIR" or "WORKDIR" or "DATADIR"
        # environment variable on sxcen
        # TODO : Problème avec l'extension "username" de l'xpid :
        # - le "put" depuis belenos ajoute le username au répertoire "xpid"
        # - le "get" depuis sxcen retire le username
        # user = self.env['USER']
        # storeroot = f'/cnrm/cen/users/NO_SAVE/{user}/cache'
        # self.diag = io.put_pro(members=self.conf.members, filename=f'PRO_{suffix}.nc',
        #        namebuild=None, storeroot=storeroot, storage='sxcen.cnrm.meteo.fr', **self.common_kw)
