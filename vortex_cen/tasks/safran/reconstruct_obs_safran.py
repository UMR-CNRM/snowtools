# -*- coding: utf-8 -*-
"""
reconstruct_obs_safran.py
-------------------------

.. autoclass:: ReconstructSafranObs
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""
from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ReconstructSafranObs(_CenResearchTask):
    """
    **Task : Reconstruct_SAFRAN_Obs**

    Task to build SAFRAN-compatible hourly observations files from reconstructed
    observation series.

    **Input:**

    - NEW_OBSERVATIONS.nc : file containing reconstructed hourly temperature "observations"
    - OBSERVATIONS.tar : archive containing real Safran-compatible surface observation files (R, S and T files)
    - listeo file : providing the metadata of all observation sites

    **Output:**

    - OBSERVATIONS.tar : archive containing Safran-compatible files with both real and reconstructed observations
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin",
            "dateend",
            "xpid",
            "geometry",
            "uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "newobs_xpid",
            "newobs_user",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        t = self.ticket

        self.sh.title('Reconstructed Observations')
        new_obs = vortex.input(
            kind='SurfaceObservation',
            nativefmt   = 'netcdf',
            model       = 'safran',
            datebegin   = self.list_dates_begin,
            dateend     = self.dict_dates_end,
            date        = '[dateend]',
            geometry    = 'SinglePoint',
            vapp        = 'safran',
            vconf       = 'france',
            experiment  = self.conf.get('newobs_xpid', 'xpid'),
            username    = self.conf.get('newobs_user', None),
            block       = 'observations',
            filename    = '[datebegin:ymdh]_[dateend:ymdh]/NEW_OBSERVATIONS.nc',
            namespace   = 'vortex.multi.fr',
            namebuild   = 'flat@cen'
        )
        print(t.prompt, 'New Obs = ', new_obs)
        print()

        # Get yearly observation packed files
#        rundate = self.conf.datebegin
#        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
#        for rundate in list_dates:
#            datebegin = rundate
#            dateend = rundate.replace(year = rundate.year + 1)
        self.sh.title('Raw Observations')
        self.obs = vortex.input(
            role           = 'Observations',
            part           = 'all',
            geometry       = self.conf.geometry,
            kind           = 'packedobs',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/OBSERVATIONS.tar',
            namespace      = 's2m.archive.fr',
            date           = self.conf.dateend.ymdh,
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            model          = 'safran',
            source         = 'surfaceobs',
            nativefmt      = 'tar',
            now            = True,
        )
        print(t.prompt, 'Raw Obs = ', self.obs)
        print()

        self.sh.title('Input listeo')
        listeo = vortex.input(
            role            = 'ListePost',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:tag]',
            geometry        = self.conf.geometry,
            kind            = 'listeo',
            model           = 'safran',
            local           = 'listeo_reanalyse',
            nativefmt       = 'ascii',
        )
        print(t.prompt, 'listeo =', listeo)
        print()

    def get_local_inputs(self):

        pass

    def algo(self):

        self.sh.title('Algo')
        algo = vortex.algo(
            kind         = 'reconstruct_observations',
            role_members = 'Observations',
            engine       = 'algo',
            ntasks       = len(self.obs),
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()
        algo.run()

    def put_outputs(self):

        self.sh.title('Reconstructed Observations')
        out = vortex.output(
            kind           = 'packedobs',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            date           = '[dateend:ymdh]',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            local          = '[datebegin:ymd6h]_[dateend:ymd6h]/OBSERVATIONS.tar',
            namespace      = 'vortex.archive.fr',
            model          = 'safran',
            source         = 'surfaceobs',
            namebuild      = 'flat@cen',
            block          = 'observations',
            nativefmt      = 'tar',
            cutoff         = 'assimilation',
            now            = True,
        )
        print(self.ticket.prompt, 'Output observations =', out)
        print()
