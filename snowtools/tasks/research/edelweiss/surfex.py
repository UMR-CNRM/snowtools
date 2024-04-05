# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


class _Offline(_VortexTask):
    """
    Abstract class common to all OFFLINE executions
    """

    def get_remote_inputs(self):
        """
        Main method to fetch all necessary input files for an OFFLINE execution
        """
        self.get_forcing()
        self.get_surfex_namelist()
        self.get_prep()
        self.get_pgd()
        self.get_const_offline()
        self.get_additionnal_inputs()
        self.get_offline()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution output (PRO file)
        """
        self.sh.title('PRO')
        self.pro = io.put_pro(*self.common_args, member=self.conf.member,
                              filename='PRO_[datebegin:ymdh]_[dateend:ymdh].nc', **self.common_kw)

    def get_forcing(self):
        """
        Main method to fetch a single or an ensemble of FORCING files
        """
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(vapp=self.conf.vapp_forcing, filename='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            datebegin=self.conf.datebegin_forcing, dateend=self.conf.dateend_forcing, member=self.conf.member))
        self.sh.title('FORCING')
        self.forcing = io.get_forcing(self.conf.xpid_forcing, self.conf.geometry_forcing, **kw)

    def get_surfex_namelist(self):
        """
        Main method to get an OPTIONS.nam namelist
        The namelist file will be modifier by the pre-processing step, so a copy must be made
        instead of a symbolic link (--> intent='inout' by default in vortexIO).
        """
        self.sh.title('NAMELIST')
        # TODO : définir un source par défaut à appliquer partout ('namel_[binary]' dans common/data/const.py)
        # --> variable à mettre dans le ficiher de conf par défaut
        self.namelist = io.get_surfex_namelist(self.conf.uenv)

    def get_prep(self):
        """
        Main method to get a PREP file
        Parsing an *alternate_xpid* to vortexIO.get_prep implies that alternate resources may
        be used if not PREP file already exists for this geometry/xpid.
        The PREP file will be modified during the execution so a copy must be made
        instead of a symbolic link (--> intent='inout').
        """
        self.sh.title('PREP')
        self.prep = io.get_prep(self.conf.xpid_prep, self.conf.geometry, date=self.conf.date_prep,
                                alternate_xpid=self.ref_reanalysis, intent='inout', **self.common_kw)

    def get_pgd(self):
        """
        Main method to get a PGD file.

        TODO : this should be included in the 'get_const_offline' method
        """
        self.sh.title('PGD')
        self.pgd = io.get_pgd(*self.common_args, **self.common_kw)

    def get_const_offline(self):
        """
        Main method to get all other OFFLINE-mandatory static resources
        """
        self.sh.title('STATIC OFFLINE INPUTS')
        io.get_const_offline(self.conf.geometry, self.conf.uenv, alternate_uenv=self.conf.default_uenv)

    def get_additionnal_inputs(self):
        """
        Method to fetch any optional input that can be used for an OFFLINE execution
        """
        pass

    def get_offline(self):
        """
        Main method to get the OFFLINE executable
        This method must be overwritten depending on the type of execution (MPI or not)
        """
        raise NotImplementedError()


class OfflineMPI(_Offline):
    """
    Launch an OFFLINE executable with MPI parallelisation
    """

    def get_offline(self):
        """
        Get OFFLINE MPI executable
        TODO : use a VortexIO like tool
        """
        self.offline = io.get_offline_mpi(self.conf.uenv, alternate_uenv=self.conf.default_uenv)

    def algo(self):
        """
        Launch a single OFFLINE execution with MPI parallelisation
        """

        # Launch *Surfex_PreProcess* (vortex/src/cen/algo/deterministic.py) algo component to
        # preprocess the namelist (adjust dates, etc.)
        # TODO : this piece of algo should be included the the OFFLINE algo component !
        t = self.ticket
        self.sh.title('ALGO : NAMELIST pre-processing')
        preprocess = toolbox.algo(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            forcingname  = self.forcing[0].container.basename,
        )
        print(t.prompt, 'Pre-process algo =', preprocess)
        print()
        preprocess.run()

        t = self.ticket
        # Algo component to produce to run the SURFEX OFFLINE simulation (MPI parallelization)
        self.sh.title('ALGO : OFFLINE MPI')
        offline = toolbox.algo(
            engine         = 'parallel',  # TODO : *engine* should be 'blind'
            binary         = 'OFFLINE',
            kind           = 'deterministic',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            dateinit       = self.conf.date_prep,
            ntasks         = self.conf.ntasks,
            verbose        = True,
        )
        print(t.prompt, 'Algo =', offline)
        print()
        self.component_runner(offline, self.offline, mpiopts=dict(nprocs=self.conf.nprocs, ntasks=self.conf.ntasks))


class OfflineEnsemble(_Offline):
    """
    Launch several instances of an OFFLINE executable (without an MPI parallelisation) in parallel.
    """

    def get_offline(self):
        """
        Get OFFLINE MPI executable
        TODO : use a VortexIO like tool
        """
        t = self.ticket
        self.sh.title('EXEC OFFLINE NO MPI')
        self.offline = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_surfex_offline_nompi',
        )
        print(t.prompt, 'Executable =', self.offline)
        print()

    def algo(self):
        """
        Launch several instances of the OFFLINE executable in parallel
        """
        t = self.ticket
        self.sh.title('ALGO OFFLINE NO MPI')

        # TODO : avoid this kind of call here
        list_geometry = self.get_list_geometry()

        algo = toolbox.algo(
            engine         = 's2m',
            kind           = "ensmeteonodet",
            multidates     = True,
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            threshold      = self.conf.threshold,
            members        = self.conf.member,
            geometry_in    = list_geometry,
            geometry_out   = self.conf.geometry.tag,
            ntasks         = 40,
            daily          = not self.conf.previ,
        )
        print(t.prompt, 'Algo =', algo)
        print()
        self.component_runner(algo, self.offline)
