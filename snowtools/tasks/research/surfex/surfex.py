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
        Main method to save an OFFLINE execution outputs
        """
        self.sh.title('PRO')
        self.pro = io.put_pro(*self.common_args, members=self.conf.members, **self.common_kw)

    def get_forcing(self):
        """
        Main method to fetch a single or an ensemble of FORCING files
        """
        self.sh.title('FORCING')
        self.forcing = io.get_forcing(*self.common_args, members=self.conf.members,
                                      filename='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc', **self.common_kw)

    def get_surfex_namelist(self):
        """
        Main method to get an OPTIONS.nam namelist
        """
        self.sh.title('NAMELIST')
        self.namelist = io.get_surfex_namelist(self.conf.uenv)

    def get_prep(self):
        """
        Main method to get a PREP file
        Parsing an *alternate_xpid* to vortexIO.get_prep implies that alternate resources may
        be used if not PREP file already exists for this geometry/xpid
        """
        self.sh.title('PREP')
        self.prep = io.get_prep(*self.common_args, date=self.conf.datespinup,
                                alternate_xpid=self.ref_reanalysis, **self.common_kw)

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
        io.get_const_offline(self.conf.geometry, self.conf.uenv)

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
        t = self.ticket
        self.sh.title('OFFLINE MPI')
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
        Launch the OFFLINE executable with MPI parallelisation
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


class OfflineEnsemble(_Offline):
    """
    Launch several instances of an OFFLINE executable in parallel.
    """

    def get_offline(self):
        """
        Get OFFLINE MPI executable
        TODO : use a VortexIO like tool
        """
        t = self.ticket
        self.sh.title('OFFLINE NO MPI')
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
        self.sh.title('Toolbox algo tb09 = OFFLINE')

        # TODO : avoid this kind of call here
        list_geometry = self.get_list_geometry()

        algo = toolbox.algo(
            engine         = 's2m',
            kind           = "ensmeteonodet",
            multidates     = True,
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            threshold      = self.conf.threshold,
            members        = self.conf.members,
            geometry_in    = list_geometry,
            geometry_out   = self.conf.geometry.tag,
            ntasks         = 40,
            daily          = not self.conf.previ,
        )
        print(t.prompt, 'Algo =', algo)
        print()
        self.component_runner(algo, self.offline)
