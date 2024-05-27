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

    Sequence of methods called :
    ----------------------------
    1. get_remote_inputs   --> _Offline
    2. get_local_inputs    --> _VortexTask
    3. algo                --> _Offline
    4. put_local_outputs   --> _VortexTask
    5. put_remote_outputs  --> _Offline

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
        self.pro = io.put_pro(member=self.conf.member, filename='PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                **self.common_kw)

    def get_forcing(self):
        """
        Main method to fetch a single or an ensemble of FORCING files
        """
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(vapp=self.conf.vapp_forcing, filename='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            datebegin=self.conf.datebegin_forcing, dateend=self.conf.dateend_forcing, member=self.conf.member,
            xpid=self.conf.xpid_forcing, geometry=self.conf.geometry_forcing))
        self.sh.title('FORCING')
        self.forcing = io.get_forcing(**kw)

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
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(xpid=self.conf.xpid_prep, geometry=self.conf.geometry_prep, date=self.conf.date_prep,
            alternate_xpid=self.ref_reanalysis, intent='inout', vapp=self.conf.vapp_prep))
        self.prep = io.get_prep(**kw)

    def get_pgd(self):
        """
        Main method to get a PGD file.

        TODO : this should be included in the 'get_const_offline' method
        """
        self.sh.title('PGD')
        self.pgd = io.get_pgd(**self.common_kw)

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

    def algo(self):
        """
       Before the launch of a single instance of any OFFLINE executable, the namelist ùmust be pre-processed.

       -->  Launch *Surfex_PreProcess* (vortex/src/cen/algo/deterministic.py) algo component to
            preprocess the namelist (adjust dates, etc.)

        TODO : this piece of algo should be included the the OFFLINE algo component !
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


class OfflineMPI(_Offline):
    """
    Launch an OFFLINE executable with MPI parallelisation

    Sequence of methods called :
    ----------------------------
    1. get_remote_inputs           --> _Offline
        a) get_forcing             --> _Offline
        b) get_surfex_namelist     --> _Offline
        c) get_prep                --> _Offline
        d) get_pgd                 --> _Offline
        e) get_const_offline       --> _Offline
        f) get_additionnal_inputs  --> _Offline
        g) get_offline             --> OfflineMPI

    2. algo                        --> OfflineMPI

    3. put_remote_outputs          --> _Offline
    """

    def get_offline(self):
        """
        Get OFFLINE MPI executable
        """
        self.offline = io.get_offline_mpi(self.conf.uenv, alternate_uenv=self.conf.default_uenv)

    def algo(self):
        """
        Launch a single OFFLINE execution with MPI parallelisation
        """

        super().algo()  # Call the common "pre-processing" required before OFFLINE executions

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

    Sequence of methods called :
    ----------------------------
    1. get_remote_inputs           --> _Offline
        a) get_forcing             --> _Offline
        b) get_surfex_namelist     --> _Offline
        c) get_prep                --> _Offline
        d) get_pgd                 --> _Offline
        e) get_const_offline       --> _Offline
        f) get_additionnal_inputs  --> _Offline
        g) get_offline             --> OfflineEnsemble

    2. algo                        --> OfflineEnsemble

    3. put_remote_outputs          --> _Offline
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

        # TODO : réorganiser l'algo `SurfexComponent` en plusieurs algos indépendants ( 1 algo / *kind*)

        """
        super().algo()  # Call the common "pre-processing" required before OFFLINE executions

        t = self.ticket
        self.sh.title('ALGO OFFLINE NO MPI')

        # TODO : réorganiser l'algo en plusieurs algos indépendants ( 1 algo / *kind*)

        algo = toolbox.algo(
            engine         = 's2m',
            kind           = "ensmeteonodet",
            multidates     = True,
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            threshold      = self.conf.threshold,
            members        = self.conf.member,
            ntasks         = 40,
            daily          = not self.conf.previ,
        )
        print(t.prompt, 'Algo =', algo)
        print()
        self.component_runner(algo, self.offline)
