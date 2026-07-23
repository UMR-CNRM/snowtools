# -*- coding: utf-8 -*-
"""
offline_ensemble.py
-------------------

Tasks designed to launch an OFFLINE executable WITHOUT MPI parallelisation
several time in parallel.

.. inheritance-diagram:: vortex_cen.tasks.surfex.offline_ensemble
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: Escroc
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: CrocO
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: EscrocResearch
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex
from bronx.stdtypes.date import Date
from vortex_cen.tasks.surfex.offline import _Offline


class Escroc(_Offline):
    """
    **Task : Escroc**

    Multiple executions of an OFFLINE binary with a single meteorological FORCING but
    different Crocus physics (namelists) and no MPI parallelization.

    Lafaysse et al. (2017) : https://tc.copernicus.org/articles/11/1173/2017/

    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "nmembers",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "drhook",
            "august_threshold",
            "ntasks",
            "nnodes",
            "nprocs",
            "subensemble+help=Name of the predefined escroc sub-ensemble to use;type=str;default=E2",
            "output_storage+help=Name of the archive / server where the output files will be stored;type=str",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_executable(self):
        self.get_executable_from_uenv(mpi=False)

    def algo(self):
        """
        Algo component to execute OFFLINE several time in parallel with different namelists.
        """
        self.sh.title('Algo OFFLINE-ESCROC')
        algo = vortex.task(
            kind           = "escroc",
            engine         = 'blind',
            # binary         = 'OFFLINE',  # unused
            verbose        = True,
            # MV TODO : gérer la conversion en Date dans l'algo
            datebegin      = Date(self.conf.datebegin),
            dateend        = Date(self.conf.dateend),
            dateinit       = Date(self.conf.get('prep_date', self.conf.datebegin)),
            # MV TODO :  La valeur par défaut de "threshold" est à sortir de la tâche
            threshold      = self.conf.get('august_threshold', -999),
            members        = self.get_list_members(),
            geometry_in    = [self.conf.geometry.tag],
            geometry_out   = self.conf.geometry.tag,
            # MV TODO : La valeur par défaut de "subensemble" est à sortir de la tâche
            subensemble    = self.conf.get('subensemble', 'E2'),
            ntasks         = self.conf.get('ntasks', self.conf.nmembers),
            reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', algo)
        print()
        return algo

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE algo component without MPI parallelisation.
        """
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable)

    def put_pro(self):

        self.sh.title('Output PRO')
        pro = vortex.output(
            local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datebegin      = self.list_dates_begin_pro,
            dateend        = self.dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            # model          = 'surfex',
            # TODO : le storage de sortie devrait être traité à plus haut niveau, en créant une variable de conf
            # dans la methode "defaults" 'research_task_base'
            storage        = self.conf.get('output_storage', None),
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pro',
            member         = self.get_list_members(),
        ),
        print(self.ticket.prompt, 'pro =', pro)
        print()

    def put_prep(self):

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'mb[member%04d]/PREP_[datevalidity:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datevalidity   = self.list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
            member         = self.get_list_members(),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

    def diff(self):
        pass


class CrocO(Escroc):
    """
    **Task : CrocO**

    Multiple executions of an OFFLINE binary with an ensemble of FORCING files
    and potentialy different Crocus physics (namelists).

    """

    # TODO (MV) : Clarifier la distinction entre les algos "escroc" (multi-physiue uniquement) et "croco"
    # (ensemble météo + multiphysique optionelle).
    # --> Faire des algos distincts

    def algo(self):
        """
        Algo component to execute OFFLINE several times in parallel
        """

        self.sh.title('Algo Offline-CorcO')
        croco_tba = vortex.task(
            engine         = 'blind',
            kind           = "croco",
            # binary         = 'OFFLINE',  # unused
            verbose        = True,
            # MV TODO : gérer la conversion en Date dans l'algo
            datebegin      = Date(self.conf.datebegin),
            dateend        = Date(self.conf.dateend),
            dateinit       = Date(self.conf.get('prep_date', self.conf.datebegin)),
            # MV TODO :  La valeur par défaut de "threshold" est à sortir de la tâche
            threshold      = self.conf.get('august_threshold', -999),
            members        = self.get_list_members(),
            geometry_in    = [self.conf.geometry.tag],
            geometry_out   = self.conf.geometry.tag,
            # MV TODO : La valeur par défaut de "subensemble" est à sortir de la tâche
            subensemble    = self.conf.get('subensemble', 'E2'),
            ntasks         = self.conf.get('ntasks', self.conf.nmembers),
            # MV : "nforcing" n'est pas un footprint de l'algo !
            # nforcing       = self.conf.nforcing,
            reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', croco_tba)
        print()
        return croco_tba


class EscrocResearch(Escroc):
    """
    **Task: EscrocResearch**

    SURFEX/OFFLINE documentation : https://umr-cnrm.github.io/snowtools-doc/misc/surfex.html

    **Inputs:**

    - FORCING.nc files(s) (near-surface meteorological conditions during the simulation period)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from the execution of the "PreProcess")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography) retrieved or produced by the GetPgd1D task
    - PREP.nc (initial conditions) retrieved or produced by the GetPrep task

    **Outputs:**

    - PRO.nc Snowpack simulations covering the entire simulation period
    - PREP.nc SURFEX/Crocus model state variables at the end of the simulation
    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()

    def get_local_inputs(self):
        # Get PGD and PREP locally because they have been retrieved or produced by a previous task
        self.get_pgd()
        self.get_prep()
        # Get namelist from the preprocess task output
        self.get_namelist_from_cache()
        # Get FORCING locally because they have already been retrieved by the preprocess task
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_executable(self):

        if "exesurfex" in self.conf:
            self.get_executable_from_path()
        else:
            self.get_executable_from_uenv()
