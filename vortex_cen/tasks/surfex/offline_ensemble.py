# -*- coding: utf-8 -*-
"""
"""

import vortex
import footprints
from bronx.stdtypes.date import Date
from vortex_cen.tasks.surfex.offline import _Offline


class Escroc(_Offline):
    """
    Task : Escroc
    =============

    Multiple executions of an OFFLINE binary with a single meteorological FORCING but
    different Crocus physics (namelists) and no MPI parallelization.

    Lafaysse et al. (2017) : https://tc.copernicus.org/articles/11/1173/2017/

    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "members",
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
            engine         = 's2m',
            verbose        = True,
            # MV TODO : gérer la conversion en Date dans l'algo
            datebegin      = Date(self.conf.datebegin),
            dateend        = Date(self.conf.dateend),
            dateinit       = Date(self.conf.get('prep_date', self.conf.datebegin)),
            # MV TODO :  La valeur par défaut de "threshold" est à sortir de la tâche
            threshold      = self.conf.get('august_threshold', -999),
            members        = footprints.util.rangex(self.conf.members),
            geometry_in    = [self.conf.geometry.tag],
            geometry_out   = self.conf.geometry.tag,
            # MV TODO : La valeur par défaut de "subensemble" est à sortir de la tâche
            subensemble    = self.conf.get('subensemble', 'E2'),
            ntasks         = self.conf.get('ntasks', len(footprints.util.rangex(self.conf.members))),
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
            member         = footprints.util.rangex(self.conf.members),
        ),
        print(self.ticket.prompt, 'pro =', pro)
        print()

    def put_prep(self):

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'mb[member%04d]/PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            date           = self.list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
            member         = footprints.util.rangex(self.conf.members),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()


class CrocO(Escroc):
    """
    Task : CrocO
    ============

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
        algo = vortex.algo(
            engine         = 's2m',
            kind           = "croco",
            verbose        = True,
            # MV TODO : gérer la conversion en Date dans l'algo
            datebegin      = Date(self.conf.datebegin),
            dateend        = Date(self.conf.dateend),
            dateinit       = Date(self.conf.get('prep_date', self.conf.datebegin)),
            # MV TODO :  La valeur par défaut de "threshold" est à sortir de la tâche
            threshold      = self.conf.get('august_threshold', -999),
            members        = footprints.util.rangex(self.conf.members),
            geometry_in    = [self.conf.geometry.tag],
            geometry_out   = self.conf.geometry.tag,
            # MV TODO : La valeur par défaut de "subensemble" est à sortir de la tâche
            subensemble    = self.conf.get('subensemble', 'E2'),
            ntasks         = self.conf.get('ntasks', len(footprints.util.rangex(self.conf.members))),
            subensemble    = self.conf.subensemble,
            # MV : "nforcing" n'est pas un footprint de l'algo !
            # nforcing       = self.conf.nforcing,
            reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', algo)
        print()
        return algo
