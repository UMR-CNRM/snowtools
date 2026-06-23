# -*- coding: utf-8 -*-
"""
Created on 18 March 2024
@author: Vernay.M
"""
import vortex
from mkjob.nodes import Task
from vortex_cen.layout.nodes import S2MTaskMixIn
from bronx.stdtypes.date import Date
from footprints.stdtypes import FPDict, FPList
from footprints.util import rangex

from vortex_cen.tools.monitoring import InputReportContext, OutputReportContext
from vortex_cen.tools.monitoring import AlgoReportContext, TestReportContext

from snowtools.utils.dates import get_list_dates_files, get_dic_dateend


class _CenResearchTask(Task, S2MTaskMixIn):
    """
    Abstract class defining the common sequence of actions for CEN's vortex tasks.

    A vortex task is the sequence of actions to execute a single algo component.

    It always follows this procedure (definied in the main `process` method) :

        1. fetch all necessary input resources (files):
            - from an archive machine --> on a transfert node ('early-fetch')
            - from the local machine --> on a compute node ('fetch')

        2. execute the algo component (an executable, a script or·a sequence of instructions)
           of inputs resources necessary to run an algo component (an executable, a script or a
           sequence of instructions) --> 'compute'

        3. save output resources (files produced or modified by the algo component)
            - to the local machine --> on a compute node ('backup')
            - to an archive machine --> on a compute node ('late-backup')

    To implement a new task inheriting from this abstract class, implement a subset of the following methods :
    * get_remote_inputs
    * get_local_inputs
    * algo
    * put_outputs

    See their respective documentation for more details.

    Doc :
    http://intra.cnrm.meteo.fr/algopy/trainings/vortex_dev2022_1/presentation/beamer/vortex_dev_jobs2_presentation.pdf

    TODO:
    1. Make a separate abstract task for real-time applications ?
    2. Move methods from S2MTaskMixIn here (or research-specific methods in case of #1) ?

    """

    def __init__(self, **kw):
        """
        Initialise class attributes for dynamic documentation.
        """

        super().__init__(**kw)
        self.MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin",
            "dateend",
            "xpid",
            "geometry",
        ]

        self.OPTIONAL_CONFIGURATION_VARIABLES = [
            "date",
            "test",
            "localtest",
            "debug",
            "io_duration",
            "namespace_out",
        ]

    def update_attributes(self, mandatory, optional, overwrite=None):
        """
        Update class attributes for dynamic documentation
        """
        if isinstance(overwrite, list):
            for var in overwrite:
                # Warning : "remove" removes only 1 element in case of duplicated values.
                # However, there should never be duplicates in MANDATORY_CONFIGURATION_VARIABLES and
                # OPTIONAL_CONFIGURATION_VARIABLES
                if var in self.MANDATORY_CONFIGURATION_VARIABLES:
                    self.MANDATORY_CONFIGURATION_VARIABLES.remove(var)
                if var in self.OPTIONAL_CONFIGURATION_VARIABLES:
                    self.OPTIONAL_CONFIGURATION_VARIABLES.remove(var)

        self.MANDATORY_CONFIGURATION_VARIABLES.extend(
            [x for x in mandatory if x not in self.MANDATORY_CONFIGURATION_VARIABLES]
        )
        self.OPTIONAL_CONFIGURATION_VARIABLES.extend(
            [x for x in optional if x not in self.OPTIONAL_CONFIGURATION_VARIABLES]
        )

    def defaults(self, extras):
        """
        Set toolbox defaults, extended with actual arguments ``extras``.
        """

        t = vortex.ticket()

        if 'localtest' in self.conf:
            vortex.active_now = False

        vortex.defaults(
            # namespace      = self.conf.get('namespace', Namespace('vortex.multi.fr')),
            # namespace      = Namespace('vortex.multi.fr'),
            # date           = '[dateend]',  # WARNING : research only
            # TODO : the 'date' footprint is to be removed for research applications
            # experiment     = self.conf.xpid,
            # geometry       = self.conf.geometry,
            # vapp           = self.conf.vapp,
            # vconf          = '[geometry:tag]',  # TODO : à modifier après changement de convention
            # model          = self.conf.model,
            # namebuild      = 'flat@cen',  # WARNING : research only !
            # nativefmt      = 'netcdf',
        )

        for optk in ('cutoff', 'geometry', 'cycle', 'vortex_set_aside'):
            if optk in self.conf:
                value = self.conf.get(optk)
                if isinstance(value, dict):
                    value = FPDict(value)
                vortex.defaults[optk] = value

        # Le nombre de process et de tâches peut être associé à la géométrie via un dictionnaire, on récupère
        # maintenant la bonne valeur
        # TODO : Sortir ce qui suit de research_task_base et essayer de simplifier
        if 'ntasks' in self.conf and isinstance(self.conf.ntasks, dict):
            if self.conf.geometry.tag in self.conf.ntasks.keys():
                self.conf.ntasks = self.conf.ntasks[self.conf.geometry.tag]
            else:
                # Default value from s2m.
                # Maybe it would be better to crash and ask the user to set an explicit value ?
                self.conf.ntasks = 80
        if 'nprocs' in self.conf and isinstance(self.conf.nprocs, dict):
            if self.conf.geometry.tag in self.conf.nprocs.keys():
                self.conf.nprocs = self.conf.nprocs[self.conf.geometry.tag]
            else:
                # Default value from s2m.
                # Maybe it would be better to crash and ask the user to set an explicit value ?
                self.conf.nprocs = 80

        # Format uenv properly : "uenv:{uenv_name}@user" in cas only {uenv_name} is provided
        for key, value in self.conf.items():
            if "uenv" in key:
                if ':' not in value:
                    value = f"uenv:{value}"
                if '@' not in value:
                    value = f'{value}@{t.env()["USER"]}'

                self.conf[key] = value

        # Define a namespace_out variable to apply to all outputs set as the *namespace_out*
        # configuration variable if provided by the user or 'vortex.multi.fr' by default
        self.namespace_out = self.conf.get('namespace_out', 'vortex.multi.fr')

        vortex.defaults(**extras)
        self.header('Toolbox defaults')
        vortex.defaults.show()

    @property
    def debug(self):
        """
        Enter 'debug' mode to preserve the working directory even after a succesfull execution.

        Associated configuration variable :
        :param debug: Enter 'debug' mode, default : False
        :type debug: bool

        """
        if 'debug' in self.conf:
            return self.conf.debug
        else:
            return False

    def preprocess(self):
        """
        Pre-processing step to set usefull class variables.

        Associated (optional) configuration variables :

        :param io_duration: Argument similar to the one of the `get_list_dates_files` method in
                            snowtools/utils/dates.py. It is used to retrieve the list of *datebegin* and
                            *dateend* footprints for IO covering sub-periods.
                            Possible values : "yearly", "monthly" or "full"
        :type io_duration: str
        """
        self.get_list_dates(duration=self.conf.get('io_duration', 'yearly'))

    def process(self):
        """
        Main method definig the task's sequence of actions
        """

        t = self.ticket

        self.preprocess()

        if 'early-fetch' in self.steps:
            # In a multi step job (MTOOL, ...), this step will be run on a TRANSFER NODE.
            # Consequently, data that may be missing from the local cache must be fetched here.
            # e.g. GCO's genv, data from the mass archive system, ...
            # Note: most of the data should be retrieved here since the use of transfer node is costless.
            with InputReportContext(self, t):
                self.get_remote_inputs()

        if 'fetch' in self.steps:
            # In a multi step job (MTOOL, ...), this step will be run, on a COMPUTE NODE,
            # just before the beginning of computations. It is the appropriate place to fetch data produced
            # by a previous task (the so-called previous task will have to use the 'backup' step
            # in order to make such data available in the local cache).
            with InputReportContext(self, t):
                self.get_local_inputs()

        if 'compute' in self.steps:
            # The actual computations... (usually a call to the run method of an AlgoComponent)
            # This is executed on a COMPUTE NODE.
            with AlgoReportContext(self, t):
                algo = self.algo()
                if 'localtest' not in self.conf:
                    self.launch_algo(algo)

        if 'backup' in self.steps or 'late-backup' in self.steps:
            # In a multi step job (MTOOL, ...), this step will be run on a TRANSFER NODE.
            # Consequently, most of the data should be archived here.
            with OutputReportContext(self, t):
                self.put_outputs()

        if 'late-backup' in self.steps:
            # Reproductibility check with reference output (retrieved from the archive on a transfer node only)
            if 'test' in self.conf and 'localtest' not in self.conf:
                with TestReportContext(self, t):
                    self.unittest()
            elif 'diff_xpid' in self.conf:
                self.diff()

            if self.debug:
                # Debug mode : make the job crash at the end to preserve the working directory
                print('============================================================================')
                print('============================================================================')
                raise Exception('INFO :The execution went well, do not take into account the following error')

    def get_remote_inputs(self):
        """
        Implement this method in your task to fetch all resources stored remotely (on Hendrix, sxcen,...) from
        a transfer node.
        """
        raise NotImplementedError()

    def get_local_inputs(self):
        """
        Implement this method in your task to fetch all resources already stored on the local (HPC) cache from a
        compute node.
        """
        # self.get_remote_inputs()  # TODO : check if really necessary / good practice
        # TODO: comment SR: definitely a problem: the same file appears twice in the effective input list
        raise NotImplementedError()

    def algo(self):
        """
        Implement this method to call your task's algo component.
        This method should return a valid AlgoComponent object.
        """
        raise NotImplementedError("method 'algo' returning a valid AlgoComponent object should be"
                "implemented in child class.")

    def launch_algo(self, algo, **kw):
        """
        Implement this method in your task's algo component.
        The implementation should define how to run the algo component, or call one of the standard
        methods: `launch_MPI_executable()`, `launch_python_algo()`
        :param algo: AlgoComponent object
        :param kw:
        """
        raise NotImplementedError("the method 'launch_algo' should be implemented in child class and might call "
                                  "'launch_MPI_executable()' or 'launch_python_algo()' if appropriate.")

    def launch_MPI_executable(self, algo, mpiopts=None):
        """
        Run executable with MPI.

        :param algo: AlgoComponent object
        :param mpiopts: dict with MPI options nnodes=..., nprocs=..., ntasks=...
        """
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # vortex.executable(...))
        # Il est possible de récupérer cet objet avec la ligne suivante :
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]

        # TODO : les valeurs de mpiopts sont définies par défaut dans la méthode "component_runner" de mkjob/nodes.py
        # à partir des variables de configuration self.conf.nnodes, self.conf.ntasks, self.conf.nprocs
        # --> Réfléchir à la pertinence de faire 2 méthodes "launch_MPI_executable" et "launch_executable" distinctes
        self.component_runner(algo, executable, mpiopts=mpiopts)

    def launch_executable(self, algo):
        """
        run executable without MPI.

        :param algo: AlgoComponent object
        """
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable)

    def launch_python_algo(self, algo, **kw):
        """
        Run your task's algo component. For algo components consisting of python code.
        """
        if algo is not None:
            algo.run(**kw)

    def put_outputs(self):
        """
        Implement this method in your task to save resources remotely (on Hendrix, sxcen,...) from a transfer node.
        """
        # raise NotImplementedError()
        pass

    def unittest(self):
        """
        Implement this method in unittest tasks to monitor the test results.
        """
        if 'diff_xpid' in self.conf and self.conf.diff_xpid != 'False':
            # TODO : trouver une façon plus élégante de désactiver ponctuellement un test de reproductibilité
            self.diff()

    def diff(self):
        """
        Implement this method in your task to compare output with a reference file.
        """
        pass

    def get_list_dates(self, duration='yearly'):
        """
        Get the list of datebegin/dateend corresponding to the different time periods covered by IO files
        from the actual simulation's datebegin/dateend arguments.

        :param duration: Time period covered by individual files.

        """
        if 'datebegin' in self.conf and 'dateend' in self.conf:
            # Get FORCING input dates
            self.list_dates_begin, list_dates_end, self.list_dates_begin_pro, self.list_dates_end_pro  = \
                get_list_dates_files(Date(self.conf.datebegin), Date(self.conf.dateend), duration)
            self.dict_dates_end = get_dic_dateend(self.list_dates_begin, list_dates_end)
            self.dict_dates_end_pro = get_dic_dateend(self.list_dates_begin_pro, self.list_dates_end_pro)
        elif 'date' in self.conf:  # Real-time only --> make a specific default class ?
            self.list_dates_begin = [self.conf.date]
            self.dict_dates_end   = {self.conf.date: self.conf.date}
        else:
            # TODO
            pass

    def get_list_members(self):
        """
        Return the complete list of ensemble members from either
        - the exact 'member' value (int) --> returns [member]
        - the 'members' list (FPList or list) --> returns the list of members
        - the number of members 'nmembers' (int) --> returns the list of 'nmembers' values starting from 1
        """
        if self.conf.member is not None:
            return rangex(self.conf.member)
        elif 'members' in self.conf:
            # members is the list of ensemble members, ex : range(35), '0-35-1', [1, 2, 3]
            return rangex(self.conf.members)
        elif 'nmembers' in self.conf:
            # nmembers is the number of ensemble members (int)
            return rangex(1, self.conf.nmembers)

    def get_forcing(self, localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc', alternate=True,
            namespace='vortex.multi.fr'):
        """
        Method to get meteorological forcing file(s) covering the simulation period.
        First, check if an existing forcing file covers the full simulation period.
        If no such file exists, check for files covering standard sub-periods (yearly or monthly files).


        Arguments:
        :param localname: *local* footprint (how to name the file in the working directory).
                          This is an algo/task-specific argument.
                          Default name depends on the actual datebegin/dateend of each file.
                          WARNING : in case a unique value is provided the user should ensure that a single
                          file will be retrieved (for example set the alternate argument to False)
        :type localname: str
        :param alternate: Allow to search for alternative files covering sub-periods.
        :type alternate: bool

        Mandatory configuration variables:
        ----------------------------------

        :param forcing_datebegin: *datebegin* footprint, default self.conf.datebegin
        :type forcing_datebegin: str, footprints.stdtypes.FPList
        :param forcing_dateend: *dateend* footprint, default self.conf.dateend
        :type forcing_dateend: str, footprints.stdtypes.FPList
        :param forcing_xpid: Experiment identifier, default self.conf.xpid
        :type forcing_xpid: str
        :param forcing_geometry: *geometry* footprint, default self.conf.geometry
        :type forcing_geometry: str, footprints.stdtypes.FPList
        :param forcing_vapp: *vapp* footprint, default self.conf.vapp
        :type forcing_vapp: str
        :param forcing_vconf: *vconf* footprint, default self.conf.vconf
        :type forcing_vconf: str
        :param forcing_block: *block* footprint, default "meteo"
        :type forcing_vconf: str
        :param forcing_namespace: *namespace* footprint, default "vortex.multi.fr" (hendrix + local cache)
        :type forcing_namespace: str

        :param forcing_date: *date* footprint (unsed with the research namebuilders), default to [dateend]
        :type forcing_date: str
        :param forcing_model: *model* footprint (to be made optional for SurfaceIO objects), default None
        :type forcing_model: str

        Optionnal configuration variables:
        ----------------------------------

        :param forcing_member: *member* footprint, default None (or *member* if provided)
        :type forcing_member: int, footprints.stdtypes.FPList
        :param forcing_namebuild: *namebuild* footprint, default "flat@cen" (will change soon)
        :type forcing_namebuild: str
        :param forcing_intent: *intent* footprint (local file permissions), default "in"
                               Possible values : "in" (read-only), "inout" (read-write)
        :type forcing_intent: str
        :param forcing_source_app: *source_app* footprint, default None
        :type forcing_source_app: str, footprints.stdtypes.FPList
        :param forcing_source_conf: *source_conf* footprint, default None
        :type forcing_source_conf: str, footprints.stdtypes.FPList
        :param forcing_source: Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
                               Possible values : 'era5', 'era40'
        :type forcing_source: str
        :param forcing_cutoff: *cutoff* footprint (to be made optional for SurfaceIO objects), default None
        :type forcing_cutoff: str
        :param io_duration: Argument similar to the one of the `get_list_dates_files` method in
                            snowtools/utils/dates.py.
                            Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
                            Possible values : "yearly", "monthly" or "full"
        :type io_duration: str
        :param forcing_vortex1: Boolean to identify resources produced with vortex1 (filename without geometry)
        :type forcing_vortex1: bool

        """

        t = self.ticket

        forcing_datebegin = self.conf.get('forcing_datebegin', self.conf.get('datebegin', None))
        forcing_dateend   = self.conf.get('forcing_dateend', self.conf.get('dateend', None))
        forcing_xpid      = self.conf.get('forcing_xpid', self.conf.xpid)
        forcing_user      = self.conf.get('forcing_user', None)
        forcing_vapp      = self.conf.get('forcing_vapp', self.conf.vapp)
        forcing_vconf     = self.conf.get('forcing_vconf', self.conf.vconf)
        forcing_block     = self.conf.get('forcing_block', 'meteo')
        forcing_member    = self.conf.get('forcing_member', self.conf.get('member', None))
        if forcing_member is not None and not isinstance(forcing_member, int):
            forcing_member = FPList(forcing_member)
        # forcing_geometry value may depend on the task's output 'geometry' value
        if 'forcing_geometry' in self.conf:
            if isinstance(self.conf.forcing_geometry, dict):
                forcing_geometry = self.conf.forcing_geometry[self.conf.geometry.tag]
            else:
                forcing_geometry = self.conf.forcing_geometry
        else:
            forcing_geometry = self.conf.geometry
        # Security : in case of an ensemble of forcing files, get the FORCING of each member in a
        # separate directory to avoid overwrinting files.
        if (isinstance(forcing_member, list) and len(forcing_member) > 1 and '[member]' not in localname):
            localname = f'mb[member]/{localname}'
        # TODO : modifier le namebuilder par defaut lorsque le nouveau incluant la
        # géométrie sera disponible
        forcing_namebuild = self.conf.get('forcing_namebuild', 'flat@cen')
        forcing_intent    = self.conf.get('forcing_intent', 'in')
        # TODO : ne pas utiliser de source_app / source_conf à l'avenir
        forcing_source_app  = self.conf.get('forcing_source_app', None)
        forcing_source_conf = self.conf.get('forcing_source_conf', None)
        forcing_cutoff = self.conf.get('forcing_cutoff', None)
        vortex1        = self.conf.get('forcing_vortex1', False),

        duration = self.conf.get('io_duration', 'yearly')
        list_dates_begin, list_dates_end, _, _ = get_list_dates_files(Date(forcing_datebegin),
                Date(forcing_dateend), duration)
        dict_dates_end = get_dic_dateend(list_dates_begin, list_dates_end)

        # Verrue pour gérer les footprints *source_app* et *source_conf* de la réanalyse S2M
        if 'forcing_source' in self.conf:
            forcing_source_app, forcing_source_conf = \
                self.get_safran_sources(list_dates_begin, era5=self.conf.forcing_source == 'era5')

        self.sh.title(f'Input forcing ({duration} duration)')
        forcing = vortex.input(
            role           = 'Forcing',  # Used for parallelisation and alternates only
            kind           = 'MeteorologicalForcing',
            nativefmt      = 'netcdf',
            datebegin      = list_dates_begin,
            dateend        = dict_dates_end,
            experiment     = forcing_xpid,  # default : self.conf.xpid
            username       = forcing_user,
            geometry       = forcing_geometry,  # default : self.conf.geometry
            local          = localname,
            vapp           = forcing_vapp,  # default : self.conf.vapp
            vconf          = forcing_vconf,  # default : self.conf.vconf
            block          = forcing_block,  # default : 'meteo' ?
            member         = forcing_member,  # default : None
            intent         = forcing_intent,  # default : 'in' ?
            namespace      = namespace,  # default : 'vortex.multi.fr',
            namebuild      = forcing_namebuild,  # default recherche : 'flat@cen', defaut oper : None
            vortex1        = vortex1,
            date           = '[dateend]',  # TODO : à supprimer (cas recherche uniquement)
            source_app     = forcing_source_app,  # default = None (ne pas refaire l'erreur)
            source_conf    = forcing_source_conf,  # default = None (ne pas refaire l'erreur)
            cutoff         = forcing_cutoff,  # TODO : à supprimer dans le cas recherche
            fatal          = True,
        ),
        print(t.prompt, 'FORCING =', forcing)
        print()
