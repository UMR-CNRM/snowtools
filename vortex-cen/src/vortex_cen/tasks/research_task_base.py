# -*- coding: utf-8 -*-
'''
'''

from vortex.layout.nodes import Task
from vortex.cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date
from footprints.stdtypes import FPDict
from vortex.tools.env import Environment
# from vortex.syntax.stdattrs import Namespace

from vortex.cen.tools.monitoring import InputReportContext, OutputReportContext, TestReportContext

from snowtools.utils.dates import get_list_dates_files, get_dic_dateend


class _CenResearchTask(Task, S2MTaskMixIn):
    '''
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
    * put_local_outputs
    * put_remote_inputs

    See their respective documentation for more details.

    Doc :
    http://intra.cnrm.meteo.fr/algopy/trainings/vortex_dev2022_1/presentation/beamer/vortex_dev_jobs2_presentation.pdf

    TODO:
    1. Make a separate abstract task for real-time applications ?
    2. Move methods from S2MTaskMixIn here (or research-specific methods in case of #1) ?

    '''

    def defaults(self, extras):
        """
        Set toolbox defaults, extended with actual arguments ``extras``.
        """

        if 'localtest' in self.conf:
            toolbox.active_now = False

        toolbox.defaults(
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
        if '@' not in self.conf.xpid:
            username = Environment()['logname']
            self.conf.xpid = f'{self.conf.xpid}@{username}'

        # Temporary security to avoid the *date* footprint to be mandatory for SurfaceIO resources.
        # This will be useless once the date footprint will be properly set as optional for research applications
        if 'date' in self.conf:
            toolbox.defaults['date'] = self.conf.date
        else:
            if 'rundate' in self.conf:
                toolbox.defaults['date'] = self.conf.rundate
            elif 'dateend' in self.conf:
                toolbox.defaults['date'] = self.conf.dateend

        for optk in ('cutoff', 'geometry', 'cycle', 'vortex_set_aside'):
            if optk in self.conf:
                value = self.conf.get(optk)
                if isinstance(value, dict):
                    value = FPDict(value)
                toolbox.defaults[optk] = value

        toolbox.defaults(**extras)
        self.header('Toolbox defaults')
        toolbox.defaults.show()

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
        if 'io_duration' in self.conf:
            self.get_list_dates(duration=self.conf.io_duration)
        else:
            self.get_list_dates()

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
            self.get_local_inputs()

        if 'compute' in self.steps:
            # The actual computations... (usually a call to the run method of an AlgoComponent)
            # This is executed on a COMPUTE NODE.
            algo = self.algo()
            if 'localtest' not in self.conf:
                self.launch_algo(algo)

        if 'backup' in self.steps:
            # In a multi step job (MTOOL, ...), this step will be run, on a COMPUTE NODE,
            # just after the computations. It is the appropriate place to put data in the local cache
            # in order to make it available to a subsequent step.
            self.put_local_outputs()

        if 'late-backup' in self.steps:
            # In a multi step job (MTOOL, ...), this step will be run on a TRANSFER NODE.
            # Consequently, most of the data should be archived here.
            with OutputReportContext(self, t):
                self.put_remote_outputs()

            if 'test' in self.conf and 'localtest' not in self.conf:
                # In test cases, some diff with reference output could be necessary.
                # In this case, implement the in the "unittest" method.
                with TestReportContext(self, t):
                    self.unittest()

        if 'late-backup' in self.steps and self.debug:
            # Debug mode : make the job crash at the end to preserve the working directory
            print('=====================================================================================')
            print('=====================================================================================')
            raise Exception('INFO :The execution went well, do not take into account the following error')

    def get_remote_inputs(self):
        """
        Implement this method in your task to fetch all resources stored remotely (on Hendrix, sxcen,...) from
        a transfer node.
        """
        # raise NotImplementedError()
        pass

    def get_local_inputs(self):
        """
        Implement this method in your task to fetch all resources already stored on the local (HPC) cache from a
        compute node.
        """
        self.get_remote_inputs()  # TODO : check if really necessary / good practice

    def algo(self):
        """
        Implement this method to call your task's algo component.
        This method should return a valid AlgoComponent object.
        """
        # raise NotImplementedError()
        return None

    def launch_algo(self, algo, **kw):
        """
        Run your task's algo component.
        """
        if algo is not None:
            algo.run()

    def put_local_outputs(self):
        """
        Implement this method in your task to save resources on the local (HPC) cache from a compute node.
        """
        self.put_remote_outputs()  # TODO : check if really necessary / good practice

    def put_remote_outputs(self):
        """
        Implement this method in your task to save resources remotely (on Hendrix, sxcen,...) from a transfer node.
        """
        # raise NotImplementedError()
        pass

    def unittest(self):
        """
        Implement this method in unittest tasks to monitor the test results.
        """
        # raise NotImplementedError()
        pass

    def get_list_dates(self, duration='yearly'):
        """
        Get the list of datebegin/dateend corresponding to the different time periods covered by IO files
        from the actual simulation's datebegin/dateend arguments.

        :param duration: Time period covered by individual files.

        """
        if 'datebegin' in self.conf and 'dateend' in self.conf:
            self.list_dates_begin, list_dates_end, _, _  = get_list_dates_files(Date(self.conf.datebegin),
                    Date(self.conf.dateend), duration)
            self.dict_dates_end = get_dic_dateend(self.list_dates_begin, list_dates_end)
        elif 'date' in self.conf:  # Real-time only --> make a specific default class ?
            self.list_dates_begin = [self.conf.date]
            self.dict_dates_end   = {self.conf.date: self.conf.date}

    def get_forcing(self, localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc', alternate=True):
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

        :param forcing_datebegin: *datebegin* footprint, default self.conf.datebegin
        :type forcing_datebegin: str, footprints.stdtypes.FPList
        :param forcing_dateend: *dateend* footprint, default self.conf.dateend
        :type forcing_dateend: str, footprints.stdtypes.FPList
        :param forcing_xpid: Experiment identifier (format "experiment_name@user"), default self.conf.xpid
        :type forcing_xpid: str
        :param forcing_geometry: *geometry* footprint, default self.conf.geometry
        :type forcing_geometry: str, footprints.stdtypes.FPList
        :param forcing_vapp: *vapp* footprint, default self.conf.vapp
        :type forcing_vapp: str
        :param forcing_vconf: *vconf* footprint, default self.conf.vconf
        :type forcing_vconf: str
        :param forcing_block: *block* footprint, default "meteo"
        :type forcing_vconf: str
        :param forcing_namespace: *namespace* footprint, default "vortex.multi.fr"
        :type forcing_namespace: str

        :param forcing_date: *date* footprint (unsed with the research namebuilders), default to [dateend]
        :type forcing_date: str
        :param forcing_model: *model* footprint (to be made optional for SurfaceIO objects), default "safran"
        :type forcing_model: str

        Optionnal configuration variables:

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

        TODO : prévoir un mécanisme pour rendre des déclarer les arguments obligatoires / optionnels pour
        chaque tâche (ex: member)

        """

        t = self.ticket

        forcing_datebegin = self.conf.get('forcing_datebegin', self.conf.datebegin)
        forcing_dateend   = self.conf.get('forcing_dateend', self.conf.dateend)
        forcing_xpid      = self.conf.get('forcing_xpid', self.conf.xpid)
        forcing_geometry  = self.conf.get('forcing_geometry', self.conf.geometry)
        forcing_vapp      = self.conf.get('forcing_vapp', self.conf.vapp)
        forcing_vconf     = self.conf.get('forcing_vconf', self.conf.vconf)
        forcing_block     = self.conf.get('forcing_block', 'meteo')
        forcing_member    = self.conf.get('forcing_member', self.conf.get('member', None))
        # Security : in case of an ensemble of forcing files, get the FORCING of each member in a
        # separate directory to avoid overwrinting files.
        if (isinstance(forcing_member, list) and len(forcing_member) > 1 and '[member]' not in localname):
            localname = f'mb[member]/{localname}'
        forcing_namespace = self.conf.get('forcing_namespace', 'vortex.multi.fr')
        # TODO : modifier le namebuilder par defaut lorsque le nouveau incluant la
        # géométrie sera disponible
        forcing_namebuild = self.conf.get('forcing_namebuild', 'flat@cen')
        forcing_intent    = self.conf.get('forcing_intent', 'in')
        # TODO : ne pas utiliser de source_app / source_conf à l'avenir
        forcing_source_app  = self.conf.get('forcing_source_app', None)
        forcing_source_conf = self.conf.get('forcing_source_conf', None)
        # Verrue pour gérer les footprints *source_app* et *source_conf* de la réanalyse S2M
        if 'forcing_source' in self.conf:
            forcing_source_app, forcing_source_conf = \
                self.get_safran_sources([forcing_datebegin], era5=self.conf.forcing_source == 'era5')
        # TODO : à supprimer après suppression de ce footprint dans les objets "SurfaceIO"
        forcing_model = self.conf.get('forcing_model', 'safran')
        # TODO : à supprimer après suppression de ce footprint dans les objets "SurfaceIO"
        forcing_cutoff = self.conf.get('forcing_cutoff', None)

        self.sh.title('Toolbox input forcing (full simulation period)')
        forcing = toolbox.input(
            role           = 'Forcing',  # Used for parallelisation and alternates only
            kind           = 'MeteorologicalForcing',
            nativefmt      = 'netcdf',
            datebegin      = forcing_datebegin,  # default : self.conf.datebegin
            dateend        = forcing_dateend,  # default : self.conf.dateend
            experiment     = forcing_xpid,  # default : self.conf.xpid
            geometry       = forcing_geometry,  # default : self.conf.geometry
            local          = localname,
            vapp           = forcing_vapp,  # default : self.conf.vapp
            vconf          = forcing_vconf,  # default : self.conf.vconf
            block          = forcing_block,  # default : 'meteo' ?
            member         = forcing_member,  # default : None
            intent         = forcing_intent,  # default : 'in' ?
            namespace      = forcing_namespace,  # default : 'vortex.multi.fr',
            namebuild      = forcing_namebuild,  # default recherche : 'flat@cen', defaut oper : None
            date           = '[dateend]',  # TODO : à supprimer (cas recherche uniquement)
            source_app     = forcing_source_app,  # default = None (ne pas refaire l'erreur)
            source_conf    = forcing_source_conf,  # default = None (ne pas refaire l'erreur)
            cutoff         = forcing_cutoff,  # TODO : à supprimer dans le cas recherche
            model          = forcing_model,  # TODO : à supprimer
            fatal          = False,  # Do not crash now, there is an alternative
        ),
        print(t.prompt, 'FORCING =', forcing)
        print()

        if alternate:

            # TODO : ne plus faire d'alternate, prescrire obligatoirement le duration (plus rapide)
            # 2 cas : 'Yearly', + exception pour 1er / dernier fichiers
            # NB : eviter les alternates dans les tâches

            # Sécurité si *forcing_datebegin* != *datebegin* ou *forcing_dateend* != *dateend*
            if 'io_duration' in self.conf:
                duration = self.conf.io_duration
            else:
                duration = 'yearly'
            list_dates_begin, list_dates_end, _, _ = get_list_dates_files(Date(forcing_datebegin),
                    Date(forcing_dateend), duration)
            dict_dates_end = get_dic_dateend(list_dates_begin, list_dates_end)

            # Verrue pour gérer les footprints *source_app* et *source_conf* de la réanalyse S2M
            if 'forcing_source' in self.conf:
                forcing_source_app, forcing_source_conf = \
                    self.get_safran_sources(list_dates_begin, era5=self.conf.forcing_source == 'era5')

            self.sh.title('Toolbox input forcing (sub-periods)')
            forcing = toolbox.input(
                alternate      = 'Forcing',
                kind           = 'MeteorologicalForcing',
                nativefmt      = 'netcdf',
                datebegin      = list_dates_begin,
                dateend        = dict_dates_end,
                experiment     = forcing_xpid,  # default : self.conf.xpid
                geometry       = forcing_geometry,  # default : self.conf.geometry
                local          = localname,
                vapp           = forcing_vapp,  # default : self.conf.vapp
                vconf          = forcing_vconf,  # default : self.conf.vconf
                block          = forcing_block,  # default : 'meteo' ?
                member         = forcing_member,  # default : None
                intent         = forcing_intent,  # default : 'in' ?
                namespace      = forcing_namespace,  # default : 'vortex.multi.fr',
                namebuild      = forcing_namebuild,  # default recherche : 'flat@cen', defaut oper : None
                date           = '[dateend]',  # TODO : à supprimer dans le cas recherche
                source_app     = forcing_source_app,  # default = None (ne pas refaire l'erreur)
                source_conf    = forcing_source_conf,  # default = None (ne pas refaire l'erreur)
                cutoff         = forcing_cutoff,  # TODO : à supprimer dans le cas recherche
                model          = forcing_model,  # TODO : à supprimer
                fatal          = True,  # This is the last try, crash in case of failure
            ),
            print(t.prompt, 'FORCING (alternate) =', forcing)
            print()
