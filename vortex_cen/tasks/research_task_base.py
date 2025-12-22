# -*- coding: utf-8 -*-
'''
Created on 18 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date
from footprints.stdtypes import FPDict
from vortex.syntax.stdattrs import Namespace

from vortex_cen.tools.monitoring import InputReportContext, OutputReportContext

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

        toolbox.defaults(
            # namespace      = self.conf.get('namespace', Namespace('vortex.multi.fr')),
            namespace      = Namespace('vortex.multi.fr'),
            # date           = '[dateend]',  # WARNING : research only
            # TODO : the 'date' footprint is to be removed for research applications
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            vapp           = self.conf.vapp,
            vconf          = '[geometry:tag]',  # TODO : à modifier après changement de convention
            # model          = self.conf.model,
            # namebuild      = 'flat@cen',  # WARNING : research only !
            nativefmt      = 'netcdf',
        )

        if 'date' not in self.conf:
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

        :param io_duration: Argument similar to the one of the `get_list_dates_files` method (snowtools/utils/dates.py).
                            Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
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

        if 'early-fetch' in self.steps:  # Executed on a TRANSFERT NODE to fetch inputs from a remote cache
            with InputReportContext(self, t):
                self.get_remote_inputs()

        if 'fetch' in self.steps:  # Executed on a COMPUTE NODE to fetch resources already in the local cache
            with InputReportContext(self, t):
                self.get_local_inputs()

        if 'compute' in self.steps:  # Algo component (1 per task) executed on the compute node
            self.algo()

        if 'backup' in self.steps:  # Execute on the COMPUTE NODE to save outputs on the local cache
            with OutputReportContext(self, t):
                self.put_local_outputs()

        if 'late-backup' in self.steps:  # Executed on a TRANSFERT NODE to save outputs to a remote destination
            with OutputReportContext(self, t):
                self.put_remote_outputs()

        # Debug mode : make the job crash at the end topreserve the working directory
        if 'late-backup' in self.steps and self.debug:
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
        # self.get_remote_inputs()  # TODO : check if really necessary / good practice
        pass

    def algo(self):
        """
        Implement this method to call your task's algo component.
        """
        # raise NotImplementedError()
        pass

    def put_local_outputs(self):
        """
        Implement this method in your task to save resources on the local (HPC) cache from a compute node.
        """
        # self.put_remote_outputs()  # TODO : check if really necessary / good practice
        pass

    def put_remote_outputs(self):
        """
        Implement this method in your task to save resources remotely (on Hendrix, sxcen,...) from a transfer node.
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

    def get_forcing(self, localname):
        """
        Method to get meteorological forcing file(s) covering the simulation period.
        First, check if an existing forcing file covers the full simulation period.
        If no such file exists, check for files covering standard sub-periods (yearly or monthly files).

        Arguments:
        :param localname: *local* footprint (how to name the file in the working directory).
                          This is an algo/task-specific argument.
        :type localname: str

        Mandatory configuration variables:

        :param forcing_datebegin: *datebegin* footprint, default self.conf.datebegin
        :type forcing_datebegin: str
        :param forcing_dateend: *dateend* footprint, default self.conf.dateend
        :type forcing_dateend: str
        :param forcing_xpid: *experiment footprint (format "experiment_name@user"), default self.conf.xpid
        :type forcing_xpid: str
        :param forcing_geometry: *geometry* footprint, default self.conf.geometry
        :type forcing_geometry: str
        :param forcing_vapp: *vapp* footprint, default self.conf.vapp
        :type forcing_vapp: str
        :param forcing_vconf: *vconf* footprint, default self.conf.vconf
        :type forcing_vconf: str

        Optionnal configuration variables:

        :param forcing_block: *block* footprint, default "meteo"
        :type forcing_vconf: str
        :param forcing_member: *member* footprint, default None
        :type forcing_vconf: int, footprints.stdtypes.FPList
        :param forcing_namespace: *namespace* footprint, default "vortex.multi.fr"
        :type forcing_namespace: str
        :param forcing_namebuild: *namebuild* footprint, default "flat@cen" (will change soon)
        :type forcing_namebuild: str
        :param forcing_intent: *intent* footprint (local file permissions), default "in"
                               Possible values : "in" (read-only), "inout" (read-write)
        :type forcing_intent: str
        :param forcing_namebuild: *namebuild* footprint, default "flat@cen" (will change soon)
        :type forcing_namebuild: str
        :param forcing_source_app: *source_app* footprint, default None
        :type forcing_source_app: str
        :param forcing_source_conf: *source_conf* footprint, default None
        :type forcing_source_conf: str
        :param forcing_source: Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
                               Possible values : 'era5', 'era40'
        :type forcing_source: str
        :param forcing_model: *model* footprint (to be made optional for SurfaceIO objects), default "safran"
        :type forcing_model: str
        :param forcing_cutoff: *cutoff* footprint (to be made optional for SurfaceIO objects), default None
        :type forcing_cutoff: str
        :param io_duration: Argument similar to the one of the `get_list_dates_files` method (snowtools/utils/dates.py).
                            Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
                            Possible values : "yearly", "monthly" or "full"
        :type io_duration: str
        """

        t = self.ticket

        if 'forcing_datebegin' not in self.conf:
            self.conf.forcing_datebegin = self.conf.datebegin
        if 'forcing_dateend' not in self.conf:
            self.conf.forcing_dateend = self.conf.dateend
        if 'forcing_xpid' not in self.conf:
            self.conf.forcing_xpid = self.conf.xpid
        if 'forcing_geometry' not in self.conf:
            self.conf.forcing_geometry = self.conf.geometry
        if 'forcing_vapp' not in self.conf:
            self.conf.forcing_vapp = self.conf.vapp
        if 'forcing_vconf' not in self.conf:
            self.conf.forcing_vconf = self.conf.vconf
        if 'forcing_block' not in self.conf:
            self.conf.forcing_block = 'meteo'
        if 'forcing_member' not in self.conf:
            self.conf.forcing_member = None
        if 'forcing_namespace' not in self.conf:
            self.conf.forcing_namespace = 'vortex.multi.fr'
        # TODO : modifier le namebuilder par defaut lorsque le nouveau incluant la
        # géométrie sera disponible
        if 'forcing_namebuild' not in self.conf:
            self.conf.forcing_namebuild = 'flat@cen'
        if 'forcing_intent' not in self.conf:
            self.conf.forcing_intent = 'in'
        # TODO : ne pas utiliser de source_app / source_conf à l'avenir
        if 'forcing_source_app' not in self.conf:
            self.conf.forcing_source_app = None
        if 'forcing_source_conf' not in self.conf:
            self.conf.forcing_source_conf = None
        # Verrue pour gérer les footprints *source_app* et *source_conf* de la réanalyse S2M
        if 'forcing_source' in self.conf:
            self.conf.forcing_source_app, self.conf.forcing_source_conf = \
                self.get_safran_sources([self.conf.forcing_datebegin], era5=self.conf.forcing_source == 'era5')
        # TODO : à supprimer après suppression de ce footprint dans les objets "SurfaceIO"
        if 'forcing_model' not in self.conf:
            self.conf.forcing_model = 'safran'
        # TODO : à supprimer après suppression de ce footprint dans les objets "SurfaceIO"
        if 'forcing_cutoff' not in self.conf:
            self.conf.forcing_cutoff = None

        self.sh.title('Toolbox input forcing (full simulation period)')
        forcing = toolbox.input(
            role           = 'Forcing',  # Used for parallelisation and alternates only
            kind           = 'MeteorologicalForcing',
            nativefmt      = 'netcdf',
            datebegin      = self.conf.forcing_datebegin,  # default : self.conf.datebegin
            dateend        = self.conf.forcing_dateend,  # default : self.conf.dateend
            experiment     = self.conf.forcing_xpid,  # default : self.conf.xpid
            geometry       = self.conf.forcing_geometry,  # default : self.conf.geometry
            local          = localname,
            vapp           = self.conf.forcing_vapp,  # default : self.conf.vapp
            vconf          = self.conf.forcing_vconf,  # default : self.conf.vconf
            block          = self.conf.forcing_block,  # default : 'meteo' ?
            member         = self.conf.forcing_member,  # default : None
            intent         = self.conf.forcing_intent,  # default : 'in' ?
            namespace      = self.conf.forcing_namespace,  # default : 'vortex.multi.fr',
            namebuild      = self.conf.forcing_namebuild,  # default recherche : 'flat@cen', defaut oper : None
            date           = '[dateend]',  # TODO : à supprimer dans le cas recherche
            source_app     = self.conf.forcing_source_app,  # default = None (ne pas refaire l'erreur)
            source_conf    = self.conf.forcing_source_conf,  # default = None (ne pas refaire l'erreur)
            cutoff         = self.conf.forcing_cutoff,  # TODO : à supprimer dans le cas recherche
            model          = self.conf.forcing_model,  # TODO : à supprimer
            fatal          = False,
        ),
        print(t.prompt, 'FORCING =', forcing)
        print()

        # Sécurité si *forcing_datebegin* != *datebegin* ou *forcing_dateend* != *dateend*
        if 'io_duration' in self.conf:
            duration = self.conf.io_duration
        else:
            duration = 'yearly'
        list_dates_begin, list_dates_end, _, _ = get_list_dates_files(Date(self.conf.forcing_datebegin),
                Date(self.conf.forcing_dateend), duration)
        dict_dates_end = get_dic_dateend(list_dates_begin, list_dates_end)

        # Verrue pour gérer les footprints *source_app* et *source_conf* de la réanalyse S2M
        if 'forcing_source' in self.conf:
            self.conf.forcing_source_app, self.conf.forcing_source_conf = \
                self.get_safran_sources(list_dates_begin, era5=self.conf.forcing_source == 'era5')

        self.sh.title('Toolbox input forcing (sub-periods)')
        forcing = toolbox.input(
            alternate      = 'Forcing',
            kind           = 'MeteorologicalForcing',
            nativefmt      = 'netcdf',
            datebegin      = list_dates_begin,
            dateend        = dict_dates_end,
            experiment     = self.conf.forcing_xpid,  # default : self.conf.xpid
            geometry       = self.conf.forcing_geometry,  # default : self.conf.geometry
            local          = localname,
            vapp           = self.conf.forcing_vapp,  # default : self.conf.vapp
            vconf          = self.conf.forcing_vconf,  # default : self.conf.vconf
            block          = self.conf.forcing_block,  # default : 'meteo' ?
            member         = self.conf.forcing_member,  # default : None
            intent         = self.conf.forcing_intent,  # default : 'in' ?
            namespace      = self.conf.forcing_namespace,  # default : 'vortex.multi.fr',
            namebuild      = self.conf.forcing_namebuild,  # default recherche : 'flat@cen', defaut oper : None
            date           = '[dateend]',  # TODO : à supprimer dans le cas recherche
            source_app     = self.conf.forcing_source_app,  # default = None (ne pas refaire l'erreur)
            source_conf    = self.conf.forcing_source_conf,  # default = None (ne pas refaire l'erreur)
            cutoff         = self.conf.forcing_cutoff,  # TODO : à supprimer dans le cas recherche
            model          = self.conf.forcing_model,  # TODO : à supprimer
            fatal          = True,
        ),
        print(t.prompt, 'FORCING (alternate) =', forcing)
        print()
