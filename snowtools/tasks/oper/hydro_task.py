# -*- coding: utf-8 -*-
"""
Created on 15 oct. 2025

@author: lafaysse
"""
from bronx.stdtypes.date import yesterday, Date
from vortex.layout.nodes import Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
import footprints


class _Hydro_Task(S2MTaskMixIn, Task):
    """
    Base class for hydrological post-processing tasks.

    Workflow:
    =========

    Inputs :
    --------
    - Meteorological forcing file(s) --> method "get_forcing"
    - Snowpack simulation (PRO) file(s) --> method "get_pro"
    - A NetCDF file describing the output hygrological units --> method "get_const"

    Outputs :
    ---------
    - HYDRO diagnostic(s) --> method "put_diagnostic"

    """

    def pre_process(self):

        # Verrue pour gérer l'équivalence entre les variables "previ" et "cutoff"
        self.conf.previ = self.conf.cutoff == 'production'

        self.datebegin, self.dateend = self.get_period()
        self.datebegin_forcing = self.get_datebegin_forcing()
        self.datebegin_pro = self.get_datebegin_pro()
        self.members = footprints.util.rangex(self.conf.members)

    def process(self):

        self.pre_process()

        if True:  # with OpInputReportContext:

            if 'early-fetch' in self.steps:  # Executed on a TRANSFERT NODE to fetch inputs from a remote cache
                self.get_remote_inputs()

            if 'fetch' in self.steps:  # Executed on a COMPUTE NODE to fetch resources already in the local cache
                self.get_local_inputs()

        if 'compute' in self.steps:  # Algo component (1 per task) executed on the compute node
            self.algo()

        if True:  # with OpOutputReportContext:

            if 'backup' in self.steps:  # Execute on the COMPUTE NODE to save outputs on the local cache
                self.put_local_outputs()

            if 'late-backup' in self.steps:  # Executed on a TRANSFERT NODE to save outputs to a remote destination
                self.put_remote_outputs()

    def get_const(self):
        """
        Get the NetCDF file describing the output hygrological units from a UserEnvironment.
        """

        self.sh.title('Toolbox input hydro areas')
        self.hydro_areas = toolbox.input(
            role        = 'HydroAreas',
            local       = 'areas.nc',
            geometry    = self.geometry_areas,
            nativefmt   = 'netcdf',
            kind        = 'surfhydro',
            model       = 'surfex',
            genv        = self.conf.cycle,
            gvar        = '[kind]_[geometry::tag]',
            fatal       = True
        ),
        print(self.ticket.prompt, 'hydro_areas =', self.hydro_areas)
        print()

    def get_forcing(self):
        """
        Get the Meteorological FORCING file(s).
        """
        self.sh.title('Toolbox input FORCING')
        self.forcing = toolbox.input(
            role          = 'SafranForecast',
            local         = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment    = self.conf.xpid_inputhydro,
            block         = 'meteo',
            geometry      = self.conf.geometry,
            date          = self.rundate_forcing,
            datebegin     = self.datebegin_forcing,
            dateend       = self.dateend,
            member        = self.members,
            nativefmt     = 'netcdf',
            kind          = 'MeteorologicalForcing',
            model         = 's2m',
            namespace     = self.conf.namespace_in,
            cutoff        = self.conf.cutoff,
            fatal         = False
        ),
        print(self.ticket.prompt, 'FORCING input =', self.forcing)
        print()

    def get_pro(self):
        """
        Get the snowpack simulation file(s)
        """

        self.sh.title('Toolbox input PRO')
        self.pro = toolbox.input(
            role         = 'CrocusForecast',
            local        = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment   = self.conf.xpid_inputhydro,
            block        = 'pro',
            geometry     = self.conf.geometry,
            date         =self.conf.rundate,
            datebegin    = self.datebegin_pro,
            dateend      = self.dateend,
            member       = self.members,
            nativefmt    = 'netcdf',
            kind         = 'SnowpackSimulation',
            model        = 'surfex',
            namespace    = self.conf.namespace_in,
            cutoff       = self.conf.cutoff,
            fatal        = False
        ),
        print(self.ticket.prompt, 'PRO input =', self.pro)
        print()

    def algo(self):
        """
        Implement this method for each actual task inherinting from this abstract class
        """

    def put_diagnostic(self):
        """
        Save the produced diagnostic file(s)
        """

        self.sh.title('Toolbox output HYDRO')
        self.hydro = toolbox.output(
            role         = 'Postproc_output',
            intent       = 'out',
            local        = 'HYDRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment   = self.conf.xpid,
            block        = 'hydro',
            geometry     = self.conf.geometry,
            date         = self.conf.rundate,
            datebegin    = self.datebegin_pro,
            dateend      = self.dateend,
            nativefmt    = 'netcdf',
            kind         = 'SnowpackSimulation',
            model        = 'postproc',
            namespace    = self.conf.namespace_out,
            cutoff       = self.conf.cutoff,
            fatal        = True,
        ),
        print(self.ticket.prompt, 'Hydro diags =', self.hydro)
        print()

    @property
    def geometry_areas(self):
        """
        Mapping between old S2M geometries (before 22/10/2022) and new ones.
        """
        oldgeometries = dict(alp27_allslopes='alp_allslopes', pyr24_allslopes='pyr_allslopes')
        if self.datebegin < Date(2022, 10, 22):
            return oldgeometries[self.conf.geometry.tag]
        else:
            return self.conf.geometry

    @property
    def rundate_forcing(self):
        return self.get_rundate_forcing()


class Hydro_Task_Analysis(_Hydro_Task):
    """
    OPERATIONNAL TASK

    Application of the hydrogical post-processing in an operationnal analysis context.

    The diagnostics are computed for the deterministic member alone.

    "Flow" resources come from a previous execution of SURFEX and are present on the local cache
    """

    def get_remote_inputs(self):
        pass

    def get_local_inputs(self):

        self.get_const()
        self.get_forcing()
        self.get_pro()

    def get_algo(self):

        self.sh.title('Toolbox algo htdro analysis')
        self.algo = toolbox.algo(
            kind        = 's2m_hydro_deter',
            varnames    = ['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'],
            dateinit    = self.datebegin,
            engine      = 's2m',
            members     = self.members,
        ),
        print(self.ticket.prompt, 'algo =', self.algo)
        print()
        self.component_runner(self.algo)

    def put_local_inputs(self):
        self.put_diagnostic()

    def put_remote_inputs(self):
        self.put_diagnostic()

    def get_datebegin_forcing(self):
        return self.datebegin

    def get_datebegin_pro(self):
        return self.datebegin


class Hydro_Task_Forecast(_Hydro_Task):
    """
    OPERATIONNAL TASK

    Application of the hydrogical post-processing in an operationnal forecast context.

    The diagnostics are computed for all 36 ensemble members.

    "Flow" resources come from a previous execution of SURFEX and are present on the local cache
    """

    def get_remote_inputs(self):
        pass

    def get_local_inputs(self):

        self.get_const()
        self.get_forcing()
        self.get_pro()

    def get_algo(self):

        self.sh.title('Toolbox algo hydro forecast')
        self.algo = toolbox.algo(
            kind        = 's2m_hydro_ensemble',
            varnames    = ['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'],
            dateinit    = self.datebegin,
            engine      = 's2m',
            members     = self.members,
        ),
        print(self.ticket.prompt, 'algo =', self.algo)
        print()

        self.component_runner(self.algo)

    def put_local_inputs(self):
        self.put_diagnostic()

    def put_remote_inputs(self):
        self.put_diagnostic()

    def get_datebegin_forcing(self):
        return self.datebegin if self.conf.rundate.hour == self.nightruntime.hour else yesterday(self.datebegin)

    def get_datebegin_pro(self):
        return self.datebegin


class Hydro_Task_Analysis_Replay(Hydro_Task_Analysis):
    """
    RESEARCH TASK

    Replay of the hydrogical post-processing in an analysis context for past situations.

    "Flow" resources may not be present on the local cache, they must be retrieved from the archive store.

    """

    def get_remote_inputs(self):
        self.get_forcing()
        self.get_pro()

    def get_local_inputs(self):
        self.get_const()
        self.get_forcing()
        self.get_pro()

    def put_local_inputs(self):
        self.put_diagnostic()

    def put_remote_inputs(self):
        self.put_diagnostic()


class Hydro_Task_Forecast_Replay(Hydro_Task_Forecast):
    """
    RESEARCH TASK

    Replay of the hydrogical post-processing in a forecast context for past situations.

    "Flow" resources may not be present on the local cache, they must be retrieved from the archive store.
    """

    def get_remote_inputs(self):
        self.get_forcing()
        self.get_pro()

    def get_local_inputs(self):
        self.get_const()
        self.get_forcing()
        self.get_pro()

    def put_local_inputs(self):
        self.put_diagnostic()

    def put_remote_inputs(self):
        self.put_diagnostic()
