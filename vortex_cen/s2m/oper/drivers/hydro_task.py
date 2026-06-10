# -*- coding: utf-8 -*-
"""
"""
from bronx.stdtypes.date import yesterday
from mkjob.nodes import Driver, Task
from vortex_cen.layout.nodes import S2MTaskMixIn
import vortex
import footprints


def setup(t, **kw):
    return Driver(
        tag='S2M_Hydro',
        ticket=t,
        nodes=[
                Hydro_Task(tag='S2M_Hydro_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail')],
        options=kw
    )


class Hydro_Task(S2MTaskMixIn, Task):
    """
    Task : Hydro_Task
    =================

    Post-processing task for the 4 seasons bulletin. Uses S2m ensemble forecasts based on PEARP.
    Calculates ensemble deciles for the 12hourly and 1day snow accumulation for the moment.

    Inputs
    ------
    - areas.nc : Description of the hydrological areas
    - FORCING.nc : Ensemble of forecast meterological forcing files
    - PRO.nc : Ensemble of forecast snowpack simulaiton files

    Outputs
    -------
    - HYDRO.nc : Hydrological post-processing file

    """
    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "rundate+help=Date of run;choices=YYYYMMDD[03 06 09 12];type=str or Date",  # used in the "get_period" method
        "previ+help=Activate forecast mode;type=bool",  # used in the "get_period" method
        "namespace_in+help=Where to look for nwp files;type=str",
        "namespace_out+help=Where to store output guess files;type=str",
        "geometry",
        "cycle+help=Alias for uenv;type=str",
    ]
    OPTIONAL_CONFIGURATION_VARIABLES = [
    ]

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()

        members_out = [35] # List of members for archive

        if self.conf.previ:
            pearpmembers, members = self.get_list_members(sytron=False)
            cutoff = 'production'
            kindalgo = 's2m_hydro_ensemble'
            if self.conf.rundate.hour == self.nightruntime.hour:
                datebegin_forcing = datebegin
            else:
                datebegin_forcing = yesterday(datebegin)
            datebegin_pro = datebegin
        else:
            members = [35] # List of members for computation
            cutoff = 'assimilation'
            kindalgo = 's2m_hydro_deter'
            datebegin_forcing = datebegin
            if self.conf.rundate.hour == self.nightruntime.hour:
                datebegin_pro = yesterday(dateend)
            else:
                datebegin_pro = datebegin

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            if True: # In order to have an indentation and facilitate the comparison with IGA Task

                self.sh.title('Toolbox input tb01')
                tb01 = vortex.input(
                    role        = 'HydroAreas',
                    local       = 'areas.nc',
                    geometry    = self.conf.geometry,
                    nativefmt   = 'netcdf',
                    kind        = 'surfhydro',
                    model       = 'surfex',
                    genv = self.conf.cycle,
                    gvar = '[kind]_[geometry::tag]',
                    fatal       = True
                ),

                print(t.prompt, 'tb01 =', tb01)
                print()

        if 'fetch' in self.steps:

            if True:  # In order to have an indentation and facilitate the comparison with IGA Task

                self.sh.title('Toolbox input tb01')
                tb02 = vortex.input(
                    role        = 'SafranForecast',
                    local       = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment  = self.conf.xpid,
                    block       = 'meteo',
                    geometry    = self.conf.geometry,
                    date        = rundate_forcing,
                    datebegin   = datebegin_forcing,
                    dateend     = dateend,
                    member      = members,
                    nativefmt   = 'netcdf',
                    kind        = 'MeteorologicalForcing',
                    model       = 's2m',
                    namespace   = self.conf.namespace_in,
                    cutoff      = cutoff,
                    fatal       = False
                ),
                print(t.prompt, 'tb02 =', tb02)
                print()

                self.sh.title('Toolbox input tb02')
                tb03 = vortex.input(
                    role        = 'CrocusForecast',
                    local       = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment  = self.conf.xpid,
                    block       = 'pro',
                    geometry    = self.conf.geometry,
                    date        = self.conf.rundate,
                    datebegin   = datebegin_pro,
                    dateend     = dateend,
                    member      = members,
                    nativefmt   = 'netcdf',
                    kind        = 'SnowpackSimulation',
                    model       = 'surfex',
                    namespace   = self.conf.namespace_in,
                    cutoff      = cutoff,
                    fatal       = False
                ),
                print(t.prompt, 'tb03 =', tb03)
                print()

        if 'compute' in self.steps:
            self.sh.title('Toolbox algo tb02 = Postprocessing')

            tb03 = tbalgo1 = vortex.task(
                kind        = kindalgo,
                varnames    = ['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'],
                dateinit    = datebegin,
                engine      = 's2m',
                members     = footprints.util.rangex(members),
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.component_runner(tbalgo1[0])

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            if True:  # In order to have an indentation and facilitate the comparison with IGA Task

                self.sh.title('Toolbox output tb04')
                tb04 = vortex.output(
                    role        = 'Postproc_output',
                    intent      = 'out',
                    local       = 'mb[member]/HYDRO.nc',
                    experiment  = self.conf.xpid,
                    block       = 'hydro',
                    geometry    = self.conf.geometry,
                    date        = self.conf.rundate,
                    datebegin   = datebegin_pro,
                    dateend     = dateend,
                    member      = members_out,
                    nativefmt   = 'netcdf',
                    kind        = 'SnowpackSimulation',
                    model       = 'postproc',
                    namespace   = self.conf.namespace_out,
                    cutoff      = cutoff,
                    fatal       = True
                ),
                print(t.prompt, 'tb04 =', tb04)
                print()

                if kindalgo == 's2m_hydro_ensemble':

                    self.sh.title('Toolbox output tb04')
                    tb04 = vortex.output(
                        role        = 'Postproc_output',
                        intent      = 'out',
                        local       = 'HYDRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment  = self.conf.xpid,
                        block       = 'hydro',
                        geometry    = self.conf.geometry,
                        date        = self.conf.rundate,
                        datebegin   = datebegin_pro,
                        dateend     = dateend,
                        nativefmt   = 'netcdf',
                        kind        = 'SnowpackSimulation',
                        model       = 'postproc',
                        namespace   = self.conf.namespace_out,
                        cutoff      = cutoff,
                        fatal       = True
                    ),
                    print(t.prompt, 'tb04 =', tb04)
                    print()
