# -*- coding: utf-8 -*-
"""
Created on 15 oct. 2025

@author: lafaysse
"""
from bronx.stdtypes.date import yesterday, Date
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
import footprints


def setup(t, **kw):
    return Driver(
        tag='S2M_Hydro',
        ticket=t,
        nodes=[
                Hydro_Task_Analysis_Replay(tag='S2M_Hydro_Task', ticket=t, **kw,
                                           delay_component_errors=True, on_error='delayed_fail')],
        options=kw
    )


class _Hydro_Task(S2MTaskMixIn, Task):

    @property
    def geometry_areas(self):
        return self.conf.geometry

    @property
    def rundate_forcing(self):
        return self.get_rundate_forcing()

    def get_dates(self):
        self.datebegin, self.dateend = self.get_period()

    @property
    def xpid_inputhydro(self):
        return self.conf.xpid

    def get_const(self):
        if 'early-fetch' in self.steps or 'fetch' in self.steps:
            if True:  # In order to have an indentation and facilitate the comparison with IGA Task

                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role='HydroAreas',
                    local='areas.nc',
                    geometry=self.geometry_areas,
                    nativefmt='netcdf',
                    kind='surfhydro',
                    model='surfex',
                    genv=self.conf.cycle,
                    gvar='[kind]_[geometry::tag]',
                    fatal=True
                ),

                print(self.ticket.prompt, 'tb01 =', tb01)
                print()

    def get_input(self):
        if 'fetch' in self.steps:
            self.get_forcing()
            self.get_pro()

    def get_forcing(self):
        pass

    def get_pro(self):
        pass

    def get_algo(self):
        pass

    def save_output(self):
        pass

    def process(self):

        # Get dates
        self.get_dates()

        # Get constant files
        self.get_const()

        # Get input
        self.get_input()

        # Get algo and run
        self.get_algo()

        # Save output
        self.save_output()


class Hydro_Task_Analysis(_Hydro_Task):

    def get_forcing(self):
        if True:  # In order to have an indentation and facilitate the comparison with IGA Task

            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role='SafranForecast',
                local='mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment=self.xpid_inputhydro,
                block='meteo',
                geometry=self.conf.geometry,
                date=self.rundate_forcing,
                datebegin=self.datebegin,
                dateend=self.dateend,
                member=[35],
                nativefmt='netcdf',
                kind='MeteorologicalForcing',
                model='s2m',
                namespace=self.conf.namespace_in,
                cutoff='assimilation',
                fatal=False
            ),
            print(self.ticket.prompt, 'tb02 =', tb02)
            print()

    def get_pro(self):
        if True:

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role='CrocusForecast',
                local='mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment=self.xpid_inputhydro,
                block='pro',
                geometry=self.conf.geometry,
                date=self.conf.rundate,
                datebegin=yesterday(self.dateend),
                dateend=self.dateend,
                member=[35],
                nativefmt='netcdf',
                kind='SnowpackSimulation',
                model='surfex',
                namespace=self.conf.namespace_in,
                cutoff='assimilation',
                fatal=False
            ),
            print(self.ticket.prompt, 'tb03 =', tb03)
            print()

    def get_algo(self):

        if 'compute' in self.steps:
            self.sh.title('Toolbox algo tb02 = Postprocessing')

            tbalgo1 = toolbox.algo(
                kind        = 's2m_hydro_deter',
                varnames    = ['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'],
                dateinit    = self.datebegin,
                engine      = 's2m',
                members     = footprints.util.rangex([35]),
            ),
            print(self.ticket.prompt, 'tbalgo1 =', tbalgo1)
            print()

            self.component_runner(tbalgo1[0])

    def save_output(self):
        if 'late-backup' in self.steps:
            if True:

                self.sh.title('Toolbox output tb04')
                tb04 = toolbox.output(
                    role='Postproc_output',
                    intent='out',
                    local='mb[member]/HYDRO.nc',
                    experiment=self.conf.xpid,
                    block='hydro',
                    geometry=self.conf.geometry,
                    date=self.conf.rundate,
                    datebegin=yesterday(self.dateend),
                    dateend=self.dateend,
                    member=[35],
                    nativefmt='netcdf',
                    kind='SnowpackSimulation',
                    model='postproc',
                    namespace=self.conf.namespace_out,
                    cutoff='assimilation',
                    fatal=True
                ),
                print(self.ticket.prompt, 'tb04 =', tb04)
                print()


class Hydro_Task_Forecast(_Hydro_Task):

    @property
    def members(self):
        pearpmembers, members = self.get_list_members(sytron=False)
        return members

    @property
    def datebegin_forcing(self):
        return self.datebegin if self.conf.rundate.hour == self.nightruntime.hour else yesterday(self.datebegin)

    def get_forcing(self):
        if True:  # In order to have an indentation and facilitate the comparison with IGA Task

            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role='SafranForecast',
                local='mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment=self.xpid_inputhydro,
                block='meteo',
                geometry=self.conf.geometry,
                date=self.rundate_forcing,
                datebegin=self.datebegin_forcing,
                dateend=self.dateend,
                member=self.members,
                nativefmt='netcdf',
                kind='MeteorologicalForcing',
                model='s2m',
                namespace=self.conf.namespace_in,
                cutoff='production',
                fatal=False
            ),
            print(self.ticket.prompt, 'tb02 =', tb02)
            print()

    def get_pro(self):
        if True:  # In order to have an indentation and facilitate the comparison with IGA Task

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role='CrocusForecast',
                local='mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment=self.xpid_inputhydro,
                block='pro',
                geometry=self.conf.geometry,
                date=self.conf.rundate,
                datebegin=self.datebegin,
                dateend=self.dateend,
                member=self.members,
                nativefmt='netcdf',
                kind='SnowpackSimulation',
                model='surfex',
                namespace=self.conf.namespace_in,
                cutoff='production',
                fatal=False
            ),
            print(self.ticket.prompt, 'tb03 =', tb03)
            print()

    def get_algo(self):

        if 'compute' in self.steps:
            self.sh.title('Toolbox algo tb02 = Postprocessing')

            tbalgo1 = toolbox.algo(
                kind        = 's2m_hydro_ensemble',
                varnames    = ['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'],
                dateinit    = self.datebegin,
                engine      = 's2m',
                members     = footprints.util.rangex(self.members),
            ),
            print(self.ticket.prompt, 'tbalgo1 =', tbalgo1)
            print()

            self.component_runner(tbalgo1[0])

    def save_output(self):
        if 'late-backup' in self.steps:
            if True:
                self.sh.title('Toolbox output tb04')
                tb04 = toolbox.output(
                    role='Postproc_output',
                    intent='out',
                    local='HYDRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment=self.conf.xpid,
                    block='hydro',
                    geometry=self.conf.geometry,
                    date=self.conf.rundate,
                    datebegin=self.datebegin,
                    dateend=self.dateend,
                    nativefmt='netcdf',
                    kind='SnowpackSimulation',
                    model='postproc',
                    namespace=self.conf.namespace_out,
                    cutoff='production',
                    fatal=True
                ),
                print(self.ticket.prompt, 'tb04 =', tb04)
                print()


class _Hydro_Task_Replay(_Hydro_Task):

    @property
    def geometry_areas(self):
        oldgeometries = dict(alp27_allslopes='alp_allslopes', pyr24_allslopes='pyr_allslopes')
        if self.datebegin < Date(2022, 10, 22):
            return oldgeometries[self.conf.geometry.tag]
        else:
            return self.conf.geometry

    @property
    def xpid_inputhydro(self):
        return self.conf.xpid_inputhydro

    def get_input(self):
        if 'early-fetch' in self.steps or 'fetch' in self.steps:
            self.get_forcing()
            self.get_pro()
