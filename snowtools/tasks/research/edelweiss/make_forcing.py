# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.scripts.extract.vortex import vortex_get


class Precipitation(_VortexTask):
    '''
    Task for the generation of FORCING-ready Snowf/Rainf variables.
    WARNING : TASK IN DEVELOPMENT
    This task will evolve in line with ongoing EDELWEISS developments.
    '''

    def get_remote_inputs(self):
        """
        Main method to fetch all input files
        """

        t = self.ticket

        # Hourly total precipitation at 1km resolution --> use get_meteo since it is not FORCING-ready
        self.sh.title('Toolbox input Precipitation 1km')
        self.precipitation = toolbox.input(
            role        = 'Precipitation',
            kind        = 'Precipitation',
            datebegin   = self.conf.datebegin_precipitation,
            dateend     = self.conf.dateend_precipitation,
            date        = '[dateend]',
            geometry    = self.conf.geometry_precipitation,
            experiment  = self.conf.xpid_precipitation,
            block       = 'hourly',
            member      = self.conf.members_precipitation,
            vapp        = self.conf.vapp_precipitation,
            local       = 'mb[member]/PRECIPITATION.nc',
            namebuild   = 'flat@cen',
        )
        print(t.prompt, 'Precipitation =', self.precipitation)
        print()

        if self.conf.dateend == '2022080106':

            # Hourly iso Wet-bulb temperatures 0°C, 1°C [, 1.5°C] --> use get_meteo (not FORCING-ready)
            self.sh.title('Toolbox input ISO WETBT/TPW')
            iso_wtbt = toolbox.input(
                datebegin   = self.conf.datebegin_lpn,
                dateend     = self.conf.dateend_lpn,
                date        = '[dateend]',
                kind        = self.conf.kind_lpn,
                geometry    = self.conf.geometry_lpn,
                experiment  = self.conf.xpid_lpn,
                block       = self.conf.kind_lpn.lower(),
                # source_app  = 'arome',
                # source_conf = '3dvarfr',
                local       = 'ISO_TPW.nc',
                namebuild   = 'flat@cen',
                source_app  = 'arome',
                source_conf = '3dvarfr'
            )
            print(t.prompt, 'iso_wtbt =', iso_wtbt)
            print()

        else:

            # Hourly iso Wet-bulb temperatures 0°C, 1°C [, 1.5°C] --> use get_meteo (not FORCING-ready)
            self.sh.title('Toolbox input ISO WETBT/TPW')
            iso_wtbt = toolbox.input(
                datebegin   = self.conf.datebegin_lpn,
                dateend     = self.conf.dateend_lpn,
                date        = '[dateend]',
                kind        = self.conf.kind_lpn,
                geometry    = self.conf.geometry_lpn,
                experiment  = self.conf.xpid_lpn,
                block       = self.conf.kind_lpn.lower(),
                # source_app  = 'arome',
                # source_conf = '3dvarfr',
                local       = 'ISO_TPW.nc',
                namebuild   = 'flat@cen',
            )
            print(t.prompt, 'iso_wtbt =', iso_wtbt)
            print()

        # Domain's DEM
        self.sh.title('Toolbox input TARGET RELIEF')
        toolbox.input(
            kind        = 'relief',
            genv        = self.conf.uenv,
            geometry    = self.conf.geometry,
            local       = 'TARGET_RELIEF.nc',
            gvar        = 'RELIEF_[geometry:tag:upper]_4326',
        )

    def algo(self):
        """
        Algo component
        """
        t = self.ticket
        self.sh.title('Toolbox algo Precipitation generator')
        tbalgo = toolbox.algo(
            kind         = 'PrecipitationConstructor',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            engine       = 'algo',  # `_CENTaylorRun` algo components familly
            members      = self.conf.members,
            ntasks       = self.conf.ntasks,
            role_members = 'Precipitation',
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution outputs
        """
        t = self.ticket

        self.sh.title('Precipitation output')
        precipitation = toolbox.output(
            kind         = 'Precipitation',
            datebegin   = self.conf.datebegin,
            dateend     = self.conf.dateend,
            date        = '[dateend]',
            geometry    = self.conf.geometry,
            experiment  = self.conf.xpid,
            block       = 'hourly',
            member      = self.conf.members,
            vapp        = self.conf.vapp,
            local       = 'mb[member]/PRECIPITATION_OUT.nc',
            namebuild   = 'flat@cen',
        )
        print(t.prompt, 'Precipitation =', precipitation)
        print()


class Forcing(_VortexTask):
    '''
    Generic task for the generation of 'hybrid' FORCING files (variables coming from different sources)
    WARNING : TASK IN DEVELOPMENT
    This task will evolve in line with ongoing EDELWEISS developments.
    '''

    def check_and_update_configuration(self):
        pass

    def get_remote_inputs(self):
        """
        Main method to fetch all input files
        """

        t = self.ticket

        # Meteorological variables that are not yet processed in EDELWEISS system
        # come from SAFRAN reanalysis.
        # It is also possible to start with an already modified FORCING file. In this case the user
        # must at least provide the xpid and optionally (if different from the task's ones) the geometry
        # and vapp of the FORCING files

        self.sh.title('FORCING input')
        self.forcing = toolbox.input(
            role        = 'MeteorologicalForcing',
            kind        = 'MeteorologicalForcing',
            local       = 'mb[member]/FORCING_IN.nc',
            vapp        = self.conf.vapp_forcing,
            datebegin   = self.conf.datebegin_forcing,
            dateend     = self.conf.dateend_forcing,
            date        = '[dateend]',
            experiment  = self.conf.xpid_forcing,
            geometry    = self.conf.geometry_forcing,
            member      = self.conf.members,
            block       = 'meteo',
            namebuild   = 'flat@cen',
        )
        print(t.prompt, 'forcing =', self.forcing)
        print()

        # TODO : Cette verrue montre que le source_conf est inutile
        # A retirer dans les toolbox de Sabine
        conf_map    = dict(
            RS   = 'RandomSampling',
            EnKF = 'EnsembleKalmanFilter',
            PF   = 'ParticleFilter',

        )
        # TODO modifier cette condition ridicule
        if self.conf.xpid_precipitation.split('@')[0][:-2] in conf_map.keys() and False:
            source_conf = conf_map[self.conf.xpid_precipitation.split('@')[0][:-2]]
        else:
            source_conf = None

        # Update Rainf/Snowf variables
        if self.conf.precipitation is not None:

            self.sh.title('Precipitation input')
            self.precipitation = toolbox.input(
                role        = 'Precipitation',
                kind        = 'Precipitation',
                datebegin   = self.conf.datebegin_precipitation,
                dateend     = self.conf.dateend_precipitation,
                date        = '[dateend]',
                geometry    = self.conf.geometry_precipitation,
                experiment  = self.conf.xpid_precipitation,
                block       = 'hourly',
                member      = self.conf.members_precipitation,
                vapp        = self.conf.vapp_precipitation,
                local       = 'mb[member]/PRECIPITATION.nc',
                namebuild   = 'flat@cen',
                source_conf = source_conf,
            )
            print(t.prompt, 'Precipitation =', self.precipitation)
            print()

        else:

            self.precipitation = False

        # Update Wind / Wind_DIR variables
        if self.conf.wind is not None:
            self.sh.title('Wind input')
            wind = toolbox.input(
                vapp           = self.conf.vapp_wind,
                experiment     = self.conf.xpid_wind,
                geometry       = self.conf.geometry_wind,
                datebegin      = self.conf.datebegin_wind,
                dateend        = self.conf.dateend,
                date           = self.conf.dateend,
                kind           = 'Wind',
                block          = 'meteo',
                filename       = 'WIND.nc',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                model          = 'devine',  # TODO : understand why *model* is required
                source_app     = 'arome',
                source_conf    = 'devine',
            )
            print(t.prompt, 'wind =', wind)
            print()

    def algo(self):
        """
        Algo component
        """
        t = self.ticket
        self.sh.title('Toolbox algo FORCING generator')
        tbalgo = toolbox.algo(
            kind         = 'ForcingConstructor',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            engine       = 'algo',  # `_CENTaylorRun` algo components familly
            members      = self.conf.members,
            # WARNING : the binding seem to be important since problems have been observed with the default
            # '80 task per node' (one random worker does nothing). Maybe something to do with the fact that ntasks
            # is not a multiple of the actual number of workers ?
            # Update 5/04 : Le BUG se produit aussi avec ntasks=nworkers ...
            ntasks       = len(self.conf.members) if self.conf.members is not None else 1,
            # ntasks       = len(self.precipitation),
            role_members = 'Precipitation' if self.precipitation else None,
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution outputs
        """

        t = self.ticket

        self.sh.title('FORCING output')
        forcing = toolbox.output(
            kind        = 'MeteorologicalForcing',
            local       = 'mb[member]/FORCING_OUT.nc',
            vapp        = self.conf.vapp,
            datebegin   = self.conf.datebegin,
            dateend     = self.conf.dateend,
            date        = '[dateend]',
            experiment  = self.conf.xpid,
            geometry    = self.conf.geometry,
            member      = self.conf.members,
            block       = 'meteo',
            namebuild   = 'flat@cen',
        )
        print(t.prompt, 'forcing =', forcing)
        print()


class CombineForcings(_VortexTask):
    '''
    Generic task for the generation of a FORCING over a gevien period from different FORCING files over
    different sub-periods.
    WARNING : TASK IN DEVELOPMENT
    This task will evolve in line with ongoing EDELWEISS developments.
    '''

    def check_and_update_configuration(self):
        pass

    def get_remote_inputs(self):
        """
        Main method to fetch all input files
        """
        vortex_get.get(
            role           = 'RefForcing',
            kind           = 'MeteorologicalForcing',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            xpid           = self.conf.xpid1,
            geometry       = self.conf.geometry,
            member         = self.conf.members,
            block          = 'meteo',
            filename       = 'FORCING1.nc'
        )

        vortex_get.get(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            xpid           = self.conf.xpid2,
            geometry       = self.conf.geometry,
            member         = self.conf.members,
            block          = 'meteo',
            filename       = 'FORCING2.nc'
        )

    def algo(self):
        """
        Algo component
        """
        t = self.ticket
        self.sh.title('Toolbox algo FORCING generator')
        tbalgo = toolbox.algo(
            kind         = 'CombineForcings',
            date         = self.conf.assimdate,
            engine       = 'algo',  # `_CENTaylorRun` algo components familly
            members      = self.conf.members,
            # WARNING : the binding seem to be important since problems have been observed with the default
            # '80 task per node' (one random worker does nothing). Maybe something to do with the fact that ntasks
            # is not a multiple of the actual number of workers ?
            # Update 5/04 : Le BUG se produit aussi avec ntasks=nworkers ...
            ntasks       = len(self.conf.members) if self.conf.members is not None else 1,
            # ntasks       = len(self.precipitation),
            role_members = 'RefForcing',
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution outputs
        """

        vortex_get.put(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            xpid           = self.conf.xpid,
            geometry       = self.conf.geometry,
            member         = self.conf.members,
            block          = 'meteo',
            filename       = 'FORCING_OUT.nc'
        )


class _PertubForcing(_VortexTask):
    '''
    Abstract class for FORCING perturbation tasks
    '''

    def check_and_update_configuration(self):
        pass

    def get_remote_inputs(self):
        """
        Main method to fetch all input files
        """

        # Meteorological variables that are not yet processed in EDELWEISS system
        # come from SAFRAN reanalysis.
        # It is also possible to start with an already modified FORCING file. In this case the user
        # must at least provide the xpid and optionally (if different from the task's ones) the geometry
        # and vapp of the FORCING files
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(
            vapp      = self.conf.vapp_forcing,
            filename  = 'FORCING_IN.nc',
            datebegin = self.conf.datebegin_forcing,
            dateend   = self.conf.dateend_forcing,
            xpid      = self.conf.xpid_forcing,
            geometry  = self.conf.geometry_forcing,
            member    = self.conf.members_forcing,
        ))
        self.sh.title('FORCING input')
        self.forcing = io.get_forcing(**kw)


class PerturbPrecipitation(_PertubForcing):
    '''
    Abstract class for precipitation perturbation
    '''

    def algo(self):
        """
        Algo component
        """

        # TODO : ensure that member 0 is the unperturbed precipitation field (control member)

        t = self.ticket
        self.sh.title('Toolbox algo PRECIP perturbation')
        tbalgo = toolbox.algo(
            kind         = 'PerturbPrecip',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            engine       = 'algo',  # `_CENTaylorRun` algo components familly
            members      = self.conf.members,
            # WARNING : the binding seem to be important since problems have been observed with the default
            # '80 task per node' (one random worker does nothing). Maybe something to do with the fact that ntasks
            # is not a multiple of the actual number of workers ?
            # Update 5/04 : Le BUG se produit aussi avec ntasks=nworkers ...
            ntasks       = len(self.conf.members),
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution outputs
        """

        self.sh.title('Unperturbed FORCING output (member 0  = control member)')
        io.put_forcing(filename='FORCING_IN.nc', member=0, **self.common_kw)

        self.sh.title('Perturbed FORCING output')
        # TODO : ensure that 0 is not in members to avoid overwriting the control member !
        io.put_forcing(filename='FORCING_OUT.nc', member=self.conf.members, **self.common_kw)

        # Un-comment these lines to save the working directory after the execution
#        print('==================================================================================================')
#        print('==================================================================================================')
#        raise Exception('INFO :The execution went well, do not take into account the following error')


class PerturbForcing(_VortexTask):
    """
    """

    def get_remote_inputs(self):

        t = self.ticket

        # TODO : always store "deterministic" forcings under mb000 ?

        # Try to find a forcing covering the full simulation period
        self.sh.title('Toolbox input forcing (entire period)')
        forcing_a = toolbox.input(
            role           = 'Forcing',
            kind           = 'MeteorologicalForcing',
            vapp           = self.conf.vapp_forcing,
            vconf          = '[geometry:tag]',
            cutoff         = 'assimilation',
            local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid_forcing,
            block          = 'meteo',
            geometry       = self.conf.geometry_forcing,
            nativefmt      = 'netcdf',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            fatal          = False,
        ),
        print(t.prompt, 'forcing_a =', forcing_a)
        print()

        # Look for yearly forcing files
        self.sh.title('Toolbox input forcing (yearly)')
        forcing_b = toolbox.input(
            alternate      = 'Forcing',
            kind           = 'MeteorologicalForcing',
            vapp           = self.conf.vapp_forcing,
            vconf          = '[geometry:tag]',
            cutoff         = 'assimilation',
            local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid_forcing,
            block          = 'meteo',
            geometry       = self.conf.geometry_forcing,
            nativefmt      = 'netcdf',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
        ),
        print(t.prompt, 'forcing_b =', forcing_b)
        print()

    def algo(self):

        t = self.ticket

        self.sh.title('Toolbox algo')
        algo = toolbox.algo(
            engine       = 's2m',
            kind         = 'perturbforcing',
            members      = self.conf.members,
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            ntasks       = len(self.conf.members),
            geometry_in  = self.conf.geometry,
        )
        print(t.prompt, 'algo =', algo)
        print()
        algo.run()

    def put_remote_outputs(self):

        t = self.ticket

        # TODO : supprimer si le membre 0 devient systématique pour les forçages déterministes
        self.sh.title('Output : reference forcing')
        forcing_ref = toolbox.output(
            alternate      = 'Forcing',
            kind           = 'MeteorologicalForcing',
            vapp           = self.conf.vapp,
            vconf          = '[geometry:tag]',
            cutoff         = 'assimilation',
            local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            member         = 0,
            block          = 'meteo',
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
        ),
        print(t.prompt, 'forcing_ref =', forcing_ref)
        print()

        self.sh.title('Output : perturbed forcings')
        forcing_perturb = toolbox.output(
            role           = 'Forcing',
            local          = 'mb[member%04d]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            vapp           = self.conf.vapp,
            experiment     = self.conf.xpid,
            member         = self.conf.members,
            geometry       = self.conf.geometry,
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            nativefmt      = 'netcdf',
            kind           = 'MeteorologicalForcing',
            model          = 'safran',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'meteo'
        ),
        print(t.prompt, 'forcing_perturb =', forcing_perturb)
        print()
