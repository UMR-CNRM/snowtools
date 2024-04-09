# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


def setup(t, **kw):
    return Driver(
        tag='forcing',
        ticket=t,
        nodes=[
            Forcing(tag='forcing', ticket=t, **kw),
        ],
        options=kw
    )


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
        # Hourly total precipitation at 1km resolution --> use get_meteo since it is not FORCING-ready
        self.sh.title('Toolbox input Precipitation 1km')
        io.get_meteo(
            kind     = 'Precipitation',
            geometry = self.conf.geometry_precipitation,
            xpid     = self.conf.xpid_precipitation,
            block    = 'hourly',
            members  = self.conf.members,
            **self.common_kw,
        )
        # Hourly iso Wet-bulb temperatures 0°C, 1°C [, 1.5°C] --> use get_meteo (not FORCING-ready)
        self.sh.title('Toolbox input ISO WETBT/TPW')
        io.get_meteo(kind='ISO_TPW', geometry=self.conf.geometry_tpw, xpid=self.conf.xpid_tpw, **self.common_kw)
        # Iso-TPW's grid relief
        self.sh.title('Toolbox input ISO WETBT/TPW RELIEF')
        io.get_const(self.conf.uenv, 'relief', self.conf.geometry_tpw, filename='SOURCE_RELIEF.nc')
        # Domain's DEM
        self.sh.title('Toolbox input TARGET RELIEF')
        io.get_const(self.conf.uenv, 'relief', self.conf.geometry, filename='TARGET_RELIEF.nc',
                gvar='RELIEF_[geometry:tag]_4326')

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

        # Meteorological variables that are not yet processed in EDELWEISS system
        # come from SAFRAN reanalysis.
        # It is also possible to start with an already modified FORCING file. In this case the user
        # must at least provide the xpid and optionally (if different from the task's ones) the geometry
        # and vapp of the FORCING files
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        kw.update(dict(vapp=self.conf.vapp_forcing, filename='FORCING_IN.nc', datebegin=self.conf.datebegin_forcing,
            dateend=self.conf.dateend_forcing))
        self.sh.title('FORCING input')
        self.forcing = io.get_forcing(self.conf.xpid_forcing, self.conf.geometry_forcing, **kw)

        # TODO : Cette verrue montre que le source_conf est inutile
        # A retirer dans les toolbox de Sabine
        conf_map    = dict(
            RS   = 'RandomSampling',
            EnKF = 'EnsembleKalmanFilter',
            PF   = 'APrticleFilter',

        )
        # TODO modifier cette condition ridicule
        if self.conf.xpid_precipitation.split('@')[0][:-2] in conf_map.keys() and False:
            source_conf = conf_map[self.conf.xpid_precipitation.split('@')[0][:-2]]
        else:
            source_conf = None

        # Update Rainf/Snowf variables
        if self.conf.precipitation is not None:
            # Update default vapp with specific conf values
            kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
            kw.update(dict(vapp=self.conf.vapp_precipitation, members=self.conf.members, source_conf=source_conf))
            self.sh.title('Precipitation input')
            self.precipitation = io.get_precipitation(self.conf.xpid_precipitation, self.conf.geometry_precipitation,
                                                    **kw)

        # Update Wind / Wind_DIR variables
        if self.conf.wind is not None:
            kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
            kw.update(dict(vapp=self.conf.vapp_wind))  # Update default vapp with specific conf value
            self.sh.title('Wind input')
            io.get_wind(self.conf.xpid_wind, self.conf.geometry_wind, **kw)

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
            ntasks       = len(self.precipitation),
            role_members = 'Precipitation',
        )
        print(t.prompt, 'tbalgo =', tbalgo)
        print()
        tbalgo.run()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution outputs
        """
        self.sh.title('FORCING output')
        io.put_forcing(self.conf.xpid, self.conf.geometry, filename='FORCING_OUT.nc', members=self.conf.members,
                **self.common_kw)
