# -*- coding: utf-8 -*-
'''
Created on 28 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


def setup(t, **kw):
    return Driver(
        tag='precipitation',
        ticket=t,
        nodes=[
            Precipitation(tag='precipitation', ticket=t, **kw),
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

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution outputs
        """
        self.sh.title('Precipitation output')
        io.put_precipitation(*self.common_args, members=self.conf.members, filename='PRECIPITATION_OUT.nc',
                **self.common_kw)
