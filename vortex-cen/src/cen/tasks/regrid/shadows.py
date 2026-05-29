# -*- coding: utf-8 -*-
"""
"""

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask


class Shadows(_CenResearchTask):
    """
    Add relief-induced solar masks to a FORCING file in a "station" geometry.

    Inputs :
    --------
    - SAFRAN-generated FORCING file in the "station" geometry.

    Outputs :
    ---------
    - FORCING file with extracted solar masks added.

    Mandatory configuration variables:
    ----------------------------------
    * `datebegin` *datebegin* of the forcing file(s)
        type: str, footprints.stdtypes.FPList
    * `dateend` *dateend* of the forcing files(s)
        type: str, footprints.stdtypes.FPList
    * `forcing_geometry` *geometry* of the input forcing file(s)
        type: str, footprints.stdtypes.FPList
    * `geometry` *geometry* of the output forcing file(s)
        type: str, footprints.stdtypes.FPList
    * `xpid` Experiment identifier
        type: str


    """

    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING.nc" in the different working sub-directories.
        """

        self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING.nc')

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Returns a "PrepareForcingComponent" algo component with the appropriate arguments.

        If the input consists of several FORCING files, they will be processed in parallel.

        Working tree :
        rootdir
        |-- datebegin1_dateend1
            |--FORCING.nc
        |-- datebegin2_dateend2
            |--FORCING.nc
        ...

        """

        t = self.ticket

        avail_forcings = t.context.sequence.effective_inputs(role='Forcing')

        self.sh.title('Algo')
        algo = vortex.task(
            engine       = 'algo',
            kind         = 'shadowsforcing',
            datebegin    = [tbinput.rh.resource.datebegin for tbinput in avail_forcings],
            dateend      = [tbinput.rh.resource.dateend for tbinput in avail_forcings],
            ntasks       = min(40, len(avail_forcings)),  # TODO : ne pas mettre ça en dur dans le code !
            role_members = 'Forcing',
            # reprod_info  = self.get_reprod_info,
        )
        print(t.prompt, 'algo =', algo)
        print()

        return algo

    def launch_algo(self, algo):
        """
        launch python algo component.
        :param algo: algorithm to launch
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """
        Save the output FORCING file(s) in the new geometry.
        WARNING : the output geometry must be in a valid "geometries.ini" file.

        Arguments:
        :param geometry: Geometry of the output file(s)
        :type geometry: str
        :param xpid: Experiment identifier
        :type xpid: str
        """

        self.sh.title('Output FORCING')
        forcing_out = vortex.output(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = self.conf.xpid,
            namebuild      = 'flat@cen',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            block          = 'meteo',  # This is SURFEX-ready
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()
