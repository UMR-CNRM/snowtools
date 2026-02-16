# -*- coding: utf-8 -*-
'''
'''

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask


class AddSlopes(_CenResearchTask):
    '''
    Add slopes to forcing file in a "flat" geometry.

    Inputs :
    --------
    - SAFRAN-generated FORCING file in a "flat" geometry.

    Outputs :
    ---------
    - FORCING file wih slopes and aspects.

    Mandatory configuration variables:
    ----------------------------------
    :param datebegin: *datebegin* of the forcing file(s)
    :type datebegin: str, footprints.stdtypes.FPList
    :param dateend: *dateend* of the forcing files(s)
    :type dateend: str, footprints.stdtypes.FPList
    :param forcing_geometry: *geometry* of the input forcing file(s)
    :type forcing_geometry: str, footprints.stdtypes.FPList
    :param geometry: *geometry* of the output forcing file(s)
    :type geometry: str, footprints.stdtypes.FPList
    :param xpid: Experiment identifier (format "{experiment_name}@{user}")
    :type xpid: str

    '''

    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING.nc" in the different working sub-directories.
        """

        self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING.nc')

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
            kind         = 'prepareforcing',
            datebegin    = [tbinput.rh.resource.datebegin for tbinput in avail_forcings],
            dateend      = [tbinput.rh.resource.dateend for tbinput in avail_forcings],
            ntasks       = min(40, len(avail_forcings)),  # TODO : ne pas mettre ça en dur dans le code !
            role_members = 'Forcing',
            # reprod_info  = self.get_reprod_info,
        )
        print(t.prompt, 'algo =', algo)
        print()

        return algo

    def put_remote_outputs(self):
        """
        Save the output FORCING file(s) in the new geometry.
        WARNING : the output geometry must be in a valid "geometries.ini" file.

        Arguments:
        :param out_geometry: Geometry of the output file(s)
        :type out_geometry: str
        :param xpid: Experiment identifier (format "experiment_name@user")
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
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING.nc',
            block          = 'meteo',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()

    def unittest(self):
        """
        Reproductibility test : compare output to reference.
        """

        self.sh.title('Diff FORCING')
        forcing_diff = vortex.diff(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = 'reference',
            username       = 'vernaym',
            namebuild      = 'flat@cen',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING.nc',
            block          = 'shadows',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'diff forcing =', forcing_diff)
        print()
