# -*- coding: utf-8 -*-
'''
'''

from vortex.cen.tasks.research_task_base import _CenResearchTask
from vortex import toolbox


class Shadows(_CenResearchTask):
    '''
    Add relief-induced solar masks to a FORCING file in a "station" geometry.

    Inputs :
    --------
    - SAFRAN-generated FORCING file in the "station" geometry.

    Outputs :
    ---------
    - FORCING file with extracted solar masks added.

    '''

    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING.nc" in the different working sub-directories.
        """

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def algo(self):
        """
        Returns a "PrepareForcingComponent" algo component with the appropriate arguments.

        If the input consists of several FORCING files in the same working directory,
        they will be processed in parallel.


        Working tree :
        rootdir
        |FORCING_datebegin1_dateend1.nc
        |FORCING_datebegin2_dateend2.nc
        ...

        Arguments:
        :param da: Massif number(s) to be extracted
        :type massifs: int, list
        :param slopes: Slope(s) to be extracted
        :type slopes: int, list
        :param elevations: Elevations(s) to be extracted
        :type elevations: int, list
        :param aspects: Aspects(s) to be extracted
        :type aspects: int, list
        """

        t = self.ticket

        avail_forcings = t.context.sequence.effective_inputs(role='Forcing')

        self.sh.title('Toolbox algo')
        algo = toolbox.algo(
            engine       = 's2m',
            kind         = 'shadowsforcing',
            datebegin    = [tbinput.rh.resource.datebegin for tbinput in avail_forcings],
            dateend      = [tbinput.rh.resource.dateend for tbinput in avail_forcings],
            ntasks       = min(40, len(avail_forcings)),
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

        self.sh.title('Toolbox output FORCING')
        forcing_out = toolbox.output(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = self.conf.xpid,
            namebuild      = 'flat@cen',
            local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            block          = 'shadows',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()

    def unittest(self):
        """
        Reproductibility test : compare output to reference.
        """

        self.sh.title('Toolbox diff FORCING')
        forcing_diff = toolbox.diff(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = 'reference@vernaym',
            namebuild      = 'flat@cen',
            local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            block          = 'shadows',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'diff forcing =', forcing_diff)
        print()
