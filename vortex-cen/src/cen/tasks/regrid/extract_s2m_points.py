# -*- coding: utf-8 -*-
"""
"""

from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ExtractS2MForcing(_CenResearchTask):
    """
    Extract a list of points from an ensemble of FORCING file(s) covering different time periods
    in the "massif" geometry according to their massif number, elevation, slope and aspect.

    Inputs :
    --------
    - SAFRAN-generated FORCING file(s) in the "massif" geometry.

    Outputs :
    ---------
    - FORCING file(s) with extracted points

    """

    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING_IN.nc" in the different working sub-directories.
        """

        self.get_forcing(forcing_geometry=self.conf.forcing_geometry, localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING_IN.nc')

    def algo(self):
        """
        Returns an "ExtractMassif" with the appropriate arguments.

        If the input consists of several FORCING files, they will be processed in parallel.
        The output FORCING files(s) are named "FORCING_OUT.nc".


        Working tree :
        rootdir
        |-- datebegin1_dateend1
            |--FORCING_IN.nc
        |-- datebegin2_dateend2
            |--FORCING_IN.nc
        ...

        Configuration variables:

        :param massifs: Massif number(s) to be extracted
        :type massifs: int, list
        :param slopes: Slope(s) to be extracted
        :type slopes: int, list
        :param elevations: Elevations(s) to be extracted
        :type elevations: int, list
        :param aspects: Aspects(s) to be extracted
        :type aspects: int, list
        """

        for footprint in ['massifs', 'slopes', 'elevations', 'aspects']:
            if footprint not in self.conf:
                self.conf[footprint] = None

        self.sh.title('Algo')
        algo = vortex.task(
            kind         = 'ExtractMassifs',
            massifs      = self.conf.massifs,
            slopes       = self.conf.slopes,
            elevations   = self.conf.elevations,
            aspects      = self.conf.aspects,
            role_members = 'Forcing',
            engine       = 'algo',
        )
        print(self.ticket.prompt, 'algo =', algo)
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

        # TODO : Changer *out_geometry* --> geometry (par convention)
        # geometry = forcing_geometry

        # Security to avoid overwriting the original FORCING file(s)
        if self.conf.out_geometry == self.conf.geometry:
            raise ValueError("The 'out_geometry' can not be the same as the input one.\n"
                             "Please provide a different 'out_geometry' configuration variable")
        else:
            self.sh.title('Output FORCING')
            forcing_out = vortex.output(
                kind           = 'MeteorologicalForcing',
                datebegin      = self.list_dates_begin,
                dateend        = self.dict_dates_end,
                geometry       = self.conf.out_geometry,
                experiment     = self.conf.xpid,
                namebuild      = 'flat@cen',
                local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
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
            geometry       = self.conf.out_geometry,
            experiment     = 'reference',
            username       = 'vernaym',
            namebuild      = 'flat@cen',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = 'meteo',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'diff forcing =', forcing_diff)
        print()
