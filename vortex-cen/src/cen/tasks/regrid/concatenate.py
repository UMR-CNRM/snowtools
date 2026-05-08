# -*- coding: utf-8 -*-
"""
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ForcingSpatialConcatenation(_CenResearchTask):
    """
    Concatenate FORCING files over a spatial dimension (Typically "Number_of_points").
    FORCING files over different years are processed in parallel.
    Ex : concatenation of "postes" simulations over different mountain ranges

    Inputs:
    --------
    - Set of FORCING files to concatenate

    Outputs:
    ---------
    - Single FORCING file

    Configuration variables:

    :param forcing_geometry: List of geometries of the FORCING files to concatenate
    :type forcing_geometry: footprints.stdtypes.FPList
    :param xpid: Experiment identifier
    :type xpid: str
    :param geometry: Geometry of the output file(s)
    :type geometry: str
    :param datebegin: begin date(s) of files
    :param dateend: end date(s) of files
    :param namespace_out: namespace of output files
    """

    def get_remote_inputs(self):
        """
        get forcing files to concatenate

        """
        if 'forcing_geometry' in self.conf:
            self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING_[geometry:tag].nc')
        else:
            raise ValueError('The "forcing_geometry" (list of FORCING geometries) configuration variable is mandatory')

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Algo component for concatenation of FORCING files
        """

        self.sh.title('Toolbox algo concatenation')
        algo = vortex.task(
            engine       = 'algo',
            kind         = 'ConcatForcings',
            role_members = 'Forcing',
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()

        return algo

    def launch_algo(self, algo):
        """
        launch the algo component.

        :param algo: Algorithm to be launched.
        :type algo: AlgoComponent
        """
        self.launch_python_algo(algo)

    def put_outputs(self):
        """
        """

        self.sh.title('Output FORCING')
        forcing_out = vortex.output(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = self.conf.xpid,
            namebuild      = 'flat@cen',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = self.conf.get('block', 'meteo'),
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()
