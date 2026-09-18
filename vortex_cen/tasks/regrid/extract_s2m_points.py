# -*- coding: utf-8 -*-
"""
extract_s2m_points.py
---------------------

.. autoclass:: ExtractS2MForcing
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ExtractS2MForcing(_CenResearchTask):
    """
    Parallel extraction of a list of points from an ensemble of FORCING file(s) covering different time periods
    in the "massif" geometry according to their massif number, elevation, slope and aspect.

    **Input:**

    - SAFRAN-generated FORCING file(s) in the "massif" geometry.

    **Outputs:**

    - FORCING file(s) with extracted points

    **Configuration variables:**

    * ``massifs`` Massif number(s) to be extracted
      type: int, list
    * ``slopes`` Slope(s) to be extracted
      type: int, list
    * ``elevations`` Elevations(s) to be extracted
      type: int, list
    * ``aspects`` Aspects(s) to be extracted
      type: int, list
    * ``geometry`` Geometry of the output file(s)
      type: str
    * ``xpid`` Experiment identifier
      type: str

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "forcing_datebegin|datebegin",
            "forcing_dateend|dateend",
            "forcing_xpid",
            "xpid",
            "forcing_geometry+help=A SAFRAN massif geometry",
            "forcing_block",
            "geometry",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "massifs",
            "slopes",
            "elevations",
            "aspects",
            "out_block+default=extract_s2m",
            "diff_xpid",
            "diff_user",
            "diff_block+default=extract_s2m",
        ]
        overwrite = [
            "datebegin",
            "dateend",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES,
                overwrite=overwrite)


    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING_IN.nc" in the different working sub-directories.
        """
        # Security to avoid overwriting the original FORCING file(s)
        if self.conf.geometry == self.conf.forcing_geometry:
            print("geometry = ", self.conf.geometry, 'forcing_geometry =', self.conf.forcing_geometry)
            raise ValueError("The output 'geometry' can not be the same as the input 'forcing_geometry' one.\n"
                             "Please provide a different 'geometry' configuration variable")

        self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING_IN.nc')

    def get_local_inputs(self):
        pass

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

        """

        self.sh.title('Algo')
        algo = vortex.task(
            kind         = 'ExtractMassifs',
            massifs      = self.conf.get('massifs', None),
            slopes       = self.conf.get('slopes', None),
            elevations   = self.conf.get('elevations', None),
            aspects      = self.conf.get('aspects', None),
            role_members = 'Forcing',
            engine       = 'algo',
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()
        return algo

    def launch_algo(self, algo, **kw):
        self.launch_python_algo(algo, **kw)

    def put_outputs(self):
        """
        Save the output FORCING file(s) in the new geometry.
        WARNING : the output geometry must be in a valid "geometries.ini" file.
        """

        self.sh.title('Output FORCING')
        forcing_out = vortex.output(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = self.conf.xpid,
            namebuild      = 'flat@cen',
            namespace=self.namespace_out,
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = self.conf.get('out_block', 'extract_s2m'),
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        self.sh.title("Reproductibility check : FORCING")
        diff = vortex.diff(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = self.conf.diff_xpid,
            username       = self.conf.get('diff_user', None),
            namebuild      = 'flat@cen',
            namespace=self.namespace_out,
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = self.conf.get('diff_block', 'extract_s2m/meteo'),
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()
