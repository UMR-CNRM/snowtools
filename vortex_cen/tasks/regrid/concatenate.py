# -*- coding: utf-8 -*-
"""
concatenate.py
--------------

.. autoclass:: ForcingSpatialConcatenation
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ForcingSpatialConcatenation(_CenResearchTask):
    """
    Concatenate FORCING files over a spatial dimension (Typically "Number_of_points").
    FORCING files over different years are processed in parallel.
    Ex : concatenation of "postes" simulations over different mountain ranges

    **Input:**

    - Set of FORCING files to concatenate

    **Output:**

    - Single FORCING file

    Mandatory Configuration variables:
    ----------------------------------

    * ``forcing_geometry`` List of geometries of the FORCING files to concatenate
      type: footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``geometry`` Geometry of the output file(s)
      type: str
    * ``datebegin`` begin date(s) of files
    * ``dateend`` end date(s) of files
    * ``namespace_out`` namespace of output files

    Optional configuration variables:
    ---------------------------------
    * ``max_ntasks`` The maximum number of parallel tasks (in case of huge memory usage)
      type: int
    * ``block`` Output block of the concatenated FORCING files
      type: str
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "forcing_datebegin|datebegin",
            "forcing_dateend|dateend",
            "forcing_xpid",
            "xpid",
            "forcing_geometry+type=list;default=None",
            "forcing_block",
            "geometry",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "concat_dim",
            "max_ntasks",
            "forcing",
            "out_block+default=concatenate",
            "diff_xpid",
            "diff_user",
            "diff_block+default=concatenate",
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
            concat_dim   = self.conf.get('concat_dim', 'Number_of_points'),
            ntasks       = self.conf.get('max_ntasks', self.conf.ntasks),
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()

        return algo

    def launch_algo(self, algo, **kwargs):
        """
        launch the algo component.

        :param algo: Algorithm to be launched.
        :type algo: AlgoComponent
        :param kwargs: Additional configuration variables. Not used
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
            block          = self.conf.get('out_block', 'concatenate'),
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
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = self.conf.get('diff_block', 'concatenate'),
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()
