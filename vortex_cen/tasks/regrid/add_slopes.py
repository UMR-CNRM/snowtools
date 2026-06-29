# -*- coding: utf-8 -*-
"""
add_slopes.py
-------------

Add slopes to forcing file(s) in the SAFRAN "flat" massif geometry.

.. autoclass:: AddSlopes
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask


class AddSlopes(_CenResearchTask):
    """
    **Task : AddSlopes**
    Add slopes to forcing file in a "flat" geometry.

    **Input:**

    - SAFRAN-generated FORCING file in a "flat" geometry.

    **Output:**

    - FORCING file wih slopes and aspects.

   """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "forcing_datebegin|datebegin",
            "forcing_dateend|dateend",
            "forcing_xpid",
            "xpid",
            "forcing_geometry+format=*_flat",
            "forcing_block",
            "geometry",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "max_ntasks",
            "out_block+default=allslopes",
            "diff_xpid",
            "diff_user",
            "diff_block+default=allslopes",
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
        Get FORCING file as "FORCING.nc" in the different working sub-directories.
        """

        self.forcingname = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_[geometry::tag].nc'

        self.get_forcing(localname=self.forcingname)

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Returns a "PrepareForcingComponent" algo component with the appropriate arguments.

        If the input consists of several FORCING files, they will be processed in parallel.

        Working tree :
        rootdir
        |FORCING_datebegin1_dateend1.nc
        |FORCING_datebegin2_dateend2.nc
        ...

        or

        rootdir
        |geometry1
            |-- FORCING_datebegin1_dateend1.nc
            |-- FORCING_datebegin2_dateend2.nc
        |geometry2
            |-- FORCING_datebegin1_dateend1.nc
            |-- FORCING_datebegin2_dateend2.nc
        ...

        """

        t = self.ticket

        avail_forcings = t.context.sequence.effective_inputs(role='Forcing')
        # TODO : Passer directement les objets "geometry" !
        if isinstance(self.conf.forcing_geometry, list):
            list_geometry = self.conf.forcing_geometry.tag
        else:
            list_geometry = [self.conf.forcing_geometry.tag]

        self.sh.title('Algo AddSlopes')
        algo = vortex.task(
            engine       = 'algo',
            kind         = 'prepareforcing',
            datebegin    = list(set([tbinput.rh.resource.datebegin for tbinput in avail_forcings])),
            dateend      = list(set([tbinput.rh.resource.dateend for tbinput in avail_forcings])),
            ntasks       = self.conf.get('max_ntasks', self.conf.ntasks),
            geometry_in  = list_geometry,
            geometry_out = self.conf.geometry.tag,
            role_members = 'Forcing',
            # reprod_info  = self.get_reprod_info,
        )
        print(t.prompt, 'algo =', algo)
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
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = self.conf.get('out_block', 'allslopes'),
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
            block          = self.conf.get('diff_block', 'allslopes'),
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()
