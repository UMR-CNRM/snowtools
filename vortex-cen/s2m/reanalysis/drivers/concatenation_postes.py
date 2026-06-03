# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.shadows import Shadows
from vortex_cen.tasks.regrid.concatenate import ForcingSpatialConcatenation


def setup(t, **kw):
    return Driver(
        tag='surfex_postes',
        ticket=t,
        nodes=[
            ForcingSpatialConcatenation(tag='concatenation', ticket=t, **kw),
            Shadows_postes(tag='shadows', ticket=t, **kw),
        ],
        options=kw,
    )


class Shadows_postes(Shadows):
    """
    In the reanalysis case, the FORCING files come from the output of the "concatenation" task and are
    not available at the execution of the transfer node.
    """

    def get_remote_inputs(self):
        pass

    def get_local_inputs(self):
        self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING.nc')
