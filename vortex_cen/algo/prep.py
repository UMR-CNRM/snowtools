"""
Algo Components for ensemble S2M simulations.
"""

from bronx.fancies import loggers
from vortex.algo.components import Parallel
from vortex.syntax.stdattrs import a_date
from snowtools.tools.change_prep import prep_tomodify

logger = loggers.getLogger(__name__)


class Prep(Parallel):
    _footprint = dict(
        info = 'AlgoComponent that runs the PREP executable',
        attr = dict(
            kind = dict(
                values = ['make_prep']  # value "prep" already used
            ),
            date = a_date,
        )
    )

    def postfix(self, rh, opts):
        """
        Set the date of the PREP file to the prescribed date
        """
        print("CHANGE DATE OF THE PREP FILE.")
        prep = prep_tomodify("PREP.nc")
        prep.change_date(self.date)
        prep.close()
        super().postfix(rh, opts)
