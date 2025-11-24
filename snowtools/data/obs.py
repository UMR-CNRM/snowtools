"""
S2M executions flow resources.
"""

from bronx.fancies import loggers

from cen.data.flow import SurfaceIO


#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


class SurfaceObservations(SurfaceIO):
    """
    Class for all kind of meteorological observation files.
    """
    _footprint = [
        dict(
            info = 'Meteorological surface observations',
            attr = dict(
                kind = dict(
                    values = ['SurfaceObservation'],
                ),
                model = dict(
                    optional = True,  # The 'model' footprint of FORCING files is mostly unknown/undefined (and unused)
                    deafult  = 'unknown',
                    values   = ['safran'],
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return self.kind
