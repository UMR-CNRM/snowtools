"""
Specific CEN executables.
"""

from bronx.fancies import loggers

from vortex.data.executables import Script, SurfaceModel

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


class Safran(SurfaceModel):
    """Base class for the Safran model executables."""

    _footprint = [
        dict(
            info = 'Safran module',
            attr = dict(
                kind = dict(
                    values = ['safrane', 'safrane_dev', 'syrpluie', 'syrpluie_dev', 'syrmrr', 'syrmrr_dev',
                              'sytist', 'sytist_dev', 'syvapr', 'syvapr_dev', 'syrper', 'syrper_dev',
                              'syvafi', 'syvafi_dev', 'sypluie', 'sypluie_dev', 'intercep', 'intercep_dev'],
                ),
                gvar = dict(
                    optional = True,
                    default = 'master_[kind]',
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return self.kind.split('_')[0]

    def command_line(self, **opts):
        return ''


class SafranGribFiltering(Script):
    """Base class for the creation of P files used by SAFRAN."""

    _footprint = [
        dict(
            info = 'Prepare the input files for SAFRAN',
            attr = dict(
                kind = dict(
                    values = ['s2m_filtering_grib']
                ),
                cpl_vconf = dict(
                    values = ['pearp', 'pearome', 'arpege', 'arome'],
                    optional = True,
                ),
                gvar = dict(
                    optional = True,
                    default = '[kind]',
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return 'gribfiltering'
