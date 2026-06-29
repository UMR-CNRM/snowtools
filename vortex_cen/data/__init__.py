"""
Specific CEN data resources.

..
    # This does not display nicely
    .. inheritance-diagram:: vortex_cen.data.flow vortex_cen.data.surfex vortex_cen.data.consts vortex_cen.data.obs
                             vortex_cen.data.meteo vortex_cen.data.assim vortex_cen.data.safran vortex_cen.data.packedfiles
                             vortex_cen.data.postprocess vortex_cen.data.executables
       :top-classes: vortex.nwp.data.consts.GenvModelGeoResource, vortex.nwp.data.consts.GenvModelResource
       :private-bases:
       :parts: 4
       :caption: Full inheritance diagram of "resource" objects


.. automodule:: vortex_cen.data.flow
.. automodule:: vortex_cen.data.surfex
.. automodule:: vortex_cen.data.consts
.. automodule:: vortex_cen.data.meteo
.. automodule:: vortex_cen.data.obs
.. automodule:: vortex_cen.data.assim
.. automodule:: vortex_cen.data.safran
.. automodule:: vortex_cen.data.packedfiles
.. automodule:: vortex_cen.data.postprocess
.. automodule:: vortex_cen.data.executables

"""

from . import executables as executables
from . import providers as providers
from . import flow as flow
from . import meteo as meteo
from . import obs as obs
from . import assim as assim
from . import safran as safran
from . import surfex as surfex
from . import packedfiles as packedfiles
from . import postprocess as postprocess
from . import consts as consts

#: No automatic export
__all__ = []
