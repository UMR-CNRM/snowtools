

import configparser
import importlib.resources

from bronx.fancies import loggers

from vortex.data import geometries

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


def load(
    inifile="@geometries.ini",
    refresh=False,
    verbose=True,
):
    """
    Load a set of pre-defined geometries from a configuration file.
    """
    iniconf = configparser.ConfigParser()
    with importlib.resources.open_text(
        "vortex_cen.data",
        "geometries.ini",
    ) as fh:
        iniconf.read_file(fh)
    geometries.add_geometries(iniconf, refresh, verbose)


# Load the plugin's geometries when this module is first imported
load(verbose=False)
