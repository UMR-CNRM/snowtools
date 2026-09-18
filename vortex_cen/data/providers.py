# -*- coding: utf-8 -*-
"""
providers.py
------------

Specific CEN providers for DATA STORED OUTSIDE THE VORTEX DATA TREE only.


.. inheritance-diagram:: vortex_cen.data.providers
   :private-bases:
   :parts: 1

.. autoclass:: S2MReanalysisProvider
   :show-inheritance:

.. autoclass:: CenSopranoDevProvider
   :show-inheritance:

"""

import importlib
import footprints
from bronx.fancies import loggers

from vortex.util.config import GenericConfigParser
from vortex.data.providers import Provider, Vortex
from vortex.syntax.stdattrs import namespacefp, block, member, scenario, FmtInt

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)

map_suffix = {'alp': '_al', 'pyr': '_py', 'cor': '_co', 'mac': '_mc', 'jur': '_ju', 'vog': '_vo'}

_config_path = 'vortex_cen.data.cen_stores_configs'
_config_file = 'cen-map-resources.ini'


class CenCfgParser(GenericConfigParser):

    def resolvedpath(self, resource, vapp, vconf, resname=None):
        """
        Shortcut to retrieve the ``resolvedpath`` entry in the ``resname`` section
        of the current config file.
        """
        if resname is None:
            resname = resource.realkind

        return self.get(resname, 'resolvedpath')


class S2MReanalysisProvider(Provider):
    """
    Provider for input of the SAFRAN reanalysis (native observation and guess files).
    """

    #: Path to the uget Store configuration file
    _config = CenCfgParser(
        importlib.resources.open_text(
            _config_path,
            _config_file,
        )
    )

    _footprint = [
        namespacefp,
        dict(
            info = 'Provider for S2M reanalysis input resources (observations and guess)',
            attr = dict(
                namespace = dict(
                    values   = ['s2m.archive.fr'],
                    optional  = False,
                ),
                storage = dict(
                    values   = ['hendrix.meteo.fr'],
                    default  = 'hendrix.meteo.fr',
                    optional = True
                ),
                tube = dict(
                    optional = True,
                    values   = ['ftp'],
                    default  = 'ftp'
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return 'reanalysis'

    def scheme(self, resource):
        """The actual scheme is the ``tube`` attribute of the current provider."""
        return self.tube

    def netloc(self, resource):
        """The actual netloc is the ``namespace`` attribute of the current provider."""
        return self.storage

    def pathname(self, resource):
        """
        The actual pathname is the directly obtained from the templated ini file
        provided through the ``_config`` class variable.
        """
        info = self.pathinfo(resource)
        info['level_one'] = resource.geometry.area
        info['level_two'] = ''
        suffix = map_suffix[info['level_one']]
        season = resource.date.nivologyseason
        if resource.realkind == 'observations':
            if resource.part in ['synop', 'precipitation', 'hourlyobs']:
                info['level_two'] = 'obs/rs' + season + suffix
            elif resource.part == 'nebulosity':
                info['level_two'] = 'neb/n' + season + suffix
        elif resource.realkind == 'guess':
            if resource.source_conf == 'era40':
                info['level_one'] = 'cep'
                info['level_two'] = ''
            else:
                info['level_two'] = 'guess/p' + season + suffix
        elif resource.realkind == 'packedguess':
            info['level_two'] = 'guess/era5'
        elif resource.realkind == 'packedobs':
            info['level_two'] = 'obs'

        self._config.setall(info)
        return self._config.resolvedpath(resource, self.vapp, self.vconf, self.realkind)


class CenSopranoDevProvider(Provider):
    """
    Provider for real-time SAFRAN input files.

    Note : This provider should not be used anymore.
    """

    _config = CenCfgParser(
        importlib.resources.open_text(
            _config_path,
            _config_file,
        )
    )

    _footprint = [
        namespacefp,
        dict(
            info = 'CEN Soprano provider',
            attr = dict(
                namespace = dict(
                    values   = ['cendev.soprano.fr'],
                    optional  = False,
                ),
                storage = dict(
                    values   = ['guppy.meteo.fr', 'sotrtm35-sidev.meteo.fr']
                ),
                tube = dict(
                    optional = True,
                    values   = ['scp', 'ftp'],
                    default  = 'ftp'
                ),
            )
        )
    ]

    def __init__(self, *args, **kw):
        logger.debug('SOPRANO dev job provider init %s', self.__class__)
        super().__init__(*args, **kw)

    @property
    def realkind(self):
        return 'cendev'

    def scheme(self, resource):
        """The actual scheme is the ``tube`` attribute of the current provider."""
        return self.tube

    def netloc(self, resource):
        """The actual netloc is the ``namespace`` attribute of the current provider."""
        return self.storage

    def pathname(self, resource):
        """
        The actual pathname is the directly obtained from the templated ini file
        provided through the ``config`` footprint attribute.
        """
        info = self.pathinfo(resource)
        info['model'] = 's2m'
        info['level_one'] = self.vconf.split('@')[0]
        suffix = map_suffix[info['level_one']]
        season = resource.date.nivologyseason
        if resource.realkind == 'observations':
            if resource.part in ['synop', 'precipitation', 'hourlyobs']:
                info['level_two'] = 'obs/rs' + season + suffix
            elif resource.part == 'radiosondage':
                info['level_two'] = 'a' + season + suffix
            elif resource.part == 'nebulosity':
                info['level_two'] = 'neb/n' + season + suffix
        elif resource.realkind == 'guess':
            info['level_two'] = 'p' + season + suffix
        elif resource.realkind == 'snowpackstate':
            info['level_two'] = 'prep' + season + suffix

        logger.debug('sopranodevprovider::pathname info %s', info)

        self._config.setall(info)
        return self._config.resolvedpath(resource, self.vapp, self.vconf, self.storage)


# class CenDevProvider(Vortex):
#     """
#     Provider for real-time simulations run by local user
#     """
#     _SPECIAL_EXPS = ()
#
#     _footprint = [
#         block,
#         member,
#         namespacefp,
#         dict(
#             info="Vortex Cen Dev provider",
#             attr=dict(
#                 experiment=dict(
#                     info="Provider experiment id",
#                     values=["OPER"],
#                     type=str,
#                     optional=False,
#                     access="rwx",
#                 ),
#                 username = dict(
#                     info="Provider username",
#                     values=["lafaysse"],
#                     type=str,
#                     optional=False,
#                 ),
#                 # vapp = dict(
#                 #     values=["s2m"]
#                 # ),
#                 member=dict(
#                     type=FmtInt,
#                     args=dict(fmt="03"),
#                 ),
#                 namespace=dict(
#                     values=[
#                         "vortex.archive-legacy.fr",
#                         "vortex.cache.fr",
#                         "vortex.archive.fr",
#                         "vortex.multi.fr",
#                         "vortex.stack.fr",
#                     ],
#                     optional=True,
#                     default=None,
#                     access="rwx",
#                 ),
#                 cache=dict(
#                     info="Whether or not to use the cache",
#                     type=bool,
#                     optional=True,
#                     default=None,
#                 ),
#                 archive=dict(
#                     info="Whether or not to use the archive",
#                     type=bool,
#                     optional=True,
#                     default=None,
#                 ),
#                 namebuild=dict(
#                     info="The object responsible for building filenames.",
#                     optional=True,
#                     doc_visibility=footprints.doc.visibility.ADVANCED,
#                 ),
#                 expected=dict(
#                     info="Is the resource expected ?",
#                     alias=("promised",),
#                     type=bool,
#                     optional=True,
#                     default=False,
#                     doc_zorder=-5,
#                 ),
#             ),
#             fastkeys={"block", "experiment"},
#         ),
#     ]
#
#     @property
#     def realkind(self):
#         return "vortex"
#
#     def netloc(self, resource):
#         """Returns the current ``namespace``."""
#         return self.namespace.netloc
#
