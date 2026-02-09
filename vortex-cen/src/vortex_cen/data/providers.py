"""
Specific CEN providers.
"""

from bronx.fancies import loggers

from vortex.util.config import GenericConfigParser
from vortex.data.providers import Provider
from vortex.syntax.stdattrs import namespacefp

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)

map_suffix = {'alp': '_al', 'pyr': '_py', 'cor': '_co', 'mac': '_mc', 'jur': '_ju', 'vog': '_vo'}


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
                config = dict(
                    type     = CenCfgParser,
                    optional = True,
                    default  = CenCfgParser('@cen-map-resources.ini')
                )
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
        provided through the ``config`` footprint attribute.
        """
        info = self.pathinfo(resource)
        info['level_one'] = self.vconf.split('@')[0]
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
            info['level_two'] = 'guess'
        elif resource.realkind == 'packedobs':
            info['level_two'] = 'obs'

        self.config.setall(info)
        return self.config.resolvedpath(resource, self.vapp, self.vconf, self.realkind)


class CenSopranoDevProvider(Provider):

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
                config = dict(
                    type     = CenCfgParser,
                    optional = True,
                    default  = CenCfgParser('@cen-map-resources.ini')
                )
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
        self.config.setall(info)
        return self.config.resolvedpath(resource, self.vapp, self.vconf, self.storage)
