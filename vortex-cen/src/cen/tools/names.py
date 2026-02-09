"""
Specific namebuilders for CEN flow resources.

The main specificity is the date managment in file names : CEN files are
often associated to a period rather than a given time.
"""

from bronx.fancies import loggers

from vortex.tools.names import AbstractActualVortexNameBuilder, AbstractVortexNameBuilderProxy

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


class AbstractActualCenNameBuilder(AbstractActualVortexNameBuilder):

    _abstract = True

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)
        self.setdefault(cen_rawbasename=None)

    # A Vortex pathname may include the following bits

    def _pack_pathname_append_block(self, pathbits, d):
        """Pack the provider's block name."""
        # THE block ENTRY MAY BE MISSING
        if 'block' in d and d['block']:
            pathbits.append('_'.join(self._pack_std_items(d['block'])))

    # A bunch of utility methods that prepares values

    def _pack_std_item_member(self, value):
        # NUMBER with 4 digits
        return 'mb{:04d}'.format(value)

    def _pack_std_item_shortcutoff(self, value, default=''):
        """Abbreviate the cutoff name."""
        # DEFAULT IS ''
        return value[0].upper() if value is not None else default

    # A Vortex basename may include the following bits

    def _pack_std_basename_prefixstuff(self, d):  # @UnusedVariable
        """Adds any info about date, cutoff ..."""
        name0 = d['radical']
        name0 += self._join_basename_bit(d, 'src', prefix='.', sep='-')
        name0 += self._join_basename_bit(d, 'filtername', prefix='.', sep='-')
        name0 += self._join_basename_bit(d, 'geo', prefix='.', sep='-')
        name0 += self._join_basename_bit(d, 'compute', prefix='.', sep='-')
        return name0

    # Methods that generates basenames

    def _pack_basename_std(self, d):
        """
        Main entry point to convert a description into a file name
        according to the so-called standard style.
        """
        if d['cen_rawbasename'] is not None:
            # ONLY AVAILABLE IN '@cen' NAMEBUILDERS
            return d['cen_rawbasename']
        else:
            return (self._pack_std_basename_prefixstuff(d) +  # NO LOWER() here
                    self._pack_std_basename_flowstuff(d) +
                    self._pack_std_basename_timestuff(d) +
                    self._pack_std_basename_suffixstuff(d))  # NO LOWER() here


class CenDateNameBuilder(AbstractActualCenNameBuilder):
    """A Standard CEN NameBuilder (with date and possibly cutoff)."""

    _footprint = dict(
        info = 'A CEN Vortex NameBuilder (with date and possibly cutoff)',
        attr = dict(
            name = dict(
                values = ['date@cen', ],
            ),
        )
    )

    # Methods that generates pathnames

    def _pack_pathname_std(self, d):
        pathbits = self._pack_pathname_init(d)
        self._pack_pathname_append_flowdate(pathbits, d)
        self._pack_pathname_append_scenario(pathbits, d)
        self._pack_pathname_append_member(pathbits, d)
        self._pack_pathname_append_block(pathbits, d)
        return pathbits


class CenFlatNameBuilder(AbstractActualCenNameBuilder):
    """'A Standard CEN NameBuilder (without date or period)."""

    _footprint = dict(
        info = 'A Standard CEN NameBuilder (without date or period in the pathname)',
        attr = dict(
            name = dict(
                values = ['flat@cen', ],
            ),
        )
    )

    # A Vortex basename may include the following bits
    def _pack_std_basename_flowtuff(self, d):
        name = ''
        if d['flow'] is not None:
            pstuff = self._pack_std_items_periodstuff(d['flow'])
            if pstuff:
                name += '.' + pstuff
        return name

    # Methods that generates pathnames
    def _pack_pathname_std(self, d):
        pathbits = self._pack_pathname_init(d)
        self._pack_pathname_append_scenario(pathbits, d)
        self._pack_pathname_append_member(pathbits, d)
        self._pack_pathname_append_block(pathbits, d)
        return pathbits


class CenNameBuilder(AbstractVortexNameBuilderProxy):

    _footprint = dict(
        info = 'Standard CEN NameBuilder Proxy',
        attr = dict(
            name = dict(
                values = ['cen', ],
                default = 'cen',
            ),
        )
    )

    def _pick_actual_builder(self, d):
        """Given the input dictionary, returns the appropriate builder object."""
        actual_builder_name = 'flat@cen'
        if 'flow' in d and isinstance(d['flow'], (tuple, list)):
            flowkeys = set()
            for item in [item for item in d['flow'] if isinstance(item, dict)]:
                flowkeys.update(item.keys())
            if 'date' in flowkeys:
                actual_builder_name = 'date@cen'
        return self._get_builder(actual_builder_name)
