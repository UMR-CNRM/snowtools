"""
This module provides some pre-defined attributes descriptions or combined sets
of attributes description that could be used in the footprint definition of any
class which follow the :class:`footprints.Footprint` syntax.
"""

from bronx.stdtypes.date import Date
import footprints

from vortex.syntax.stddeco import namebuilding_insert, generic_pathname_insert


#: Usual definition of a date period for CEN resources.

cendateperiod = footprints.Footprint(info='Abstract date period',
                                     attr=dict(datebegin=dict(info="The resource's begin date.",
                                                              alias=('begindate',),
                                                              type=Date,
                                                              optional=False),
                                               dateend=dict(info="The resource's end date.",
                                                            alias=('enddate',),
                                                            type=Date,
                                                            optional=False),
                                               ))

cendateperiod_deco = footprints.DecorativeFootprint(
    cendateperiod,
    decorator=[namebuilding_insert('cen_period', lambda self: [self.datebegin.ymdh, self.dateend.ymdh]),
               generic_pathname_insert('datebegin', lambda self: self.datebegin, setdefault=True),
               generic_pathname_insert('dateend', lambda self: self.dateend, setdefault=True)])


def show():
    """Returns available items and their type."""
    dmod = globals()
    for stda in sorted(filter(lambda x: x.startswith('a_') or isinstance(dmod[x], footprints.Footprint),
                              dmod.keys())):
        print('{} ( {} ) :\n  {}\n'.format(stda, type(dmod[stda]).__name__, dmod[stda]))
