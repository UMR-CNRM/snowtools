# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from vortex_cen.tasks.forcing.extract_s2m_points import ExtractS2MForcing


def setup(t, **kw):
    # Manually set the "iniconf" argument for local test
    # Otherwise, this is done by the JobAssistant
    if 'iniconf' in kw:
        iniconf = kw.pop('iniconf')
    else:
        iniconf = None
    return Driver(
        tag='extracts2m',
        ticket=t,
        nodes=[
            ExtractS2MForcing(tag='extracts2mforcing', ticket=t, **kw),
        ],
        options=kw,
        iniconf = iniconf
    )
