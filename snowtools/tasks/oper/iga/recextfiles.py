#!/usr/bin/env python
# -*- coding:Utf-8 -*-

__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

import vortex
from vortex import toolbox
from vortex.layout.nodes import Driver

from iga.tools.apps import OpTask
from . import safran_analyse_ensemble as anaens

def setup(t, **kw):
    return Driver(
        tag    = 'recextfiles',
        ticket = t,
        nodes  = [
            anaens.Safran(tag='safran', ticket=t, **kw),
        ],
    )
