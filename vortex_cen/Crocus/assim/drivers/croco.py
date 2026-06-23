# -*- coding: utf-8 -*-
"""
The "escroc" driver allows to loop over an ensemble of OFFLINE executions without MPI parallelisation,
followed by a SODA assimilation step in a research context.
The ensemble members can combine an ensemble of meteorological forcings and different Crocus physics.
"""

from mkjob.nodes import Driver, WorkshareFamily, LoopFamily
from vortex_cen.tasks.surfex.pre_process import Soda_Namelist_Preprocess, Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.offline_ensemble import CrocO
from vortex_cen.tasks.surfex.soda import Soda


def setup(t, **kw):
    return Driver(
        tag = 'croco',
        ticket = t,
        nodes = [
            # Common namelist pre-processing
            Soda_Namelist_Preprocess(tag='soda_preprocess', ticket=t, **kw),
            Preprocess_Uenv_Namelist(tag='offline_preprocess', ticket=t, **kw),
            # assim sequence
            LoopFamily(
                tag='dates',
                ticket=t,
                nodes =[
                    # offline tasks are launched from assimdate_prev to assimdate
                    # -> last propagation from assimdate[-1] to enddate is outside the loop family.
                    WorkshareFamily(
                        tag='offline',
                        ticket = t,
                        workshareconf='members,members_id',
                        worksharename='membersnode,idsnode',
                        worksharesize=10,
                        worksharelimit='nnodes',

                        nodes = [
                            CrocO(tag = 'offline', ticket=t, **kw),
                        ], **kw),
                    Soda(tag='soda', ticket=t,
                        active_callback=lambda s: not s.conf.openloop and s.conf.stopdate_next is not None,
                        **kw),
                ],
                loopconf='stopdates',  # stopdates = assimdates.append(enddate)
                loopsuffix='+d{:s}',  # format the loop iterator (assimdate(s) as itself ( a string)
                **kw),
        ],
        options=kw
    )
