#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 27 mars 2019

@author: cluzetb
task for running SODA-SNOW sequence on HPC MULTINODE
'''

from vortex.layout.nodes import Driver, WorkshareFamily, LoopFamily
from tasks.crampon_soda import Soda_Task
from tasks.crampon_offline import Offline_Task


def setup(t, **kw):
    return Driver(
        tag = 'CRAMPON_sequence',  # change the tag ?
        ticket = t,
        nodes = [
            LoopFamily(
                tag='dates',
                ticket=t,
                nodes =[
                    # offline tasks are launched from assimdate_prev to assimdate
                    # -> last propagation from assimdate[-1] to enddate is outside the loop family.
                    WorkshareFamily(
                        tag='offline',
                        ticket = t,
                        workshareconf='members,allids',
                        worksharename='membersnode,idsnode',
                        worksharesize=10,
                        worksharelimit='nnodes',

                        nodes = [
                            Offline_Task(tag = 'offline', ticket=t, **kw),
                        ], **kw),
                    Soda_Task(tag='soda', ticket=t, active_callback=lambda s: 'off' in s.conf.openloop and s.conf.stopdate_next is not None, **kw),
                ],
                loopconf='stopdates',  # stopdates = assimdates.append(enddate)
                loopsuffix='+d{:s}',  # format the loop iterator (assimdate(s) as itself ( a string)
                **kw),
        ],
        options=kw
    )
