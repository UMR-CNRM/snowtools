#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 27 mars 2019

@author: cluzetb
task for running SODA-SNOW sequence on HPC MULTINODE
'''
from tasks.crocO_common import CrocO_In
from tasks.crocO_offline import Offline_Task
from tasks.crocO_soda import Soda_Task
from vortex.layout.nodes import Driver, WorkshareFamily, LoopFamily


def setup(t, **kw):
    return Driver(
        tag = 'CrocO_sequence',  # change the tag ?
        ticket = t,
        nodes = [
            # fetching common files
            CrocO_In(tag='common', ticket=t, **kw),

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
                            Offline_Task(tag = 'offline', ticket=t, **kw),
                        ], **kw),
                    Soda_Task(tag='soda', ticket=t, active_callback=lambda s: 'off' in s.conf.openloop and s.conf.stopdate_next is not None, **kw),
                ],
                loopconf='stopdates',  # stopdates = assimdates.append(enddate)
                loopsuffix='+d{:s}',  # format the loop iterator (assimdate(s) as itself ( a string)
                **kw),
            # CrocO_Out(tag = 'pp_out', ticket = t, active_callback=lambda s: 'on' in s.conf.pickleit, **kw)
        ],
        options=kw
    )
