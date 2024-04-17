# -*- coding: utf-8 -*-
'''
Created on 18 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Task
from cen.layout.nodes import S2MTaskMixIn


class _VortexTask(Task, S2MTaskMixIn):  # Inherits from the standard Vortex Task and CEN-specific methods
    '''
    Abstract class defining the common sequence of actions for CEN's vortex tasks.

    A vortex task is the sequence of actions to execute a single algo component.

    It always follow this procedure (definied in the main `process` method) :

        1. fetch all necessary input resources (files):
            - from an archive machine --> on a transfert node ('early-fetch')
            - from the local machine --> on a compute node ('fetch')

        2. execute the algo component (an executable, a script or·a sequence of instructions)
           of inputs resources necessary to run an algo component (an executable, a script or a
           sequence of instructions) --> 'compute'

        3. save output resources (files produced or modified by the algo component)
            - to the local machine --> on a compute node ('backup')
            - to an archive machine --> on a compute node ('late-backup')

    To implement a new task inheriting from this abstract class, implement at least the following methods :
    * get_remote_inputs
    * algo
    * put_remote_inputs

    See their respective documentation for more details.
    '''

    def process(self):
        """
        Main method definig the task's sequence of actions
        """
        self.common_args = [self.conf.xpid, self.conf.geometry]
        # TODO : passer toute la conf en kw ?
        self.common_kw   = dict(
            datebegin = self.conf.datebegin,
            dateend   = self.conf.dateend,
            uenv      = self.conf.uenv,
            vapp      = self.conf.vapp,
        )

        if 'early-fetch' in self.steps:  # Executed on a TRANSFERT NODE to fetch inputs from a remote
            self.get_remote_inputs()

        if 'fetch' in self.steps:  # Executed on a COMPUTE NODE to fetch resources already in the local cache
            self.get_local_inputs()

        if 'compute' in self.steps:  # Algo component (1 per task) executed on the compute node
            self.algo()

        if 'backup' in self.steps:  # Execute on the COMPUTE NODE to save outputs on the local cache
            self.put_local_outputs()

        if 'late-backup' in self.steps:  # Executed on a TRANSFERT NODE to save outputs to a remote destination
            self.put_remote_outputs()

    def get_remote_inputs(self):
        """
        Implement this method in your task to fetch all resources stored remotely (on Hendrix, sxcen,...).
        You can either use standard Vortex input toolboxes or the CEN-specific `vortexIO` tool.

        With vortexIO imported as `io`, the call sequence of this methods should look like::

            io.get_forcing(self.conf.datebegin, self.conf.dateend, self.conf.xpid_forcing, self.conf.geometry)

            io.get_prep(self.conf.datebegin, self.conf.dateend, self.conf.xpid_prep, self.conf.geometry)

            io.get_pgd()

            io.get_namelist()
        """
        raise NotImplementedError()

    def get_local_inputs(self):
        """
        Implement this method in your task to fetch all resources already stored on the local (HPC) cache.
        You can either use standard Vortex input toolboxes or the CEN-specific `vortexIO` tool.
        """
        # self.get_remote_inputs()  # TODO : check if really necessary / good practice
        pass

    def algo(self):
        """
        Implement this method to call your task's ONLY algo component.
        You can either use standard Vortex input toolboxes or the CEN-specific `vortexAlgo` tool.
        """
        raise NotImplementedError()

    def put_local_outputs(self):
        """
        Implement this method in your task to save resources on the local (HPC) cache.
        You can either use standard Vortex input toolboxes or the CEN-specific `vortexIO` tool.
        """
        # self.put_remote_outputs()  # TODO : check if really necessary / good practice
        pass

    def put_remote_outputs(self):
        """
        Implement this method in your task to save resources remotely (on Hendrix, sxcen,...).
        You can either use standard Vortex input toolboxes or the CEN-specific `vortexIO` tool.
        """
        raise NotImplementedError()
