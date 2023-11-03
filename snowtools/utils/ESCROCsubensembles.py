# -*- coding: utf-8 -*-
'''
Created on 22 mai 2018

:Authors:
    M. Lafaysse
'''

import numpy as np


class ESCROC_subensembles(dict):
    """
    Define the different subensembles of ESCROC and provide the corresponding namelist components

    See methods for available subensemble name and description

    :param subensemble: The subensemble name
    :type subensemble: str
    :param members: Passed to the subsensemble definition
    :param randomDraw: if True, perform a random Draw for E1-like subensemble
    :type randomDraw: bool
    """

    def __init__(self, subensemble, members, randomDraw = False):

        self.dicoptrad = {"B60": "B92", "B10": "B92", "TAR": "T17", "TA+": "T17", "T17": "T17"}
        self.dicageing = {"B60": 60., "B10": 10., "B120": 120., "B180": 180., "B240": 240., "TAR": 60., "TA+": 60., "T17": 60.}
        self.dicimpurdry = {"B60": [0, 0], "B10": [0, 0], "T17": 0, "TAR": [1.E-14, 0], "TA+": [5.E-13, 0]}
        self.dicimpurwet = {"B60": [0, 0], "B10": [0, 0], "T17": 0, "TAR": [3.5E-14, 0], "TA+": [3.5E-13, 0]}
        # In Lafaysse et al, 2017 with TAR option the impurity concentration dry increase rate was 4 ng/g/day
        # = 4E-12/3600 g kg-1 s-1 at the surface with an exponential decrease factor exp(-0.5z/0.05)
        # For a typical first layer of 1 cm and 250 kg/m3 this is equivalent to :
        # 4E-12/3600 * exp(-1) * 0.1 * 250 g m-2 s-1 = 1.02 E-14 ~ 1 E-14 g m-2 s-1
        # which is the new unit of deposition fluxes

        # In Lafaysse et al, 2017 with TAR option the impurity concentration wet inital value was 5 ng/g
        # = 5E-12 * 0.1 * 250 g m-2 for a 1 cm new layer.
        # Assuming a 1 cm / hour precipitation rate, this is equivalent to a deposition rate of
        # 5E-12 * 0.1 * 250 / 3600. ~ 3.5E-14 g m-2 s-1

        self.dicoptturb = {"RIL": "RIL", "RI1": "RIL", "RI2": "RIL", "M98": "M98"}

        self.dicz0 = {"RIL": 0.001, "RI1": 0.001, "RI2": 0.001, "M98": 0.001}
        self.dicrimax = {"RIL": 0.2, "RI1": 0.1, "RI2": 0.026, "M98": 0.026}
        self.dicxcvheatf = {"CV50000": 1.0, "CV30000": 0.6, "CV10000": 0.2}

        if subensemble == "E1":
            self.physical_options, self.snow_parameters, self.members = self.E1(members, randomDraw)
        elif subensemble == "E1B21":
            self.physical_options, self.snow_parameters, self.members = self.E1B21(members, randomDraw)
        elif subensemble == "E1tartes":
            self.physical_options, self.snow_parameters, self.members = self.E1tartes(members, randomDraw)

        elif subensemble == "E1notartes":
            self.physical_options, self.snow_parameters, self.members = self.E1notartes(members, randomDraw)

        elif subensemble in ["E2", "E2open"]:
            self.physical_options, self.snow_parameters, self.members = self.E2(members)

        elif subensemble == "E2MIP":
            self.physical_options, self.snow_parameters, self.members = self.E2MIP(members)

        elif subensemble == "E2B21":
            self.physical_options, self.snow_parameters, self.members = self.E2B21(members)

        elif subensemble == "E2MIPB21":
            self.physical_options, self.snow_parameters, self.members = self.E2MIPB21(members)

        elif subensemble == "E2tartes":  # Tuzet et al TC 2020 (Lautaret)
            self.physical_options, self.snow_parameters, self.members = self.E2tartes(members)

        elif subensemble == "E2MIPtartes":  # Dumont, Tuzet et al JGR 2020 (Russia)
            self.physical_options, self.snow_parameters, self.members = self.E2MIPtartes(members)

        elif subensemble in ["Crocus"]:
            self.physical_options, self.snow_parameters, self.members = self.Crocus(members)
        else:
            raise Exception("The subensemble selected is not defined in ESCROCsubensembles.py")

    def E1(self, members, randomDraw = False):
        """
        E1 subensemble: the most complete one
        """

        self.snowflist = ['V12', 'S02', 'A76']
        self.metamlist = ['C13', 'F06', 'S-F']
        self.radlist = ['B60', 'B10', 'TAR', 'TA+']
        self.turblist = ['RIL', 'RI1', 'RI2', 'M98']
        self.condlist = ['Y81', 'I02']
        self.holdlist = ['B92', 'SPK', 'B02']
        self.complist = ['B92', 'S14', 'T11']
        self.cvlist = ['CV10000', 'CV30000', 'CV50000']

        self.size = len(self.snowflist) * len(self.metamlist) * len(self.radlist) * len(self.turblist) * len(self.condlist) * len(self.holdlist) * len(self.complist) * len(self.cvlist)
        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)

        return physical_options, snow_parameters, memberslist

    def E1B21(self, members, randomDraw = False):
        """
        E1 subensemble: the most complete one with B21 metamorphism
        """

        self.snowflist = ['V12', 'S02', 'A76']
        self.metamlist = ['B21', 'F06', 'S-F']
        self.radlist = ['B60', 'B10', 'TAR', 'TA+']
        self.turblist = ['RIL', 'RI1', 'RI2', 'M98']
        self.condlist = ['Y81', 'I02']
        self.holdlist = ['B92', 'SPK', 'B02']
        self.complist = ['B92', 'S14', 'T11']
        self.cvlist = ['CV10000', 'CV30000', 'CV50000']

        self.size = len(self.snowflist) * len(self.metamlist) * len(self.radlist) * len(self.turblist) * len(self.condlist) * len(self.holdlist) * len(self.complist) * len(self.cvlist)
        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)

        return physical_options, snow_parameters, memberslist


    def E1tartes(self, members, randomDraw):
        """
        E1tartes is a random draw inside the big Tartes ensemble
        members is a sorted list of members id (ex 1...35)

        .. warning::
           Member identities will be different from one run to another

        NEW : 12/11/18 : activate/deactivate random draw

        """

        self.snowflist = ['V12', 'S02', 'A76']
        self.metamlist = ['C13', 'F06', 'S-F']
        self.radlist = ['T17']
        self.turblist = ['RIL', 'RI1', 'RI2', 'M98']
        self.condlist = ['Y81', 'I02']
        self.holdlist = ['B92', 'SPK', 'B02']
        self.complist = ['B92', 'S14', 'T11']
        self.cvlist = ['CV10000', 'CV30000', 'CV50000']
        self.size = len(self.snowflist) * len(self.metamlist) * len(self.radlist) * len(self.turblist) * len(self.condlist) * len(self.holdlist) * len(self.complist) * len(self.cvlist)
        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)
        return physical_options, snow_parameters, memberslist

    def E1notartes(self, members, randomDraw):
        """
        Quasi duplicate from E1tartes
        E1notartes is a duplicate from E1 with T17 replaced by B92

        .. warning::
           member identities will be different from one run to another
           the returned models are randomly drawn among E1notartes

        """
        self.snowflist = ['V12', 'S02', 'A76']
        self.metamlist = ['C13', 'F06', 'S-F']
        self.radlist = ['B60']
        self.turblist = ['RIL', 'RI1', 'RI2', 'M98']
        self.condlist = ['Y81', 'I02']
        self.holdlist = ['B92', 'SPK', 'B02']
        self.complist = ['B92', 'S14', 'T11']
        self.cvlist = ['CV10000', 'CV30000', 'CV50000']
        self.size = len(self.snowflist) * len(self.metamlist) * len(self.radlist) * len(self.turblist) * len(self.condlist) * len(self.holdlist) * len(self.complist) * len(self.cvlist)
        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)

        return physical_options, snow_parameters, memberslist

    def E2(self, members):
        """
        Optimal subensemble at Col de Porte
        """

        allmembers = {1: ['V12', 'C13', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      2: ['V12', 'C13', 'B60', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      3: ['V12', 'C13', 'B60', 'RI2', 'Y81', 'B92', 'S14', 'CV30000'],
                      4: ['V12', 'C13', 'B10', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      5: ['V12', 'C13', 'B60', 'RI1', 'I02', 'SPK', 'T11', 'CV50000'],
                      6: ['V12', 'C13', 'B60', 'RI2', 'I02', 'SPK', 'S14', 'CV50000'],
                      7: ['V12', 'C13', 'B10', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      8: ['V12', 'F06', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV30000'],
                      9: ['V12', 'F06', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      10: ['V12', 'F06', 'B60', 'RIL', 'I02', 'B92', 'T11', 'CV50000'],
                      11: ['V12', 'S-F', 'B60', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      12: ['V12', 'S-F', 'B10', 'RI2', 'I02', 'SPK', 'B92', 'CV30000'],
                      13: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      14: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV30000'],
                      15: ['V12', 'S-F', 'B60', 'M98', 'Y81', 'SPK', 'B92', 'CV30000'],
                      16: ['V12', 'S-F', 'B10', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'C13', 'B60', 'M98', 'Y81', 'B92', 'S14', 'CV30000'],
                      18: ['S02', 'C13', 'B10', 'RI1', 'I02', 'B92', 'B92', 'CV30000'],
                      19: ['S02', 'F06', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      20: ['S02', 'F06', 'B60', 'M98', 'I02', 'B92', 'B92', 'CV30000'],
                      21: ['S02', 'F06', 'B60', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      22: ['S02', 'F06', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      23: ['S02', 'F06', 'B10', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      24: ['S02', 'F06', 'B10', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      25: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'B92', 'B92', 'CV50000'],
                      26: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      27: ['S02', 'S-F', 'B60', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      28: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      29: ['A76', 'F06', 'B60', 'M98', 'I02', 'B02', 'S14', 'CV30000'],
                      30: ['A76', 'F06', 'B60', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      31: ['A76', 'F06', 'B10', 'RIL', 'I02', 'B92', 'B92', 'CV30000'],
                      32: ['A76', 'S-F', 'B10', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      33: ['A76', 'S-F', 'B10', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      34: ['A76', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      35: ['A76', 'S-F', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)
        self.size = 35
        return physical_options, snow_parameters, members

    def E2B21(self, members):
        """
        Optimal subensemble at Col de Porte
        """

        allmembers = {1: ['V12', 'B21', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      2: ['V12', 'B21', 'B60', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      3: ['V12', 'B21', 'B60', 'RI2', 'Y81', 'B92', 'S14', 'CV30000'],
                      4: ['V12', 'B21', 'B10', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      5: ['V12', 'B21', 'B60', 'RI1', 'I02', 'SPK', 'T11', 'CV50000'],
                      6: ['V12', 'B21', 'B60', 'RI2', 'I02', 'SPK', 'S14', 'CV50000'],
                      7: ['V12', 'B21', 'B10', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      8: ['V12', 'F06', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV30000'],
                      9: ['V12', 'F06', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      10: ['V12', 'F06', 'B60', 'RIL', 'I02', 'B92', 'T11', 'CV50000'],
                      11: ['V12', 'S-F', 'B60', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      12: ['V12', 'S-F', 'B10', 'RI2', 'I02', 'SPK', 'B92', 'CV30000'],
                      13: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      14: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV30000'],
                      15: ['V12', 'S-F', 'B60', 'M98', 'Y81', 'SPK', 'B92', 'CV30000'],
                      16: ['V12', 'S-F', 'B10', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'B21', 'B60', 'M98', 'Y81', 'B92', 'S14', 'CV30000'],
                      18: ['S02', 'B21', 'B10', 'RI1', 'I02', 'B92', 'B92', 'CV30000'],
                      19: ['S02', 'F06', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      20: ['S02', 'F06', 'B60', 'M98', 'I02', 'B92', 'B92', 'CV30000'],
                      21: ['S02', 'F06', 'B60', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      22: ['S02', 'F06', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      23: ['S02', 'F06', 'B10', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      24: ['S02', 'F06', 'B10', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      25: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'B92', 'B92', 'CV50000'],
                      26: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      27: ['S02', 'S-F', 'B60', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      28: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      29: ['A76', 'F06', 'B60', 'M98', 'I02', 'B02', 'S14', 'CV30000'],
                      30: ['A76', 'F06', 'B60', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      31: ['A76', 'F06', 'B10', 'RIL', 'I02', 'B92', 'B92', 'CV30000'],
                      32: ['A76', 'S-F', 'B10', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      33: ['A76', 'S-F', 'B10', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      34: ['A76', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      35: ['A76', 'S-F', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)
        self.size = 35
        return physical_options, snow_parameters, members

    def E2B21(self, members):
        """
        Optimal subensemble at Col de Porte
        """

        allmembers = {1: ['V12', 'B21', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      2: ['V12', 'B21', 'B60', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      3: ['V12', 'B21', 'B60', 'RI2', 'Y81', 'B92', 'S14', 'CV30000'],
                      4: ['V12', 'B21', 'B10', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      5: ['V12', 'B21', 'B60', 'RI1', 'I02', 'SPK', 'T11', 'CV50000'],
                      6: ['V12', 'B21', 'B60', 'RI2', 'I02', 'SPK', 'S14', 'CV50000'],
                      7: ['V12', 'B21', 'B10', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      8: ['V12', 'F06', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV30000'],
                      9: ['V12', 'F06', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      10: ['V12', 'F06', 'B60', 'RIL', 'I02', 'B92', 'T11', 'CV50000'],
                      11: ['V12', 'S-F', 'B60', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      12: ['V12', 'S-F', 'B10', 'RI2', 'I02', 'SPK', 'B92', 'CV30000'],
                      13: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      14: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV30000'],
                      15: ['V12', 'S-F', 'B60', 'M98', 'Y81', 'SPK', 'B92', 'CV30000'],
                      16: ['V12', 'S-F', 'B10', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'B21', 'B60', 'M98', 'Y81', 'B92', 'S14', 'CV30000'],
                      18: ['S02', 'B21', 'B10', 'RI1', 'I02', 'B92', 'B92', 'CV30000'],
                      19: ['S02', 'F06', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      20: ['S02', 'F06', 'B60', 'M98', 'I02', 'B92', 'B92', 'CV30000'],
                      21: ['S02', 'F06', 'B60', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      22: ['S02', 'F06', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      23: ['S02', 'F06', 'B10', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      24: ['S02', 'F06', 'B10', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      25: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'B92', 'B92', 'CV50000'],
                      26: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      27: ['S02', 'S-F', 'B60', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      28: ['S02', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      29: ['A76', 'F06', 'B60', 'M98', 'I02', 'B02', 'S14', 'CV30000'],
                      30: ['A76', 'F06', 'B60', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      31: ['A76', 'F06', 'B10', 'RIL', 'I02', 'B92', 'B92', 'CV30000'],
                      32: ['A76', 'S-F', 'B10', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      33: ['A76', 'S-F', 'B10', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      34: ['A76', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      35: ['A76', 'S-F', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)

        return physical_options, snow_parameters, members

    def E2MIP(self, members):
        """
        Optimal subensemble on MIP sites
        """

        allmembers = {1: ['V12', 'C13', 'B60', 'RI2', 'Y81', 'SPK', 'T11', 'CV30000'],
                      2: ['V12', 'C13', 'TAR', 'RI1', 'Y81', 'B02', 'S14', 'CV30000'],
                      3: ['V12', 'C13', 'TA+', 'M98', 'Y81', 'SPK', 'S14', 'CV10000'],
                      4: ['V12', 'F06', 'B60', 'RI2', 'Y81', 'SPK', 'T11', 'CV10000'],
                      5: ['V12', 'F06', 'B10', 'RI2', 'I02', 'SPK', 'B92', 'CV50000'],
                      6: ['V12', 'F06', 'B10', 'M98', 'Y81', 'SPK', 'B92', 'CV10000'],
                      7: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'T11', 'CV10000'],
                      8: ['V12', 'S-F', 'B60', 'RI1', 'Y81', 'SPK', 'T11', 'CV50000'],
                      9: ['V12', 'S-F', 'B60', 'RI1', 'I02', 'B92', 'T11', 'CV50000'],
                      10: ['V12', 'S-F', 'TA+', 'M98', 'I02', 'B02', 'B92', 'CV10000'],
                      11: ['S02', 'C13', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      12: ['S02', 'C13', 'B60', 'M98', 'Y81', 'SPK', 'S14', 'CV30000'],
                      13: ['S02', 'C13', 'B10', 'RIL', 'I02', 'B92', 'S14', 'CV50000'],
                      14: ['S02', 'C13', 'B10', 'M98', 'I02', 'SPK', 'T11', 'CV10000'],
                      15: ['S02', 'C13', 'TA+', 'RI2', 'Y81', 'B02', 'B92', 'CV50000'],
                      16: ['S02', 'F06', 'B60', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'F06', 'B60', 'RI2', 'Y81', 'B92', 'T11', 'CV10000'],
                      18: ['S02', 'F06', 'B10', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      19: ['S02', 'F06', 'TAR', 'RI2', 'I02', 'B02', 'B92', 'CV50000'],
                      20: ['S02', 'F06', 'TA+', 'RIL', 'I02', 'SPK', 'T11', 'CV10000'],
                      21: ['S02', 'F06', 'TA+', 'M98', 'I02', 'B92', 'B92', 'CV50000'],
                      22: ['S02', 'S-F', 'B10', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      23: ['S02', 'S-F', 'TAR', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      24: ['A76', 'C13', 'B60', 'RI1', 'Y81', 'SPK', 'T11', 'CV30000'],
                      25: ['A76', 'C13', 'B60', 'RI2', 'I02', 'B92', 'S14', 'CV50000'],
                      26: ['A76', 'C13', 'B10', 'RIL', 'I02', 'B02', 'B92', 'CV10000'],
                      27: ['A76', 'C13', 'B10', 'M98', 'I02', 'SPK', 'T11', 'CV50000'],
                      28: ['A76', 'C13', 'TAR', 'RI1', 'Y81', 'B92', 'B92', 'CV50000'],
                      29: ['A76', 'C13', 'TA+', 'RIL', 'I02', 'B02', 'S14', 'CV50000'],
                      30: ['A76', 'F06', 'B10', 'M98', 'I02', 'B92', 'T11', 'CV30000'],
                      31: ['A76', 'F06', 'TA+', 'RI1', 'Y81', 'B92', 'T11', 'CV10000'],
                      32: ['A76', 'F06', 'TA+', 'RI2', 'I02', 'B92', 'T11', 'CV10000'],
                      33: ['A76', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      34: ['A76', 'S-F', 'B10', 'M98', 'Y81', 'B02', 'T11', 'CV10000'],
                      35: ['A76', 'S-F', 'TAR', 'RI1', 'Y81', 'B02', 'S14', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)
        self.size = 35
        return physical_options, snow_parameters, members

    def E2MIPB21(self, members):
        """
        Optimal subensemble on MIP sites
        """

        allmembers = {1: ['V12', 'B21', 'B60', 'RI2', 'Y81', 'SPK', 'T11', 'CV30000'],
                      2: ['V12', 'B21', 'TAR', 'RI1', 'Y81', 'B02', 'S14', 'CV30000'],
                      3: ['V12', 'B21', 'TA+', 'M98', 'Y81', 'SPK', 'S14', 'CV10000'],
                      4: ['V12', 'F06', 'B60', 'RI2', 'Y81', 'SPK', 'T11', 'CV10000'],
                      5: ['V12', 'F06', 'B10', 'RI2', 'I02', 'SPK', 'B92', 'CV50000'],
                      6: ['V12', 'F06', 'B10', 'M98', 'Y81', 'SPK', 'B92', 'CV10000'],
                      7: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'T11', 'CV10000'],
                      8: ['V12', 'S-F', 'B60', 'RI1', 'Y81', 'SPK', 'T11', 'CV50000'],
                      9: ['V12', 'S-F', 'B60', 'RI1', 'I02', 'B92', 'T11', 'CV50000'],
                      10: ['V12', 'S-F', 'TA+', 'M98', 'I02', 'B02', 'B92', 'CV10000'],
                      11: ['S02', 'B21', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      12: ['S02', 'B21', 'B60', 'M98', 'Y81', 'SPK', 'S14', 'CV30000'],
                      13: ['S02', 'B21', 'B10', 'RIL', 'I02', 'B92', 'S14', 'CV50000'],
                      14: ['S02', 'B21', 'B10', 'M98', 'I02', 'SPK', 'T11', 'CV10000'],
                      15: ['S02', 'B21', 'TA+', 'RI2', 'Y81', 'B02', 'B92', 'CV50000'],
                      16: ['S02', 'F06', 'B60', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'F06', 'B60', 'RI2', 'Y81', 'B92', 'T11', 'CV10000'],
                      18: ['S02', 'F06', 'B10', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      19: ['S02', 'F06', 'TAR', 'RI2', 'I02', 'B02', 'B92', 'CV50000'],
                      20: ['S02', 'F06', 'TA+', 'RIL', 'I02', 'SPK', 'T11', 'CV10000'],
                      21: ['S02', 'F06', 'TA+', 'M98', 'I02', 'B92', 'B92', 'CV50000'],
                      22: ['S02', 'S-F', 'B10', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      23: ['S02', 'S-F', 'TAR', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      24: ['A76', 'B21', 'B60', 'RI1', 'Y81', 'SPK', 'T11', 'CV30000'],
                      25: ['A76', 'B21', 'B60', 'RI2', 'I02', 'B92', 'S14', 'CV50000'],
                      26: ['A76', 'B21', 'B10', 'RIL', 'I02', 'B02', 'B92', 'CV10000'],
                      27: ['A76', 'B21', 'B10', 'M98', 'I02', 'SPK', 'T11', 'CV50000'],
                      28: ['A76', 'B21', 'TAR', 'RI1', 'Y81', 'B92', 'B92', 'CV50000'],
                      29: ['A76', 'B21', 'TA+', 'RIL', 'I02', 'B02', 'S14', 'CV50000'],
                      30: ['A76', 'F06', 'B10', 'M98', 'I02', 'B92', 'T11', 'CV30000'],
                      31: ['A76', 'F06', 'TA+', 'RI1', 'Y81', 'B92', 'T11', 'CV10000'],
                      32: ['A76', 'F06', 'TA+', 'RI2', 'I02', 'B92', 'T11', 'CV10000'],
                      33: ['A76', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      34: ['A76', 'S-F', 'B10', 'M98', 'Y81', 'B02', 'T11', 'CV10000'],
                      35: ['A76', 'S-F', 'TAR', 'RI1', 'Y81', 'B02', 'S14', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)
        self.size = 35
        return physical_options, snow_parameters, members

    def E2MIPB21(self, members):
        """
        Optimal subensemble on MIP sites
        """

        allmembers = {1: ['V12', 'B21', 'B60', 'RI2', 'Y81', 'SPK', 'T11', 'CV30000'],
                      2: ['V12', 'B21', 'TAR', 'RI1', 'Y81', 'B02', 'S14', 'CV30000'],
                      3: ['V12', 'B21', 'TA+', 'M98', 'Y81', 'SPK', 'S14', 'CV10000'],
                      4: ['V12', 'F06', 'B60', 'RI2', 'Y81', 'SPK', 'T11', 'CV10000'],
                      5: ['V12', 'F06', 'B10', 'RI2', 'I02', 'SPK', 'B92', 'CV50000'],
                      6: ['V12', 'F06', 'B10', 'M98', 'Y81', 'SPK', 'B92', 'CV10000'],
                      7: ['V12', 'S-F', 'B60', 'RIL', 'Y81', 'SPK', 'T11', 'CV10000'],
                      8: ['V12', 'S-F', 'B60', 'RI1', 'Y81', 'SPK', 'T11', 'CV50000'],
                      9: ['V12', 'S-F', 'B60', 'RI1', 'I02', 'B92', 'T11', 'CV50000'],
                      10: ['V12', 'S-F', 'TA+', 'M98', 'I02', 'B02', 'B92', 'CV10000'],
                      11: ['S02', 'B21', 'B60', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      12: ['S02', 'B21', 'B60', 'M98', 'Y81', 'SPK', 'S14', 'CV30000'],
                      13: ['S02', 'B21', 'B10', 'RIL', 'I02', 'B92', 'S14', 'CV50000'],
                      14: ['S02', 'B21', 'B10', 'M98', 'I02', 'SPK', 'T11', 'CV10000'],
                      15: ['S02', 'B21', 'TA+', 'RI2', 'Y81', 'B02', 'B92', 'CV50000'],
                      16: ['S02', 'F06', 'B60', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'F06', 'B60', 'RI2', 'Y81', 'B92', 'T11', 'CV10000'],
                      18: ['S02', 'F06', 'B10', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      19: ['S02', 'F06', 'TAR', 'RI2', 'I02', 'B02', 'B92', 'CV50000'],
                      20: ['S02', 'F06', 'TA+', 'RIL', 'I02', 'SPK', 'T11', 'CV10000'],
                      21: ['S02', 'F06', 'TA+', 'M98', 'I02', 'B92', 'B92', 'CV50000'],
                      22: ['S02', 'S-F', 'B10', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      23: ['S02', 'S-F', 'TAR', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      24: ['A76', 'B21', 'B60', 'RI1', 'Y81', 'SPK', 'T11', 'CV30000'],
                      25: ['A76', 'B21', 'B60', 'RI2', 'I02', 'B92', 'S14', 'CV50000'],
                      26: ['A76', 'B21', 'B10', 'RIL', 'I02', 'B02', 'B92', 'CV10000'],
                      27: ['A76', 'B21', 'B10', 'M98', 'I02', 'SPK', 'T11', 'CV50000'],
                      28: ['A76', 'B21', 'TAR', 'RI1', 'Y81', 'B92', 'B92', 'CV50000'],
                      29: ['A76', 'B21', 'TA+', 'RIL', 'I02', 'B02', 'S14', 'CV50000'],
                      30: ['A76', 'F06', 'B10', 'M98', 'I02', 'B92', 'T11', 'CV30000'],
                      31: ['A76', 'F06', 'TA+', 'RI1', 'Y81', 'B92', 'T11', 'CV10000'],
                      32: ['A76', 'F06', 'TA+', 'RI2', 'I02', 'B92', 'T11', 'CV10000'],
                      33: ['A76', 'S-F', 'B60', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      34: ['A76', 'S-F', 'B10', 'M98', 'Y81', 'B02', 'T11', 'CV10000'],
                      35: ['A76', 'S-F', 'TAR', 'RI1', 'Y81', 'B02', 'S14', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)

        return physical_options, snow_parameters, members

    def E2tartes(self, members):
        """
        Like E2 but with ``T17`` rad option (Tartes)
        """

        allmembers = {1: ['V12', 'C13', 'T17', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      2: ['V12', 'C13', 'T17', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      3: ['V12', 'C13', 'T17', 'RI2', 'Y81', 'B92', 'S14', 'CV30000'],
                      4: ['V12', 'C13', 'T17', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      5: ['V12', 'C13', 'T17', 'RI1', 'I02', 'SPK', 'T11', 'CV50000'],
                      6: ['V12', 'C13', 'T17', 'RI2', 'I02', 'SPK', 'S14', 'CV50000'],
                      7: ['V12', 'C13', 'T17', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      8: ['V12', 'F06', 'T17', 'RIL', 'Y81', 'B92', 'S14', 'CV30000'],
                      9: ['V12', 'F06', 'T17', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      10: ['V12', 'F06', 'T17', 'RIL', 'I02', 'B92', 'T11', 'CV50000'],
                      11: ['V12', 'S-F', 'T17', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      12: ['V12', 'S-F', 'T17', 'RI2', 'I02', 'SPK', 'B92', 'CV30000'],
                      13: ['V12', 'S-F', 'T17', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      14: ['V12', 'S-F', 'T17', 'RIL', 'Y81', 'SPK', 'S14', 'CV30000'],
                      15: ['V12', 'S-F', 'T17', 'M98', 'Y81', 'SPK', 'B92', 'CV30000'],
                      16: ['V12', 'S-F', 'T17', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'C13', 'T17', 'M98', 'Y81', 'B92', 'S14', 'CV30000'],
                      18: ['S02', 'C13', 'T17', 'RI1', 'I02', 'B92', 'B92', 'CV30000'],
                      19: ['S02', 'F06', 'T17', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      20: ['S02', 'F06', 'T17', 'M98', 'I02', 'B92', 'B92', 'CV30000'],
                      21: ['S02', 'F06', 'T17', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      22: ['S02', 'F06', 'T17', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
                      23: ['S02', 'F06', 'T17', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      24: ['S02', 'F06', 'T17', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      25: ['S02', 'S-F', 'T17', 'RIL', 'I02', 'B92', 'B92', 'CV50000'],
                      26: ['S02', 'S-F', 'T17', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      27: ['S02', 'S-F', 'T17', 'RI1', 'I02', 'SPK', 'B92', 'CV30000'],
                      28: ['S02', 'S-F', 'T17', 'RIL', 'I02', 'SPK', 'S14', 'CV50000'],
                      29: ['A76', 'F06', 'T17', 'M98', 'I02', 'B02', 'S14', 'CV30000'],
                      30: ['A76', 'F06', 'T17', 'M98', 'I02', 'SPK', 'B92', 'CV30000'],
                      31: ['A76', 'F06', 'T17', 'RIL', 'I02', 'B92', 'B92', 'CV30000'],
                      32: ['A76', 'S-F', 'T17', 'RIL', 'Y81', 'B92', 'B92', 'CV30000'],
                      33: ['A76', 'S-F', 'T17', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      34: ['A76', 'S-F', 'T17', 'RIL', 'Y81', 'SPK', 'S14', 'CV50000'],
                      35: ['A76', 'S-F', 'T17', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)
        self.size = 35
        return physical_options, snow_parameters, members

    def E2MIPtartes(self, members):
        """
        Like E2MIP but with ``T17`` rad option (Tartes)
        """

        allmembers = {1: ['V12', 'C13', 'T17', 'RI2', 'Y81', 'SPK', 'T11', 'CV30000'],
                      2: ['V12', 'C13', 'T17', 'RI1', 'Y81', 'B02', 'S14', 'CV30000'],
                      3: ['V12', 'C13', 'T17', 'M98', 'Y81', 'SPK', 'S14', 'CV10000'],
                      4: ['V12', 'F06', 'T17', 'RI2', 'Y81', 'SPK', 'T11', 'CV10000'],
                      5: ['V12', 'F06', 'T17', 'RI2', 'I02', 'SPK', 'B92', 'CV50000'],
                      6: ['V12', 'F06', 'T17', 'M98', 'Y81', 'SPK', 'B92', 'CV10000'],
                      7: ['V12', 'S-F', 'T17', 'RIL', 'Y81', 'SPK', 'T11', 'CV10000'],
                      8: ['V12', 'S-F', 'T17', 'RI1', 'Y81', 'SPK', 'T11', 'CV50000'],
                      9: ['V12', 'S-F', 'T17', 'RI1', 'I02', 'B92', 'T11', 'CV50000'],
                      10: ['V12', 'S-F', 'T17', 'M98', 'I02', 'B02', 'B92', 'CV10000'],
                      11: ['S02', 'C13', 'T17', 'RIL', 'Y81', 'B92', 'S14', 'CV50000'],
                      12: ['S02', 'C13', 'T17', 'M98', 'Y81', 'SPK', 'S14', 'CV30000'],
                      13: ['S02', 'C13', 'T17', 'RIL', 'I02', 'B92', 'S14', 'CV50000'],
                      14: ['S02', 'C13', 'T17', 'M98', 'I02', 'SPK', 'T11', 'CV10000'],
                      15: ['S02', 'C13', 'T17', 'RI2', 'Y81', 'B02', 'B92', 'CV50000'],
                      16: ['S02', 'F06', 'T17', 'RIL', 'I02', 'SPK', 'B92', 'CV30000'],
                      17: ['S02', 'F06', 'T17', 'RI2', 'Y81', 'B92', 'T11', 'CV10000'],
                      18: ['S02', 'F06', 'T17', 'RI1', 'I02', 'B92', 'S14', 'CV30000'],
                      19: ['S02', 'F06', 'T17', 'RI2', 'I02', 'B02', 'B92', 'CV50000'],
                      20: ['S02', 'F06', 'T17', 'RIL', 'I02', 'SPK', 'T11', 'CV10000'],
                      21: ['S02', 'F06', 'T17', 'M98', 'I02', 'B92', 'B92', 'CV50000'],
                      22: ['S02', 'S-F', 'T17', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      23: ['S02', 'S-F', 'T17', 'RI2', 'I02', 'B92', 'B92', 'CV30000'],
                      24: ['A76', 'C13', 'T17', 'RI1', 'Y81', 'SPK', 'T11', 'CV30000'],
                      25: ['A76', 'C13', 'T17', 'RI2', 'I02', 'B92', 'S14', 'CV50000'],
                      26: ['A76', 'C13', 'T17', 'RIL', 'I02', 'B02', 'B92', 'CV10000'],
                      27: ['A76', 'C13', 'T17', 'M98', 'I02', 'SPK', 'T11', 'CV50000'],
                      28: ['A76', 'C13', 'T17', 'RI1', 'Y81', 'B92', 'B92', 'CV50000'],
                      29: ['A76', 'C13', 'T17', 'RIL', 'I02', 'B02', 'S14', 'CV50000'],
                      30: ['A76', 'F06', 'T17', 'M98', 'I02', 'B92', 'T11', 'CV30000'],
                      31: ['A76', 'F06', 'T17', 'RI1', 'Y81', 'B92', 'T11', 'CV10000'],
                      32: ['A76', 'F06', 'T17', 'RI2', 'I02', 'B92', 'T11', 'CV10000'],
                      33: ['A76', 'S-F', 'T17', 'RIL', 'I02', 'SPK', 'T11', 'CV50000'],
                      34: ['A76', 'S-F', 'T17', 'M98', 'Y81', 'B02', 'T11', 'CV10000'],
                      35: ['A76', 'S-F', 'T17', 'RI1', 'Y81', 'B02', 'S14', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*allmembers[mb])
            physical_options.append(po)
            snow_parameters.append(sp)
        self.size = 35
        return physical_options, snow_parameters, members

    def Crocus(self, members):
        """
        The deterministic member
        """

        members = {1: ['V12', 'C13', 'B60', 'RI1', 'Y81', 'B92', 'B92', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*members[mb])
            physical_options.append(po)
            snow_parameters.append(sp)

        return physical_options, snow_parameters, members

    def drawMembers(self, members, randomDraw):
        """
        if randomDraw : random draw (whithout replacement) of len(members) in the whole population
        else: returns the members corresponding to members
        be careful +1 because starts from 0

        :meta private:
        """
        if randomDraw:
            memberslist = 1 + np.random.choice(self.size, len(members), replace = False)
        else:
            memberslist = members
        physical_options = [None] * len(memberslist)
        snow_parameters = [None] * len(memberslist)
        mb = 0
        for snowfall in self.snowflist:
            for metamo in self.metamlist:
                for radiation in self.radlist:
                    for turb in self.turblist:
                        for cond in self.condlist:
                            for holding in self.holdlist:
                                for compaction in self.complist:
                                    for cv in self.cvlist:
                                        mb += 1
                                        if mb in memberslist:
                                            ind = list(memberslist).index(mb)
                                            po, sp = self.convert_options(
                                                snowfall, metamo, radiation, turb, cond, holding, compaction, cv)
                                            physical_options[ind] = po
                                            snow_parameters[ind] = sp
        return list(physical_options), list(snow_parameters), memberslist

    def convert_options(self, snowfall, metamo, radiation, turb, cond, holding, compaction, cv):
        """
        :meta private:
        """

        physical_options = dict(
            csnowfall=snowfall,
            csnowmetamo=metamo,
            csnowrad=self.dicoptrad[radiation],
            csnowcond=cond,
            csnowcomp=compaction,
            csnowhold=holding,
            csnowres=self.dicoptturb[turb]
        )

        snow_parameters = dict(
            xvaging_noglacier = self.dicageing[radiation],
            xz0sn = self.dicz0[turb],
            x_ri_max = self.dicrimax[turb],
            xcvheatf = self.dicxcvheatf[cv],
            ximpur_dry = self.dicimpurdry[radiation],
            ximpur_wet = self.dicimpurwet[radiation]
        )

        return physical_options, snow_parameters
