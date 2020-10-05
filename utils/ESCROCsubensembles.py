'''
Created on 22 mai 2018

@author: lafaysse
'''
import numpy as np


class ESCROC_subensembles(dict):
    '''Define the different subensembles of ESCROC and provide the corresponding namelist components'''

    def __init__(self, subensemble, members, randomDraw = False):

        self.dicoptrad = {"B60": "B92", "B10": "B92", "TAR": "TA3", "TA+": "TA4", "T17": "T17"}
        self.dicageing = {"B60": 60., "B10": 10., "B120": 120., "B180": 180., "B240": 240., "TAR": 60., "TA+": 60., "T17": 60.}

        self.dicoptturb = {"RIL": "RIL", "RI1": "RIL", "RI2": "RIL", "M98": "M98"}

        self.dicz0 = {"RIL": 0.001, "RI1": 0.001, "RI2": 0.001, "M98": 0.001}
        self.dicrimax = {"RIL": 0.2, "RI1": 0.1, "RI2": 0.026, "M98": 0.026}
        self.dicxcvheatf = {"CV50000": 1.0, "CV30000": 0.6, "CV10000": 0.2}

        if subensemble == "E1":
            self.physical_options, self.snow_parameters, self.members = self.E1(members, randomDraw)

        elif subensemble == "E1tartes":
            self.physical_options, self.snow_parameters, self.members = self.E1tartes(members, randomDraw)

        elif subensemble == "E1notartes":
            self.physical_options, self.snow_parameters, self.members = self.E1notartes(members, randomDraw)

        elif subensemble in ["E2", "E2open"]:
            self.physical_options, self.snow_parameters, self.members = self.E2(members)

        elif subensemble in ["E2Tartes", "E2tartes"]: # Tuzet et al TC 2020
            self.physical_options, self.snow_parameters, self.members = self.E2tartes(members)

        elif subensemble in ["Crocus"]:
            self.physical_options, self.snow_parameters, self.members = self.Crocus(members)
        else:
            raise Exception("The subensemble selected is not defined in ESCROCsubensembles.py")

    def E1(self, members, randomDraw = False):

        self.snowflist = ['V12', 'S02', 'A76']
        self.metamlist = ['C13', 'F06', 'S-F']
        self.radlist = ['B60', 'B10', 'TAR', 'TA+']
        self.turblist = ['RIL', 'RI1', 'RI2', 'M98']
        self.condlist = ['Y81', 'I02']
        self.holdlist = ['B92', 'SPK', 'B02']
        self.complist = ['B92', 'S14', 'T11']
        self.cvlist = ['CV10000', 'CV30000', 'CV50000']

        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)

        return physical_options, snow_parameters, memberslist

    def E1tartes(self, members, randomDraw):
        """
        E1tartes is a random draw inside the big Tartes ensemble
        members is a sorted list of members id (ex 1...35)

        /!\ member identities will be different from one run to another

        NEW : 12/11/18 :
                activate/deactivate random draw

        """

        self.snowflist = ['V12', 'S02', 'A76']
        self.metamlist = ['C13', 'F06', 'S-F']
        self.radlist = ['T17']
        self.turblist = ['RIL', 'RI1', 'RI2', 'M98']
        self.condlist = ['Y81', 'I02']
        self.holdlist = ['B92', 'SPK', 'B02']
        self.complist = ['B92', 'S14', 'T11']
        self.cvlist = ['CV10000', 'CV30000', 'CV50000']

        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)
        return physical_options, snow_parameters, memberslist

    def E1notartes(self, members, randomDraw):
        """
        /!\ quasi duplicate from E1tartes
        E1notartes is a duplicate from E1 with T17 replaced by B92

        /!\ member identities will be different from one run to another
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
        physical_options, snow_parameters, memberslist = self.drawMembers(members, randomDraw)

        return physical_options, snow_parameters, memberslist

    def E2(self, members):

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

        return physical_options, snow_parameters, members

    def E2tartes(self, members):

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

        return physical_options, snow_parameters, members

    def Crocus(self, members):

        members = {1: ['V12', 'C13', 'B60', 'RI1', 'Y81', 'B92', 'B92', 'CV30000']}

        physical_options = []
        snow_parameters = []

        for mb in members:

            po, sp = self.convert_options(*members[mb])
            physical_options.append(po)
            snow_parameters.append(sp)

        return physical_options, snow_parameters, members

    def drawMembers(self, members, randomDraw):
        # if randomDraw : random draw (whithout replacement) of len(members) in the whole population
        # else: returns the members corresponding to members
        # be careful +1 because starts from 0
        if randomDraw:
            memberslist = 1 + np.random.choice(len(self.snowflist) * len(self.metamlist) * len(self.radlist) * len(self.turblist) * len(self.condlist) * len(self.holdlist) * len(self.complist) * len(self.cvlist), len(members), replace = False)
        else:
            memberslist = members
        physical_options = []
        snow_parameters = []
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
                                                po, sp = self.convert_options(snowfall, metamo, radiation, turb, cond, holding, compaction, cv)
                                                physical_options.append(po)
                                                snow_parameters.append(sp)
        return physical_options, snow_parameters, memberslist

    def convert_options(self, snowfall, metamo, radiation, turb, cond, holding, compaction, cv):

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
            xvaging_noglacier=self.dicageing[radiation],
            xz0sn=self.dicz0[turb],
            x_ri_max=self.dicrimax[turb],
            xcvheatf=self.dicxcvheatf[cv],
        )

        return physical_options, snow_parameters
