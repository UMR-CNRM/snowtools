'''
Created on 22 mai 2018

@author: lafaysse
'''
import numpy as np


class ESCROC_subensembles(dict):
    '''Define the different subensembles of ESCROC and provide the corresponding namelist components'''

    def __init__(self, subensemble, members):

        self.dicoptrad = {"B60": "B92", "B10": "B92", "TAR": "TA3", "TA+": "TA4", "T17": "T17"}
        self.dicageing = {"B60": 60., "B10": 10., "B120": 120., "B180": 180., "B240": 240., "TAR": 60., "TA+": 60., "T17": 60.}

        self.dicoptturb = {"RIL": "RIL", "RI1": "RIL", "RI2": "RIL", "M98": "M98"}

        self.dicz0 = {"RIL": 0.001, "RI1": 0.001, "RI2": 0.001, "M98": 0.001}
        self.dicrimax = {"RIL": 0.2, "RI1": 0.1, "RI2": 0.026, "M98": 0.026}
        self.dicxcvheatf = {"CV50000": 1.0, "CV30000": 0.6, "CV10000": 0.2}

        if subensemble == "E1":
            self.physical_options, self.snow_parameters, self.members = self.E1(members)

        elif subensemble == "E1tartes":
            self.physical_options, self.snow_parameters, self.members = self.E1tartes(members)

        elif subensemble == "E1notartes":
            self.physical_options, self.snow_parameters, self.members = self.E1notartes(members)

        elif subensemble in ["E2", "E2CLEAR"]:
            self.physical_options, self.snow_parameters, self.members = self.E2(members)

        elif subensemble in ["Crocus"]:
            self.physical_options, self.snow_parameters, self.members = self.Crocus(members)

    def E1(self, members):

        physical_options = []
        snow_parameters = []
        mb = 0

        for snowfall in ['V12', 'S02', 'A76']:
            for metamo in ['C13', 'F06', 'S-F']:
                for radiation in ['B60', 'B10', 'TAR', 'TA+']:
                    for turb in ['RIL', 'RI1', 'RI2', 'M98']:
                        for cond in ['Y81', 'I02']:
                            for holding in ['B92', 'SPK', 'B02']:
                                for compaction in ['B92', 'S14', 'T11']:
                                    for cv in ['CV10000', 'CV30000', 'CV50000']:

                                            mb += 1
                                            if mb in members:
                                                po, sp = self.convert_options(snowfall, metamo, radiation, turb, cond, holding, compaction, cv)
                                                physical_options.append(po)
                                                snow_parameters.append(sp)

        return physical_options, snow_parameters, members

    def E1tartes(self, members):
        """
        E1tartes is a random draw inside the big Tartes ensemble
        members is a sorted list of members id (ex 1...35)

        /!\ member identities will be different from one run to another

        """
        physical_options = []
        snow_parameters = []
        snowflist = ['V12', 'S02', 'A76']
        metamlist = ['C13', 'F06', 'S-F']
        radlist = ['T17']
        turblist = ['RIL', 'RI1', 'RI2', 'M98']
        condlist = ['Y81', 'I02']
        holdlist = ['B92', 'SPK', 'B02']
        complist = ['B92', 'S14', 'T11']
        cvlist = ['CV10000', 'CV30000', 'CV50000']
        # random draw (whithout replacement) of nmembers members in the whole population
        # be careful +1 because starts from 0
        memberslist = 1 + np.random.choice(len(snowflist) * len(metamlist) * len(radlist) * len(turblist) * len(condlist) * len(holdlist) * len(complist) * len(cvlist) + 1, len(members), replace = False)
        print(memberslist)

        mb = 0
        for snowfall in snowflist:
            for metamo in metamlist:
                for radiation in radlist:
                    for turb in turblist:
                        for cond in condlist:
                            for holding in holdlist:
                                for compaction in complist:
                                    for cv in cvlist:
                                            mb += 1
                                            if mb in memberslist:
                                                po, sp = self.convert_options(snowfall, metamo, radiation, turb, cond, holding, compaction, cv)
                                                physical_options.append(po)
                                                snow_parameters.append(sp)

        return physical_options, snow_parameters, memberslist

    def E1notartes(self, members):
        """
        /!\ quasi duplicate from E1tartes
        E1notartes is a duplicate from E1 with T17 replaced by B92

        /!\ member identities will be different from one run to another

        """
        physical_options = []
        snow_parameters = []
        snowflist = ['V12', 'S02', 'A76']
        metamlist = ['C13', 'F06', 'S-F']
        radlist = ['B60']
        turblist = ['RIL', 'RI1', 'RI2', 'M98']
        condlist = ['Y81', 'I02']
        holdlist = ['B92', 'SPK', 'B02']
        complist = ['B92', 'S14', 'T11']
        cvlist = ['CV10000', 'CV30000', 'CV50000']
        # random draw (whithout replacement) of nmembers members in the whole population
        # be careful +1 because starts from 0
        memberslist = 1 + np.random.choice(len(snowflist) * len(metamlist) * len(radlist) * len(turblist) * len(condlist) * len(holdlist) * len(complist) * len(cvlist) + 1, len(members), replace = False)
        print(memberslist)

        mb = 0
        for snowfall in snowflist:
            for metamo in metamlist:
                for radiation in radlist:
                    for turb in turblist:
                        for cond in condlist:
                            for holding in holdlist:
                                for compaction in complist:
                                    for cv in cvlist:
                                            mb += 1
                                            if mb in memberslist:
                                                po, sp = self.convert_options(snowfall, metamo, radiation, turb, cond, holding, compaction, cv)
                                                physical_options.append(po)
                                                snow_parameters.append(sp)

        return physical_options, snow_parameters, memberslist
        
    def E2(self, members):

        members = {1: ['V12', 'C13', 'B60', 'RI1', 'Y81', 'SPK', 'B92', 'CV30000'],
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

            po, sp = self.convert_options(*members[mb])
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
