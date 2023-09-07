# -*- coding: utf-8 -*-
'''
Created on 19 juin 2018

@author: cluzetb, adapted from R. Champavier Talagrand_rank class
'''

import random

import numpy as np


def rankDiagram(ens, observations, isSorted = False, nbins = 36, printrank=False, param=None, titreFig=None, pathFig=None):
    """"
    @author : B. Cluzet, june 2018
    generic CRPS computation from Cluzet first version, Lafaysse and Champavier talagrand function
    Champavier code would be a good source of inspiration for optimisation through pandas (avoiding the for loop)
    @params : ens : Nens x NDate matrix (np.array)
              obs : NDate vector
    /!\
        - same dates for ens and obs
        - the user should adapt ens and obs beforehand to make it directly comparable
        - convention : nan value when a member is not defined
        - obs: no nan
        - ens not necessarily sorted
        - nbins better <= NMembers +1  ???
    """
    random.seed()
    if not isSorted:
        # pour chaque date, on trie les membres
        ens = np.sort(ens, axis=0)
    nbMembres = ens.shape[0]  # Nb total membres (définis ou non)
    fact = float(nbins) / float(nbMembres + 1)
    rank = np.zeros(nbins)

    for iDate, obs in enumerate(observations):  # loop over obs (dates)
        ensDate = np.ma.masked_invalid(ens[:, iDate])
        obsDate = np.ma.masked_invalid(obs)
        # mask membres that have nan values for this date and count them
        nEnsDate = np.ma.count(ensDate)  # Nb total membres  définis à cette date
        nObsDate = np.ma.count(obsDate)
        if nEnsDate > 0 and nObsDate > 0:
            # Matthieu :
            # P :  n° de la prévision auquelle l'obs est directement inférieure
            # on gère le cas où plusieurs membres ont la même valeur que l'obs (0 le + souvent)
            # Dans ce cas on attribue une position aléatoire entre les positions PL et PR
            # sinon PR=PL+1 donc Randint n'a qu'une possibilité

            PL = np.searchsorted(ensDate, obsDate, side="left") + 1
            PR = np.searchsorted(ensDate, obsDate, side="right") + 1
            P = random.randint(PL, PR)
            # Conversion d'unité
            BIN = int(round(P * fact))

            if P == 1:
                rank[0] += 1.
            elif P == nEnsDate + 1:
                rank[nbins - 1] += 1.
            else:
                if nEnsDate == nbMembres:
                    rank[BIN - 1] += 1.
                else:  # on convertit l'histogramme de résolution nEnsDate en résolution nbMembres
                    BIN = int(round(float(nbMembres / nEnsDate) * P * fact))
                    rank[BIN - 1] += 1.

    if np.sum(rank) > 0.:
        frequency = rank / float(np.sum(rank))
    else:
        frequency = rank
    return frequency, np.sum(rank)
