#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 19 juin 2018

@author: cluzetb, adapted from R. Champavier Talagrand_rank class
'''
# from __future__ import unicode_literals
# Author : Robin C

import os
import random

import pandas

import matplotlib.pyplot as plt
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


def Talagrand(Fich_MOD, Fich_OBS, NMembre, head, head_OBS, Date, Fmt_Date, Param, titre, path):
    """
    @author : R; Champavier
     Fonction pour diagramme de rang, qui prend en entrée deux fichiers csv ou txt, un modèle et un OBS, avec une colonne de date puis les colonnes de membres
     pour l'ensemble et une colonne date suivie des colonnes obs. Les données doivent être triées au péalable

     head == True or False, si true en tete dans le fichier OBS si non false, en tete a creer
     head_OBS == En tête dans le fichier liste de string a creer 

     Date = True or False, si True formatage des dates si non false date déjà formatées
     Fmt_Date ==  '' ou '%d/%m/%y' ou '%y-%m-%d' selon le besoin, fonctionne aussi avec les heures. Attention, selon le formatage de base des dates fichiers, il peut 
     y avoir des petits bugs avec inversion mois et jours. Si Date=False alors Fmt_Date = '' les fichiers sont deja sous le même format date

     Param ==  Nom du string dans les OBS a comparer aux membres. Il doit avoir exactement la même syntax que le string dans la liste head_OBS ou dans le fichier

     titre et path pour figure, si path = '' si pas de sauvegarde

    ### Exemple ==== Talagrand('Test.csv', 'All_Station.csv', 35, head=True, head_OBS='' , Date=False, Fmt_Date='', Param='LP_NG', titre='TEST', path='') ####
    """
    ListeM0 = list(range(0, NMembre))
    Nummembre = list(range(1, NMembre + 1))  # Nombre de colonne après la date
    ListeM1 = ', '.join(map(str, ListeM0))
    ListeMtot = ListeM1.split(', ')
    ListeMtot.insert(0, 'Date')

    with open(str(Fich_MOD), 'r') as MOD, open(str(Fich_OBS), 'r') as OBS:

        if head == True:
            head_OBS = []
            df_MOD = pandas.read_table(MOD, delimiter=',', header=None, names=ListeMtot)
            df_OBS = pandas.read_table(OBS, delimiter=',', header=0)

        elif head == False:
            df_MOD = pandas.read_table(MOD, delimiter=',', header=None, names=ListeMtot)
            df_OBS = pandas.read_table(OBS, delimiter=',', header=None, names=head_OBS)

        if Date == True:
            df_OBS['Date'] = pandas.to_datetime(df_OBS.Date)
            df_OBS['Date'] = df_OBS.Date.dt.strftime(Fmt_Date)
            df_MOD['Date'] = pandas.to_datetime(df_MOD.Date)
            df_MOD['Date'] = df_MOD.Date.dt.strftime(Fmt_Date)
        elif Date == False:
            pass
        # Merge sur les dates
        df_Tri = df_MOD.merge(df_OBS, how='outer', on='Date')
        # Enleve les -1.0 qui apparaissent lors d'une extraction sans valeurs
        df_Tri.iloc[:, Nummembre] = np.where(df_Tri.iloc[:, Nummembre] < 0.0, np.nan, df_Tri.iloc[:, Nummembre])
        # Soustrait l'OBS aux membres en valeurs absolues, puis renvoie l'index min correspondant au membre
        df_Tri['Freq_All'] = df_Tri.iloc[:, Nummembre].sub(df_Tri[str(Param)], axis=0).abs().idxmin(axis=1).dropna()

        # Extraction des rangs sans les dates pour appliquer fonction sur tous les membres
        # Attention, si un jour ne comprend pas tous les membres, il est exclut (ce que les prévis font lors d'un run incomplet)
        df_Rank = df_Tri.iloc[:, Nummembre].join(df_Tri['Freq_All']).dropna()

        df_Rank['Freq_All'] = pandas.to_numeric(df_Rank['Freq_All'])
        # Compte les nombre de 0 dans les membres
        df_Rank['Freq_0'] = (df_Rank.iloc[:, Nummembre] == 0).sum(axis=1)
        # Renvoie une valeurs aléatoires comprises dans le nombre de zéro
        df_Rank['Frequency'] = np.where(df_Rank['Freq_0'] != 0, np.round(np.random.uniform(0, df_Rank['Freq_0'], len(df_Rank)), 0), df_Rank['Freq_All'])

        df_Rank['Frequency'].hist(bins=NMembre)
        plt.xlabel('Membres')
        plt.ylabel('Frequence')

        plt.suptitle(str(titre), fontsize=25)

        if path == '':
            pass
        else:
            os.chdir(path)
            plt.savefig(str(titre) + '.png')

        return plt.show()

        plt.close()
