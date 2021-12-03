# -*- coding: utf-8 -*-

'''
Created on 6 avr. 2017

Authors:
    - Pascal Hagenmuller

Python dictionnaries containing:

- The list of SURFEX/MEPRA grain shapes associated to its common names
- The colors associated to all grain shapes according to ICSSG

'''

import numpy as np
from matplotlib.colors import LinearSegmentedColormap

MEPRA_dict = {'PP': 0,      # fr
              'PP+DF': 1,   # fr_lb
              'DF': 2,      # lb
              'DF+RG': 3,   # lb_fin
              'DF+FC': 4,   # lb_ang
              'PPgp': 5,    # roul
              'RG': 6,      # fin
              'MF+RG': 7,   # fin_ar
              'RG+FC': 8,   # fin_ang
              'FC': 9,      # pl
              'FC+DH': 10,  # pl_gob
              'DH': 11,     # gob
              'MF': 12,     # gel
              'MF+DH': 13,  # gob_fon
              'MF+FC': 14   # ron_ang
              }

MEPRA_labels = ['PP', 'PP+DF', 'DF', 'DF+RG', 'DF+FC', 'PPgp', 'RG', 'MF+RG', 'RG+FC', 'FC', 'FC+DH', 'DH', 'MF', 'MF+DH', 'MF+FC']

coloring = {'PP': np.array([0, 255, 0]) / 255.0,
            'MM': np.array([255, 215, 0]) / 255.0,
            'DF': np.array([34, 139, 34]) / 255.0,
            'RG': np.array([255, 182, 193]) / 255.0,
            'FC': np.array([173, 216, 230]) / 255.0,
            'DH': np.array([0, 0, 255]) / 255.0,
            'SH': np.array([250, 0, 255]) / 255.0,
            'MF': np.array([255, 0, 0]) / 255.0,
            'MFcr': np.array([255, 255, 255]) / 255.0,
            'IF': np.array([0, 255, 255]) / 255.0,
            'NO': np.array([200, 200, 200]) / 255.0}

color_grain = {MEPRA_dict['PP']: coloring['PP'],
               MEPRA_dict['PP+DF']: 0.5 * (coloring['PP'] + coloring['DF']),
               MEPRA_dict['DF']: coloring['DF'],
               MEPRA_dict['DF+RG']: 0.5 * (coloring['DF'] + coloring['RG']),
               MEPRA_dict['DF+FC']: 0.5 * (coloring['DF'] + coloring['FC']),
               MEPRA_dict['PPgp']: [0, 0, 0],
               MEPRA_dict['RG']: coloring['RG'],
               MEPRA_dict['MF+RG']: 0.5 * (coloring['MF'] + coloring['RG']),
               MEPRA_dict['RG+FC']: 0.5 * (coloring['RG'] + coloring['FC']),
               MEPRA_dict['FC']: coloring['FC'],
               MEPRA_dict['FC+DH']: 0.5 * (coloring['FC'] + coloring['DH']),
               MEPRA_dict['DH']: coloring['DH'],
               MEPRA_dict['MF']: coloring['MF'],
               MEPRA_dict['MF+DH']: 0.5 * (coloring['MF'] + coloring['DH']),
               MEPRA_dict['MF+FC']: 0.5 * (coloring['MF'] + coloring['FC'])}

grain_colormap = LinearSegmentedColormap.from_list("custom", [[i / 14., color_grain[i]] for i in range(15)])
