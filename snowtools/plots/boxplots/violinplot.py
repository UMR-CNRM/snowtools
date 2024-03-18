#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

Collection of functions to create violinplots comparing several products over topographic clusters

Ex : See figures 5,7 and 9 of
https://editor.copernicus.org/index.php?_mdl=msover_md&_jrl=778&_lcm=oc73lcm74a&_acm=get_manuscript_file&_ms=115831&id=2253714&salt=11477330121431287512

'''

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns


def plot_ange(dataplot, var, figname):
    """
    WORK IN PROGRESS
    """

    # Ange's paper color palette
    # TODO : automatiser le choix de la palette
    colors = ["silver", "#D65F5F", "#D65F5F", "#4878D0", "#4878D0", "#6ACC64", "#6ACC64", "#EE854A", "#EE854A"]

    sns.set(rc={"figure.figsize": (12, 15)})
    sns.set_theme(style="whitegrid", font_scale=1.7)
    sns.violinplot(
        dataplot,  # data
        y = 'Elevation Bands (m)',  # y-axis
        x = var,  # X-axis
        inner = 'box',  # ?
        hue = 'experiment',  # Legend 'title'
        scale = 'width',  # ?
        bw = 'scott',  # ?
        cut = 0,  # ?
        orient = 'h',  # Horizontal violinplots
        palette=(color for color in colors),
    )
    plt.ylim(reversed(plt.ylim()))
    plt.xlim([0, 375])

#    # Set Ange's hatches for simulations without pappus
#    for violin in g.findobj(mpl.collections.PolyCollection)[0:-1:2]:
#        violin.set_hatch(r'\\\\')

    # Set Ange's color and hatches
    # TODO : à automatiser
    circ0 = mpatches.Patch(facecolor=colors[0], label='Sentinel 2 A obs')
    circ1 = mpatches.Patch(facecolor=colors[1], hatch=r'\\\\', label='Safran')
    circ2 = mpatches.Patch(facecolor=colors[2], label='Safran Pappus')
    circ3 = mpatches.Patch(facecolor=colors[3], hatch=r'\\\\', label='Raw ANTILOPE')
    circ4 = mpatches.Patch(facecolor=colors[4], label='Raw ANTILOPE Pappus')
    circ5 = mpatches.Patch(facecolor=colors[5], hatch=r'\\\\', label='AS-ANTILOPE')
    circ6 = mpatches.Patch(facecolor=colors[6], label='AS-ANTILOPE Pappus')
    plt.legend(handles = [circ0, circ1, circ2, circ3, circ4, circ5, circ6])

    plt.savefig(f'{figname}.pdf', format='pdf')

    plt.close('all')
