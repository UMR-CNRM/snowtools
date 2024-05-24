#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

Collection of functions to create violinplots comparing several products over topographic clusters

Ex : See figures 5,7 and 9 of
https://editor.copernicus.org/index.php?_mdl=msover_md&_jrl=778&_lcm=oc73lcm74a&_acm=get_manuscript_file&_ms=115831&id=2253714&salt=11477330121431287512

'''

import numpy as np

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
#  import matplotlib.patches as mpatches
import seaborn as sns


var_labels = dict(
    DSN_T_ISBA = 'Snow Depth (m)',
)


def plot_ange(dataplot, var, figname=None, xmax=7, yaxis='Elevation Bands (m)', title=None, violinplot=True):
    """
    WORK IN PROGRESS
    * xmax     : set maximum x-axis value. Default for HTN / DSN_T_ISBA variable (6m)
    * dataplot : Pandas DataFrame
                    Elevation Bands (m)       experiment  DSN_T_ISBA
        0                    2050.0              obs    1.937780
        1                    2050.0              obs    1.741148
        ...                     ...              ...         ...
        xxxxxx               3250.0            XPID1    0.773394
        xxxxxx               3250.0            XPID1    0.446745
        ...                     ...              ...         ...
        104214               3250.0            XPID2    3.284174
        104215               3250.0            XPID2    0.253465
    """

    # Ange's paper color palette
    # TODO : automatiser le choix de la palette
    colors = ["silver", "#D65F5F", "#4878D0", "#6ACC64", "#EE854A"]
    # colors = ["silver", "#D65F5F", "#D65F5F", "#4878D0", "#4878D0", "#6ACC64", "#6ACC64", "#EE854A", "#EE854A"]

    bands = np.flip(dataplot[yaxis].unique())

    sns.set(rc={"figure.figsize": (12, 15)})
    sns.set_theme(style="whitegrid", font_scale=1.7)
    if violinplot:
        myplot = sns.violinplot(
            dataplot,  # data
            y            = yaxis,  # y-axis
            x            = var,  # X-axis
            inner        = 'box',  # Representation of the data in the violin interior ('box' --> mini-boxplot)
            hue          = 'experiment',  # Legend 'title'
            order        = bands,
            density_norm = 'width',  # all violin will have the same width
            bw_adjust    = 0.5,  # Factor that scales the bandwidth to use more or less smoothing
            cut          = 0,  # Limit the violin within the data range
            orient       = 'h',  # Horizontal violinplots
            palette      = (color for color in colors),
        )
    else:
        myplot = sns.boxplot(
            data         = dataplot,  # data
            y            = yaxis,  # y-axis
            x            = var,  # X-axis
            hue          = 'experiment',  # Legend 'title'
            order        = bands,
            orient       = 'h',  # Horizontal violinplots
            palette      = (color for color in colors),
            notch        = True,
            flierprops   = {"marker": "x"},
            medianprops  = {"linewidth": 3},
        )

    if title is not None:
        myplot.set(title=title)

    # Set horizontal lines positions
    minor_ticks = [value - 0.5 for value in myplot.yaxis.get_majorticklocs()]
    myplot.yaxis.set_minor_locator(ticker.FixedLocator(minor_ticks))
    myplot.yaxis.grid(True, which='minor')
    # Custom yaxis ticks labels
    myplot.set_yticklabels(label_format(bands))
    # Set x-axis limits and label
    plt.xlim([0, xmax])  # DSN_T_ISBA / HTN (m)
    if var in var_labels.keys():
        label = var_labels[var]
    else:
        label = var
    myplot.set_xlabel(label)

    plt.tight_layout()

#    # Set Ange's hatches for simulations without pappus
#    for violin in g.findobj(mpl.collections.PolyCollection)[0:-1:2]:
#        violin.set_hatch(r'\\\\')

    # Set Ange's color and hatches
    # TODO : à automatiser
#    circ0 = mpatches.Patch(facecolor=colors[0], label='Sentinel 2 A obs')
#    circ1 = mpatches.Patch(facecolor=colors[1], hatch=r'\\\\', label='Safran')
#    circ2 = mpatches.Patch(facecolor=colors[2], label='Safran Pappus')
#    circ3 = mpatches.Patch(facecolor=colors[3], hatch=r'\\\\', label='Raw ANTILOPE')
#    circ4 = mpatches.Patch(facecolor=colors[4], label='Raw ANTILOPE Pappus')
#    circ5 = mpatches.Patch(facecolor=colors[5], hatch=r'\\\\', label='AS-ANTILOPE')
#    circ6 = mpatches.Patch(facecolor=colors[6], label='AS-ANTILOPE Pappus')
#    plt.legend(handles = [circ0, circ1, circ2, circ3, circ4, circ5, circ6])

    # Add number of pixels associated to each violin plot
    tmp = dataplot.set_index([yaxis, 'experiment'])
    npixels = tmp.groupby(level=tmp.index.names).count()
    subensemble_size = np.flip(npixels.values.flatten())
    #                                         DSN_T_ISBA
    # Elevation Bands (m) experiment
    # 2050.0              ANTILOPE_pappus        5588
    #                     obs                     645
    # 2350.0              ANTILOPE_pappus        6132
    #                     obs                     771
    # 2650.0              ANTILOPE_pappus        3514
    #                     obs                     436
    # 2950.0              ANTILOPE_pappus        1052
    #                     obs                     112
    # 3250.0              ANTILOPE_pappus         100
    #                     obs                       3
    # for idx, np in enumerate(npixels.values.flatten().flip()):
    #
    # Compute configuration-specific values for annotation positions
    nexperiments = len(dataplot.experiment.unique())
    dy = 1 / (nexperiments + 1)
    for idx, pixels in enumerate(subensemble_size):
        txt = f'n={pixels:d}'
        plt.text(xmax * 0.5, (idx + np.floor(idx / nexperiments) - 1) * dy, txt, fontsize='small')

    if figname is None:
        figname = f'{var}.pdf'
    elif '.pdf' not in figname:
        figname = f'{figname}.pdf'
    plt.legend()
    plt.savefig(figname, format='pdf')
    plt.close('all')


def label_format(elevations):
    """
    Formats an array of elevation values into a string representation with line breaks.

    Args:
        band (list): A list of numerical elevation values.

    Returns:
        list: A new list containing formatted strings representing the elevation ranges.
    """

    out = []
    dz = int(np.abs(elevations[1] - elevations[0]) / 2)
    for elevation in elevations:
        # Format each string with elevation range and a newline character
        formatted_string = f"{int(elevation)-dz}-{int(elevation)+dz}"
        out.append(formatted_string)

    return out
