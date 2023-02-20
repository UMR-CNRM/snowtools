#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import argparse
import textwrap

TYPES_GRAPH = ['standard', 'massif', 'membre', 'height']
version = '2.0'

parser = argparse.ArgumentParser(
        prog='proplotter',
        #       0         1         2         3         4         5         6         7         8
        description=textwrap.dedent("""\
                Proplotter is a tool to plot with (or without) a beautiful GUI by Mathieu
                Fructus.

                The program is able to plot snowpack stratigraphy evolution across time
                and profiles of the snowpack at a given date.
        """),
        # TODO: Full description of different use cases  <07-09-21, Léo Viallon-Galinier> #
        epilog=textwrap.dedent("""\
        """),
        formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument('filename', nargs='?', help="Input file path", type=str)
parser.add_argument("--debug", action="count", help="Increase logging for debugging purpose.")
parser.add_argument('-o', '--output', default=None, help="Output filename (for use without GUI)", dest='output')
parser.add_argument("-b", "--begin", default=None, help="Begin date", dest='begin')
parser.add_argument("-e", "--end", default=None, help="End date", dest='end')
parser.add_argument("-t", "--type", help="Type of graph ({})".format(', '.join(TYPES_GRAPH)), type=str,
                    choices=TYPES_GRAPH, default=TYPES_GRAPH[0], dest='type')
parser.add_argument("-v", "--variable", help="Variable to plot", type=str, dest='variable')
parser.add_argument("-p", "--variable-profil", help="Variable for profil plot (if relevant)", type=str,
                    dest='variable_profil')
parser.add_argument("--point", help="Point number to select", type=int)
parser.add_argument("-s", "--select", type=str, action='append', dest='select',
                    help="Selection of point with constraints on data: use as many '-s variable=value' as necessary to \
                          select the point of interest. Note that if -p is given, these options are ignored.")

# Options for plotting 1D profile instead of time evolution
parser.add_argument("--profil", action="store_true", default=False,
                    help="Plot profil at given date (-d is compulsory) instead of a time evolution", dest='profil')
parser.add_argument("-d", "--date", default=None, help="Date for vertical profile", dest='date')
# Options for height plot
parser.add_argument("--direction", help="direction for plot (up or down, useful for height plots)", type=str,
                    default='up', choices=['up', 'down'])
parser.add_argument("--height", help="Height in centimeters for height plots", type=float, default=10)
# Subsampling
parser.add_argument("--sampling", help="Maximum time length before subsampling (-1 is never)", type=int)
# Print version
parser.add_argument("--version", action="version", version='%(prog)s - version ' + str(version))
args = parser.parse_args()

# Debug options
if args.debug > 0:
    pass

# Open files if provided
if args.filename is None or len(args.filename) == 0:
    from snowtools.plots.stratiprofile.proplotter import read_file
    logging.debug('Open file(s) {}.'.format(', '.join(args.filename)))
    files = read_file(args.filename)

    # Select a point or check provided point
    point = None
else:
    files = None
    point = None

if args.output is None:
    # Open the GUI
    from snowtools.plots.stratiprofile.proplotter_gui import main
    main(fileobj=files, point=point)
else:
    # Plot and save the plot
    raise NotImplementedError('Direct saving of graoh not yet implemented')
