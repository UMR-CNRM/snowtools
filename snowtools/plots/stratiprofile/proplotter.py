#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import argparse
import textwrap

TYPES_GRAPH = ['standard', 'multiple profil', 'multiple saison', 'height', 'member profil', 'member saison', 'compare']

version = '1.0'


def main(version=version):

    parser = argparse.ArgumentParser(
            prog='proplotter',
            #       0         1         2         3         4         5         6         7         8
            description=textwrap.dedent("""\
                    Proplotter is a tool to plot with (or without) the Graphical User Interface
                    which aims at providing representations and exploration tools for modelled
                    snowpack structures. It is primarily designed to deal with SURFEX/ISBA-Crocus
                    simulation output.
            """),
            # TODO: Full description of different use cases  <07-09-21, Léo Viallon-Galinier> #
            epilog=textwrap.dedent("""\
            """),
            formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('filename', nargs='*', help="Input file path", type=str)
    parser.add_argument("--debug", action="count", help="Increase logging for debugging purpose.")
    parser.add_argument('-o', '--output', default=None, dest='output',
                        help="Output filename (for use without GUI). Only available for standard graph for the moment.")
    parser.add_argument("-b", "--begin", default=None, help="Begin date", dest='begin')
    parser.add_argument("-e", "--end", default=None, help="End date", dest='end')
    parser.add_argument("-t", "--type", help="Type of graph ({})".format(', '.join(TYPES_GRAPH)), type=str,
                        choices=TYPES_GRAPH, default=TYPES_GRAPH[0], dest='type')
    parser.add_argument("-v", "--variable", help="Variable to plot", type=str, dest='variable')
    parser.add_argument("-p", "--variable-profil", help="Variable for profil plot (if relevant)", type=str,
                        dest='variable_profil')
    parser.add_argument("--point", help="Point number to select", type=int)
    parser.add_argument("-s", "--select", type=str, action='append', dest='select',
                        help="Selection of point with constraints on data: use as many '-s variable=value' as necessary\
                               to select the point of interest. Note that if -p is given, these options are ignored.")

# Options for plotting 1D profile instead of time evolution
    parser.add_argument("--profil", action="store_true", default=False,
                        help="Plot profil at given date (-d is compulsory) instead of a time evolution", dest='profil')
    parser.add_argument("-d", "--date", default=None, help="Date for vertical profile", dest='date')
# Subsampling
# parser.add_argument("--sampling", help="Maximum time length before subsampling (-1 is never)", type=int)
# Print version
    parser.add_argument("--version", action="version", version='%(prog)s - version ' + str(version))
    args = parser.parse_args()

# Logging and debug
    logging_level = logging.WARNING
    logging_format = "%(asctime)s %(levelname)s: %(message)s"
    logging_dateformat = "%Y-%m-%d %H:%M:%S"
    if args.debug is not None:
        if args.debug == 1:
            logging_level = logging.INFO
            logging_format = "%(asctime)s %(levelname)s: %(message)s (%(name)s)"
        if args.debug >= 2:
            logging_level = logging.DEBUG
            logging_format = "%(asctime)s %(levelname)s: %(message)s (%(name)s: %(filename)s l.%(lineno)s)"
    logging.basicConfig(level=logging_level, format=logging_format, style='%', datefmt=logging_dateformat)

# Argument interpretation
    arguments = {'type': args.type, }

    if len(args.filename) > 0:
        from snowtools.plots.stratiprofile import proreader
        fileobj = proreader.read_file(args.filename)
        if args.variable is not None:
            variable = fileobj.variable_desc(args.variable)
            if variable is not None:
                arguments['variable'] = variable['full_name']
        if args.variable_profil is not None:
            variable_p = fileobj.variable_desc(args.variable_profil)
            if variable_p is not None:
                arguments['variable_profil'] = variable_p['full_name']
        if args.point is not None:
            point = args.point
        elif args.select is not None:
            selector = {}
            for e in args.select:
                sp = e.split('=')
                if len(sp) != 2:
                    raise ValueError('-s option misused. could not parse \'{}\''.format(e))
                key = sp[0]
                value = float(sp[1])
                selector[key] = value
            point = fileobj.get_point(selector=selector)
        else:
            point = None
        # TODO: Implement --begin/--end  <08-03-23, Léo Viallon-Galinier> #
    else:
        fileobj = None
        point = None

    if args.output is None:
        from snowtools.plots.stratiprofile import proplotter_gui as gui
        gui.main(fileobj=fileobj, point=point, arguments=arguments)
    else:
        if fileobj is None:
            raise ValueError("No file provided while -o option used")
        if point is None and fileobj._npoints > 1:
            raise ValueError("No point selected while -o option used")
        if variable is None:
            raise ValueError("No variable selected while -o option used")

        if not args.type == 'standard':
            raise ValueError('-o option is implemented only for standard graph')

        import matplotlib
        matplotlib.use('Agg')
        import matplotlib.pyplot as plt

        from snowtools.plots.stratiprofile import proplotter_functions

        fig = plt.Figure()
        axs = proplotter_functions.create_axis_for_figure(fig, 1)
        # TODO: Implement additional options argument (tile)  <06-02-24, Léo Viallon-Galinier> #
        data = proplotter_functions.get_data(fileobj, point, variable['name'])  # additional_options=additional_options)
        args_plot = proplotter_functions.give_master_args_std(axs['ax1'], data)
        proplotter_functions.masterfig(**args_plot)
        fig.savefig(args.output)

        # TODO: Implement:
        # - --profil / -d options
        # <10-02-23, Léo Viallon-Galinier> #


if __name__ == "__main__":
    main()
