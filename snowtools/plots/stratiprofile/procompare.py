#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging
import argparse
import textwrap

version = '1.0'


def main(version=version):

    parser = argparse.ArgumentParser(
            prog='procompare',
            #       0         1         2         3         4         5         6         7         8
            description=textwrap.dedent("""\
                    ProCompare is a tool to compare two (or more) snowpack structures as simulated
                    by snow cover models, such as SURFEX/ISBA-Crocus.
            """),
            # TODO: Full description of different use cases  <07-09-21, Léo Viallon-Galinier> #
            epilog=textwrap.dedent("""\
            """),
            formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('filenames', nargs='*', help="Input file path", type=str)
    parser.add_argument("--debug", action="count", help="Increase logging for debugging purpose.")
    parser.add_argument('-o', '--output', default=None, dest='output',
                        help="Output filename (for use without GUI). Only available for standard graph for the moment.")
    parser.add_argument("-b", "--begin", default=None, help="Begin date", dest='begin')
    parser.add_argument("-e", "--end", default=None, help="End date", dest='end')
    parser.add_argument("-v", "--variable", help="Variable to plot", type=str, dest='variable')
    parser.add_argument("--point", help="Point number to select", type=int)
    parser.add_argument("-s", "--select", type=str, action='append', dest='select',
                        help="Selection of point with constraints on data: use as many '-s variable=value' as necessary\
                               to select the point of interest. Note that if -p is given, these options are ignored. \
                               The point selection is done on the first given file. If files have different point \
                               numbering, please do not use this option and rather use the GUI interface.")

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
    arguments = {}
    fileobjs = []
    point = None

    if len(args.filenames) > 0:
        from snowtools.plots.stratiprofile import proreader
        if args.filenames is not None:
            for filename in args.filenames:
                fileobjs.append(proreader.read_file(filename))
        if len(fileobjs) > 0 and fileobjs[0] is not None:
            if args.variable is not None:
                variable = fileobjs[0].variable_desc(args.variable)
                if variable is not None:
                    arguments['variable'] = variable['full_name']
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
                point = fileobjs[0].get_point(selector=selector)
        # TODO: Implement --begin/--end  <08-03-23, Léo Viallon-Galinier> #

    if args.output is None:
        from snowtools.plots.stratiprofile import procompare_gui as gui
        gui.main(fileobjs=fileobjs, point=point, arguments=arguments)
    else:
        # TODO: Implement:
        # - -o to write a file rather than opening GUI
        # - --profil / -d options
        # <10-02-23, Léo Viallon-Galinier> #
        raise NotImplementedError('-o option is not yet implemented')


if __name__ == "__main__":
    main()
