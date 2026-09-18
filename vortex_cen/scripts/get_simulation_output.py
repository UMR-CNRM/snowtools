# -*- coding: utf-8 -*-
"""
get_simulation_output.py
------------------------

API for the extraction of files archived with vortex in a research context.

Usage examples:

With a default configuration file under vortex_cen/conf providing the full resource(s) description.
In this example, the `S2MReanalysis.ini` configuration file allows to extract data from
the S2M reanalysis over the entire period and all geometries covered by the dataset.

Before any actual extraction, the list of files that will be extracted can be checked as follows :

.. code-block:: bash

    get_simulation_output -c S2MReanalysis.ini --configsection SafranFlatReanalysis --verbose --checkonly

.. note::

   The actual extraction can be launched by removing the "--checkonly" argument.

   **WARNING : never do that for the entire reanalysis dataset**

In order to limit the extraction to one year and one geometry :

.. code-block::

    get_simulation_output -c S2MReanalysis.ini --configsection SafranFlatReanalysis --verbose --checkonly
    -b 2024080106 -e 2025080106 -g cor2_flat

In order to use a custom configuration file, provide the absolute or relative path to the target file.
If this target file contains only the "DEFAULT" section or a single named section, simply type :

.. code-block:: python

    get_simulation_output -c path/to/user/file.ini

"""

import argparse
from vortex_cen.tools.vortex_extractor import get_data


def parse_args():
    parser = argparse.ArgumentParser(description="Extract files archived with vortex in a research contex")

    parser.add_argument("-b", "--datebegin",
                        help="Date of beginning of the data to extract, format YYYYMMDDHH")

    parser.add_argument("-e", "--dateend",
                        help="Date of end of the data to extract, format YYYYMMDDHH")

    parser.add_argument("-d", "--datevalidity",
                        help="Date of validity in case of a PREP file")

    parser.add_argument("-g", "--geometry",
                        dest="geometry",
                        help="geometry of the data to extract")

    parser.add_argument("-x", "--xpid",
                        dest="experiment",
                        help="Experiment identifier of the data to extract")

    parser.add_argument("-u", "--user",
                        default=None, dest="username",
                        help="Name of the producer of the data")

    parser.add_argument("-k", "--kind",
                        help="Kind of data to extract",
                        choices = ["PREP", "PRO", "FORCING"])

    parser.add_argument("-m", "--member",
                        default=None,
                        help="Ensemble member(s) to extract (ex : 'first-last-step')")

    parser.add_argument("--vapp",
                        help="Vapp of the data to extract",
                        choices = ["s2m", "edelweiss", "Crocus"])

    parser.add_argument("--vconf",
                        help="Vconf of the data to extract",
                        choices = ["reanalysis", "reforecast", "deterministic"])

    parser.add_argument("-c", "--configfile",
                        help="Configuration file containing a (potentially partial) description "
                        "of the data to extract",)

    parser.add_argument("--configsection",
                        help="Name of the target section of the data in the configuration file",)

    parser.add_argument("--block",
                        help="Block (~ producing task) of the data to extract",)

    parser.add_argument("--duration",
                        choices=['yearly', 'monthly', 'full'],
                        help="Duration of files. Default is yearly files. "
                        "Use 'monthly' for monthly files "
                        "and 'full' for one sigle file that covers the whole period",
                        )

    parser.add_argument("--checkonly",
                        action="store_true",
                        help="If '--checkonly', only check the data location")

    parser.add_argument("--verbose",
                        action="store_true",
                        help="Display full output information")

    args = parser.parse_args()
    return args


def main():
    args = parse_args()
    rh = get_data(**vars(args))

    if args.checkonly and args.verbose:
        print("Target resources:")
        print("=================")
        idx = 0
        for resource in rh:
            idx += 1
            print(f"{idx}. ", resource.locate())


if __name__ == "__main__":
    main()
