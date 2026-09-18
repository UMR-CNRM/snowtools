# -*- coding: utf-8 -*

"""
This script is designed to launch Surfex simulations based on the SAFRAN reanalysis with a dynamic
walltime depending on the simulation's geometry and duration.

It takes the following arguments :
    * A configuration file ('-c' shortcut) similar to the '-c' argument of mkjob, but allowing to provide a file name
      instead of an absolute path
    * The simulation's datebegin with a "-b" alias. If not provided, the datebegin's hour is set to 6h.
    * The simulation's dateend with a "-e" alias. If not provided, the datebegin's hour is set to 6h.
    * The simulation's geometry with a "-g" alias.
      The geometries values are limited to the standard SAFRAN-reanalysis geometries
    * A "-a" argument to parse additional command-line arguments as in mkjob

Unlike mkjob, this script allowss to avoid setting the walltime of the job explicitly.
It is instead computed as a multiplication between the number of simulation years and the standard walltime
for a 1-year simulation in the given geometry and with a standard SURFEX parametrisation.

Usage example :

s2m_reanalysis -b 20200801 -e 20250801 -g cor2_allslopes -c s2m_reanalysis.ini

To overwrite the automatic walltime, use the standard mkjob command or the "-a" argument :

s2m_reanalysis -b 20200801 -e 20250801 -g cor2_allslopes -c s2m_reanalysis.ini -a time=2:00:00

"""

import vortex
import vortex_cen
import argparse
from pathlib import Path

from snowtools.utils.dates import check_and_convert_date
from snowtools.utils.dates import WallTimeException
from bronx.stdtypes.date import Period

sh = vortex.ticket().sh

# Get S2M oper root directory from the actual vortex_cen install in the virtual environment
# TODO : à changer si une solution "externalisée" est mise en place
rootdir = sh.path.join(vortex_cen.__path__[0], 's2m', 'reanalysis')
# This script is for the surfex job only
surfex_job_description_file = sh.path.join(rootdir, 'jobs', 'surfex.jobs')

jobname_map = dict(
    alp27_allslopes = 'surfex_reanalysis_alp',
    pyr24_allslopes = 'surfex_reanalysis_pyr',
    cor2_allslopes  = 'surfex_reanalysis_cor',
    mac11_allslopes = 'surfex_reanalysis_mac',
    jur4_allslopes  = 'surfex_reanalysis_jur',
    vog3_allslopes  = 'surfex_reanalysis_vog',
    postes          = 'surfex_reanalysis_postes',
)


def parse_command_line():

    parser = argparse.ArgumentParser(description='Surfex reanalysis simulations launcher')

    parser.add_argument(
        "-b",
        "--datebegin",
        help = "Date of the beginning of the simulation.",
        type = str,
        required = True,
    )
    parser.add_argument(
        "-e",
        "--dateend",
        help = "Date of the end of the simulation.",
        type = str,
        required = True,
    )
    parser.add_argument(
        "-g",
        "--geometry",
        help = "geometry of the simulation",
        choices = jobname_map.keys(),
        type = str,
        required = True,
    )
    parser.add_argument(
        "-c",
        "--config",
        type=Path,
        help="Path to the configuration file",
        required = True,
    )
    parser.add_argument(
        "-a",
        "--additional",
        nargs = "*",
        help = "Additional command line arguments (dev only)",
        default = list(),
    )

    args = parser.parse_args()

    return args


def guess_walltime(datebegin, dateend, geometry):
    """
    Try to guess the simulation's walltime for the given simulation period and geometries
    This only works for standard S2M geometries.
    The walltime can explicitely set in the command line with the '-a' arguement (ex : -a time=1:00:00)
    """
    # minutes per year for one member computing all points
    minutes_peryear = dict(alp27_allslopes=20, pyr24_allslopes=20, alp27_flat=7, pyr24_flat=7,
                           mac11_allslopes=5, jur4_allslopes=2, vog3_allslopes=2,
                           mac11_flat=2, jur4_flat=1, vog3_flat=1, cor2_allslopes=2, cor2_flat=1,
                           postes=5, postes_2022=5, postes_2026=5)

    estimation = Period(minutes=minutes_peryear[geometry]) * \
        max(1, (dateend.year - datebegin.year))

    if estimation >= Period(hours=24):
        raise WallTimeException(estimation.hms)
    else:
        return estimation.hms


def execute(jobname, conf, datebegin, dateend, geometry, additional):
    """
    Build the mkjob command-line and execute it
    """
    cmd = f"mkjob -f {surfex_job_description_file} -c {conf} -n {jobname} " \
        f"-a datebegin={datebegin} dateend={dateend} {additional}"
    sh.spawn(cmd, output=False, shell=True)


def check_config_path(conf_path):
    """
    Check that the config argument can be associated to an actual file and return its absolute path.
    """

    if sh.path.isfile(conf_path):
        # conf_path is a valid path to the configuration file
        return conf_path
    elif sh.path.isfile(sh.path.join(rootdir, 'conf', conf_path)):
        return sh.path.join(rootdir, 'conf', conf_path)
    else:
        raise FileNotFoundError(conf_path, sh.path.join(rootdir, 'conf', conf_path))


def main():
    args = parse_command_line()
    additional = args.additional

    jobname = jobname_map[args.geometry]

    config = check_config_path(args.config)

    # Standard S2M date management (SAFRAN starts and stops simulations at 6h)
    datebegin = check_and_convert_date(args.datebegin)
    dateend = check_and_convert_date(args.dateend)

    additional = ' '.join(args.additional)
    if 'time=' not in additional:
        walltime = guess_walltime(datebegin, dateend, args.geometry)
        additional = f'{additional} time={walltime}'

    execute(jobname, config, datebegin.ymdh, dateend.ymdh, args.geometry, additional)


if __name__ == "__main__":
    main()
