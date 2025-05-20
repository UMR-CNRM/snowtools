#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# General python modules
import os
import argparse
import glob
import configparser

from snowtools.DATA import SNOWTOOLS_DIR
from snowtools.tools.execute import callSystemOrDie


def parse_command_line():

    description = "Extract a subset of massifs from a FORCING file \n" \
                  "EXAMPLE: \n" \
                  "extract_massif.py -b 2000080106 -e 2001080106 --vapp=adamont --geometry=massif_40 --massifs=40 " \
                  "--geometry_in=cor_flat --model=adamont " \
                  "--xpid=CLMcom_CCLM4_8_17_MPI_M_MPI_ESM_LR_HISTORICAL@samacoitsr"

    parser = argparse.ArgumentParser(description=description)

    parser.add_argument("-b", "--datebegin", type=str, dest="datebegin", required=True,
                        help="Date of the beginning of the simulation period. Format YYYYMMDDHH\n"
                             " ")

    parser.add_argument("-e", "--dateend", type=str, dest="dateend", required=True,
                        help="Date of the end of the simulation period. Format YYYYMMDDHH\n"
                             " ")

    parser.add_argument("--xpid", type=str, dest="xpid", required=True, nargs='+',
                        help="Target experiment's identifiers (xpid). \n"
                             "A list can be provided, in that case the script will loop over the different xpids "
                             "and launch a different job for each of them. \n"
                             "format: xpid@{anyuser} \n"
                             "The output xpid will be identical, except that the username will be set "
                             "to the current user (you): xpid@{username} \n"
                             " ")

    parser.add_argument("--geometry_in", type=str, dest="geometry_in", required=True,
                        help="Input FORCING's geometry."
                             "The geometry must also be defined in a 'geometries.ini' file (either in the Vortex's"
                             "'conf' directory or under the '.vortexrc' directory in your $HOME.\n"
                             " ")

    parser.add_argument("--geometry", type=str, dest="geometry", required=True,
                        help="Output FORCING's geometry (after massifs extraction)."
                             "The geometry must also be defined in a 'geometries.ini' file (either in the Vortex's"
                             "'conf' directory or under the '.vortexrc' directory in your $HOME.\n"
                             " ")

    parser.add_argument("--vapp", type=str, dest="vapp", required=True,
                        help="*vapp* footprint of the FORCING file \n"
                             " ")

    parser.add_argument("--model", type=str, dest="model", required=True,
                        help="*model* footprint of the FORCING file \n"
                             " ")

    parser.add_argument("--massifs", nargs="+", type=int, dest="massifs", required=True,
                        help="(List of) massifs numbers to extract. \n"
                             " ")

    parser.add_argument("--member", type=str, dest="member", default=None, required=False,
                        help="(List of) ensemble members. \n"
                             "Possible formats :\n"
                             "{number of members} (starts at 0 by default)\n"
                             "'{first_member}:{last-member}'\n"
                             " ")

    parser.add_argument("--source_app", type=str, dest="source_app", required=False, default=None,
                        help="*source_app* footprint of the FORCING file \n"
                             " ")

    parser.add_argument("--source_conf", type=str, dest="source_conf", required=False, default=None,
                        help="*source_conf* footprint of the FORCING file \n"
                             " ")

    args = parser.parse_args()
    return vars(args)


def set_env(xpid, **kw):
    """
    Set up the proper Vortex job-launching environement.

    :param xpid: Experiment ID (single value)
    :type xpid: str
    """

    os.chdir(os.environ['WORKDIR'])

    workdir = os.path.join(xpid, kw['vapp'], kw['geometry'].lower())
    if not os.path.isdir(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    # Ensure that the Vortex module is the right one
    if os.path.islink("vortex"):
        os.remove("vortex")
    os.symlink(os.environ["VORTEX"], "vortex")

    # Ensure that the snowtools module is the right one
    if os.path.islink("snowtools"):
        os.remove("snowtools")
    os.symlink(SNOWTOOLS_DIR, "snowtools")

    # Add required repositories
    for directory in ["conf", "jobs", "tasks"]:
        if not os.path.isdir(directory):
            os.mkdir(directory)

    # Make a link to all snowtools research tasks to be more generic
    known_tasks = glob.glob(os.path.join(SNOWTOOLS_DIR, "tasks", "research", "*", "*"))
    for task in known_tasks:
        taskname = os.path.basename(task)
        if not (os.path.exists(f"tasks/{taskname}") or taskname.startswith('__')):
            os.symlink(task, f"tasks/{taskname}")

    # Put the default 'geometries.ini' file (from snowtools/conf) in the user's '.vortexrc' directory if necessary
    geometries = os.path.join(os.environ["HOME"], ".vortexrc", 'geometries.ini')
    if not os.path.isfile(geometries):
        os.symlink(os.path.join(SNOWTOOLS_DIR, "conf", "geometries.ini"), geometries)


def make_conf_file(taskname, **kw):
    """
    Write the configuration file from the command-line arguments.

    :param taskname: Name of the configuration file to create.
    :type taskname: str
    """

    conffilename = os.path.join('conf', f'{kw["vapp"]}_{kw["geometry"]}.ini')

    # Ensure that the configuration file is up to date
    if os.path.exists(conffilename):
        os.remove(conffilename)

    config = configparser.ConfigParser()

    # preprocess values:
    # - None values not supported ("TypeError: option values must be strings")
    # - lists must be formated as "a, b, c" instead of "[a, b, c]"
    def preprocess(value):
        if value is None:
            value = 'None'
        elif isinstance(value, list):
            value = ', '.join(map(str, value))
        return value
    actual_args = {k: preprocess(v) for k, v in kw.items()}

    # Add sections and key-value pairs
    config[taskname] = actual_args

    # Write the configuration to a file
    with open(conffilename, 'w') as configfile:
        config.write(configfile)


def run(datebegin, dateend, taskname, jobname, xpid, **kw):
    """
    Once the environement is ready, lauch the job with mkjob.

    :param datebegin: Begin date of the simulation
    :type datebegin: str
    :param dateeend: End date of the simulation
    :type dateend: str
    :param taskname: Name of the configuration file to create.
    :type taskname: str
    :param jobname: Name of the job to launch.
    :type jobname: str
    """

    mkjob = "../vortex/bin/mkjob.py"
    cmd = f"{mkjob} -j name={jobname} task={taskname} profile=rd-belenos-mt jobassistant=cen xpid={xpid} " \
        f"datebegin={datebegin} dateend={dateend}"
    os.chdir("jobs")
    print("Run command: " + cmd + "\n")
    callSystemOrDie(cmd)


if __name__ == "__main__":
    args = parse_command_line()
    datebegin = args.pop('datebegin')
    dateend = args.pop('dateend')

    # Task-specific variables
    jobname  = 'extract_massifs'
    taskname = 'extract_massifs'

    # 1 job per xpid, // over the dates
    for xpid in args.pop('xpid'):
        set_env(xpid, **args)
        make_conf_file(taskname=taskname, jobname=jobname, **args)
        run(datebegin, dateend, taskname=taskname, jobname=jobname, xpid=xpid, **args)
