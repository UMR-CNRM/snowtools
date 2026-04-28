# -*- coding: utf-8 -*

import argparse
import time

import vortex
from bronx.stdtypes.date import Date, Time
from vortex.util.config import GenericConfigParser

from snowtools.tools.execute import callSystemOrDie


def parse_command_line():

    parser = argparse.ArgumentParser(
        description="Launch a SURFEX/Crocus experiment with snow data assimilation. \n"
        "Such an experiment is loop over the following sequence of actions over a set of assimilation dates:\n"
        "1. Run an ensemble of SURFEX/Crocus simulations (OFFLINE executable) with an MPI parallelisation until "
        "an assimilation date. All simulation members are initialised with the same initial conditions (PREP file).\n"
        '--> Associated task : "offline_openloop"\n'
        "2. Assimilate an available snow observation at the assimilation date with a Particle Filter "
        "(SODA executable)\n"
        "3. Run an ensemble of SURFEX/Crocus simulations (OFFLINE executable) with an MPI parallelisation from "
        "the last assimilation date, until the next assimilaiton date (or the date of end simiulation).\n"
        "The difference with the execution of step 1 is that this time, each simuaiton member is initialised by"
        "specific initial conditions (PREP file) coming from step 2 (SODA analysis).\n"
        '--> Associated task : "offline_assim"\n'
    )

    parser.add_argument("-b", "--datebegin", type=str, help="Date of the beginning of the simulation.")

    parser.add_argument("-e", "--dateend", type=str, help="Date of the end of the simulation.")

    parser.add_argument("--vapp", help="Target application (ex: edelweiss)", type=str, required=True)

    parser.add_argument("--vconf", help="Target configuration", type=str, required=True)

    parser.add_argument("-c", "--conf", help="Path to the simulation's configuration file", type=str, required=True)

    # Temporary argument for script debuging
    parser.add_argument(
        "--keep_existing_prep",
        action="store_true",
        default=False,
        help="Do not remove existing PREP files to avoid waiting (DBUG mode only)",
        required=False,
    )

    #    parser.add_argument("-x", "--xpid",
    #            help="Experiment identifier", type=str, required=False)
    #
    #    parser.add_argument("-g", "--geometry",
    #            help="Simulation's geometry", type=str, required=False)
    #
    #    parser.add_argument("-n", "--nmembers",
    #            help="Number of simulation members", type=int, required=False)

    args = parser.parse_args()

    return args


def mkjob_command(jobname, taskname, datebegin=None, dateend=None, date=None):
    """
    Build a valid mkjob command line.

    One of *date* or (*datebegin* and *dateend*) argument must be provided
    """
    base = f"mkjob -j name={jobname} task={taskname} profile=rd-belenos-mt jobassistant=cen"
    # TODO : ajouter la période pour éviter d'avoir à mettre datebegin / dateend dans le fichier de conf
    if date is not None:
        dateinfo = f"date={date}"
    else:
        dateinfo = f"datebegin={datebegin} dateend={dateend}"
        # Verrue pour gérer le journée manquante du 20220801
        if datebegin == "2021080206":
            dateinfo = f"{dateinfo} prep_date=2021080106"

    confinfo = f"-c {args.conf}"

    cmd = " ".join([base, dateinfo, confinfo])

    return cmd


def mkjob_list_commands(taskname, njobs=17, mb0=0, datebegin=None, dateend=None, date=None):
    """
    Method to construct the actual list of job creation commands.

    One of *date* or (*datebegin* and *dateend*) argument must be provided

    Let's consider that we want to launch an execution of
    SURFEX with an MPI parallelisation for every FORCING file of an N
    members ensemble.
    We want all these jobs to share a common 'surfex_mpi' section in the
    configuration file.

    Naming the jobs 'surfex_mpi_mb1', 'surfex_mpi_mb2', ...,  'surfex_mpi_mbN'
    will lead the Vortex's job launcher to do this :
    - Get each job 'member' from the mbX extension : member=int(X)
    - put the associated value direcly in the job (variable RD_MEMBER in the
      template file, then interpreted as 'member' in the configuration
      dictionary)
    - rename all jobs 'surfex_mpi' so that they all use the same section of
      the configuration file
    """

    jobname = f"{taskname}_job"

    mkjob_list = []
    if njobs == 1:
        mkjob_list.append(mkjob_command(jobname=jobname, taskname=taskname, datebegin=datebegin, dateend=dateend, date=date))
    else:
        for job_number in range(mb0, mb0 + njobs):
            mkjob_list.append(
                mkjob_command(
                    jobname=f"{jobname}_mb{str(job_number)}",
                    taskname=taskname,
                    datebegin=datebegin,
                    dateend=dateend,
                    date=date,
                )
            )

    return mkjob_list


if __name__ == "__main__":
    args = parse_command_line()
    iniparser = GenericConfigParser(inifile=args.conf)

    nmembers = int(iniparser.get("DEFAULT", "nmembers"))
    geometry = iniparser.get("DEFAULT", "geometry")
    xpid = iniparser.get("DEFAULT", "xpid")
    # walltime tells the method "wait_mandatory_input" how long it has to wait before crashing
    walltime = Time(iniparser.get("offline_openloop_job", "time"))

    datebegin = args.datebegin
    dateend = args.dateend

    # This is the first run:
    # launch a set of offline_MPI tasks with a single PREP file as initial conditions
    offline = mkjob_list_commands("offline_openloop", datebegin=datebegin, dateend=dateend)

    for mkjob in offline:
        print("Run command: " + mkjob + "\n")
        callSystemOrDie(mkjob)
