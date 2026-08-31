# -*- coding: utf-8 -*
"""
This script is designed to launch the real-time S2M chain.

It takes the following arguments :
    * The simulation's rundate (wich entirely defines the simulation period)
    * The target job description file

Additional arguments are available for development use only :
    * The simulation's region (which defines both the "geometry" and the "vconf").
      By default, simulations are launched for all operationnal regions.
    * A "-a" argument to parse additional command-line arguments as in mkjob

Standard usage example:

s2m_oper -j surfex_ana -d 2026081306

Usage example in a development context (simulation over one region only):

s2m_oper -j surfex_ana -d 2026081306 -r cor

"""

import vortex
import vortex_cen
import argparse


t = vortex.ticket()
log = t.sh.path.join(t.sh.env()['HOME'], "LOG_CRON")
# Get S2M oper root directory from the actual vortex_cen install in the virtual environment
rootdir = t.sh.path.join(vortex_cen.__path__[0], 's2m')


def parse_command_line():

    parser = argparse.ArgumentParser(description='Real-time S2M simulations launcher')

    parser.add_argument(
        "-j",
        "--jobname",
        help = "Target job name",
        type = str,
        choices = ['prepsafran_ana', 'prepsafran_prv', 'safran_ana', 'safran_prv', 'surfex_ana', 'surfex_prv'],
        required = True,
    )

    parser.add_argument(
        "-d",
        "--rundate",
        help = "Date of the oper run (hour must be in (03, 06, 09, 12)",
        type = str,
        required = True,
    )

    parser.add_argument(
        "-r",
        "--region",
        "--vconf",
        dest = "region",
        help = "vconf of the operational run",
        choices = ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur', 'postes'],
        type = str,
        nargs = '*',
    )

    parser.add_argument(
        "-a",
        "--add",
        nargs = "*",
        help = "Additional command line arguments (dev only)",
        default = list(),
    )

    args = parser.parse_args()
    return args


def execute(job, conf, rundate, additional, domain=None):
    log_name = t.sh.path.basename(job)
    if domain is not None:
        log_name = f'{log_name}_{domain}'
    cmd = f"mkjob -f {job}.job -c {conf} -a rundate={rundate} {additional} >> {log}/{log_name}_{rundate} 2>&1"
    # print(cmd)
    t.sh.spawn(cmd, output=False, shell=True)


def main():
    args = parse_command_line()
    jobname = args.jobname
    rundate = args.rundate
    additional = ' '.join(args.add)

    if jobname in ['prepsafran_ana', 'prepsafran_prv']:
        job = t.sh.path.join(rootdir, f'oper/jobs/{jobname}')
        conf = t.sh.path.join(rootdir, 'oper/conf/s2m_common.ini')
        execute(job, conf, rundate, additional)

    elif jobname in ['safran_ana', 'safran_prv']:
        if args.region is None:
            args.region = ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur']
        for dom in args.region:
            job = t.sh.path.join(rootdir, dom, 'jobs', jobname)
            conf = t.sh.path.join(rootdir, dom, 'conf', f's2m_{dom}.ini')
            execute(job, conf, rundate, additional, domain=dom)

    elif jobname in ['surfex_ana', 'surfex_prv']:
        if args.region is None:
            args.region = ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur', 'postes']
        for dom in args.region:
            job = t.sh.path.join(rootdir, dom, 'jobs', jobname)
            conf = t.sh.path.join(rootdir, dom, 'conf', f's2m_{dom}.ini')
            execute(job, conf, rundate, additional, domain=dom)


if __name__ == "__main__":
    main()
