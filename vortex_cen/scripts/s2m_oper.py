# -*- coding: utf-8 -*

import os
import vortex_cen
import subprocess
import argparse


log = os.path.join(os.environ["HOME"], "LOG_CRON")
# Get S2M oper root directory
rootdir = os.path.join(vortex_cen.__path__[0], 's2m')


def parse_command_line():

    parser = argparse.ArgumentParser(description='mkjob command line helper')

    parser.add_argument(
        "-j",
        "--jobname",
        help = "Target job name",
        type = str,
        choices = ['prepsafran_ana', 'prepsafran_prv', 'safran_ana', 'safran_prv', 'surfex_ana', 'surfex_prv'],
    )

    parser.add_argument(
        "-d",
        "--rundate",
        help = "Date of the oper run (hour must be in (03, 06, 09, 12)",
        type = str,
    )

    parser.add_argument(
        "-r",
        "--region",
        help = "vconf of the operational run",
        choices = ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur', 'postes'],
        type = str,
        nargs = '*',
    )

    args = parser.parse_args()
    return args


def execute(job, conf, rundate, domain=None):
    job_name = os.path.basename(job)
    if domain is not None:
        job_name = f'{job_name}_{domain}'
    subprocess.call(
        f"mkjob -f {job} -c {conf} -a rundate={rundate} >> {log}/{job_name}_{rundate} 2>&1",
        shell=True
    )


def main():
    args = parse_command_line()
    jobname = args.jobname
    rundate = args.rundate

    if jobname in ['prepsafran_ana', 'prepsafran_prv']:
        job = os.path.join(rootdir, f'oper/jobs/{jobname}')
        conf = os.path.join(rootdir, 'oper/conf/s2m_common.ini')
        execute(job, conf, rundate)

    elif jobname in ['safran_ana', 'safran_prv']:
        if args.region is None:
            args.region = ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur']
        for dom in args.region:
            job = os.path.join(rootdir, dom, 'jobs', jobname)
            conf = os.path.join(rootdir, dom, 'conf', f's2m_{dom}.ini')
            execute(job, conf, rundate, dom)

    elif jobname in ['surfex_ana', 'surfex_prv']:
        if args.region is None:
            args.region = ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur', 'postes']
        for dom in args.region:
            job = os.path.join(rootdir, dom, 'jobs', jobname)
            conf = os.path.join(rootdir, dom, 'conf', f's2m_{dom}.ini')
            execute(job, conf, rundate, dom)


if __name__ == "__main__":
    main()
