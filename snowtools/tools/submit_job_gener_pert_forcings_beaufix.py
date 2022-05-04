#!/usr/local/bin/python2.7
# encoding: utf-8
'''
Created on 12 févr. 2020

@author: cluzetb & deschampsbc
Submit job_gener_pert_forcings.py to a computation node on beaufix via sbatch.
check PertParser in job_gener_pert_forcings for the list of base arguments.
'''

import os
from snowtools.tools.job_gener_pert_forcings import PertParser

if __name__ == "__main__":
    parser = PertParser()
    parser.argparser.add_argument("-walltime_run", dest = 'walltime_run', help= 'HH:MM:SS format', default = '00:30:00')

    args = parser.parse()

    # prepare the transfer script
    job_pert_name = "job_pert_forcings_out.bash"
    job_pert_path = os.environ['HOME'] + "/script/"
    tmp = os.path.expanduser("~")
    username = tmp.split('/')[-1]
    if os.path.exists(job_pert_path + job_pert_name):
        os.remove(job_pert_path + job_pert_name)
    with open(job_pert_path + job_pert_name, 'a') as job_pert_local:
        job_pert_local.write("#!/bin/bash\n#SBATCH --verbose\n#SBATCH --job-name=pertforc" + "\n" +
                             "#SBATCH --nodes=1\n#SBATCH --ntasks=1\n" +
                             "#SBATCH --time=" + args.walltime_run + "\n#SBATCH --verbose\n" +
                             "#SBATCH --partition=normal64\n\n")

        # pass the arguments to job_gener
        args_gener = ' '.join(['-{0} {1}'.format(attr, args.__dict__[attr]) if attr not in ['walltime_run', ] else '' for attr in args.__dict__])
        job_pert_local.write('python3 ' + os.environ['SNOWTOOLS_CEN'] + '/tools/job_gener_pert_forcings.py ' + args_gener)

    # submit the perturbations
    os.system('sbatch ' + job_pert_path + job_pert_name)
