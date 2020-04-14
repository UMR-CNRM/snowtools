#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:15:00
#SBATCH --verbose
#SBATCH --job-name=pertforc
#SBATCH --partition=normal64

python3 /home/cluzetb/assim/jobs/job_gener_pert_forcings.py