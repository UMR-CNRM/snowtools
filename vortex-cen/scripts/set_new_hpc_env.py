#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import shutil
from snowtools.DATA import SNOWTOOLS_DIR

parser = argparse.ArgumentParser(description='Create a launchable, vortex-driven environment for HPC tasks.')

parser.add_argument("-v", "--vapp",
        help="Nom de l'application (par exemple 's2m' ou 'edelweiss')", type=str, required=True)

parser.add_argument("-f", "--vconf",
        help="Configuration de l'application (par exemple 'reanalysis', 'pra')", type=str, required=True)

parser.add_argument("-c", "--conf",
        help="Path to configuration file", type=str, required=False)

parser.add_argument("-r", "--rootdir",
        help="Root directory (default: '/scratch/work/{username}", required=False)

parser.add_argument("-t", "--task",
        help="Path to actual task / driver file", required=False)

parser.add_argument("-s", "--snowtools",
        help="Path to a specific snowtools repository", required=False)

args = parser.parse_args()

if args.rootdir is not None:
    rootdir = args.rootdir
else:
    rootdir = os.path.join('/scratch/work', os.environ['USER'])

# 1. Create tree
basedir = os.path.join(rootdir, args.vapp, args.vconf)
os.makedirs(basedir, exist_ok=True)
os.chdir(basedir)

os.makedirs('jobs', exist_ok=True)
os.makedirs('tasks', exist_ok=True)
os.makedirs('conf', exist_ok=True)
if os.path.exists('vortex'):
    os.remove('vortex')
os.symlink("/home/mf/dp/marp/verolive/vortex/vortex-cen", "vortex")

if os.path.exists('snowtools'):
    os.remove('snowtools')
if args.snowtools is not None:
    os.symlink(args.snowtools, "snowtools")
else:
    os.symlink(SNOWTOOLS_DIR, "snowtools")

# 2. Fill "tasks" repository with relevant drivers
os.chdir('tasks')
if not os.path.exists("__init__.py"):
    os.mknod("__init__.py")
if args.task is not None:
    taskname = os.path.basename(args.task)
    if os.path.exists(taskname):
        os.remove(taskname)
    os.symlink(args.task, taskname)
os.chdir(basedir)

# 3. Fill "conf repository with configuration file
os.chdir('conf')
if args.conf is not None:
    confname = f'{args.vapp}_{args.vconf}.ini'
    if os.path.exists(confname):
        os.remove(confname)
    shutil.copyfile(args.conf, confname)

os.chdir(os.path.join(basedir, 'jobs'))
print(f'Create your jobs under {os.getcwd()}')
print('with the following command-line :')
print('../vortex/bin/mkjob.py -j name={your_job_name} task={your_task_name} profile=rd-belenos-mt jobassistant=cen')
