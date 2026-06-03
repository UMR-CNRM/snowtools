#!/usr/bin/env python3

import os
import vortex
import argparse
import importlib

from bronx.stdtypes.date import Date

from mkjob.nodes import Driver

parser = argparse.ArgumentParser(description='Test the toolbw call of a given task locally.')

parser.add_argument("-t", "--task",
        help="Name of the Task (class) to be tested", type=str, required=True)

parser.add_argument("-m", "--module",
        help="Name of the module in which is the task to be tested",
        type=str, required=True)

parser.add_argument("-d", "--directory",
        help="Directory in which the module of the task to be tested is stored",
        type=str, required=True)

parser.add_argument("-c", "--configuration",
        help="Path to the test's configuration file",
        type=str, required=True)

parser.add_argument("-w", "--workdir",
        help="The test's working directory",
        default = os.path.join(os.environ['HOME'], 'tmpdir'),
        required=False)

parser.add_argument("-a", "--add", nargs='+',
        help="Additional mandatory configuration variables not in the configuration file",
        required=False)

parser.add_argument("-s", "--steps",
        help="Path to a specific snowtools repository", choices=['early-fetch', 'compute', 'late-backup'],
        nargs='+', required=False, default=['early-fetch', 'compute', 'late-backup'])

args = parser.parse_args()
if args.add is not None:
    args.add = {item.split('=')[0]: item.split('=')[1] for item in args.add}
else:
    args.add = dict()

t = vortex.ticket()
t.rundir = args.workdir

module = importlib.import_module(f'vortex_cen.tasks.{args.directory}.{args.module}')
task = getattr(module, args.task)

user = os.environ['USER']
default_args = dict(localtest=True, xpid='localtest', datebegin=Date('2020080106'), dateend=Date('2021080106'),
        steps=args.steps)
default_args.update(**args.add)
# Ensure proper date management as in the mkjob launcher
for key, value in default_args.items():
    if 'date' in key:
        default_args[key] = Date(value)

driver = Driver(
    tag=args.task.lower(),
    ticket=t,
    nodes=[
        task(tag=args.task.lower(), ticket=t, **default_args),
    ],
    options=default_args,
    iniconf = args.configuration,
)

driver.setup()
driver.run()
