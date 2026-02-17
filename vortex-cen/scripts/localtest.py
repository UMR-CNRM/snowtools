#!/usr/bin/env python3

import os
import vortex
import argparse
import importlib

from vortex.layout.nodes import Driver

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
        required=False)

parser.add_argument("-s", "--steps",
        help="Path to a specific snowtools repository", choices=['early-fetch', 'compute', 'late-backup'],
        nargs='+', required=False, default=['early-fetch', 'compute', 'late-backup'])

args = parser.parse_args()

t = vortex.ticket()

module = importlib.import_module(f'vortex_cen.tasks.{args.directory}.{args.module}')
task = getattr(module, args.task)


def setup(t, **kw):
    return Driver(
        tag=args.task.lower(),
        ticket=t,
        nodes=[
            task(tag=args.task.lower(), ticket=t, **kw),
        ],
        options=kw,
        iniconf = args.configuration,
    )


# TODO :
# - Déplacer l'exécution dans un "workdir" à l'extérieur du dépot
# - Créer un driver [fichier de conf ?] spécifique dans vortex-cen/tests
# - Transformer en script avec la task et la step à tester en arguments

user = os.environ['USER']
default_args = dict(localtest=True)
driver = setup(t, steps=args.steps, **default_args)
driver.setup()
driver.run()
