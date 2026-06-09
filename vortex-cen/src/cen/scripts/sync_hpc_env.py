#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import shutil
from snowtools.DATA import SNOWTOOLS_CEN

parser = argparse.ArgumentParser(description='Create a copy of a launchable, vortex-driven environment for HPC tasks.')

parser.add_argument("-a", "--vapp",
        help="Nom de l'application (par exemple 's2m' ou 'edelweiss')", type=str, required=True)

parser.add_argument("-c", "--vconf",
        help="Configuration de l'application (par exemple 'reanalysis', 'pra')", type=str, required=True)

parser.add_argument("-r", "--rootdir",
        help="Root directory (default: '/scratch/work/{username}", required=False)

parser.add_argument("-s", "--snowtools",
        help="Path to a specific snowtools repository", required=False)

parser.add_argument("-v", "--vortex",
        help="Path to a specific vortex repository (Vortex-1 only)", required=False)

args = parser.parse_args()

if args.rootdir is not None:
    rootdir = args.rootdir
else:
    rootdir = os.path.join('/scratch/work', os.environ['USER'])

if args.snowtools is not None:
    snowtools = args.snowtools
else:
    snowtools = SNOWTOOLS_CEN

# 1. Create tree
dst = os.path.join(rootdir, args.vapp, args.vconf)
os.makedirs(dst, exist_ok=True)
os.chdir(dst)
# TODO : check if vapp / vconf exists in snowtools
for folder in ['jobs', 'drivers']:
    src = os.path.join(snowtools, 'vortex-cen', args.vapp, args.vconf, folder)
    # Sync folder from the snowtools repo
    if os.path.exists(src):
        if os.path.exists(folder):
            shutil.rmtree(folder)
        shutil.copytree(src, folder)
    else:
        # TODO
        pass

# Vortex-1 only : create link to Vortex repo
if os.path.exists('vortex'):
    os.remove('vortex')
if args.vortex is not None:
    os.symlink(args.vortex, "vortex")
else:
    os.symlink("/home/mf/dp/marp/verolive/vortex/vortex-cen", "vortex")

if os.path.exists('snowtools'):
    os.remove('snowtools')
os.symlink(snowtools, "snowtools")

if os.path.exists('conf'):
    shutil.rmtree('conf')
src = os.path.join(SNOWTOOLS_CEN, 'vortex-cen', args.vapp, args.vconf, 'conf')
shutil.copytree(src, 'conf')


print(f'The following rootdir has been created / updated : {os.getcwd()}')
