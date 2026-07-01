# -*- coding: utf-8 -*

import os
import shutil
import argparse


parser = argparse.ArgumentParser(description='Put SURFEX executables in a UEnv')

parser.add_argument("-e", "--exesurfex",
        help="Path to SURFEX executables to put in the UEnv", type=str, required=True)

parser.add_argument("-s", "--suffix",
        help="Suffix to add to the executable names", type=str)

parser.add_argument("-u", "--uenv",
        help="Name of the UEnv to create", type=str)

parser.add_argument("--mpi",
        help="Whether or not the executables have been compiled with MPI", action='store_true')

args = parser.parse_args()

if args.suffix is None:
    git_info = os.path.join(os.path.dirname(args.exesurfex), '.git_info')
    with open(git_info, 'r') as f:
        args.suffix = f.readline().split(' ')[0]

if args.uenv is None:
    if args.mpi:
        args.uenv = f'surfex_mpi_{args.suffix}'
    else:
        args.uenv = f'surfex_nompi_{args.suffix}'

HOME = os.environ['HOME']
USER = os.environ['USER']
uenv_path = os.path.join(HOME, '.vortexrc/hack/uget', USER)
env = os.path.join(uenv_path, 'env')
data = os.path.join(uenv_path, 'data')
if not os.path.exists(env):
    os.makedirs(env)
if not os.path.exists(data):
    os.makedirs(data)

uenv_file = os.path.join(env, args.uenv)
if os.path.exists(uenv_file):
    print(f"WARNING : The UEnv {uenv_file} already exists, doing nothing")
else:
    with open(os.path.join(env, args.uenv), 'w') as f:
        for executable in ['OFFLINE', 'PGD', 'PREP', 'SODA']:
            src = os.path.join(args.exesurfex, executable)
            if args.mpi:
                filename = f'{executable}_MPI_{args.suffix}'
                dst = os.path.join(data, filename)
                f.write(f'MASTER_{executable}_MPI="uenv:{filename}@{USER}"\n')
            else:
                filename = f'{executable}_NOMPI_{args.suffix}'
                dst = os.path.join(data, filename)
                f.write(f'MASTER_{executable}_NOMPI="uenv:{filename}@{USER}"\n')
            shutil.copyfile(src, dst)
