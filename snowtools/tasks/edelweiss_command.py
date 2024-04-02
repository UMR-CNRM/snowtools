#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 28 march. 2024

@author: Vernay
"""

# General python modules
import argparse
import sys

# Import snowtools modules
from snowtools.tasks.edelweiss_kitchen import Edelweiss_kitchen


class ParseKwargs(argparse.Action):
    """
    Proxy tool used to parse arguments as dictionnaries with argparse.

    Source :
    --------
    https://sumit-ghosh.com/posts/parsing-dictionary-key-value-pairs-kwargs-argparse-python/
    """

    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, dict())
        for value in values:
            key, value = value.split(':')
            getattr(namespace, self.dest)[key] = value


class Edelweiss_command(object):
    """
    Edelweiss-related tasks launcher
    """

    def __init__(self, args):
        self.mandatory_arguments = dict()
        # Read the options provided by the user
        self.options = self.parse_options(args)

        self.execute()

    def parse_options(self, arguments):

        description = """Command that allow to launch any EDELWEISS-related task."""
        parser = argparse.ArgumentParser(prog="edelweiss",
                                         description=description,
                                         epilog="For detailed documentation of general use case, "
                                         "edelweiss --help",
                                         )

        parser.add_argument("-b", "--datebegin", type=str, dest="datebegin", required=True,
                            help="Date of the beginning of the simulation period. Format YYYYMMDDHH")

        parser.add_argument("-e", "--dateend", type=str, dest="dateend", required=True,
                            help="Date of the end of the simulation period. Format YYYYMMDDHH")

        parser.add_argument("-x", "--xpid", type=str, dest="xpid", required=True,
                            help="Experiment's identifier (xpid). Use this xpid argument to 'tag' your execution."
                                 "A same xpid can (must) be used to launch the different tasks associated to a single"
                                 "SURFEX simulation (from the FORCING generation to the post-processing tasks), as well"
                                 "as for identical SURFEX simulations (same namelist, executables, FORCING origin,...)"
                                 "over different time periods or different geometries."
                                 "On the contrary, a different XPID must be used when a different namelist or"
                                 "executable (or any other input file) is used, or when something changed in the"
                                 "FORCING generation pprocess")

        parser.add_argument("-g", "--geometry", type=str, dest="geometry", required=True,
                            help="Geometry of the simulation. EDELWEISS simulations are always on a regular"
                                 "lat/loni grid. The geometry name should include information on the simulation area"
                                 "and on the resolution (for example 'GrandesRousses250m')."
                                 "The geometry must also be defined in a 'geometries.ini' file (either in the Vortex's"
                                 "'conf' directory or under the '.vortexrc' directory in your $HOME.")

        parser.add_argument("--task", dest="task", action='store', type=str,
                            required='--family' not in sys.argv,  # either --task or --family must be provided
                            choices = ['make_precipitation', 'make_forcing'],  # TODO : give the full list of choices
                            # TODO : ensure all tasks in this list are in the default edelweiss.ini file
                            help="The name of the (single) task to launch."
                                 "This argument must be used by default")

        parser.add_argument("--family", dest="family", action='store', type=str, nargs="+",
                            required="--task" not in sys.argv,  # either --task or --family must be provided
                            help="ADVANCED USERS ONLY (alternative to --task argument)"
                                 "List of tasks names or single family name to be launched."
                                 "This option can be used to launch several tasks in one command.")

        parser.add_argument("-u", "--uenv", type=str, dest="uenv",
                            help="User Environment (UEnv). Use this argument to provide all constant files required"
                                 "for your simulation that are different from the default ones."
                                 "If a fitting UEnv already exists (genarated by a previous execution, another user or"
                                 "manually), use the following syntax : "
                                 "'-u uenv:{UEnv_name}@{username}'"
                                 "In any other case, put all concerned files in a specific directory (copy files or"
                                 "symbolic links) and use the following syntax :"
                                 "'-u {absolute/path/to/the/directory}'")

        parser.add_argument("-m", "--members", action="store", type=str, dest="members", default=None,
                            help="Ensemble members in case of an ensemble simulation"
                                 "Possible formats :"
                                 "-m {number of members} (starts at 0 by default)"
                                 "-m '{first_member}:{last-member}'")

        parser.add_argument("-l", "--model", type=str, dest="model", default=None,
                            help="Name of the model associated to the simulation (if any)")

        parser.add_argument("-n", "--namespace", type=str, dest="namespace", default='multi',
                            choices=['cache', 'archive', 'multi', 'sxcen'],
                            help="Vortex's namespace defining where the produced files will be stored."
                                 "'cache'   : local (HPC) cache only"
                                 "'archive' : archive (Hendrix) cache only"
                                 "'multi'   : local and archive caches"
                                 "'sxcen'   : on sxcen (post-processing)"
                            )

        parser.add_argument("-c", "--configuration", action="store", type=int, dest="defaultconf", default=None,
                            # TODO : implementatin + doc
                            help="Default user-defined configuration file."
                                 "This option can be used to avoid to parser too many arguments in the command line")

        parser.add_argument("-v", "--vapp", action="store", type=str, dest="vapp", default='edelweiss',
                            help="Application name (if not 'edelweiss')")

        parser.add_argument("--walltime", "--time", action="store", type=str, dest="time", default=None,
                            help="specify your job walltime (format hh:mm:ss)")

        parser.add_argument("--ntasks", action="store", type=int, dest="ntasks", default=None,
                            help="Number of tasks (and procs) per node.")

        parser.add_argument("--nnodes", action="store", type=int, dest="nnodes", default=1,
                            help="Number of nodes")

        # Input-specific arguments :
        # =========================

        # TODO Faire une section "inputs"
        # - 1 argument / input type, valeurs possibles :
        #       * abspath du fichier
        #       * Footrpint's dict (format : {xpid=..., geometry=..., vapp=...} par exemple)
        #       [+ possibilité de passer tous ces arguments dans un fichier de conf par défaut]
        #       ==> à gérer dans edelweiss_kitchen
        # - Imposer les arguments nécessaire en foncion de ce qui est passé à --task :
        #   required="task_name" in sys.argv

        parser_forcing = parser.add_argument_group('Specific input description')

        parser_forcing.add_argument("--forcing", dest="forcing", action=ParseKwargs, nargs='*',
                                    type=str, default=None,
                                    # TODO : what if already in the user conf file ? --> overwrite ?
                                    required="make_forcing" in sys.argv,  # Mandatory argument for 'make_forcing' task
                                    help="Definition (footprint-like dict) of the default forcing variables input"
                                         "Format : --forcing key1:var1 key2:var2 ..."
                                         "Known dictionary keys : ['xpid', 'geometry', 'vapp', 'datebegin', 'dateend']")

        parser_forcing.add_argument("--precipitation", dest="precipitation", action=ParseKwargs, nargs='*',
                                    type=str, default=None,
                                    # TODO : what if already in the user conf file ? --> overwrite ?
                                    help="Definition (footprint-like) dict of precipitation variables input"
                                         "Format : --forcing key1:var1 key2:var2 ..."
                                         "Possible dictionary keys : ['xpid', 'geometry', 'vapp']")

        parser_forcing.add_argument("--wind", dest="wind", action=ParseKwargs, nargs='*',
                                    type=str, default=None,
                                    # TODO : what if already in the user conf file ? --> overwrite ?
                                    help="Definition (footprint-like) dict of wind variables input"
                                         "Format : --forcing key1:var1 key2:var2 ..."
                                         "Possible dictionary keys : ['xpid', 'geometry', 'vapp']")

        options  = parser.parse_args(arguments)

        return options  # Return a dictionnary object

    def execute(self):
        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options()
        # Build vortex task
        Edelweiss_kitchen(self.options)

    def check_and_convert_options(self):
        if self.options.members is not None:
            if ':' in self.options.members:
                first, last = self.options.members.split(':')
                self.options.members = f'{first}-{last}-1'
            else:
                self.options.members = int(self.options.members)

        args_to_dict = vars(self.options)  # Convert self.options *Namepace* object to a *dictionnary* object
        # Convert *dictionary* arguments to proper configuration variables
        for specific_input in ['forcing', 'precipitation', 'wind']:
            # Check if a value has been parsed
            if args_to_dict[specific_input] is not None:
                # Convert dictionnary into proper configuration entries
                for key, value in args_to_dict[specific_input].items():
                    setattr(self.options, f'{key}_{specific_input}', value)
            # Ensure that all "optionnal" conf variables are set
            # Keys not provided by the user are set to the task's one
            for key in ['geometry', 'vapp', 'datebegin', 'dateend']:
                if not hasattr(self.options, f'{key}_{specific_input}'):
                    setattr(self.options, f'{key}_{specific_input}', args_to_dict[key])

    def check_mandatory_arguments(self, **kw):
        missing_options = list()
        self.mandatory_arguments.update(**kw)

        for mandatory, opt in self.mandatory_arguments.items():
            if hasattr(self.options, opt):
                if getattr(self.options, opt) is None:
                    missing_options.append(mandatory)
            else:
                missing_options.append(mandatory)

        if len(missing_options) > 0:
            print('The following mandatory options are missing : ' + ','.join(missing_options))
            sys.exit(1)


def main():
    Edelweiss_command(sys.argv[1:])


if __name__ == "__main__":
    main()
