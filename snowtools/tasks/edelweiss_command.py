#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 28 march. 2024

@author: Vernay
"""

# General python modules
import os
import sys
import argparse
from argparse import RawTextHelpFormatter

# Import snowtools modules
from snowtools.tasks.edelweiss_kitchen import Edelweiss_kitchen


class ParseKwargs(argparse.Action):
    """
    Proxy tool used to parse arguments as dictionnaries with argparse.
    Syntax :
        * --arg key1=value1 key2=value2 --> returns `dict(key1=value1, key2=value2)`

    Source :
    --------
    https://sumit-ghosh.com/posts/parsing-dictionary-key-value-pairs-kwargs-argparse-python/
    """

    def __call__(self, parser, namespace, values, option_string=None):

        setattr(namespace, self.dest, dict())
        for value in values:
            key, value = value.split('=')
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

        description = """
                      Command that allow to launch any EDELWEISS-related task.\n
                      ========================================================
                      """
        parser = argparse.ArgumentParser(prog="edelweiss",
                                         description=description,
                                         epilog="For detailed documentation of general use case, "
                                         "edelweiss --help",
                                         formatter_class=RawTextHelpFormatter,  # Preserve string format
                                         )

        parser.add_argument("-b", "--datebegin", type=str, dest="datebegin",
                            required='--tasksinfo' not in sys.argv,  # Print informations only
                            help="Date of the beginning of the simulation period. Format YYYYMMDDHH\n"
                                 " ")

        parser.add_argument("-e", "--dateend", type=str, dest="dateend",
                            required='--tasksinfo' not in sys.argv,  # Print informations only

                            help="Date of the end of the simulation period. Format YYYYMMDDHH\n"
                                 " ")

        parser.add_argument("-x", "--xpid", type=str, dest="xpid",
                            required='--tasksinfo' not in sys.argv,  # Print informations only
                            help="Experiment's identifier (xpid). Use this xpid argument to 'tag' your execution."
                                 "A same xpid can (must) be used to launch the different tasks associated to a single"
                                 "SURFEX simulation (from the FORCING generation to the post-processing tasks), as well"
                                 "as for identical SURFEX simulations (same namelist, executables, FORCING origin,...)"
                                 "over different time periods or different geometries.\n"
                                 "On the contrary, a different XPID must be used when a different namelist or"
                                 "executable (or any other input file) is used, or when something changed in the"
                                 "FORCING generation pprocess\n"
                                 " ")

        parser.add_argument("-g", "--geometry", type=str, dest="geometry",
                            required='--tasksinfo' not in sys.argv,  # Print informations only
                            help="Geometry of the simulation. EDELWEISS simulations are always on a regular"
                                 "lat/loni grid. The geometry name should include information on the simulation area"
                                 "and on the resolution (for example 'GrandesRousses250m').\n"
                                 "The geometry must also be defined in a 'geometries.ini' file (either in the Vortex's"
                                 "'conf' directory or under the '.vortexrc' directory in your $HOME.\n"
                                 " ")

        parser.add_argument("--task", dest="task", type=str,
                            action='store',
                            # action=ParseKwargs, nargs='*',
                            required='--tasksinfo' not in sys.argv,  # either --task or --tasksinfo must be provided
                            # TODO : créer un dictionnaire contenant les tâches standard
                            # TODO : give the full list of known choices but allow other values
                            # choices = ['make_precipitation', 'make_forcing'],
                            # TODO : ensure all tasks in this list are in the default edelweiss.ini file
                            help="The class name(s) of the task(s) to launch."
                                 "This argument must be used by default.\n"
                                 "To get more information on the possible arguments, use :\n"
                                 "edelweiss --tasksinfo\n"
                                 " ")

#        parser.add_argument("--family", dest="family", action='store', type=str, nargs="+",
#                            required="--task" not in sys.argv,  # either --task or --family must be provided
#                            help="ADVANCED USERS ONLY (alternative to --task argument)"
#                                 "List of tasks names or single family name to be launched."
#                                 "This option can be used to launch several tasks in one command.")

        parser.add_argument("-u", "--uenv", type=str, dest="uenv",
                            help="User Environment (UEnv). Use this argument to provide all constant files required"
                                 "for your simulation that are different from the default ones.\n"
                                 "If a fitting UEnv already exists (genarated by a previous execution, another user or"
                                 "manually), use the following syntax :\n"
                                 "'-u uenv:{UEnv_name}@{username}'\n"
                                 "In any other case, put all concerned files in a specific directory (copy files or"
                                 "symbolic links) and use the following syntax :\n"
                                 "'-u {absolute/path/to/the/directory}'\n"
                                 " ")

        parser.add_argument("-m", "--members", action="store", type=str, dest="members", default=None,
                            help="Output ensemble members in case of an ensemble simulation.\n"
                                 "Each member correspond to a single execution of the algo component\n"
                                 "Possible formats :\n"
                                 "-m {number of members} (starts at 0 by default)\n"
                                 "-m '{first_member}:{last-member}'\n"
                                 " ")

        parser.add_argument("-n", "--namespace", type=str, dest="namespace", default='multi',
                            choices=['cache', 'archive', 'multi', 'sxcen'],
                            help="Vortex's namespace defining where the produced files will be stored.\n"
                                 " * cache   : local (HPC) cache only\n"
                                 " * archive : archive (Hendrix) cache only\n"
                                 " * multi   : local and archive caches\n"
                                 " * sxcen   : on sxcen (post-processing)\n"
                                 " ")

        parser.add_argument("-v", "--vapp", action="store", type=str, dest="vapp", default='edelweiss',
                            help="Application name (if not 'edelweiss')\n"
                                 " ")

        conf_parser = parser.add_argument_group("Optional arguments not bounded to a specific task :\n"
                                               "-----------------------------------------------------\n"
                                               " ")

        conf_parser.add_argument("--tasksinfo", dest="tasksinfo", action='store_true',
                                 # TODO : use a sub-parser ?
                                 # required='--task' not in sys.argv,  # either --task or --tasksinfo must be provided
                                 # TODO : implémenter cette option (lire la variable "info" de chaque tâche du fichier
                                 # de conf par défaut
                                 help="Get information on the various tasks currently implemented\n"
                                      " ")

        conf_parser.add_argument("-c", "--configuration", action="store", type=int, dest="defaultconf", default=None,
                                 # TODO : implementatin + doc
                                 help="Default user-defined configuration file.\n"
                                      "This option can be used to avoid to parser too many arguments in"
                                      "the command line\n"
                                      " ")

        conf_parser.add_argument("-a", "--assimdates", action="store", nargs='+', dest="assimdates", default=None,
                                 help="Assimilation date(s) for croco task\n")

        job_parser = parser.add_argument_group("Job configuration arguments :\n"
                                               "-----------------------------\n"
                                               " ")

        job_parser.add_argument("--parallelisation", action="store", type=str,
                               dest="parallelisation", default="mpi",
                               choices=['mpi', 'forcing', 'namelist', 'multi'],
                               help="Type of parallelisation for one job :\n"
                                    " * mpi      : parallelisation over the simulation domain for each FORCING file\n"
                                    " * forcing  : parallelisation over FORCING files only (no MPI parallelisation)\n"
                                    " * namelist : multi-physic (1 exec / namelist) parallelisation (ESCROC)\n"
                                    " * multi    : forcing AND namelist parallelisation (CrocO)\n"
                                    " ")

        job_parser.add_argument("--nnodes", action="store", type=int, dest="nnodes", default=1,
                                help="Number of nodes used with this command\n"
                                     " ")

        job_parser.add_argument("--walltime", "--time", action="store", type=str, dest="time", default=None,
                                help="specify your job walltime (format hh:mm:ss)\n"
                                     " ")

#        parser.add_argument("--njobs", action="store", type=int, dest="njobs", default=None,
#                            help="Number of jobs (/mkjob command) to launch")

        job_parser.add_argument("--ntasks", action="store", type=int, dest="ntasks", default=None,
                                help="Number of tasks (and procs) per node.\n"
                                     " ")

        job_parser.add_argument("-l", "--model", type=str, dest="model", default=None,
                                help="Name of the model associated to the simulation (if any)\n"
                                     " ")

        task_parser = parser.add_argument_group("Optional task-specific arguments :\n"
                                               "-----------------------------------\n"
                                               " ")

        task_parser.add_argument("--extraction_dates", type=str, dest="extraction_dates", default=None,
                                help="List of extraction dates for the 'extract_dates' surfex post-processing task\n"
                                     "separeted by commas\n"
                                     "Syntax : --extraction_dates yyyymmddhh,yyyymmddhh, ... \n"
                                     "The final value in the configuration file should be :\n"
                                     "extraction_dates = list(yyyymmddhh,yyyymmddhh)")

        # TODO : ajouter une option pour conserver le répertoire d'exécution

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

        parser_input = parser.add_argument_group("Specific input description :\n"
                                                 "----------------------------\n"
                                                 " ")

        parser_input.add_argument("--forcing", dest="forcing", action=ParseKwargs, nargs='*',
                                  type=str, default=None,
                                  # TODO : what if already in the user conf file ? --> overwrite ?
                                  required="make_forcing" in sys.argv,  # Mandatory argument for 'make_forcing' task
                                  help="Definition (footprint-like dict) of the default forcing variables input.\n"
                                       "Format : --forcing key1=var1 key2=var2 ...\n"
                                       "Known dictionary keys :\n"
                                       " * xpid      (str) : Experiment identifier (Format : {xpid}@{username})\n"
                                       " * geometry  (str) : Experiment geometry"
                                       " * vapp      (str) : Application that produced the FORCING (s2m/edelweiss)\n"
                                       " * members   (int) : Number of input members (format 'X:Y' valid)\n"
                                       " * datebegin (str) : If different from '-b' argument (format YYYYMMDDHH)\n"
                                       " * dateend   (str) : If different from '-e' argument (format YYYYMMDDHH)\n"
                                       " ")

        parser_input.add_argument("--prep", dest="prep", action=ParseKwargs, nargs='*',
                                  type=str, default=None,
                                  # TODO : what if already in the user conf file ? --> overwrite ?
                                  help="Definition (footprint-like dict) of the default forcing variables input.\n"
                                       "Format : --prep key1=var1 key2=var2 ...\n"
                                       "Known dictionary keys :\n"
                                       " * xpid      (str) : Experiment identifier (Format : {xpid}@{username})\n"
                                       " * geometry  (str) : Experiment geometry\n"
                                       " * vapp      (str) : Application that produced the FORCING (s2m/edelweiss)\n"
                                       " * date      (str) : Validity date (default = *datebegin*)\n"
                                       " ")

        parser_input.add_argument("--precipitation", dest="precipitation", action=ParseKwargs, nargs='*',
                                  type=str, default=None,
                                  # TODO : what if already in the user conf file ? --> overwrite ?
                                  help="Definition (footprint-like) dict of precipitation variables input.\n"
                                       "Format : --precipitation key1=var1 key2=var2 ...\n"
                                       "Known dictionary keys :\n"
                                       " * xpid      (str) : Experiment identifier (Format : {xpid}@{username})\n"
                                       " * geometry  (str) : Experiment geometry\n"
                                       " * vapp      (str) : Application that produced the FORCING (s2m/edelweiss)\n"
                                       " * members   (int) : Number of input members (format 'X:Y' valid)\n"
                                       " * datebegin (str) : If different from '-b' argument (format YYYYMMDDHH)\n"
                                       " * dateend   (str) : If different from '-e' argument (format YYYYMMDDHH)\n"
                                       " ")

        parser_input.add_argument("--lpn", dest="lpn", action=ParseKwargs, nargs='*',
                                  type=str, default=None,
                                  # TODO : what if already in the user conf file ? --> overwrite ?
                                  help="Definition (footprint-like) dict of the variable to be used to compute the"
                                       "rain snow limit.\n"
                                       "Format : --precipitation key1=var1 key2=var2 ...\n"
                                       "Known dictionary keys :\n"
                                       " * xpid      (str) : Experiment identifier (Format : {xpid}@{username})\n"
                                       " * kind      (str) : Kind of file (iso-t level or full vertical profile\n"
                                       " * geometry  (str) : Experiment geometry\n"
                                       " * datebegin (str) : If different from '-b' argument (format YYYYMMDDHH)\n"
                                       " * dateend   (str) : If different from '-e' argument (format YYYYMMDDHH)\n"
                                       " ")

        parser_input.add_argument("--wind", dest="wind", action=ParseKwargs, nargs='*',
                                  type=str, default=None,
                                  # TODO : what if already in the user conf file ? --> overwrite ?
                                  help="Definition (footprint-like) dict of wind variables input"
                                       "Format : --wind key1=var1 key2=var2 ...\n"
                                       "Known dictionary keys :\n"
                                       " * xpid      (str) : Experiment identifier (Format : {xpid}@{username})\n"
                                       " * geometry  (str) : Experiment geometry\n"
                                       " * vapp      (str) : Application that produced the FORCING (s2m/edelweiss)\n"
                                       " * members   (int) : Number of input members (format 'X:Y' valid)\n"
                                       " * datebegin (str) : If different from '-b' argument (format YYYYMMDDHH)\n"
                                       " * dateend   (str) : If different from '-e' argument (format YYYYMMDDHH)\n"
                                       " ")

        parser_input.add_argument("--pro", dest="pro", action=ParseKwargs, nargs='*',
                                  type=str, default=None,
                                  help="Definition (footprint-like dict) of the PRO file input.\n"
                                       "Format : --pro key1=var1 key2=var2 ...\n"
                                       "Known dictionary keys :\n"
                                       " * xpid      (str) : Experiment identifier (Format : {xpid}@{username})\n"
                                       " * geometry  (str) : Experiment geometry"
                                       " * vapp      (str) : Application that produced the FORCING (s2m/edelweiss)\n"
                                       " * members   (int) : Number of input members (format 'X:Y' valid)\n"
                                       " * datebegin (str) : If different from '-b' argument (format YYYYMMDDHH)\n"
                                       " * dateend   (str) : If different from '-e' argument (format YYYYMMDDHH)\n"
                                       " ")

        options  = parser.parse_args(arguments)

        return options  # Return a dictionnary object

    def execute(self):
        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options()
        # Build vortex task
        Edelweiss_kitchen(self.options)

    def check_and_convert_options(self):

        # Convert *members* argument into a rangex object
        if self.options.members is not None:
            self.options.members = self.convert_members(self.options.members)

        if '@' not in self.options.xpid:
            self.options.xpid = f'{self.options.xpid}@{os.getlogin()}'

        args_to_dict = vars(self.options)  # Convert self.options *Namepace* object to a *dictionnary* object
        # Convert *dictionary* arguments to proper configuration variables
        for specific_input in ['forcing', 'precipitation', 'wind', 'prep', 'pro', 'lpn']:

            # TODO : return a footprint-like dictionnary to be used directly !

            # Check if a value has been parsed
            if args_to_dict[specific_input] is not None:
                # Convert dictionnary into proper configuration entries
                for key, value in args_to_dict[specific_input].items():
                    if key == 'members':
                        value = self.convert_members(value)
                    setattr(self.options, f'{key}_{specific_input}', value)
            # Ensure that all "optional" conf variables are set
            # Keys not provided by the user are set to the task's one
            # The FORCING's 'member' variable is managed separately (depending on the type of execution)
            if specific_input == 'prep':
                # PREP files only need a validity *date*
                optional_keys = ['xpid', 'geometry', 'vapp', 'date']
            else:
                # All other files cover a period
                # TODO : members_forcing ne devrait pas être optionnel puisqu'il est utilisé pour définir le nombre
                # de jobs
                # --> gérer ça plus proprement
                optional_keys = ['kind', 'xpid', 'geometry', 'vapp', 'datebegin', 'dateend', 'members']
            for key in optional_keys:
                if not hasattr(self.options, f'{key}_{specific_input}'):
                    if key == 'date':
                        # Default PREP validity *date* is *datebegin* of the simulation
                        setattr(self.options, f'{key}_{specific_input}', args_to_dict['datebegin'])
                    elif key == 'members':
                        # If no *members* attribute is given
                        setattr(self.options, f'{key}_{specific_input}', None)
                    elif key == 'kind':
                        setattr(self.options, f'{key}_{specific_input}', specific_input)
                    else:
                        setattr(self.options, f'{key}_{specific_input}', args_to_dict[key])

        if self.options.extraction_dates is not None:
            # Ensure that this variable will be interprested as a list (FPList) by footprints
            self.options.extraction_dates = f'list({self.options.extraction_dates})'

    def convert_members(self, string):
        """
        Convert a *members* attribute (int-convertible or 'X-Y' string) into a X-Y-1 members 'list'
        """
        if ':' in string:
            first, last = string.split(':')
        else:
            first = 0
            last  = int(string) - 1
        # Return a rangex to ensure that it will be considered as a list in crocO task
        return f'rangex(start:{first} end:{last})'

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
