# -*- coding: utf-8 -*

import os
import argparse
import importlib
import sys
import glob

import vortex
from vortex_cen.tasks.configuration_variables import standard_variables


def parse_command_line():

    parser = argparse.ArgumentParser(description='mkjob command line helper')

    parser.add_argument("-a", "--vapp",
        help="Target application name",
        type=str,
        choices=['edelweiss', 's2m', 'Crocus'],
        # default='Crocus',
    )

    parser.add_argument("-c", "--vconf",
        help="Target configuration name",
        type=str,
        choices=['reanalysis', 'reforecast', 'deterministic', 'escroc', 'assim'],
        # default='deterministic',
    )

    parser.add_argument("-d", "--driver",
        help="Target driver name",
        type=str,
    )  # noqa

    parser.add_argument("-p", "--path",
        help="Absolute path to the target driver",
        type=str,
        required=False,
    )  # noqa

    parser.add_argument("--bytask",
        help="Print configuration information for each individual task of the driver (WARNING : verbose !)",
        action = 'store_true',
    )  # noqa

    parser.add_argument("--verbose",
        help="Print all configuration variables instead of variable 'groups' when possible",
        action = 'store_true',
    )  # noqa

    args = parser.parse_args()

    if args.driver and not args.driver.endswith('.py'):
        args.driver = f'{args.driver}.py'

    return args


def get_module(target):
    module_name = os.path.basename(target).removesuffix('.py')
    spec = importlib.util.spec_from_file_location(module_name, target)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


def get_driver(module):
    # Retrieve the driver
    t = vortex.ticket()
    driver = module.setup(t)
    return driver


def print_driver_help(driver):

    # Print the driver's tree
    print('    Driver tree:')
    print('    ************')
    print('\n'.join([f'    {x}' for x in driver.tree_str().split('\n')]))

    for task in driver.contents:
        print(task.__doc__)


def get_configuration(driver, bytask):

    # Initialisation of the lists of variabes to document
    mandatory_conf_vars = dict()
    optional_conf_vars = dict()

    def update_configuration_variables(conf):
        if key not in known_vars or bytask:
            # Deal with special keys
            if key in standard_variables.keys():
                conf.update({key: standard_variables.get(key)})
                known_vars.append(key)
            elif '|' in key:
                # var1|var2 means that setting either var1 or var2 gives the same result
                newkey = ' or '.join([k for k in key.split('|')])
                known_vars.extend([k for k in key.split('|')])
                value = dict(
                    help = ' | '.join([standard_variables[k]['help'] for k in key.split('|')]),
                )
                conf.update({newkey: value})
            elif ':' in key:
                # var1:var2,var3  means that setting var1 makes var2 and va3 mandatory / relevant
                newkey = key.split(':')[0]
                value = standard_variables[newkey]
                enforce = key.split(':')[1]
                value.update({'enforce': ', '.join(enforce.split(','))})
                conf.update({newkey: value})
                known_vars.append(newkey)
            elif '+' in key:
                # var+type=list  overwrites the "type" of var
                newkey = key.split('+')[0]
                value = standard_variables[newkey]
                for k, v in [x.split('=') for x in key.split('+')[1].split(';')]:
                    value[k] = v
                conf.update({newkey: value})
            else:
                print(f'WARNING : Undocumented configuration variable : {key}')

        return conf

    # Add mandatory and optional configuration variables for each task of the driver
    # Make 2 separate loops to remove mandatory variables from the "optional" category
    known_vars = list()
    for task in driver.contents:
        mandatory_conf_vars[task] = dict()
        if 'MANDATORY_CONFIGURATION_VARIABLES' not in dir(task):
            print(f"WARNING : no 'MANDATORY_CONFIGURATION_VARIABLES' for task {type(task).__name__}, "
                "probably a mistake !")
        else:
            for key in task.MANDATORY_CONFIGURATION_VARIABLES:
                mandatory_conf_vars[task] = update_configuration_variables(mandatory_conf_vars[task])

    for task in driver.contents:
        optional_conf_vars[task] = dict()
        if 'OPTIONAL_CONFIGURATION_VARIABLES' not in dir(task):
            print(f"WARNING : no 'OPTIONAL_CONFIGURATION_VARIABLES' for task {type(task).__name__}, "
                "probably a mistake !")
        else:
            for key in task.OPTIONAL_CONFIGURATION_VARIABLES:
                optional_conf_vars[task] = update_configuration_variables(optional_conf_vars[task])

    return mandatory_conf_vars, optional_conf_vars


def print_configuration_help(mandatory_configuration_variables, optional_configuration_variables,
        bytask, verbose):

    # Create a parser with all documented mandatory and optional configuration variables
    helper = argparse.ArgumentParser(
        formatter_class=argparse.RawTextHelpFormatter,  # Enable multi-line help messages
        usage=argparse.SUPPRESS,  # Remove USAGE from help message
        add_help=False,  # Remove --help optin from help message
    )

    def get_variable_infos(key, value, optional_entries):
        varname = key
        infos = [value['help']]
        for entry in optional_entries:
            if entry in value.keys():
                infos.append(f"{entry} : " + value[entry])
        return varname, infos

    def add_arg(gp, arg, infos):
        desc = '\n'.join(infos) + '\n'
        if arg not in documented_variables:
            gp.add_argument(arg, help=desc)
            documented_variables.append(arg)

    def build_doc(gp, key, value, optional_entries):
        # Build variable documentation
        if 'metavar' in value.keys():
            value.pop('metavar')
            if verbose and not bytask:
                for actualkey in value['values']:
                    actualvalue = standard_variables[actualkey]
                    arg, infos = get_variable_infos(actualkey, actualvalue, optional_entries)
                    add_arg(gp, arg, infos)
            else:
                arg = f'{key.split("_")[0]}_*'
                infos = [value['help'], 'actual variables : ' + ', '.join(value['values'])]
                add_arg(gp, arg, infos)
        elif 'singular' in value.keys():
            # Print singular variables (for testing and debuging) only in verbose mode
            if verbose:
                arg, infos = get_variable_infos(key, value, optional_entries)
                add_arg(gp, arg, infos)
        else:
            arg, infos = get_variable_infos(key, value, optional_entries)
            add_arg(gp, arg, infos)

    if not bytask:
        # Print only mandatory and optional variables for the entire driver
        mandatory = helper.add_argument_group(
            'Mandatory configuration variables\n'
            '---------------------------------\n'
        )
        optional = helper.add_argument_group(
            'Optional configuration variables\n'
            '--------------------------------\n'
        )

    documented_variables = list()  # Avoid duplicates
    for task in mandatory_configuration_variables.keys():
        if bytask:
            # Print configuration variables by task
            mandatory = helper.add_argument_group(
                f'Mandatory configuration variables for task {task}\n'
                '--------------------------------------------------\n'
            )
            optional = helper.add_argument_group(
                f'Optional configuration variables for task {task}\n'
                '-------------------------------------------------\n'
            )
            documented_variables = list()  # Avoid duplicates
        # print_commons()
        # Print all configuration variables at once (default)
        for key, value in mandatory_configuration_variables[task].items():
            build_doc(mandatory, key, value, optional_entries=['format', 'choices', 'type'])

        for key, value in optional_configuration_variables[task].items():
            build_doc(optional, key, value, optional_entries=['format', 'choices', 'default', 'type', 'enforce'])

    # Print the helper parser "help"
    helper.print_help()


def main():
    args = parse_command_line()

    avail_drivers = list()
    if args.path:
        target = args.path
    else:
        if args.vapp:
            if args.vconf:
                if args.driver:
                    target = os.path.join(
                        os.environ['SNOWTOOLS_CEN'],
                        'vortex_cen',
                        args.vapp,
                        args.vconf,
                        'drivers',
                        args.driver,
                    )
                else:
                    target = os.path.join(
                        os.environ['SNOWTOOLS_CEN'],
                        'vortex_cen',
                        args.vapp,
                        args.vconf,
                        '__init__.py',
                    )
                    avail_drivers = glob.glob(os.path.dirname(target) + '/drivers/*.py*')
            else:
                target = os.path.join(
                    os.environ['SNOWTOOLS_CEN'],
                    'vortex_cen',
                    args.vapp,
                    '__init__.py',
                )
        else:
            print('====================================')
            print('Overview of available configurations')
            print('====================================')
            for app in ['edelweiss', 's2m', 'Crocus']:
                target = os.path.join(
                    os.environ['SNOWTOOLS_CEN'],
                    'vortex_cen',
                    app,
                    '__init__.py',
                )
                if os.path.isfile(target):
                    module = get_module(target)
                    # Print the module's doc
                    print(module.__doc__)
                else:
                    raise FileNotFoundError(target)

    if os.path.isfile(target):
        module = get_module(target)
        # Print the module's doc
        if module.__doc__ is None:
            print(module)
        else:
            print(module.__doc__)
        if os.path.basename(target) != '__init__.py':
            # target should be a driver
            driver = get_driver(module)
            print_driver_help(driver)
            mandatory, optional = get_configuration(driver, args.bytask)
            print_configuration_help(mandatory, optional, args.bytask, args.verbose)
        else:
            # target is an __init__.py file
            # In case only -a and -c options have been provided (targeting a specific configuration), list
            # available drivers for this configuration
            if len(avail_drivers) > 0:
                print('Available drivers for this configuration (argument "-d") :\n')
                for driver in avail_drivers:
                    basename = os.path.basename(driver).removesuffix('.py')
                    if basename != '__init__':
                        module = get_module(driver)
                        print(f'  *   {basename} : ' + module.__doc__)
    else:
        raise FileNotFoundError(target)


if __name__ == "__main__":
    main()
