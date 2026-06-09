# -*- coding: utf-8 -*

import os
import argparse
import importlib
import sys

import vortex
from vortex_cen.tasks.configuration_variables import standard_variables


def parse_command_line():

    parser = argparse.ArgumentParser(description='mkjob command line helper')

    parser.add_argument("-a", "--vapp",
        help="Target application name",
        type=str,
        choices=['edelweiss', 's2m', 'Crocus'],
        default='Crocus',
    )

    parser.add_argument("-c", "--vconf",
        help="Target configuration name",
        type=str,
        choices=['reanalysis', 'reforecast', 'deterministic', 'escroc', 'assim'],
        default='deterministic',
    )

    parser.add_argument("-d", "--driver",
        help="Target driver name",
        type=str,
        default='surfex',)

    parser.add_argument("-p", "--path",
        help="Absolute path to the target driver",
        type=str,
        required=False,)

    args = parser.parse_args()

    return args


def get_driver(args):

    if args.path:
        target_driver = args.path
    else:
        target_driver = os.path.join(
            os.environ['SNOWTOOLS_CEN'], 'vortex_cen', args.vapp, args.vconf, 'drivers', args.driver + '.py'
        )

    if not os.path.isfile(target_driver):
        raise FileNotFoundError(target_driver)

    # Get the mandatory and optional configuration variables for this specific driver
    module_name = os.path.basename(target_driver).rstrip('.py')
    spec = importlib.util.spec_from_file_location(module_name, target_driver)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    # Print the module's doc
    print(module.__doc__)
    # Retrieve the driver
    t = vortex.ticket()
    driver = module.setup(t)
    # Print the driver's tree
    driver.tree_str

    return driver


def print_driver_help(driver):

    for task in driver.contents:
        print(task.__doc__)


def print_configuration_help(driver):

    # Initialisation of the lists of variabes to document
    mandatory_configuration_variables = {key: standard_variables.get(key) for key in
        ['datebegin', 'dateend', 'geometry', 'xpid']}
    optional_configuration_variables = dict()

    # Add mandatory and optional configuration variables for each task of the driver
    for task in driver.contents:
        if 'MANDATORY_CONFIGURATION_VARIABLES' not in dir(task):
            print(f"WARNING : no 'MANDATORY_CONFIGURATION_VARIABLES' for task {type(task).__name__}, "
                "probably a mistake !")
        else:
            mandatory_configuration_variables.update(
                {key: standard_variables.get(key) for key in task.MANDATORY_CONFIGURATION_VARIABLES}
            )

        if 'OPTIONAL_CONFIGURATION_VARIABLES' not in dir(task):
            print(f"WARNING : no 'OPTIONAL_CONFIGURATION_VARIABLES' for task {type(task).__name__}, "
                "probably a mistake !")
        else:
            optional_configuration_variables.update(
                {key: standard_variables.get(key) for key in task.OPTIONAL_CONFIGURATION_VARIABLES}
            )

    # Create a parser with all documented mandatory and optional configuration variables
    helper = argparse.ArgumentParser(
        formatter_class=argparse.RawTextHelpFormatter,  # Enable multi-line help messages
        usage=argparse.SUPPRESS,  # Remove USAGE from help message
        add_help=False,  # Remove --help optin from help message
    )
    mandatory = helper.add_argument_group(
        'Mandatory configuration variables\n'
        '---------------------------------\n'
    )
    for key, value in mandatory_configuration_variables.items():
        infos = [value['help'], "type : " + value['type']]
        for entry in ['format', 'choices']:
            if entry in value.keys():
                infos.append(f"{entry} : " + value[entry])
        mandatory.add_argument(key, help='\n'.join(infos) + '\n')

    optional = helper.add_argument_group(
        'Optional configuration variables\n'
        '--------------------------------\n'
    )
    for key, value in optional_configuration_variables.items():
        infos = [value['help'], "type : " + value['type']]
        for entry in ['format', 'alias', 'choices', 'default']:
            if entry in value.keys():
                infos.append(f"{entry} : " + value[entry])
        optional.add_argument(key, help='\n'.join(infos) + '\n')

    # Print the helper parser "help"
    helper.print_help()


def main():
    args = parse_command_line()
    driver = get_driver(args)
    print_driver_help(driver)
    print_configuration_help(driver)


if __name__ == "__main__":
    main()
