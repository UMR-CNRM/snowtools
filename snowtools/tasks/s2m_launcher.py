# -*- coding: utf-8 -*-

"""
Created on 30 Aug. 2017

:Authors:
    M. Lafaysse

Importing this module will check the snowtools installation and make
``_S2M_command`` abstract class available. All s2m command executions
inherit from this class.
"""

# General python modules
import six

if six.PY2:
    print('*************************************************************')
    print('Depreciation warning: You run s2m with python 2.')
    print('Support for python 2.7 is not guaranteed after December 2021.')
    print('Consider switching to python3. If you use the s2m alias, it')
    print('may be necessary to modify its definition in your ~/.bashrc')
    print('*************************************************************')

try:
    from snowtools.utils.resources import check_snowtools_install
    from snowtools.utils.resources import InstallException
    from snowtools.DATA import SNOWTOOLS_DIR
    print("SNOWTOOLS Directory", SNOWTOOLS_DIR)
    check_snowtools_install()
    print('Snowtools installation has been successfully checked.')
except ImportError or InstallException:
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    print('Incorrect snowtools installation. Check the documentation.')
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    raise

# Import snowtools modules


class _S2M_command(object):
    """Abstract class for common S2M launching commands"""

    def __init__(self, args):
        self.mandatory_arguments = dict()
        # Read the options provided by the user
        self.options = self.parse_options(args)
        self.check_mandatory_arguments(**{'--surfex': 'surfex', '--safran': 'safran',
                                          '--oper': 'oper'})
        self.execute()

    def parse_options(self, arguments):
        """Mandatory method to define for each application"""
        pass

    def execute(self):
        pass

    def usage(self):
        pass

    def check_and_convert_options(self, vortex=False):
        pass

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
            self.exit_usage()
