#! /usr/bin/python
# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse
'''

# General python modules
import os

try:
    from utils.resources import check_snowtools_install
    from utils.resources import InstallException
    print (os.environ["SNOWTOOLS_CEN"])
    check_snowtools_install()
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    print('Snowtools installation has been successfully checked.')
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
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
                                          '-r': 'region', '--oper': 'oper'})
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
