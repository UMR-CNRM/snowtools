#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from distutils.core import setup
import os

HERE = os.path.dirname(os.path.realpath(__file__))

PYTHON_MIN_VERSION = (3, 6)


# Get the full list of packages available
def get_package_list(basename, basepath):
    r = [basename]
    for elem in os.listdir(basepath):
        if (len(elem) > 2 and elem[0:2] != '__') and \
                os.path.isdir(os.path.join(basepath, elem)) and \
                os.path.isfile(os.path.join(basepath, elem, '__init__.py')):
            r += get_package_list(basename + '.' + elem, os.path.join(basepath, elem))
    return r


def package_discover(package_name):
    return get_package_list(package_name, os.path.join(HERE, package_name))


snowtools_packages = package_discover('snowtools')
bronx_packages = package_discover('bronx')
all_packages = snowtools_packages + bronx_packages
all_packages_data = {elem: ['*'] for elem in all_packages}
all_packages_data['snowtools'] = [
        'tests/test_soda/*',
        'tests/namelists/*',
        'conf/*',
        'DATA/*',
        'DATA/OPER/*',
        'plots/logos/*',
        'tasks/oper/iga/conf/*',
        'plots/stratiprofile/*',
        ]

setup(
        name='Snowtools',
        version='2.0.0',
        description="Snowtools is a recommended python package to pre-process, run and post-process simulations with the SURFEX-Crocus snowpack model.",
        long_description="""
        Snowtools is a series of mostly python scripts that are designed to make our life simpler
        in terms of pre- and post-processing of SURFEX-Crocus snow model.

        Note that this package is only useful for people interested in using numerical codes of
        snowpack modelling on a Linux environment. The package does not include any meteorological
        or snow data. It must be used in association with the SURFEX project
        """,
        maintainer='Snowtools team, leaded by Matthieu Lafaysse, Meteo-France',
        url='https://opensource.umr-cnrm.fr/projects/snowtools_git/wiki/Procedure_for_new_users',
        packages=all_packages,
        package_data=all_packages_data,
        classifiers=[
            'Operating System :: POSIX :: Linux',

            'Programming Language :: Python :: 3.6',
            'Programming Language :: Python :: 3.7',
            'Programming Language :: Python :: 3.8',
            'Programming Language :: Python :: 3.9',
            'Programming Language :: Python :: 3.10',

            'Topic :: Scientific/Engineering',
            ],
        entry_points={
            'console_scripts': [
                's2m = snowtools.tasks.s2m_command:main',
                'proreader = snowtools.plots.stratiprofile.proplotter:main',
                ],
            },
        install_requires=[
             'matplotlib',
             'netCDF4',
             'numpy',
             'scipy',
             'python-dateutil',
             'pyproj',
             'shapely',
             'psycopg2',
             'pyshp',
             'pandas',
             'xarray',
             'cartopy',
            ],
        python_requires='>=' + '.'.join(str(n) for n in PYTHON_MIN_VERSION),
        )
