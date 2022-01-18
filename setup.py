#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from distutils.core import setup

setup(
        name='Snowtools',
        version='2.0.0',
        description="Snowtools is a recommended python package to pre-process, run and post-process simulations with the SURFEX-Crocus snowpack model",
        long_description="""
        Snowtools is a series of mostly python scripts that are designed to make our life simpler
        in terms of pre- and post-processing of SURFEX-Crocus snow model.

        Note that this package is only useful for people interested in using numerical codes of
        snowpack modelling on a Linux environment. The package does not include any meteorological
        or snow data. It must be used in association with the SURFEX project
        """,
        maintainer='Snowtools team, leaded by Matthieu Lafaysse',
        url='https://opensource.umr-cnrm.fr/projects/snowtools_git/wiki/Procedure_for_new_users',
        package_dir={
            'snowtools': 'snowtools',
            'bronx': 'bronx',
            },
        # package_data={
        #     'snowtools.DATA': 'snowtools/DATA/*',
        #     },
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
                'proreader = snowtools.plots.GUI_Proreader:main',
                ],
            },
        install_requires=[
             'matplotlib',
             'netCDF4',
             'numpy',
             'scipy',
             'six',
             'python-dateutil',
             'pyproj',
             'shapely',
             'psycopg2',
             'pyshp',
             'pandas',
             'xarray',
             'cartopy',
            ],
        )
