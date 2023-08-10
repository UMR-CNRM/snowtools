# -*- coding: utf-8 -*-

"""
This module is designed to provide tools to parse snowpack profiles as produced by the SNOWPACK snow cover model.
See the `snowpack website <https://snowpack.slf.ch/>`_ for more details on the production and structure of such files.

NB : For easier readability and maintainability of the code, this implementation requires a strict
form of the data :

 * A blank line must separate two sections.
 * Comments are allowed only if the first non-space character of the line is ``#``
 * Inline comments are not supported.
 * DATA is the last block that is parsed

Authored by Léo Viallon Galinier from previous work of Neige Calonne and WSL/SLF.

This script requires python>=3.8.
"""

import re
import datetime

import numpy as np

VARIABLES = {'0500': 'time',    # compulsory
             '0501': 'height',  # compulsory
             '0502': 'density',
             '0503': 'temperature',
             '0504': 'lwc',
             '0508': 'dendricity',
             '0509': 'sphericity',
             '0510': 'coordination_number',
             '0511': 'bond_size',
             '0512': 'grain_size',
             '0513': 'grain_type',
             '0514': 'sh_at_surface',
             '0515': 'ice_content',
             '0516': 'air_content',
             '0517': 'stress',
             '0518': 'viscosity',
             '0520': 'temperature_gradient',
             '0521': 'thermal_conductivity',
             '0523': 'viscous_deformation_rate',
             '0530': 'stab_indices',
             '0531': 'stab_deformation_rate',
             '0532': 'sn38',
             '0533': 'sk38',
             '0534': 'hand_hardness',
             '0535': 'opt_equ_grain_size',
             '0601': 'shear_strength',
             '0603': 'hardness_difference',
             '0604': 'ssi',
             '0606': 'crit_cut_length',
             '1501': 'height_nodes',
             '1532': 'sn38_nodes',
             '1533': 'sk38_nodes',
             '0540': 'date_of_birth',
             }

SECTION_NAMES = {'metadata_point': 'STATION_PARAMETERS',
                 'metadata_variables': 'HEADER',
                 'data': 'DATA'
                 }
SEP = ','  # The data separator used


def _parse_metadata_point(ff):
    """
    Parse the section STATION_PARAMETERS
    The section should finish with a blank line

    :param ff: opened file
    :return: Parsed data
    :rtype: dict
    """
    data = {}

    while l := ff.readline():
        l = l.strip()
        if l.startswith('#'):
            continue
        if len(l) < 3:
            break
        if '=' in l:
            # splitting and identifying parts
            sp = l.split(sep='=', maxsplit=1)
            key = sp[0].strip()
            value = sp[1].strip()

            # Check and convert if we have numbers
            try:
                value = int(value)
            except ValueError:
                try:
                    value = float(value)
                except ValueError:
                    pass

            # Store the parsed values
            data[key] = value
        else:
            break

    return data


def _parse_header(ff, variables=VARIABLES):
    """
    Parse the section HEADER
    The section should finish with a blank line

    :param ff: opened file
    :return: Parsed data
    :rtype: dict
    """
    data = {}

    while l := ff.readline():
        ls = l.strip()
        if ls.startswith('#'):
            continue
        if len(ls) < 3:
            break
        if l.startswith('     '):
            # Here, we get rid of continuated lines
            continue
        if SEP in ls:
            sp = ls.split(sep=SEP, maxsplit=3)
            code = sp[0]
            if code not in variables:
                continue
            key = variables[code]
            value = sp[-1]

            # length = 1
            # if len(sp) == 3:
            #     length = sp[1]
            #     try:
            #         length = int(sp)
            #     except ValueError:
            #         length = None

            data[key] = {'code': code,
                         'long_name': value,
                         # 'length': length,
                         }

    return data


def _parse_data(ff, variables=VARIABLES, soil=False):
    """
    Parse the section DATA

    :param ff: opened file
    :return: Parsed data
    :rtype: dict
    """
    data = []

    # Variables used in each time step to help the parsing
    tmp_data = {}
    i_limit_soil_snow = None
    n_soil_and_snow = None
    n_snow = None

    # Iteratively parse lines
    while l := ff.readline():
        l = l.strip()
        if l.startswith('#'):
            continue
        if len(l) < 3:
            break

        # Manage time dimension
        if l.startswith('0500'):
            # time
            if len(tmp_data) > 0:
                data.append(tmp_data)
            sp = l.split(sep=SEP, maxsplit=1)
            time = datetime.datetime.strptime(sp[1], '%d.%m.%Y %H:%M:%S')
            tmp_data = {'time': time}

        # Manage height dimension
        elif l.startswith('0501'):
            sp = l.split(sep=SEP, maxsplit=-1)
            code = sp[0]
            n = int(sp[1])
            n_soil_and_snow = n - 1
            value = np.array(sp[2:], dtype=float)
            i_limit_soil_snow = np.where((value == 0))[0].item()
            n_snow = n_soil_and_snow - i_limit_soil_snow
            tmp_data['height'] = value[i_limit_soil_snow + 1:]
            if soil:
                tmp_data['height_soil'] = value[:i_limit_soil_snow]

        # Other variables
        else:
            m = re.match('([0-9]*),', l)
            if m is None:
                continue
            code = m.group(1)
            if code not in variables:
                continue

            key = variables[code]
            sp = l.split(sep=SEP, maxsplit=-1)
            n = int(sp[1])
            try:
                value = np.array(sp[2:], dtype=float)
            except ValueError:
                value = np.array(sp[2:])

            if n == n_soil_and_snow:
                tmp_data[key] = value[i_limit_soil_snow:]
                if soil:
                    tmp_data[key + '_soil'] = value[:i_limit_soil_snow]
            else:
                tmp_data[key] = value[:]

    return data


def read_snowpack_profile(filepath, variables: dict = VARIABLES, soil: bool = False) -> dict:
    """
    Parse the snowpack output file '.pro' and return the raw data

    :param filepath: Path to the snowpack output PRO file (containing the profile)
    :type filepath: str or path-like object
    :param variables: The variables to be parsed (if present): dictionnary associating the SNOWPACK code
                      (on the form of a string) to the name to give to the variable (str).
    :type variables: dict
    :param soil: Wether or not to read soil information. If set to True, additional variables (suffixed by
                 ``_soil`` will be created to store soil information
    :type soil: bool
    :rtype: dict
    """

    # Prepare output objects
    metadata_point = {}
    metadata_variables = {}
    data = {}

    with open(filepath, 'r') as ff:

        # Parse the different metadata available (information on considered
        # simulation point and header of variables).
        while l:= ff.readline():
            if len(l) == 0:
                continue
            elif l.startswith('#'):
                continue
            elif l.startswith('[{}]'.format(SECTION_NAMES['metadata_point'])):
                metadata_point = _parse_metadata_point(ff)
                continue
            elif l.startswith('[{}]'.format(SECTION_NAMES['metadata_variables'])):
                metadata_variables = _parse_header(ff, variables=variables)
                continue
            elif l.startswith('[{}]'.format(SECTION_NAMES['data'])):
                data = _parse_data(ff, variables=variables)
                break

    return {'data': data,
            'metadata_point': metadata_point,
            'metadata_variables': metadata_variables,
            }


class snowpack_reader():
    """
    The object that provide access to SNOWPACK profiles simulation output
    through an interface that is similar to the one provided by prosimu
    for the Crocus simulations.
    """

    def __init__(self, filepath, variables=VARIABLES, soil=False):
        """
        Parse the snowpack output file '.pro' and store/provide access to the parsed data.

        :param filepath: Path to the snowpack output PRO file (containing the profile)
        :type filepath: str or path-like object
        :param variables: The variables to be parsed (if present): dictionnary associating the SNOWPACK code
                          (on the form of a string) to the name to give to the variable (str). Allow to
                          reduce the amount of data parsed and stored and to parse additional variables
                          that would not be parsed by default.
        :type variables: dict
        :param soil: Wether or not to read soil information. If set to True, additional variables (suffixed by
                     ``_soil`` will be created to store soil information
        :type soil: bool
        :rtype: dict
        """
        import pandas as pd

        # Actually read the file
        data = read_snowpack_profile(filepath, variables, soil=soil)

        # Store the metadata
        self.metadata_point = data['metadata_point']
        self.metadata_variables = data['metadata_variables']

        # Post-process data
        # Dimensions = (time[, snow_layer or other dimension])
        raw_data = data['data']
        self.time = np.array([v['time'] for v in raw_data])
        self.data = {}
        for code, key in variables.items():
            value = pd.DataFrame([v[key] if key in v else [] for v in raw_data]).values
            if value.size > 0:
                self.data[key] = np.squeeze(value)
        if soil:
            for code, key in variables.items():
                key = key  + '_soil'
                pd.DataFrame([v[key] if key in v else [] for v in raw_data]).values
            if value.size > 0:
                self.data[key] = value

    def get_time(self):
        """
        Return the time dimension of the dataset on a similar form than provided by
        prosimu for Crocus model (numpy array of datetime.datetime objects).

        NB : If you are interested by the same variable on the form of a numpy
        array of integers or np.datetime64 values, use ``read_var('time')``.

        :returns: time array
        :rtype: numpy array (datetime.datetime objects)
        """
        return self.time

    def listvar(self):
        """
        Return the list of available variables

        :returns: List of available variables
        :rtype: list
        """
        return self.data.keys()

    def read_var(self, variable, time=None):
        """
        Get the data for a specified variable. It is also possible to select a relevant time step.

        :param variable: The variable name
        :type variable: str
        :param time: A datetime object selecting the time step to get. If None (default),
                     returns the values for the whole season.
        :type time: datetime.datetime
        """
        # Check variable exist
        if variable not in self.data.keys():
            raise ValueError('variable {} not found'.format(variable))

        # Time selection (if needed)
        if time is not None:
            select = self.time == time
            if select.sum() < 1:
                raise ValueError('time provided not found in the dataset')
        else:
            select = slice(None, None, None)

        return self.data[variable][select, ...]
