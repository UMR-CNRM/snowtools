# -*- coding: utf-8 -*-
"""
Created on 6 févr. 2019

@author: cluzetb

utils suited for crocO interface only
"""

import datetime

import netCDF4
import numpy as np
from mpl_toolkits.axes_grid1 import make_axes_locatable

from vortex.util.config import GenericConfigParser
from vortex.layout.nodes import ConfigSet


def dictsAspect():
    """
    returns the dict for aspect and its reverse.
    """

    gg1 = {'N': 0, 'NE': 45, 'E': 90, 'SE': 135, 'S': 180, 'SW': 225, 'W': 270, 'NW': 315, 'flat': -1}

    return gg1, {v: k for k, v in gg1.items()}


def setSubsetclasses(pgd, selE, selA, selS):
    """
    BC 5/02/19
    duplicate from SodaXP.setSubset classes with some midfs
    mask is True when the class in whithin the subset
    """
    subsetClass = []
    dictAsp, revdictAsp = dictsAspect()
    dictAsp['all'] = np.unique(pgd.aspect)
    dictElev = {'all': np.unique(pgd.elev)}
    if 'all' not in selE:
        classesE = map(int, selE)
    else:
        classesE = dictElev['all']

    classesA = []
    if 'all' not in selA:
        for cl, asp in enumerate(selA):
            classesA.append(dictAsp[asp])
    else:  # avoid having a list of list
        classesA = dictAsp['all']
        
    classesS = selS
    # print(classesE, classesA, classesS)
    if type(classesS) is 'str':
        classesS = [classesS]
    mask = []
    for cl in range(pgd.npts):
        if pgd.elev[cl] in classesE and (
                (str(int(np.arctan(pgd.slope[cl]) * 180. / np.pi)) in classesS and pgd.aspect[cl] in classesA) or
                (pgd.slope[cl] < 0.01 and '0' in classesS)):

            subsetClass.append(cl)
            mask.append(True)
        else:
            mask.append(False)
    
    return subsetClass, np.array(mask)


def dictvarsPrep():
    return {'b1': 'SPM_VEG1', 'b2': 'SPM_VEG2', 'b3': 'SPM_VEG3',
            'b4': 'SPM_VEG4', 'b5': 'SPM_VEG5', 'b6': 'SPM_VEG6',
            'b7': 'SPM_VEG7',
            'r53': 'r53', 'r52': 'r52', 'r51': 'r51', 'r54': 'r54', 'r21': 'r21', 'r23': 'r23', 'r24': 'r24',
            'DEP': 'DEP_TOT'}


def dictvarsWrite():
    return {'b1': 'B1', 'b2': 'B2', 'b3': 'B3',
            'b4': 'B4', 'b5': 'B5', 'b6': 'B6',
            'b7': 'B7',
            'r53': 'r53', 'r52': 'r52', 'r51': 'r51', 'r54': 'r54', 'r21': 'r21', 'r23': 'r23', 'r24': 'r24',
            'DEP': 'DEP'}


def niceName(pgd, cl, tolist=False):
    _, revdictAsp = dictsAspect()
    print(type(revdictAsp[int(pgd.aspect[cl])]))
    return str(int(pgd.elev[cl])) + '_' + revdictAsp[pgd.aspect[cl]] + '_' + str(int(np.arctan(pgd.slope[cl]) * 180.
                                                                                     / np.pi))


class Pgd(object):
    """
    class to read a semi-distributed PGD file
    slope is the TANGENT of the angle of slope.
    """
    def __init__(self, pathPGD):

        pgd = netCDF4.Dataset(pathPGD)
        self.elev = np.squeeze(pgd.variables['MIN_ZS'][:])  # lower altitude
        self.slope = np.squeeze(pgd.variables['SSO_SLOPE'][:])
        self.aspect = np.squeeze(pgd.variables['SSO_DIR'][:])
        self.npts = len(self.elev)
        self.lat = np.squeeze(pgd.variables['XY'][:])
        self.lon = np.squeeze(pgd.variables['XX'][:])
        #self.lat = 45.11517 * np.ones(self.npts)
        #self.lon = 6.2186 * np.ones(self.npts)
        pgd.close()
        # print 'pgd loaded'


def convertdate(date):
    """
    YYYYMMDDHH to datetime.datetime
    """
    return datetime.datetime(int(date[0:4]), int(date[4:6]), int(date[6:8]), int(date[8:10]), 0, 0)


def colorbar(mappable):
    """
    from http://joseph-long.com/writing/colorbars/
    """
    ax = mappable.axes
    fig = ax.figure
    divider = make_axes_locatable(ax)
    cax = divider.append_axes("right", size="5%", pad=0.05)
    return fig.colorbar(mappable, cax=cax)


def setlistvars_obs(arg):
    """
    BC 6/02/19
    convert a crocO argument options.vars into a list of OBS variables names in soda format
    """
    gg = dictvarsWrite()
    if arg == 'all':
        listvar = ['B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'DEP']
    else:
        listvar = []
        for var in arg:
            listvar.append(gg[var])  # 'b*' -> 'B*'
    return listvar


def setlistvars_var(arg):
    """
    BC 6/02/19
    convert a crocO argument options.vars into a list of VAR variables names in soda format
    TODO : same stuff for DEP/ SCF etc.
    """
    if arg == 'all':
        listvar = ['PB1', 'PB2', 'PB3', 'PB4', 'PB5', 'PB6', 'PB7', 'DEP']
    else:
        listvar = []
        for var in arg:
            if var == 'DEP':
                listvar.append('DEP')
            else:
                listvar.append('P' + var.upper())  # 'b*' -> 'PB*'
    return listvar


def read_conf(pathconf):

    """
    B. Cluzet
    duplicated from evalSODA.util
    """
    print(pathconf)
    """
    Config = ConfigParser.ConfigParser()

    if os.path.exists(pathconf):
        Config.read(pathconf)
    else:
        raise FileNameException(pathconf)
    Config.sections()
    for item in Config.sections():
        print 'gg'
        print(item)
        print(Config.iniconf.items(item))
    """
    iniparser = GenericConfigParser(pathconf)
    thisconf  = iniparser.as_dict(merged=False)
    # print(thisconf)
    updconf = thisconf.get('defaults', dict())
    conf = ConfigSet()
    conf.update(updconf)

    return conf


def set_errors(argsoda):
    """
    BC 06/02/19
    set soda canonical errors for the prescribed vars (Wright et al., Charrois et al.)
    """
    
    dicterrors = {'B1': 0.00071, 'B2': 0.00046, 'B3': 0.00056,'B4': 0.00056, 'B5': 0.002, 'B6': 0.0015, 'B7': 0.00078,
                  'SCF': 0.2, 'DEP': 0.1}
    ret = []
    for el in argsoda:
        ret.append(dicterrors[el])
    
    return ret


def set_factors(argsoda, fact):
    """
    BC 06/02/19
    properly set the error factors for namelist Writing
    fact is prescribed in namelist :
    - if only one value (default or lazy case), apply it to all variables
    - if list of values (exhaustive): ok
    """
    if len(argsoda) == len([fact]):  # default with 1 var, lazy with 1 var, exhaustive
        if len([fact]) == 1:
            return [fact]
        else:
            return fact
    else:
        if len([fact]) == 1:  # lazy/default
            return [fact] * len(argsoda)
        else:
            raise Exception('you should either apply the same fact to all vars or specify it for each one')

