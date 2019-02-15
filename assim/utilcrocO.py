#! /usr/bin/python
# -*- coding: utf-8 -*-
'''
Created on 6 févr. 2019

@author: cluzetb

utils suited for crocO interface only
'''
from Code.Dev.evalSODA.util import dictsAspect
import numpy as np
from vortex.util.config import GenericConfigParser

def setSubsetclasses(pgd, selE, selA, selS):
    """
    BC 5/02/19
    duplicate from SodaXP.setSubset classes with some midfs
    mask is True when the class in whithin the subset
    """
    subsetClass = []
    dictAsp, revdictAsp = dictsAspect()
    dictAsp['all'] = np.unique(pgd.aspectClass)
    dictElev = {'all': np.unique(pgd.elevClass)}
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
        if pgd.elevClass[cl] in classesE and (
                (str(int(np.arctan(pgd.slopeClass[cl]) * 180. / np.pi)) in classesS and pgd.aspectClass[cl] in classesA) or
                (pgd.slopeClass[cl] < 0.01 and '0' in classesS)):

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
            'sd':'DEP_TOT'}
    
def dictvarsWrite():
    return {'b1': 'B1', 'b2': 'B2', 'b3': 'B3',
            'b4': 'B4', 'b5': 'B5', 'b6': 'B6',
            'b7': 'B7',
            'r53': 'r53', 'r52': 'r52', 'r51': 'r51', 'r54': 'r54', 'r21': 'r21', 'r23': 'r23', 'r24': 'r24',
            'sd':'DEP_TOT'}


def setlistvars_obs(arg):
    """
    BC 6/02/19
    convert a crocO argument options.vars into a list of OBS variables names in soda format
    """
    if arg == 'all':
        listvar = ['B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', ]
    else:
        listvar = []
        for var in arg:
            listvar.append(var.upper())  # 'b*' -> 'B*'
    return listvar

def setlistvars_var(arg):
    """
    BC 6/02/19
    convert a crocO argument options.vars into a list of VAR variables names in soda format
    TODO : same stuff for SD/ SCF etc.
    """
    if arg == 'all':
        listvar = ['PB1', 'PB2', 'PB3', 'PB4', 'PB5', 'PB6', 'PB7', ]
    else:
        listvar = []
        for var in arg:
            listvar.append('P' + var.upper())  # 'b*' -> 'PB*'
    return listvar
    
def read_conf(pathconf):
    
    '''
    B. Cluzet
    duplicated from evalSODA.util
    '''
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
    '''
    BC 06/02/19
    set soda canonical errors for the prescribed vars (Wright et al., Charrois et al.)
    '''
    
    dicterrors = {'B1': 0.00071, 'B2': 0.00046, 'B3': 0.00056, 'B4': 0.00056, 'B5': 0.002, 'B6': 0.0015, 'B7': 0.00078, 'SCF': 0.2,}
    ret = []
    for el in argsoda:
        ret.append(dicterrors[el])
    
    return ret

def set_factors(argsoda, fact):
    '''
    BC 06/02/19
    properly set the error factors for namelist Writing
    fact is prescribed in namelist : 
    - if only one value (default or lazy case), apply it to all variables
    - if list of values (exhaustive): ok
    '''
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
    