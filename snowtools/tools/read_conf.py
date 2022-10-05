'''
Created on 11 juin 2020
@author: cluzetb
read_conf method comes from the CrocO_toolbox but is necessary inside croco_vortex_kitchen, which must run without CrocO_toolbox. So I copy-pasted the utilitaries here.
'''
import datetime
import os
import re
import shutil


class ConfObj1L(dict):
    def __init__(self, **kwargs):
        self.__dict__.update(self, **kwargs)
        for name, val in kwargs.items():
            super().__setitem__(name, val)

    def __getattr__(self, name):
        if name in self:
            return self[name]
        else:
            raise AttributeError("No such attribute: " + name)

    def __setattr__(self, name, val):
        self[name] = val
        super().__setitem__(name, val)  # useful for object description
        self.__dict__[name] = val  # useful for iteration over self.__dict__

    def __str__(self):
        return '<ConfObj1L(' + ', '.join(['{0}:{1}'.format(key, val) for key, val in self.__dict__.items()]) + ')>'

    def __repr__(self):
        return '<ConfObj1L(' + ', '.join(['{0}:{1}'.format(key, val) for key, val in self.__dict__.items()]) + ')>'


def read_conf(pathconf, useVortex=True):
    '''
    B. Cluzet
    duplicated from evalSODA.util
    '''
    if not os.path.exists(pathconf):
        if os.path.exists(pathconf[0:-4] + '.foo'):
            shutil.copyfile(pathconf[0:-4] + '.foo', pathconf)
        else:
            raise FileNotFoundError('no conf file at this path :', pathconf)

    def open_conf_no_vtx(pathconf):
        try:
            from configparser import ConfigParser
        except ImportError:
            print('please install configparser or vortex in order to parse the conf file.')
        conf = ConfigParser()
        conf.read(pathconf)
        conf = conf2obj(conf)
        return conf
    if useVortex is True:
        try:
            from vortex.layout.nodes import ConfigSet
            from vortex.util.config import GenericConfigParser
            iniparser = GenericConfigParser(pathconf)
            thisconf  = iniparser.as_dict(merged=False)
            updconf = thisconf.get('defaults', dict())
            conf = ConfigSet()
            conf.update(updconf)
        except ImportError:
            print("you asked vortex to parse conf file but vortex is not installed")
            print("since it is not installed, we use the alternative config parser")
            print("this alternative config parser may not appropriately parse complex vorte types")
            conf = open_conf_no_vtx(pathconf)
    else:
        conf = open_conf_no_vtx(pathconf)
    return conf


def unpack_conf(arg):
    """
    BC 01/04/20
    supported types:
    arg = aa,bb,cc : return ['aa','bb','cc']
    arg = 'aa','bb','cc' : return ['aa','bb','cc']
    arg = "'toto'" or '"toto"' : return 'toto'
    arg = '2' : return 2
    arg = '2.3' : return 2.3
    arg = 'toto.nam' :'return toto.nam'
    arg = '2.2,2.3' : return [2.2,2.3]
    arg = None : return None (NoneType)

    /!| tricky case:
    arg='2016080106' (YYYYmmddHH format) : return '2016080106' (and not 2016080106)
    arg=rangex(start: a end: b) : return list(range(a,b+1)) (a vortex type...)
    arg=rangex(start:a end:b): return list(range(a,b+1))
    exception cases :
    - badly formatted str
    - mixed types in lists
    """
    # first deal with the vortex case
    if "rangex" in arg:
        try:
            start = int(re.search(r"start: (\d+)", arg).group(1))
        except AttributeError:
            start = int(re.search(r"start:(\d+)", arg).group(1))
        try:
            end = int(re.search(r"end: (\d+)", arg).group(1))
        except AttributeError:
            end = int(re.search(r"end:(\d+)", arg).group(1))
        ret = list(range(start, end + 1))
    elif arg == 'None':
        ret = None
    else:
        if "," in arg:
            ret = list(map(unpack_conf, arg.split(',')))
        # unpack strings:
        elif arg.startswith("'") or arg.startswith('"'):
            if arg[0] == arg[-1]:
                ret = arg[1:-1]
            else:
                raise ValueError(arg + ': badly formatted string.')
        elif '.' in arg:
            try:
                ret = float(arg)
            except ValueError:  # must be a path ^^
                ret = arg
        else:
            try:
                _ = datetime.datetime.strptime(arg, '%Y%m%d%H')
                ret = arg
            except ValueError:
                try:
                    ret = int(arg)
                except ValueError:
                    ret = arg
    if isinstance(ret, list):
        if all(isinstance(r, type(ret[0])) for r in ret):
            return ret
        else:
            raise TypeError('unconsistent types in list: {0}'.format(ret))
    else:
        return ret


def conf2obj(conf):
    '''
    BC 01/04/20
    convert a ConfParser to a one-level dict then a one level dot.dict.
    '''

    dict1 = dict()
    default_sec = conf.default_section
    for k in conf[default_sec]:
        dict1[k] = unpack_conf(conf[default_sec][k])
    for k in conf._sections:
        for kk in conf[k]:
            dict1[kk] = unpack_conf(conf[k][kk])
    return ConfObj1L(**dict1)
