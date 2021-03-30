#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 4 oct. 2012

@author: lafaysse
'''


class DirNameException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Unknown directory : " + self.path

    def __reduce__(self):
        red = list(super(DirNameException, self).__reduce__())
        red[1] = tuple([self.path])  # Les arguments qui seront passes a __init__
        return tuple(red)


class DirFileException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "The following path refers to a file instead of a directory: " + self.path

    def __reduce__(self):
        red = list(super(DirFileException, self).__reduce__())
        red[1] = tuple([self.path])  # Les arguments qui seront passes a __init__
        return tuple(red)


class FileNameException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Unknown file : " + self.path

    def __reduce__(self):
        red = list(super(FileNameException, self).__reduce__())
        red[1] = tuple([self.path])  # Les arguments qui seront passes a __init__
        return tuple(red)


class FileOpenException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Can not open the file : " + self.path

    def __reduce__(self):
        red = list(super(FileOpenException, self).__reduce__())
        red[1] = tuple([self.path])  # Les arguments qui seront passes a __init__
        return tuple(red)


class FileParseException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Impossible to parse the xml file: " + self.path

    def __reduce__(self):
        red = list(super(FileParseException, self).__reduce__())
        red[1] = tuple([self.path])  # Les arguments qui seront passes a __init__
        return tuple(red)


class VarNameException(Exception):

    def __init__(self, varname, path):
        self.varname = varname
        self.path = path

    def __str__(self):
        return "Variable inexistante : " + self.varname + " dans le fichier : " + self.path

    def __reduce__(self):
        red = list(super(VarNameException, self).__reduce__())
        red[1] = (self.varname, self.path)  # Les arguments qui seront passes a __init__
        return tuple(red)


class TimeException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Temps incohérent dans le fichier : " + self.path

    def __reduce__(self):
        red = list(super(TimeException, self).__reduce__())
        red[1] = tuple([self.path])  # Les arguments qui seront passes a __init__
        return tuple(red)


class TimeListException(Exception):

    def __init__(self, path, dimtime):
        self.path = path
        self.dimtime = dimtime

    def __str__(self):

        info = ""
        for p, path in enumerate(self.path):
            info += path + ":" + str(self.dimtime[p]) + " dates\n"

        return "Times do not have consistent lengths in the different files to merge :\n" + info

    def __reduce__(self):
        red = list(super(TimeListException, self).__reduce__())
        red[1] = (self.path, self.dimtime)  # Les arguments qui seront passes a __init__
        return tuple(red)


class VarWriteException(Exception):

    def __init__(self, varname, varshape, varfileshape):
        self.varname = varname
        self.varshape = varshape
        self.varfileshape = varfileshape

    def __str__(self):
        return "Impossible to write the variable: " + self.varname + "\n" + \
            "Shape of the variable in the code: " + str(self.varshape) + "\n" + \
            "Shape of the variable in the file: " + str(self.varfileshape)

    def __reduce__(self):
        red = list(super(VarWriteException, self).__reduce__())
        red[1] = (self.varname, self.varshape, self.varfileshape)  # Les arguments qui seront passes a __init__
        return tuple(red)


class GeometryException(Exception):
    def __init__(self, altmin, altmax):
        self.altmin = altmin
        self.altmax = altmax

    def __str__(self):
        return "The provided forcing file does not contain any point corresponding to your requirements." + \
               "\n Elevation range in the file: " + str(self.altmin) + " - " + str(self.altmax)

    def __reduce__(self):
        red = list(super(GeometryException, self).__reduce__())
        red[1] = (self.altmin, self.altmax)  # Les arguments qui seront passes a __init__
        return tuple(red)


class UnknownGridTypeException(Exception):
    def __init__(self, gridtype, projtype):
        self.gridtype = gridtype
        self.projtype = projtype

    def __str__(self):
        return "The grid or projection type is not implemented:" + str(self.gridtype) + " - " + str(self.projtype)

    def __reduce__(self):
        red = list(super(UnknownGridTypeException, self).__reduce__())
        red[1] = (self.gridtype, self.projtype)  # Les arguments qui seront passes a __init__
        return tuple(red)


class VarDimensionException(Exception):
    def __init__(self, varname, var, expectedrank = 1)
        self.varname = varname
        self.var = var
        self.expectedrank = expectedrank

    def __str__(self):
        return "Variable " + self.varname + " has rank " + str(len(self.var.shape)) + " instead of " + str(self.expectedrank)

    def __reduce__(self):
        red = list(super(VarDimensionException, self).__reduce__())
        red[1] = (self.varname, self.var, self.expectedrank)
        return tuple(red)

class MassifException(Exception):
    def __init__(self, massifrequest, massifavail):
        self.massifrequest = massifrequest
        self.massifavail = massifavail

    def __str__(self):
        return "The provided forcing file does not contain any point corresponding to your requirements." + \
               "\n Requested massifs: " + str(self.massifrequest) + \
               "\n Available massifs in the provided forcing files (-f): " + str(self.massifavail)

    def __reduce__(self):
        red = list(super(GeometryException, self).__reduce__())
        red[1] = (self.massifrequest, self.massifavail)  # Les arguments qui seront passes a __init__
        return tuple(red)


class ModuleImportException(Exception):

    def __init__(self, module, message=""):
        self.module = module
        self.message = message

    def __str__(self):
        return "Fail to import module :" + self.module + "\n" + \
            +self.message


class MultipleValueException(Exception):

    def __str__(self):
        return "Multiple values match the selection"
