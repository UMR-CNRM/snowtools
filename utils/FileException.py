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
        print("in init FileNameException")
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


class ModuleImportException(Exception):

    def __init__(self, module, message=""):
        self.module = module
        self.message = message

    def __str__(self):
        return "Fail to import module :" + self.module + "\n" + \
            +self.message
