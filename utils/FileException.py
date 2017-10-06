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


class DirFileException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "The following path refers to a file instead of a directory: " + self.path


class FileNameException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Unknown file : " + self.path


class FileOpenException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Impossible d'ouvrir le fichier : " + self.path


class FileParseException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Impossible to parse the xml file: " + self.path


class VarNameException(Exception):

    def __init__(self, varname, path):
        self.varname = varname
        self.path = path

    def __str__(self):
        return "Variable inexistante : " + self.varname + " dans le fichier : " + self.path


class TimeException(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return "Temps incohérent dans le fichier : " + self.path


class VarWriteException(Exception):

    def __init__(self, varname, varshape, varfileshape):
        self.varname = varname
        self.varshape = varshape
        self.varfileshape = varfileshape

    def __str__(self):
        return "Impossible to write the variable: " + self.varname + "\n" + \
            "Shape of the variable in the code: " + str(self.varshape) + "\n" + \
            "Shape of the variable in the file: " + str(self.varfileshape)


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
