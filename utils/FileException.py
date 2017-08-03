#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 4 oct. 2012

@author: lafaysse
'''
class DirNameException(Exception):
    
    def __init__(self,path):
        self.path = path
    
    def __str__(self):
        return "Unknown directory : "+self.path  

class FileNameException(Exception):
    
    def __init__(self,path):
        self.path = path
    
    def __str__(self):
        return "Unknown file : "+self.path  

class FileOpenException(Exception):
        
    def __init__(self,path):
        self.path = path
    
    def __str__(self):
        return "Impossible d'ouvrir le fichier : "+self.path
    
class VarNameException(Exception):
    
    def __init__(self,varname,path):
        self.varname = varname
        self.path = path
    
    def __str__(self):
        return "Variable inexistante : "+self.varname+" dans le fichier : "+self.path     
    
class TimeException(Exception):
    
    def __init__(self,path):
        self.path = path
    
    def __str__(self):
        return "Temps incohérent dans le fichier : "+self.path  