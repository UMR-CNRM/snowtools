#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Author: M. Lafaysse 24/05/2017
# Recoding of functionalities of faitNAMetPGD in snowtools1 for projects snowtools2 and vortex

from utils.prosimu import prosimu
import numpy as np
import os
import sys

# For compatibility python 2 / python 3
import six

class update_surfex_namelist(object):
    """Class with routines to update SURFEX namelist"""

    def __init__(self,datebegin,forcing="FORCING.nc",updateloc=True):
        """Call the subroutines for updating the SURFEX namelists. Updateloc is only optional."""
        dic=self.update_dates(datebegin)
        if updateloc:
            dic=self.updates_loc(forcing, dic)
        self.modify_namelist(dic)
    
    def update_dates(self,datebegin,dic={}):
        """Dictionnary describing the lines to modify in SURFEX namelist for defining the beginning of the simulation."""
        
        dic["NYEAR"]="    NYEAR = " + str(datebegin.year)
        dic["NMONTH"]="    NMONTH = " +str(datebegin.month)
        dic["NDAY"]="    NDAY = " +str(datebegin.day)
        dic["XTIME"]="    XTIME = " +str(datebegin.hour*3600.)
        
        return dic
        
    def updates_loc(self,forcing="FORCING.nc",dic={}):
        """Dictionnary describing the lines to modify in SURFEX namelist for defining the coordinates of the simulation points."""

        #Read coordinates in FORCING file
        forc=prosimu(forcing)
        latitudes1d=forc.read("LAT")
        longitudes1d=forc.read("LON")
        forc.close()
    
        #Constant dlat/dlon
        dlat1d=np.zeros_like(latitudes1d)+0.5
        dlon1d=np.zeros_like(longitudes1d)+0.5
    
        # Strings to write in the namelist
        dic["XY"] = "    XY = "
        dic["XX"] = "    XX = "
        dic["XDY"] = "    XDY = "
        dic["XDX"]= "    XDX = "
    
        for val in latitudes1d : dic["XY"] = dic["XY"] + str(val) + ","
        for val in longitudes1d : dic["XX"] = dic["XX"] + str(val) + ","
        for val in dlat1d : dic["XDY"] = dic["XDY"] + str(val) + ","
        for val in dlon1d : dic["XDX"] = dic["XDX"] + str(val) + ","
    
        #Number of simulation points
        dic["NPOINTS"] = "    NPOINTS = " + str(len(longitudes1d))
    
        return dic
        
    def modify_namelist(self,dic):
        """Routine to modify a reference SURFEX namelist according to a dictionnary of fields to modify."""    

        
        if not os.path.isfile("OPTIONS.nam"):
            sys.exit("ERREUR A GERER")
            
        os.rename("OPTIONS.nam","OPTIONS_base.nam")
        
        # Open reference namelist
        namSURFEX_base=open("OPTIONS_base.nam",'r')
        # Open new namelist
        namSURFEX=open("OPTIONS.nam",'w')
    
        # Loop over the lines of the old namelist. Copy everything but lines including the keys of the dictionnary
        # These lines are replaced by the values of the dictionnary
        for line in namSURFEX_base:
            for key,value in six.iteritems(dic): 
                if key in line:
                    namSURFEX.write(value+'\n')
                else:
                    namSURFEX.write(line)
        
        # Close both namelists
        namSURFEX_base.close()
        namSURFEX.close()
    
    
