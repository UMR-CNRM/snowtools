#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Author: M. Lafaysse 24/05/2017
# Recoding of functionalities of faitNAMetPGD in snowtools1 for projects snowtools2 and vortex


# General python modules
import numpy as np
import os
import sys

# For compatibility python 2 / python 3
import six

# Snowtools modules
from utils.prosimu import prosimu
from utils.dates import checkdatebefore,checkdateafter
from utils.FileException import FileNameException


class update_surfex_namelist(object):
    """Class with routines to update SURFEX namelist"""

    def __init__(self,datebegin,forcing="FORCING.nc",dateend=None,updateloc=True,dateforcbegin=None,dateforcend=None):
        """Call the subroutines for updating the SURFEX namelists. Updateloc is only optional."""
        dic=self.update_dates(datebegin)
        if updateloc:
            dic=self.updates_loc(forcing, dic)
            
        if dateforcbegin is not None:
            self.update_forcingdates(datebegin, dateend, dateforcbegin, dateforcend, dic)
            
        self.modify_namelist(dic)
    
    def update_dates(self,datebegin,dic={}):
        """Dictionnary describing the lines to modify in SURFEX namelist for defining the beginning of the simulation."""
        """The values of the dictionnary are tuples: first element is the name of the namelist, second element is the field itself."""

        
        dic["NYEAR"] =("NAM_PREP_SURF_ATM","    NYEAR = " + str(datebegin.year))
        dic["NMONTH"]=("NAM_PREP_SURF_ATM","    NMONTH = " +str(datebegin.month))
        dic["NDAY"]  =("NAM_PREP_SURF_ATM","    NDAY = " +str(datebegin.day))
        dic["XTIME"] =("NAM_PREP_SURF_ATM","    XTIME = " +str(datebegin.hour*3600.))
        
        return dic

    def update_forcingdates(self,datebegin,dateend, dateforcbegin,dateforcend,dic={}):
        """Dictionnary describing the lines to modify in SURFEX namelist for limiting the dates to read in a FORCING file longer than the simulation."""
        """The values of the dictionnary are tuples: first element is the name of the namelist, second element is the field itself."""
                
        checkdateafter(datebegin,dateforcbegin)
        checkdatebefore(dateend,dateforcend)
        
        if datebegin>dateforcbegin or dateend<dateforcend:
            dic["LDELAYEDSTART_NC"]=("NAM_IO_OFFLINE","    LDELAYEDSTART_NC = T")
        if dateend<dateforcend:
            dic["NDATESTOP"]=("NAM_IO_OFFLINE","    NDATESTOP = "+dateend.strftime("%Y, %m, %d, ")+str(dateend.hour*3600) )  
        
        
    def updates_loc(self,forcing="FORCING.nc",dic={}):
        """Dictionnary describing the lines to modify in SURFEX namelist for defining the coordinates of the simulation points."""
        """The values of the dictionnary are tuples: first element is the name of the namelist, second element is the field itself."""
        
        #Read coordinates in FORCING file
        forc=prosimu(forcing)
        latitudes1d=forc.read("LAT")
        longitudes1d=forc.read("LON")
        forc.close()
    
        #Constant dlat/dlon
        dlat1d=np.zeros_like(latitudes1d)+0.5
        dlon1d=np.zeros_like(longitudes1d)+0.5
    
        # Strings to write in the namelist
        dic["XY"] = ["NAM_LONLATVAL", "    XY = "]
        dic["XX"] = ["NAM_LONLATVAL", "    XX = "]
        dic["XDY"]= ["NAM_LONLATVAL", "    XDY = "]
        dic["XDX"]= ["NAM_LONLATVAL", "    XDX = "]
    
        for val in latitudes1d : dic["XY"][1] = dic["XY"][1] + str(val) + ","
        for val in longitudes1d : dic["XX"][1] = dic["XX"][1] + str(val) + ","
        for val in dlat1d : dic["XDY"][1] = dic["XDY"][1] + str(val) + ","
        for val in dlon1d : dic["XDX"][1] = dic["XDX"][1] + str(val) + ","
    
        for key in ["XY","XX","XDY","XDX"]:
            dic[key]=tuple(dic[key])
    
        #Number of simulation points
        dic["NPOINTS"] = ("NAM_LONLATVAL","    NPOINTS = " + str(len(longitudes1d)))
    
        return dic
        
    def modify_namelist(self,dic):
        """Routine to modify a reference SURFEX namelist according to a dictionnary of fields to modify."""    

        if not os.path.isfile("OPTIONS.nam"):
            raise FileNameException(os.getcwd()+"/OPTIONS.nam")
            
        os.rename("OPTIONS.nam","OPTIONS_base.nam")
        
        # Open reference namelist
        namSURFEX_base=open("OPTIONS_base.nam",'r')
                
        # Open new namelist
        namSURFEX=open("OPTIONS.nam",'w')
        namelistopen=""
        # Loop over the lines of the old namelist. Copy everything but lines including the keys of the dictionnary
        # These lines are replaced by the values of the dictionnary
        for line in namSURFEX_base:
            newline=line
            if "&NAM" in line:
                # This is the opening key of the namelist
                # Get the name of the namelist
                namelistopen=line.split()[0][1:]
            else:
                key_to_remove=[]
                field_to_add_at_the_end=[]
                for key,value in six.iteritems(dic):
                    (namelist,field)=value[:]
                    # If the namelist corresponds to the current namelist
                    if namelist==namelistopen:
                        if key in line:
                            # Replace the line by the field 
                            newline = field + '\n'
                            # Remove the field to update in the dictionnary
                            key_to_remove.append(key)

                        elif "/" in line:
                            field_to_add_at_the_end.append(field)                           
                            key_to_remove.append(key)
                
                if "/" in line and len(field_to_add_at_the_end)>0:
                    # This is the end of the namelist.
                    namelistopen=""
                    newline=""
                    for i,field in enumerate(field_to_add_at_the_end):
                        if i==len(field_to_add_at_the_end)-1:
                            sep='\n/\n'
                        else:
                            sep=",\n"
                        
                        newline=newline+field+sep                   
                        
                
                # Remove keys already written
                map(dic.pop,key_to_remove)
                                                                 

            # Write the new line in the new namelist
            namSURFEX.write(newline)    
        # Close both namelists
        namSURFEX_base.close()
        namSURFEX.close()
    

