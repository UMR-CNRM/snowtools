#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 4 oct. 2012

@author: lafaysse
'''
import os
import netCDF4
import numpy as np
from FileException import *

# Fichier PRO.nc issu d'une simulation SURFEX post-traitée

class prosimu():
    def __init__(self,path,format='NETCDF3_CLASSIC'):
        
        if type(path) is list:
            for fichier in path:
                if not os.path.isfile(fichier):
                    raise FileNameException(fichier)
            
                else: 
                    tempdataset=netCDF4.Dataset(fichier,"a",format=format)
                    tempdataset.variables["time"].calendar="standard"
                    tempdataset.close()
                    
            self.dataset=netCDF4.MFDataset(path,"r")          
            self.path=path[0]
            self.mfile=1
            
        # Vérification du nom du fichier
        elif os.path.isfile(path):
            self.path=path
            self.mfile=0
            try:
                self.dataset=netCDF4.Dataset(path,"r",format=format)
            except:
                raise FileOpenException(path)
        else:
            raise FileNameException(path)

    def listdim(self):
        return init_forcing_file.dimensions.copy()

    def listvar(self):
        return self.dataset.variables.keys()

    def getdimvar(self,varname):        
        return np.array(self.dataset.variables[varname].dimensions)

    def getrankvar(self,varname):        
        return len(self.dataset.variables[varname].shape)
    
    def listattr(self,varname):
        return self.dataset.variables[varname].ncattrs()
    
    def getattr(self,varname,attname):
        return getattr(self.dataset.variables[varname],attname)
    
    def gettypevar(self,varname):
        return dataset.variables[varname].dtype
    
    def getfillvalue(self,varname):
        return self.dataset.variables[varname]._FillValue
    
    def infovar(self,varname):
        # Vérification du nom de la variable
        if not varname in self.listvar() :
            raise VarNameException(varname,self.path)
        
        return self.gettypevar(varname),self.getrankvar(varname),self.getdimvar(varname),self.getfillvalue(varname),self.listattr(varname)     
        
    
    def readtime_for_copy(self):
        time=self.dataset.variables["time"]
        return time,time.units
        
    
    def readtime(self):
                
        # Vérification du nom de la variable
        if not "time" in self.dataset.variables.keys() :
            raise VarNameException("time",self.path)
    
        if(self.mfile==1):   
            time_base = self.dataset.variables["time"]
            time_base.calendar='standard'           
            time = netCDF4.MFTime(time_base)
#             time=time_base  
        else:
            time=self.dataset.variables["time"]
            
        return np.array(netCDF4.num2date(time[:], time.units))
    
    def extract(self,varname,var,selectpoint=-1):
        
        rank=len(var.shape)
        if selectpoint==-1:
            if "tile" in self.dataset.variables[varname].dimensions or 'Number_of_Tile' in self.dataset.variables[varname].dimensions:
                if rank==1:
                    # Pour cas de la variable tile dans comparaisons automatiques
                    var_extract=var[0]                 
                if rank==2:
                    var_extract=var[:,0]
                elif rank==3:
                    var_extract=var[:,0,:]
                elif rank==4:
                    var_extract=var[:,0,:,:] 
                elif rank==5:
                    var_extract=var[:,0,:,:,:]                    
            else:
                if rank==0:
                    var_extract=var
                elif rank==1:
                    var_extract=var[:]
                elif rank==2:
                    var_extract=var[:,:]
                elif rank==3:
                    var_extract=var[:,:,:]
                elif rank==4:                                
                    var_extract=var[:,:,:,:]
                elif rank==5:
                    var_extract = var[:,:,:,:,:]                    
        else:
            if "tile" in self.dataset.variables[varname].dimensions or 'Number_of_Tile' in self.dataset.variables[varname].dimensions:
                if rank==1:
                    # Pour cas de la variable tile dans comparaisons automatiques
                    var_extract=var[0]                
                elif rank==3:
                    var_extract=var[:,0,selectpoint]
                elif rank==4:
                    var_extract=var[:,0,:,selectpoint] 
                elif rank==5:
                    var_extract=var[:,0,:,:,selectpoint]                       
            else:
                if rank==0:
                    var_extract=var
                elif rank==1:
                    var_extract=var[selectpoint]
                elif rank==2:
                    var_extract=var[:,selectpoint]
                elif rank==3:
                    var_extract=var[:,:,selectpoint]
                elif rank==4:                                
                    var_extract=var[:,:,:,selectpoint]            
                elif rank==5:
                    var_extract = var[:,:,:,:,selectpoint] 

        return var_extract            
    
    def read(self,varname,fill2zero=False,selectpoint=-1,keepfillvalue=False):
        
        # Vérification du nom de la variable
        if not varname in self.listvar() :
            raise VarNameException(varname,self.path)

        # Sélection de la variable        
        var=self.dataset.variables[varname]
        
        avail_fillvalue = "_FillValue" in var.ncattrs()
        if avail_fillvalue:
            fillvalue=var._FillValue        
        
        # Sélection d'un point si demandé 
        # Suppression dimension tile si nécessaire
        var=self.extract(varname, var, selectpoint=selectpoint)

         # Remplissage des valeurs manquantes si nécessaire         
        if len(var.shape)>1 and not keepfillvalue:
            try:
                if fill2zero:
                    array=var.filled(fill_value=0)
                    print "Fill data with 0 for variable "+varname                    
                else:
                    array=var.filled(fill_value=np.nan)
                    print "Fill data with np.nan for variable "+varname                    
            except:
                if avail_fillvalue:
                    if fill2zero:
                        array=np.where(var==fillvalue,0,var)
                        print "Fill data with 0 for variable "+varname+ " (old method)"                        
                    else:
                        array=np.where(var==fillvalue,np.nan,var)
                        print "Fill data with np.nan for variable "+varname+ " (old method)"                             
                else:
                    print "Unable to fill data with 0 or np.nan for variable "+varname    
     
        else:
            array=var

        array=var
        # Lecture de la variable
        return array
    
    # Pour compatibilité anciens codes, on conserve les routines obsolètes read1d et read2d
    def read1d(self,varname,fill2zero=False,indpoint=0):
        return self.read(varname,fill2zero=fill2zero,indpoint=indpoint) 

    def read2d(self,varname,fill2zero=False):
        return self.read(varname,fill2zero=fill2zero)
    
    def checktime(self,nametime,timeref):
        
        newtime=self.read(nametime)
        
#        Provisoirement on compare brutalement. A terme il faudra faire en fonction du units ou sélectionner une période commune.
        if (newtime[:] != timeref[:]).all():
            raise TimeException(self.path)
            
    def close(self):
        self.dataset.close()
        
    def integration(self,variable,nstep,start=0):
        #Renvoie les valeurs cumulées tous les nstep pas de temps
        cumsum=np.cumsum(np.insert(variable,0,0,axis=0),axis=0)
        if len(variable.shape)==1:
            temp=cumsum[nstep:]-cumsum[:-nstep]
            return temp[np.arange(0,len(variable)-nstep,nstep)]
        elif len(variable.shape)==2:
            temp=cumsum[nstep:,:]-cumsum[:-nstep,:]
            return temp[np.arange(start,len(variable)-nstep,nstep),:]                 
        else:
            sys.exit("integration of 3D variables not implemented")
            
    def moytempo(self,precip,nstep,start=0):
        return self.integration(precip,nstep,start=start)/nstep            