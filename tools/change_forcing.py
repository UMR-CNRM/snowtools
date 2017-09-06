#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 3 Aug. 2017

@author: lafaysse
'''

import os
import sys
import numpy as np
import netCDF4

# For compatibility python 2 / python 3
import six

from utils.sun import sun
from utils.prosimu import prosimu
from utils.infomassifs import infomassifs
from utils.FileException import *

class forcinput_tomodify:
    def __init__(self,forcin,forcout,*args,**kwargs):
        '''Generic method to open an initial forcing file to read and to create a modified forcing file'''
        
        if 'forcenetcdf4' in kwargs.keys():
            forcenetcdf4=kwargs['forcenetcdf4']
        else:
            forcenetcdf4=False
        
        if type(forcin) is int:
            forcin=str(forcin)
        
        if not os.path.isfile(forcin):
            raise FileNameException(forcin)
        
        dirout=os.path.dirname(forcout)

        if not (dirout == '' or os.path.isdir(dirout)) :
            raise DirNameException(dirout)
            
        init_forcing_file = prosimu(forcin)
        self.filename=forcin

        if forcenetcdf4:
            new_forcing_file = netCDF4.Dataset(forcout,"w",format='NETCDF4_CLASSIC')
        else:
            new_forcing_file = netCDF4.Dataset(forcout,"w",format='NETCDF3_CLASSIC')

        self.modify(init_forcing_file,new_forcing_file,args)

        init_forcing_file.close()
        new_forcing_file.close()

    
    def modify(self,init_forcing_file,new_forcing_file,*args):
        pass


class forcinput_select(forcinput_tomodify):
    ''' 
    This class was first implemented by G. Lecourt for spatial reduction of a forcing file
    M Lafaysse generalized the method to both FORCING and PRO files (June 2016)
    M Lafaysse added a treatement to increase the number of slopes (July 2016)
    M Lafaysse added a treatment to create coordinates for direct compatibilty with the new SAFRAN output (August 2017)'''

    def modify(self,init_forcing_file,new_forcing_file,*args):
                
        ''' Modify a forcing file towards a prescribed geometry'''
        
        print "Modify forcing file towards the prescribed geometry:"
        
        (list_massif_number,min_alt,max_alt,liste_pentes,list_exp)=args[:][0]

        list_exp_degres=list_exp[:] # A copy is necessary to not modify this value in the calling module for next iteration
        liste_pentes_int = map(int,liste_pentes)

        init_massif_nb_sop = init_forcing_file.read("massif_number",keepfillvalue=True)
        b_points_massif = np.in1d(init_massif_nb_sop,list_massif_number)

        init_alt = init_forcing_file.read("ZS",keepfillvalue=True)
        b_points_alt = (init_alt>=min_alt) * (init_alt<=max_alt)

        init_slopes = init_forcing_file.read("slope",keepfillvalue=True)
        init_exp = init_forcing_file.read("aspect",keepfillvalue=True)
        
        if "0" in liste_pentes:
            nb_slope_angles_notflat=len(liste_pentes)-1
            nb_aspect_angles_notflat=len(list_exp_degres)-1
            nb_slopes_bylevel=1+nb_slope_angles_notflat*nb_aspect_angles_notflat
        else:
            nb_slope_angles_notflat=len(liste_pentes)
            nb_aspect_angles_notflat=len(list_exp_degres)
            nb_slopes_bylevel=nb_slope_angles_notflat*nb_aspect_angles_notflat

        extendaspects= nb_slopes_bylevel>1 and np.all(init_exp==-1)
        extendslopes = not extendaspects and ( len(liste_pentes) > len(np.unique(init_slopes)) )
        
        if extendaspects:
            print "Extend aspects of the input forcing file"
        if extendslopes:
            print "Extend slopes of the input forcing file"
        
        if extendaspects:
            b_points_slope = np.in1d(init_slopes,[0])
            b_points_aspect = np.in1d(init_exp,[-1])
            if "0" in liste_pentes:
                liste_pentes_int.remove(0)
                list_exp_degres.remove(-1)
        else:
            b_points_slope = np.in1d(init_slopes,liste_pentes_int)            
            b_points_aspect = np.in1d(init_exp,list_exp_degres)

        # Identify points to extract
        index_points = np.where(b_points_massif * b_points_alt * b_points_slope * b_points_aspect)[0]

        # It is possible to exclude somme massifs or elevations before duplicating the slopes
        if extendslopes:
            points_to_duplicate = np.invert(np.in1d(init_exp[index_points],[-1]))
            if "0" in liste_pentes:
                liste_pentes_int.remove(0)
                list_exp_degres.remove(-1)

        init_forcing_file_dimensions = init_forcing_file.listdim()

        massif_dim_name = "massif"
        nbpoints_dim_name = "Number_of_points"
        loc_dim_name = "location"
        
        # Il faut créer la dimension time en premier (obligatoire au format NETCDF4_CLASSIC)
        new_forcing_file.createDimension("time",None)
        

        if massif_dim_name in init_forcing_file_dimensions:
            init_massif = init_forcing_file.read("massif",keepfillvalue=True)
            index_massif = np.where(np.in1d(init_massif,list_massif_number))[0]            
            len_dim = len(index_massif)
            new_forcing_file.createDimension(massif_dim_name,len_dim)
            del init_forcing_file_dimensions[massif_dim_name]
        
        if nbpoints_dim_name in init_forcing_file_dimensions :
            spatial_dim_name=nbpoints_dim_name
        elif loc_dim_name in init_forcing_file_dimensions :
            spatial_dim_name=loc_dim_name
        else:
            spatial_dim_name="missing"
        
        if spatial_dim_name in init_forcing_file_dimensions :
#             print extendaspects
#             print "NB slopes by level"
#             print nb_slopes_bylevel
#             print len(index_points)
            if extendaspects:
                len_dim=len(index_points)*nb_slopes_bylevel
            elif extendslopes:
                nslopes_to_create=len(liste_pentes)-2
                len_dim=len(index_points)+np.sum(points_to_duplicate)* nslopes_to_create
                
#                 print nb_slope_angles_notflat*len(list_exp_degres)+1
#                 print nb_slope_angles_notflat
#                 print len(list_exp_degres)
#                 print list_exp_degres
#                 sys.exit()
                
                indflat=np.arange(0,len_dim,nb_slope_angles_notflat*len(list_exp_degres)+1)
                indnoflat=np.delete(np.arange(0,len_dim,1),indflat)
                
#                 print len(indflat)
#                 print len(indnoflat)
#                 
#                 print len_dim
                
            else:
                len_dim = len(index_points)
            print "create dimension :"+ spatial_dim_name +" "+str(len_dim)
            len_dim_spatial=len_dim 
            new_forcing_file.createDimension(spatial_dim_name,len_dim)
            del init_forcing_file_dimensions[spatial_dim_name]                       
            
        for dimname,dim in six.iteritems(init_forcing_file_dimensions):
            print "Create dimension "+dimname+" "+str(len(dim))
            if not dimname=="time":
#                 print len(dim)
                new_forcing_file.createDimension(dimname,len(dim))
        
        savevar={}
        
        listvar=init_forcing_file.listvar()

        for varname in listvar:

            vartype, rank, array_dim, varFillvalue, var_attrs = init_forcing_file.infovar(varname)

            if len(array_dim)>0:
                index_dim_massif = np.where(array_dim == massif_dim_name)[0]
                index_dim_nbpoints = np.where(array_dim == spatial_dim_name)[0]
                var_array = init_forcing_file.read(varname,keepfillvalue=True)
            else:
                index_dim_massif =[]
                index_dim_nbpoints =[]          
                var_array = init_forcing_file.read(varname,keepfillvalue=True).getValue() 
     
            if len(index_dim_massif) == 1:
                var_array = np.take(var_array,index_massif,index_dim_massif[0])
            if len(index_dim_nbpoints) == 1:
                var_array = np.take(var_array,index_points,index_dim_nbpoints[0])
                
                if extendaspects or extendslopes:
                    if varname=="aspect":
                        expo_1level_notflat=np.tile(list_exp_degres,nb_slope_angles_notflat)
                        if "0" in liste_pentes:
                            expo_1level=np.append(-1,expo_1level_notflat)
                        else:
                            expo_1level=expo_1level_notflat
 
                        if extendaspects:
                            var_array=np.tile(expo_1level,len(index_points))
                        elif extendslopes:
                            var_array=np.tile(expo_1level,len(indflat))                       
                        
                        
                    elif varname=="slope":
                        slope_1level_notflat=np.repeat(liste_pentes_int,nb_aspect_angles_notflat)
                        if "0" in liste_pentes:
                            slope_1level=np.append(0,slope_1level_notflat)
                        else:
                            slope_1level=slope_1level_notflat
                            
                        if extendaspects:
                            var_array=np.tile(slope_1level,len(index_points))
                        elif extendslopes:
                            var_array=np.tile(slope_1level,len(indflat)) 

                
#                 print extendslopes
#                 print extendaspects
                
                if not varname in ["aspect","slope"]:

#                     print rank
                    if rank>=2:
                        if extendaspects:
                            var_array=np.repeat(var_array,nb_slopes_bylevel,axis=1)
                        elif extendslopes:
                            
                            newvar_array=np.empty((var_array.shape[0],len_dim_spatial),vartype)
                            newvar_array[:,indflat]=var_array[:,~points_to_duplicate]                            
                            newvar_array[:,indnoflat]=np.repeat(var_array[:,points_to_duplicate],1+nslopes_to_create,axis=1)
                            var_array=newvar_array[:]
                            
                    elif rank>=1:
                        if extendaspects:
                            var_array=np.repeat(var_array,nb_slopes_bylevel)
                        elif extendslopes:
                            
                            newvar_array=np.empty(len_dim_spatial)
                            newvar_array[indflat]=var_array[~points_to_duplicate]
                            
#                             print "nslopes_to_create"
#                             print nslopes_to_create
                            
                            newvar_array[indnoflat]=np.repeat(var_array[points_to_duplicate],1+nslopes_to_create)
                            
#                             print indflat
#                             print indnoflat
                            var_array=newvar_array[:]

            var = new_forcing_file.createVariable(varname,vartype,array_dim,fill_value=varFillvalue)

            for attname in var_attrs:
                if not attname == u'_FillValue':
                    setattr(var,attname,init_forcing_file.getattr(varname,attname))
            try:
                if not (varname in ["DIR_SWdown","SCA_SWdown"] and  ( extendaspects or extendslopes )):
                # do not write direct solar radiations if aspects and slopes were extended because we need to recompute the values
                    if rank==0:
                        var[:]=var_array
                    elif rank==1:
                        var[:]=var_array
                    elif rank==2:
                        var[:,:]=var_array
                    elif rank==3:
                        var[:,:,:]=var_array
                    elif rank==4:
                        var[:,:,:,:]=var_array
                    elif rank==5:
                        var[:,:,:,:,:]=var_array
            except:
                raise VarWriteException(varname,var_array.shape,var.shape)

            # Some variables need to be saved for solar computations
            if varname in ["time"]:
                savevar[varname]=init_forcing_file.readtime()
            if varname in ["LAT","LON","aspect","slope","DIR_SWdown","SCA_SWdown","massif_number"]:
                savevar[varname]=var_array
            if varname == "massif_number":
                save_array_dim=array_dim

        if not "LAT" in init_forcing_file.listvar():
            lat,lon=self.addCoord(new_forcing_file,savevar["massif_number"],save_array_dim,varFillvalue)
        else:
            lat=savevar["LAT"]
            lon=savevar["LON"]

        # Compute new solar radiations according to the new values of slope and aspect
        if extendaspects or extendslopes:
            direct,diffus=sun().slope_aspect_correction(savevar["DIR_SWdown"],savevar["SCA_SWdown"],savevar["time"],lat,lon,savevar["aspect"],savevar["slope"])
            new_forcing_file.variables["DIR_SWdown"][:]=direct
            new_forcing_file.variables["SCA_SWdown"][:]=diffus
            
    def addCoord(self, forcing, massifnumber, dimension,varFillValue):
        '''Routine to add coordinates in the forcing file for the SAFRAN massifs'''
        
        INFOmassifs=infomassifs()
        dicLatLon=INFOmassifs.getAllMassifLatLon()
                
        lat=np.empty_like(massifnumber)
        lon=np.empty_like(massifnumber)
        
        for point in range(0,len(massifnumber)):
            latlon=dicLatLon[massifnumber[point]]
            lat[point]=latlon[0]
            lon[point]=latlon[1]        

        var = forcing.createVariable("LAT", massifnumber.dtype, dimension, fill_value=varFillValue)
        setattr(var, u'long_name', u'latitude')
        setattr(var, u'units', u'degrees_north')
        var[:]=lat
        var = forcing.createVariable("LON",massifnumber.dtype,dimension,fill_value=varFillValue)
        setattr(var, u'long_name', u'longitude')
        setattr(var, u'units', u'degrees_east')
        var[:]=lon
        
        return lat,lon  
    
# For test    
if __name__ == "__main__":
            
    list_massifs=range(1,24)
    min_alt=600
    max_alt=3600
    list_pentes=["0","20","40"]
    list_expo=xrange(0,9)
    forcin=os.environ["HOME"]+"/FORCING_OLD.nc"
    forcout=os.environ["HOME"]+"/FORCING_NEW.nc"
    f=forcinput_select(forcin,forcout,list_massifs,min_alt,max_alt,list_pentes,list_expo)
    

    
