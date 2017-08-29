#! /usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import math
from utils.FileException import ModuleImportException

try:
    from scipy.interpolate import interp1d
except:
    raise ModuleImportException("interp1d from scipy.interpolate","On profix and beaufix: module load gcc python openblas")

class sun():

    def __init__(self):
        self.missingvalue=-9999999.


    def slope_aspect_correction(self,direct,diffus,time,lat_in,lon_in,aspect_in,slope_in,list_list_azim=None,list_list_mask=None,lnosof_surfex=True) :
       
        '''This routine corrects the direct solar radiation because due to explicit slope or surrounding masks
          Input : array of direct solar radiation over an infinite flat surface (time,loc) or (time,x,y)
                  time vector
                  latitude vector
                  longitude vector
                  aspect vector
                  slope vector
          Optional input : list of azimuths and height of masks
          Optional input : lnosof_surfex (subgrid orography deactivated in Surfex)
          Output : corrected direct solar radiation'''
       
        tab_direct = direct[:]
        tab_diffus = diffus[:]
        tab_global = tab_direct+tab_diffus
   
        slope = self.upscale_tab(slope_in,tab_direct.shape )
        aspect = self.upscale_tab(aspect_in,tab_direct.shape )

        lon = self.upscale_tab(lon_in,tab_direct.shape )
        lat = self.upscale_tab(lat_in,tab_direct.shape )

        #extraction of date and computation of fractional Julian day, j_2 (1 january is 0)        

# M. Lafaysse : convert date in date at the middle of the time step to compute angles more representative of the fluxes
        deltatime=time[1]-time[0]
        tab_time_date=time-deltatime/2
                
#        tab_time_date_2 = np.ones(tab_time_date.shape,datetime.datetime)
        
        j_2 = np.ones(tab_time_date.shape,'f')
        h_2 = np.ones(tab_time_date.shape,'f')
        for i in range(len(tab_time_date)):
#            tab_time_date_2[i] = time_init + datetime.timedelta(seconds = tab_time_date[i])
            j = tab_time_date[i].timetuple()
            j_2[i] = j[7] # extract Julian day (integer)
            h_2[i] = j[3] # extrac time in day

        j = self.upscale_tab_time(j_2,tab_direct.shape)
        h = self.upscale_tab_time(h_2,tab_direct.shape)

        # all tabs now have the same dimension, which is that of tab_direct.
        # method from crocus meteo.f90 original file (v2.4)

        # variables needed for solar computations

        VSOL1=9.9,
        VSOL2=2.,
        VSOL3=.986,
        VSOL4=100.,
        VSOL5=7.7,
        VSOL6=2.,
        VSOL7=.4,
        VSOL8=80.,
        VSOL9=1370.,
        VSOL10=11.7,
        VSOL11=15.,
        VSOL12=.001,
        UDG2RD = math.pi/180.
        NUZENI = 12.
        UH2LON = 15.
        NUH2M = 60.
        UEPSI = 0.001
        URD2DG = 180./math.pi

        # Conversion of slope and aspect to rad:

        slope, aspect = slope*UDG2RD, aspect*UDG2RD

        # Time equation
        ZDT=VSOL1*np.sin((VSOL2*(VSOL3*j-VSOL4))*UDG2RD)\
        -VSOL5*np.sin((VSOL3*j-VSOL6)*UDG2RD)

        # sun declination
        ZSINDL=VSOL7*np.sin(UDG2RD*(VSOL3*j-VSOL8))
        ZDELTA=np.arcsin(ZSINDL)

        # M Lafaysse : remove this threshold because there are controls in SAFRAN and because there are numerical issues at sunset.
        # theoretical maximum radiation
        ZRSI0=VSOL9*(1.-ZSINDL/VSOL10)

        # solar angular time
        ZOMEGA=VSOL11*(h-NUZENI+ZDT/NUH2M+lon/UH2LON)*UDG2RD

        # solar angular height

        ZLAT=lat*UDG2RD
        ZSINGA=np.sin(ZLAT)*ZSINDL+np.cos(ZLAT)*np.cos(ZDELTA)*np.cos(ZOMEGA)
        ZSINGA = np.where(ZSINGA < UEPSI,VSOL12,ZSINGA)
        ZGAMMA=np.arcsin(ZSINGA)

        # direct incident radiation (maximum value authorized is the theoretical maximum radiation)
        ZRSI=tab_direct/ZSINGA
        
        # M Lafaysse : remove this threshold because there are controls in SAFRAN and because there are numerical issues at sunset.
        ZRSI = np.where(ZRSI >= ZRSI0,ZRSI0,ZRSI)

        #solar zenith angle
        ZSINPS=np.cos(ZDELTA)*np.sin(ZOMEGA)/np.cos(ZGAMMA)
        
        # F. Besson/M. Lafaysse : Threshold on ZSINPS (bug on some cells of SIM-FRANCE)
        ZSINPS=np.where(ZSINPS<-1.0,-1.0,ZSINPS)
        ZSINPS=np.where(ZSINPS>1.0,1.0,ZSINPS)

#        Not used : ML comment this instruction
#        ZCOSPS=(np.sin(ZLAT)*ZSINGA-ZSINDL)/(np.cos(ZLAT)*np.cos(ZGAMMA))
        
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # Before summer 2017, we had this mistake :
        # # # # ZPSI = np.arcsin(ZSINPS)  # solar azimuth, 0. is South # # # # # NEVER USE THIS WRONG FORMULA
        # This gives a wrong trajectory in summer when the azimuth should be <90° or >270° 
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        # The new computation of azimuth below is extracted from the theoricRadiation method
        # Who knows where it comes from ?
        
        sunvector0=-np.sin(ZOMEGA)*np.cos(ZDELTA)  # component x of azimuth angle
        sunvector1=np.sin(ZLAT)*np.cos(ZOMEGA)*np.cos(ZDELTA)-np.cos(ZLAT)*np.sin(ZDELTA) # component y of azimuth angle
        sunvector2=np.cos(ZLAT)*np.cos(ZOMEGA)*np.cos(ZDELTA)+np.sin(ZLAT)*np.sin(ZDELTA)  # =cos of zenith angle
        
        sunvector2 = np.where(sunvector2<0.,0.,sunvector2)
        mu = sunvector2  # mu = cos of zenith angle
        
        azimuth = np.where((sunvector0==0)&(sunvector1==0),0.,\
        (np.pi)-np.arctan(sunvector0/sunvector1))
        azimuth = np.where(sunvector1<=0.,np.pi + azimuth,azimuth)
        azimuth = np.where(azimuth >= 2.*np.pi,azimuth - 2.*np.pi,azimuth)
        
#         # diffuse/global theorical ratio for clear sky (SBDART modelling by M. Dumont for Col de Porte)
        a=-2.243613
        b=5.199838
        c=-4.472389
        d=-0.276815
#         
        ratio_clearsky=np.exp(a * (mu**3)+b*(mu**2)+c*mu+d)
        ratio_clearsky=np.where(ratio_clearsky<=1,ratio_clearsky,1)
#         
        # Note that it is not sufficient to apply the threshold on the ratio, it is necessary to apply a threshold to the direct radiation
                
        # Compute theorical maximum global radiation (clear sky) 
        a3 = -0.25070845442
        a2 =  0.56643807637
        a1 =  0.620060537777
        a0 = -0.025767794921
         
        ZTHEOR = ZRSI0 * (a3 * mu**3 + a2 * mu**2 + a1 * mu + a0 )
        ZTHEOR=np.where(ZTHEOR<0.,0.,ZTHEOR)
        
        # Compute theorical components
        theor_diffus=ratio_clearsky*ZTHEOR
        theor_direct=ZTHEOR-theor_diffus
        
        # Apply a threshold to the direct radiation (can not exceed the theorical direct component, otherwise, the projection can provide very strange values        
        tab_direct=np.where(tab_direct<=theor_direct,tab_direct,theor_direct)
        
        # Conserve energy by a transfer to the diffuse component
        tab_diffus=tab_global-tab_direct
                
        # direct incident radiation (maximum value authorized is the theoretical maximum radiation)
        ZRSI=tab_direct/ZSINGA
        
        # M Lafaysse : remove this threshold because there are controls in SAFRAN and because there are numerical issues at sunset.
        ZRSI = np.where(ZRSI >= ZRSI0,ZRSI0,ZRSI)

        ##projection on slope/aspect surface - note that aspect is taken from North.
        #ZRSIP=ZRSI*(np.cos(ZGAMMA)*np.sin(slope)*np.cos(ZPSI - (np.pi + aspect))+ZSINGA*np.cos(slope))
# 
#         print "WRONG ZRSIP"
#         print ZRSIP[t,:]        
        ZRSIP=ZRSI*(np.cos(ZGAMMA)*np.sin(slope)*np.cos(azimuth - aspect)+ZSINGA*np.cos(slope))
        ZRSIP = np.where(ZRSIP <=0., 0., ZRSIP)        

        # take solar masks into account 
        #(S. Morin 2014/06/27, taken from meteo.f90 in Crocus)
        # Matthieu 2014/09/16 : réécriture parce que les masques changent d'un point à l'autre
        
        if not list_list_mask is None :

            ZPSI1=azimuth*URD2DG  # solar azimuth, 0. is North 
            ZMASK=np.zeros_like(ZPSI1)
         
            for i,list_azim in enumerate(list_list_azim):
                f=interp1d(list_azim,list_list_mask[i])
                ZMASK[:,i]=f(ZPSI1[:,i])
            
            ZRSIP = np.where(ZMASK > ZGAMMA*URD2DG,0.,ZRSIP) # set to zero direct radiation values when solar angle is below mask angle (computed as f(ZPSI1))  

        if lnosof_surfex:
            #Now this is the normal case
            tab_direct = ZRSIP
        else:
            # Not recommended
            # put the result back on the horizontal ; surfex will carry out the inverse operation when lnosof=f.
            tab_direct = ZRSIP/np.cos(slope) #

        return tab_direct,tab_diffus
        
        
    # The two following routines from JM Willemet should be rewritten without the loops 
    def upscale_tab(self,var, theshape) :

        # array var can be considered at the same shape than the other arrays (simplify matricial traitment)
        bigvar = np.ma.zeros(theshape) + self.missingvalue

        shapein=len(var.shape)

        if len(theshape) == 2 :
            for i in xrange(0,var.shape[0]) :
                bigvar[:,i] = var[i]
        elif len(theshape) == 3 :
            if shapein==2:
                for ilat in xrange(0,var.shape[0]) :
                    for ilon in xrange(0,var.shape[1]) :
                        bigvar[:,ilat,ilon] = var[ilat,ilon]
            elif shapein==1:
                for i in xrange(0,var.shape[0]) :
                    bigvar[:,:,i] = var[i]
        elif len(theshape) == 4 :
            for i in xrange(0,var.shape[0]) :
                bigvar[:,:,:,i] = var[i]
        elif len(theshape) == 5 :
            for ilat in xrange(0,var.shape[0]) :
                for ilon in xrange(0,var.shape[1]) :
                    bigvar[:,:,:,ilat,ilon] = var[ilat,ilon]
        else : print "error on indices in upscale_tab"

        return bigvar

    def upscale_tab_time(self,var, theshape) :

        # array slope can be considered at the same shape than the other arrays (simplify matricial traitment)
        bigvar = np.ma.zeros(theshape)

        if len(theshape) == 2 :
            for i in xrange(0,theshape[1]) :
                bigvar[:,i] = var[:]
        elif len(theshape) == 3 :
            for ilat in xrange(0,theshape[1]) :
                for ilon in xrange(0,theshape[2]) :
                    bigvar[:,ilat,ilon] = var[:]

        else : print "error on indices in upscale_tab_time"

        return bigvar
        
               
