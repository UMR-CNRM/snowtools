#! /usr/bin/env python
# -*- coding: utf-8 -*-

class sun():
    def slope_aspect_correction(self,direct,time,lat_in,lon_in,aspect_in,slope_in,list_list_azim=None,list_list_mask=None,lnosof_surfex=False) :
       
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
   
        slope = self.upscale_tab(slope_in,tab_direct.shape )
        aspect = self.upscale_tab(aspect_in,tab_direct.shape )

        lon = self.upscale_tab(lon_in,tab_direct.shape )
        lat = self.upscale_tab(lat_in,tab_direct.shape )

        #extraction of date and computation of fractional Julian day, j_2 (1 january is 0)        

# M. Lafaysse : convert date in date at the middle of the time step to compute angles more representative of the fluxes
        deltatime=time[1]-time[0]
        tab_time_date_2=time-deltatime/2
                
#        tab_time_date_2 = np.ones(tab_time_date.shape,datetime.datetime)
        
        j_2 = np.ones(tab_time_date.shape,'f')
        h_2 = np.ones(tab_time_date.shape,'f')
        for i in range(len(tab_time_date)):
#            tab_time_date_2[i] = time_init + datetime.timedelta(seconds = tab_time_date[i])
            j = tab_time_date_2[i].timetuple()
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
        
        ZPSI = np.arcsin(ZSINPS)  # solar azimuth, 0. is South

        ##projection on slope/aspect surface - note that aspect is taken from North.
        ZRSIP=ZRSI*(np.cos(ZGAMMA)*np.sin(slope)*np.cos(ZPSI-(math.pi + aspect))+ZSINGA*np.cos(slope))
        ZRSIP = np.where(ZRSIP <=0., 0., ZRSIP)

        # take solar masks into account 
        #(S. Morin 2014/06/27, taken from meteo.f90 in Crocus)
        # Matthieu 2014/09/16 : réécriture parce que les masques changent d'un point à l'autre
        
        if not list_list_mask is None :

            ZPSI1=(ZPSI+math.pi)*URD2DG   # solar azimuth, 0. is North (in contrast to ZPSI for which 0. is South)
            ZMASK=np.zeros_like(ZPSI1)
         
            for i,list_azim in enumerate(list_list_azim):
                f=interp1d(list_azim,list_list_mask[i])
                ZMASK[:,i]=f(ZPSI1[:,i])
            
            ZRSIP = np.where(ZMASK > ZGAMMA*URD2DG,0.,ZRSIP) # set to zero direct radiation values when solar angle is below mask angle (computed as f(ZPSI1))  

        if lnosof_surfex:
            tab_direct = ZRSIP
        else:
            # put the result back on the horizontal ; surfex will carry out the inverse operation.
            tab_direct = ZRSIP/np.cos(slope) #

        return tab_direct
        
        
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
        
               