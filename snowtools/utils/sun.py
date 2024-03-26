# -*- coding: utf-8 -*-

import numpy as np
import math


from snowtools.utils.resources import print_used_memory

# TODO temporary
import matplotlib.pyplot as plt
import pysolar
from pysolar.solar import *
from datetime import timezone

def interp1d(x, y, tab):
    """
    Return the linear interpolation of y for the tab values of the x coordinate
    Method designed to avoid the use of scipy.interpolate (avoid the dependency to an external module and environment problems on Bull)

    .. warning::
       This is not relevant anymore

    :meta private:
    """
    ind_after = np.searchsorted(x, tab)
    ind_before = ind_after - 1
    x_before = np.take(x, ind_before)
    x_after = np.take(x, ind_after)
    y_before = np.take(y, ind_before)
    y_after = np.take(y, ind_after)
    return ((x_after - tab) * y_before + (tab - x_before) * y_after) / ((x_after - x_before) * 1.)


class sun():
    """
    A class for computation around sun and radiations
    """

    printmemory = False

    def __init__(self):
        self.missingvalue = -9999999.

    def slope_aspect_correction(self, direct, diffus, time, lat_in, lon_in, aspect_in, slope_in, list_list_azim=None, list_list_mask=None, lnosof_surfex=True, convert_time = True, return_angles = False):
        """
        This routine corrects the direct solar radiation because due to explicit slope or surrounding masks

        :param direct: array of direct solar radiation over an infinite flat surface (time,loc) or (time,x,y)
        :param time: time vector
        :param latitude: latitude vector
        :param logitude: longitude vector
        :param aspect: aspect vector
        :param slope: slope vector
        :param list_list_azim: list of azimuths of masks on each point(radian or degrees ?)
        :param list_list_mask: list of heights of masks on each point(radian or degrees ?)
        :param lnosof_surfex: (subgrid orography deactivated in Surfex)
        :type lnosof_surfex: bool
        :param convert_time: take time at the middle of the time step to compute angles more representative of the fluxes
        :type convert_time: bool
        :param return_angles: if True, also return the sun angle
        :type return_angles: bool

        :returns: corrected direct solar radiation and optionally, the angular positions of sun
        """
        tab_direct = direct[:]
        tab_diffus = diffus[:]
        tab_global = tab_direct + tab_diffus

        # --------- Upscale tabs ---------
        # (all tab will have the same size as input tab_direct)
        slope = self.upscale_tab(slope_in, tab_direct.shape)
        aspect = self.upscale_tab(aspect_in, tab_direct.shape)

        lon = self.upscale_tab(lon_in, tab_direct.shape)
        lat = self.upscale_tab(lat_in, tab_direct.shape)

        # --------- Extraction of days and time ---------
        if convert_time is True:
            # M. Lafaysse : convert date in date at the middle of the time step to compute angles more representative of the fluxes
            # SAFRAN gives the hourly solar radiation at hour-30 minutes,
            # then SURFEX uses this flux between H-1 and H.
            deltatime = time[1] - time[0]
            tab_time_date = time - deltatime / 2
        else:
            tab_time_date = time
            
        julian_days = np.ones(tab_time_date.shape, 'f')
        decimal_hours = np.ones(tab_time_date.shape, 'f')
        
        for i in range(len(tab_time_date)):
            # timetuple list description: 
            # j[0]: year, j[1]: month, j[2]: day, 
            # j[3]: hour, j[4]: minute, j[5]: second,
            # j[6]: day of the week, j[7]: day of the year,
            # j[8]: daylight saving time
            timetup = tab_time_date[i].timetuple()
            julian_days[i] = timetup[7]  # extract Julian day (integer, 1st january is 1)
            # L. Roussel: fix to decimal hour instead of integer hour
            decimal_hours[i] = timetup[3] + timetup[4] / 60 + timetup[5] / 3600  # extrac time in hour

        j = self.upscale_tab_time(julian_days, tab_direct.shape)
        h = self.upscale_tab_time(decimal_hours, tab_direct.shape)

        # all tabs now have the same dimension, which is that of tab_direct.
        # method used id from crocus meteo.f90 original file (v2.4)

        # --------- Math constants ---------
        eps = 0.0001
        PI = math.pi
        DG2RD = PI / 180. # degree to radian
        RD2DG = 180. / PI # radian to degree

        # --------- Convert to radian ---------
        slope, aspect = slope * DG2RD, aspect * DG2RD

        # Later computations are almost all approximations used for engineering
        
        # --------- Equation of Time --------- 
        # (time shift in minutes, take in account orbit eccentricity, and obliquity)
        eot = 9.9 * np.sin(2 * (j - 101.4) / 365 * 2 * PI) \
            - 7.7 * np.sin((j - 2.027) / 365 * 2 * PI)
        
        # --------- Sun declination (delta) ---------
        # (angular distance of the sun's rays north (or south) of the equator):
        # L. Roussel 03/2024, small changes here, but easier to understand
        # old formula : 0.4 * np.sin((0.986 * j - 80) * DG2RD)
        sin_delta = 0.4 * np.sin((j - 81) / 365 * 2 * PI) 
        delta = np.arcsin(sin_delta)

        # --------- Solar angular time ---------
        # TODO do not take in account the shift from year to year
        # h - 12: centered at noontime
        # eot / 60: equation of time in hour
        # lon / 15: position from Greenwhich (french Alps around 7° East, i.e ~30 min shift)
        # / 24: in days
        # * 2 * PI: to angle
        omega_time = (h - 12 + eot / 60 + lon / 15) / 24 * 2 * PI

        # --------- Solar angular height ---------
        # cos(theta) = sin(gamma)
        #
        #   zenith, vertical
        #   | 
        #   |  solar zenith angle, sza (theta)
        #   |---->x
        #   |    x^
        #   |   x |
        #   |  x  | solar altitude angle (gamma)
        #   | x   | ~ solar angular height
        #   |x    |
        #   +--------- horizon
        # 
        lat_radian = lat * DG2RD
        sin_gamma = np.sin(lat_radian) * sin_delta + np.cos(lat_radian) * np.cos(delta) * np.cos(omega_time)
        # L. Roussel: set as 0 if negative, not 0.001
        sin_gamma = np.where(sin_gamma < eps, 0, sin_gamma)
        gamma = np.arcsin(sin_gamma)
        
        
        # M Lafaysse : remove this threshold because there are controls in SAFRAN and because there are numerical issues at sunset.
        # --------- Theoretical maximum radiation ---------
        max_theor_radiation = 1370 * (1 - sin_delta / 11.7)

        ######## Not used anymore, old code ########
        # solar zenith angle
        # ZSINPS = np.cos(ZDELTA) * np.sin(ZOMEGA) / np.cos(ZGAMMA)

        # F. Besson/M. Lafaysse : Threshold on ZSINPS (bug on some cells of SIM-FRANCE)
        # ZSINPS = np.where(ZSINPS < -1.0, -1.0, ZSINPS)
        # ZSINPS = np.where(ZSINPS > 1.0, 1.0, ZSINPS)

        # Not used : ML comment this instruction
        # ZCOSPS=(np.sin(ZLAT)*ZSINGA-ZSINDL)/(np.cos(ZLAT)*np.cos(ZGAMMA))
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # Before summer 2017, we had this mistake :
        # # # # ZPSI = np.arcsin(ZSINPS)  # solar azimuth, 0. is South # # # # # NEVER USE THIS WRONG FORMULA
        # This gives a wrong trajectory in summer when the azimuth should be <90° or >270°
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        #############################################
        
        # The new computation of azimuth below is extracted from the theoricRadiation method
        # Who knows where it comes from ?

        # --------- Azimuth ---------
        # component x of azimuth angle:
        x_azimuth_angle = - np.sin(omega_time) * np.cos(delta)
        # component y of azimuth angle:
        y_azimuth_angle = np.sin(lat_radian) * np.cos(omega_time) * np.cos(delta) - np.cos(lat_radian) * np.sin(delta)  
        
        azimuth = np.where((x_azimuth_angle == 0) & (y_azimuth_angle == 0), 0.,
                    (np.pi) - np.arctan(x_azimuth_angle / y_azimuth_angle))
        azimuth = np.where(y_azimuth_angle <= 0., azimuth + np.pi, azimuth)
        azimuth = np.where(azimuth >= 2. * np.pi, azimuth - 2. * np.pi, azimuth) # back into [0, 2 * PI]

        ######## Recompute diffuse/direct ########
        # diffuse/global theorical ratio for clear sky (SBDART modelling by M. Dumont for Col de Porte)
        a = -2.243613
        b = 5.199838
        c = -4.472389
        d = -0.276815

        # mu = sunvector2  # mu = cos of zenith angle
        mu = sin_gamma.copy()
        ratio_clearsky = np.exp(a * (mu ** 3) + b * (mu ** 2) + c * mu + d)
        ratio_clearsky = np.where(ratio_clearsky <= 1, ratio_clearsky, 1)
        # Note that it is not sufficient to apply the threshold on the ratio, it is necessary to apply a threshold to the direct radiation
        # Compute theorical maximum global radiation (clear sky)
        a3 = -0.25070845442
        a2 = 0.56643807637
        a1 = 0.620060537777
        a0 = -0.025767794921

        ZTHEOR = max_theor_radiation * (a3 * mu**3 + a2 * mu**2 + a1 * mu + a0)
        ZTHEOR = np.where(ZTHEOR < 0., 0., ZTHEOR)

        # Compute theorical components
        theor_diffus = ratio_clearsky * ZTHEOR
        theor_direct = ZTHEOR - theor_diffus
        
        # plt.plot(tab_time_date, theor_diffus, marker=".", label="theor_diffus")
        # plt.plot(tab_time_date, theor_direct, marker=".", label="theor_direct")
        # plt.plot(tab_time_date, tab_direct, marker=".", label="tab_direct before")

        # Apply a threshold to the direct radiation (can not exceed the theorical direct component, 
        # otherwise, the projection can provide very strange values)
        tab_direct = np.where(tab_direct <= theor_direct, tab_direct, theor_direct)
        # plt.plot(tab_time_date, tab_direct, marker=".", label="tab_direct after")
        # plt.legend()
        # plt.show()
        # Conserve energy by a transfer to the diffuse component
        # TODO L. Roussel why we keep radiation in deep valleys ?
        # tab_diffus = tab_global - tab_direct
        ###########################################

        # direct incident radiation (maximum value authorized is the theoretical maximum radiation)
        # If you don't pass `out` the indices where (sin_gamma == 0) will be uninitialized !
        direct_incident = np.divide(tab_direct, sin_gamma, out=np.zeros_like(tab_direct), where=sin_gamma!=0)

        # M Lafaysse : remove this threshold because there are controls in SAFRAN and because there are numerical issues at sunset.
        # TODO L. Roussel: this one is useless ? 
        direct_incident = np.where(direct_incident >= max_theor_radiation, max_theor_radiation, direct_incident)

        # --------- Projection on slope/aspect surface --------- 
        # (note that aspect is taken from North)
        # old code : ZRSIP=ZRSI*(np.cos(ZGAMMA)*np.sin(slope)*np.cos(ZPSI - (np.pi + aspect))+ZSINGA*np.cos(slope))
        direct_plane_projected = direct_incident * (np.cos(gamma) * np.sin(slope) * np.cos(azimuth - aspect) + sin_gamma * np.cos(slope))
        direct_plane_projected = np.where(direct_plane_projected <= 0., 0., direct_plane_projected)

        # --------- Solar masking ---------
        # if mask are available, mask direct when relief between sun and location
        # (S. Morin 2014/06/27, taken from meteo.f90 in Crocus)
        # Matthieu 2014/09/16 : réécriture parce que les masques changent d'un point à l'autre
        direct_plane_projected_masked = direct_plane_projected.copy()
        if list_list_mask is not None: 
            simu_azimuth_degree = azimuth * RD2DG  # solar azimuth of the simulation, 0. is North
            simu_interp_mask = np.zeros_like(simu_azimuth_degree) # init for computed solar mask (time, loc) or (time, x, y)

            for i, list_azim in enumerate(list_list_azim): # for each point
                # interp1d(x, y, interp_x)
                simu_interp_mask[:, i] = interp1d(list_azim, list_list_mask[i], simu_azimuth_degree[:, i])
            
            # set to zero direct radiation values when solar angle is below mask angle
            direct_plane_projected_masked = np.where(simu_interp_mask > gamma * RD2DG, 0., direct_plane_projected)  
        
        
        # --------- Compare with ground truth ---------
        pysolar_radiation = []
        for date in tab_time_date:
            date = date.replace(tzinfo=timezone.utc)
            altitude_deg = get_altitude(lat_in[0], lon_in[0], date)
            current_radiation = pysolar.radiation.get_radiation_direct(date, altitude_deg)
            pysolar_radiation.append(current_radiation)
        # décalage sur le maximum,pas la meme allure .. 

        # plt.plot(tab_time_date, tab_direct, marker=".", label="tab_direct")
        # plt.plot(tab_time_date, max_theor_radiation, marker=".", label="max_theor_radiation")
        # plt.plot(tab_time_date, direct_incident, marker=".", label="direct_incident")
        # plt.plot(tab_time_date, direct_plane_projected, marker=".", label="direct_plane_projected")
        # plt.plot(tab_time_date, direct_plane_projected_masked, marker=".", label="direct_plane_projected_masked")
        # plt.plot(tab_time_date, tab_diffus, marker=".", label="diffus")
        # plt.plot(tab_time_date, pysolar_radiation, marker=".", label="pysolar_radiation")
        # plt.legend()
        # plt.show()
        
        
        if lnosof_surfex:
            # Now this is the normal case
            tab_direct = direct_plane_projected_masked
        else:
            # Not recommended
            # put the result back on the horizontal ; surfex will carry out the inverse operation when lnosof=f.
            tab_direct = direct_plane_projected_masked / np.cos(slope)

        if self.printmemory:
            print_used_memory()

        if return_angles:
            return tab_direct, tab_diffus, gamma, azimuth
        else:
            return tab_direct, tab_diffus

    # The two following routines from JM Willemet should be rewritten without the loops
    def upscale_tab(self, var, theshape):

        # array var can be considered at the same shape than the other arrays (simplify matricial traitment)
        bigvar = np.ma.zeros(theshape) + self.missingvalue

        shapein = len(var.shape)

        if len(theshape) == 2:
            for i in range(0, var.shape[0]):
                bigvar[:, i] = var[i]
        elif len(theshape) == 3:
            if shapein == 2:
                for ilat in range(0, var.shape[0]):
                    for ilon in range(0, var.shape[1]):
                        bigvar[:, ilat, ilon] = var[ilat, ilon]
            elif shapein == 1:
                for i in range(0, var.shape[0]):
                    bigvar[:, :, i] = var[i]
        elif len(theshape) == 4:
            for i in range(0, var.shape[0]):
                bigvar[:, :, :, i] = var[i]
        elif len(theshape) == 5:
            for ilat in range(0, var.shape[0]):
                for ilon in range(0, var.shape[1]):
                    bigvar[:, :, :, ilat, ilon] = var[ilat, ilon]
        else:
            print("error on indices in upscale_tab")

        return bigvar

    def upscale_tab_time(self, var, theshape):

        # array slope can be considered at the same shape than the other arrays (simplify matricial traitment)
        bigvar = np.ma.zeros(theshape)

        if len(theshape) == 2:
            for i in range(0, theshape[1]):
                bigvar[:, i] = var[:]
        elif len(theshape) == 3:
            for ilat in range(0, theshape[1]):
                for ilon in range(0, theshape[2]):
                    bigvar[:, ilat, ilon] = var[:]

        else:
            print("error on indices in upscale_tab_time")

        return bigvar

    def coszenith(self, tab_time_date, lat, lon, slope, aspect):
        """
        Cosinus of solar zenith angle

        :param tab_time_date: time
        :param lat: latitude
        :param lon: longitude
        :param slope: slope (degrees)
        :param aspect: aspect (degrees)
        """
        # TODO L. Roussel did not correct everything here

        julian_days = np.ones(tab_time_date.shape, 'f')
        decimal_hours = np.ones(tab_time_date.shape, 'f')
        for i in range(len(tab_time_date)):
            #            tab_time_date_2[i] = time_init + datetime.timedelta(seconds = tab_time_date[i])
            timetup = tab_time_date[i].timetuple()
            julian_days[i] = timetup[7]  # extract Julian day (integer)
            # L. Roussel: fix to decimal hour instead of integer hour
            decimal_hours[i] = timetup[3] + timetup[4] / 60 + timetup[5] / 3600  # extrac time in hour
            
        j = self.upscale_tab_time(julian_days, (tab_time_date.shape[0], 1))
        h = self.upscale_tab_time(decimal_hours, (tab_time_date.shape[0], 1))

        # all tabs now have the same dimension, which is that of tab_direct.
        # method from crocus meteo.f90 original file (v2.4)

        # variables needed for solar computations

        VSOL1 = 9.9,
        VSOL2 = 2.,
        VSOL3 = .986,
        VSOL4 = 100.,
        VSOL5 = 7.7,
        VSOL6 = 2.,
        VSOL7 = .4,
        VSOL8 = 80.,
        VSOL9 = 1370.,
        VSOL10 = 11.7,
        VSOL11 = 15.,
        VSOL12 = .001,
        DG2RD = math.pi / 180.
        NUZENI = 12.
        UH2LON = 15.
        NUH2M = 60.
        UEPSI = 0.001

        # Conversion of slope and aspect to rad:

        slope, aspect = slope * DG2RD, aspect * DG2RD

        # Time equation
        ZDT = VSOL1 * np.sin((VSOL2 * (VSOL3 * j - VSOL4)) * DG2RD)\
            - VSOL5 * np.sin((VSOL3 * j - VSOL6) * DG2RD)

        # sun declination
        ZSINDL = VSOL7 * np.sin(DG2RD * (VSOL3 * j - VSOL8))
        ZDELTA = np.arcsin(ZSINDL)

        # theoretical maximum radiation
        ZRSI0 = VSOL9 * (1. - ZSINDL / VSOL10)  # pylint: disable=possibly-unused-variable

        # solar angular time
        ZOMEGA = VSOL11 * (h - NUZENI + ZDT / NUH2M + lon / UH2LON) * DG2RD

        # solar angular height

        ZLAT = lat * DG2RD
        ZSINGA = np.sin(ZLAT) * ZSINDL + np.cos(ZLAT) * np.cos(ZDELTA) * np.cos(ZOMEGA)
        ZSINGA = np.where(ZSINGA < UEPSI, VSOL12, ZSINGA)
        ZGAMMA = np.arcsin(ZSINGA)

        return np.cos(math.pi / 2. - ZGAMMA)

    def directdiffus(self, SWglo, time, lat, lon, slope, aspect, site):
        """
        Separation of direct and diffuse  short wave radiations

        :param SWglo: global short wave
        :param time: time
        :param lat: latitude
        :param lon: longitude
        :param slope: The slope angle (degrees)
        :param aspect: The aspect (degrees)
        :param site: The site code for clear sky decomposition. Must be one of wfj, snb, swa, sap, sod, rme, oas, obs, ojp or cdp)

        :returns: direct short wave, diffuse short wave
        """

        # Clear sky decomposition by Marie Dumont
        if site == "wfj":
            a = -2.449042
            b = 5.639542
            c = -4.745762
            d = -0.555133
        elif site == "snb":
            a = -2.492804
            b = 5.743704
            c = -4.870253
            d = -0.464349
        elif site == "swa":
            a = -2.481555
            b = 5.717005
            c = -4.838604
            d = -0.484631
        elif site == "sap":
            a = -1.444924
            b = 3.506267
            c = -3.342956
            d = 0.050087
        elif site == "sod":
            a = -2.171899
            b = 5.040324
            c = -4.335702
            d = -0.256246
        elif site == "rme":
            a = -2.375236
            b = 5.481626
            c = -4.653772
            d = -0.421807
        elif site == "oas":
            a = -2.252006
            b = 5.208891
            c = -4.443042
            d = -0.339813
        elif site == "obs":
            a = -2.254170
            b = 5.213592
            c = -4.446855
            d = -0.340464
        elif site == "ojp":
            a = -2.250787
            b = 5.205990
            c = -4.440420
            d = -0.339316
        elif site == "cdp":
            a = -2.243613
            b = 5.199838
            c = -4.472389
            d = -0.276815

        coszenith = self.coszenith(time, lat, lon, slope, aspect)[:, 0]

        ratio = np.exp(a * (coszenith**3) + b * (coszenith**2) + c * coszenith + d)

        SWdif = np.where(ratio <= 1, ratio * SWglo, SWglo)
        SWdir = SWglo - SWdif

#         for hour in range(1, 24):
#             print hour, coszenith[ndays * 24 + hour - 1], ratio[ndays * 24 + hour - 1], SWdir[ndays * 24 + hour - 1], SWdif[ndays * 24 + hour - 1], SWglo[ndays * 24 + hour - 1]

        return SWdir, SWdif
