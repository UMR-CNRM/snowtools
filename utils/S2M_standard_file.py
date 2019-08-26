# -*- coding: utf-8 -*-

'''
Created on 20 août 2019

@author: lafaysse
'''

import netCDF4
import datetime
import numpy as np
from utils.FileException import VarNameException


class _StandardNC(netCDF4.Dataset):
    '''
    abstract class for S2M netcdf file based on ACDD conventions
    '''

    def GlobalAttributes(self):

        self.title = 'S2M reanalysis'
        self.summary = 'This file takes part from a 60-years reanalysis of meteorological and snow conditions in the French Alps, Pyrenees and Corsica from 1958 to 2018. The simulations are performed over relatively homogeneous units designed to represent the main drivers of the spatial variability observed in mountain ranges (elevation, slope and aspect). The meteorological reanalysis is performed by the SAFRAN system, which adjusts a guess from a Numerical Weather Prediction model (ERA-40 reanalysis from 1958 to 2002, ARPEGE from 2002 to 2018) with the best possible set of available in-situ meteorological observations. SAFRAN outputs are used to force the Crocus detailed snowpack model within the land surface scheme SURFEX/ISBA. This provides the evolution of the snowpack and the associated avalanche hazard accounting for the main physical processes involved in a multilayer snowpack.'
        self.keywords = 'MOUNTAINS'
        self.keywords_vocabulary = 'GCMD'
        self.Conventions = 'ACDD-1.3,CF-1.7'
        self.id = 'reanalysis_2019'
        self.naming_authority = 'fr.umr-cnrm.cen'
        self.acknowledgement = 'Many contributors have developed and maintained the codes until today. The full list of contributors can be found in the associated publications. CNRM is a research unit of Météo-France and CNRS. CNRM/CEN, is part of LabEX OSUG@2020 (ANR10 LABX56).'
        self.standard_name_vocabulary = 'SURFEX (non-standard vocabulary)'
        self.date_created = datetime.datetime.today().replace(second=0, microsecond=0).isoformat()
        self.creator_name = "CNRM / Centre d'Etudes de la Neige"
        self.creator_type = 'institution'
        self.creator_url = 'https://www.umr-cnrm.fr/spip.php?rubrique85'
        self.creator_email = 's2m.reanalysis@meteo.fr'
        self.institution = "CNRM / Centre d'Etudes de la Neige"
        self.publisher_name = "CNRM / Centre d'Etudes de la Neige"
        self.publisher_type = 'institution'
        self.publisher_url = 'https://www.umr-cnrm.fr/spip.php?rubrique85'
        self.publisher_email = 's2m.reanalysis@meteo.fr'

        self.project = 'S2M'
        self.platform = 'MODELS_ANALYSES.REANALYSIS_MODELS'
        self.platform_vocabulary = 'GCMD,AERIS'
        self.processing_level = 'L4'

        lat, lon, alti = self.get_coord()

        self.geospatial_lat_min = np.min(lat)
        self.geospatial_lat_max = np.max(lat)
        self.geospatial_lon_min = np.min(lon)
        self.geospatial_lon_max = np.max(lon)
        self.geospatial_lat_units = "degree_north"
        self.geospatial_lon_units = "degree_east"
        self.geospatial_vertical_min = np.min(alti)
        self.geospatial_vertical_max = np.max(alti)
        self.geospatial_vertical_units = 'm'
        self.geospatial_vertical_positive = 'up'

        time = self.readtime()

        self.time_coverage_start = time[0].isoformat()
        self.time_coverage_end = time[-1].isoformat()
        self.time_coverage_duration = str(time[-1] - time[0])
        self.time_coverage_resolution = str(time[1] - time[0])
        self.contributor_name = 'Matthieu Vernay ; Matthieu Lafaysse'
        self.contributor_role = 'Matthieu Vernay collected the input data and ran the SAFRAN reanalysis ; Matthieu Lafaysse ran the SURFEX-ISBA/Crocus simulations.'
        self.references = 'https://doi.org/10.1175/2008JAMC1808.1;https://doi.org/10.1175/2009JAMC1810.1;new reference to come in 2020'

    def standard_names(self):
        return dict(ZS = 'surface_altitude',
                    time       = 'time'
                    )

    def readtime(self):
        # Vérification du nom de la variable
        if "time" not in list(self.variables.keys()):
            raise VarNameException("time", self.path)

        time = self.variables["time"]

        return np.array(netCDF4.num2date(time[:], time.units))

    def get_coord(self):

        latname = self.getlatname()
        lonname = self.getlonname()
        altiname = "ZS"

        if latname not in list(self.variables.keys()):
            raise VarNameException(latname, self.path)

        if lonname not in list(self.variables.keys()):
            raise VarNameException(lonname, self.path)

        return self.variables[latname], self.variables[lonname], self.variables[altiname]

    def add_standard_names(self):

        dicstd = self.standard_names()
        for varname in self.variables.keys():
            if varname in dicstd.keys():
                self.variables[varname].standard_name = dicstd[varname]


class StandardSAFRAN(_StandardNC):

    def GlobalAttributes(self):
        super(StandardSAFRAN, self).GlobalAttributes()
        self.title = self.title + ": meteorological variables"
        self.source = 'SAFRAN git tag ' + self.id
        self.summary = self.summary + ' This yearly file provides the SAFRAN meteorological fields'
        self.keywords = self.keywords + ',INCOMING SOLAR RADIATION,LONGWAVE RADIATION,SHORTWAVE RADIATION,AIR TEMPERATURE,SURFACE TEMPERATURE,ABSOLUTE HUMIDITY,RELATIVE HUMIDITY,WIND DIRECTION,WIND SPEED,SURFACE WINDS,RAIN,LIQUID PRECIPITATION,HOURLY PRECIPITATION AMOUNT,SOLID PRECIPITATION'

    def getlatname(self):
        return 'LAT'

    def getlonname(self):
        return 'LON'

    def standard_names(self):

        dicfather = super(StandardSAFRAN, self).standard_names()

        dicson = dict(PSurf = 'surface_air_pressure',
                      Tair  = 'air_temperature',
                      Qair  = 'specific_humidity',
                      Wind_DIR = 'wind_from_direction',
                      Wind = 'wind_speed',
                      Rainf = 'rainfall_flux',
                      Snowf = 'snowfall_flux',
                      LWdown = 'surface_downwelling_longwave_flux_in_air',
                      DIR_SWdown = 'surface_direct_downwelling_shortwave_flux_in_air',
                      SCA_SWdown = 'surface_diffuse_downwelling_shortwave_flux_in_air',
                      NEB = 'cloud_area_fraction',
                      HUMREL = 'relative_humidity',
                      CO2air = 'mass_concentration_of_carbon_dioxide_in_air',
                      isoZeroAltitude = 'freezing_level_altitude',
                      LAT        = 'latitude',
                      LON        = 'longitude',
                      )

        dicfather.update(dicson)

        return dicfather


class StandardCROCUS(_StandardNC):

    def GlobalAttributes(self):
        super(StandardCROCUS, self).GlobalAttributes()
        self.title = self.title + ": snow variables"
        self.source = 'SAFRAN and SURFEX/ISBA-Crocus git tag ' + self.id
        self.summary = self.summary + ' This yearly file provides the snowpack properties of the Crocus model.'
        self.keywords = self.keywords + ',SNOW WATER EQUIVALENT,SNOW,ALBEDO,AVALANCHE,FREEZE/THAW,SNOW COVER,SNOW DENSITY,SNOW DEPTH,SNOW ENERGY BALANCE,SNOW MELT,SNOW WATER EQUIVALENT,SNOW/ICE TEMPERATURE'

    def getlatname(self):
        return 'latitude'

    def getlonname(self):
        return 'longitude'

    def standard_names(self):

        dicfather = super(StandardCROCUS, self).standard_names()

        dicson = dict(WSN_T_ISBA = 'surface_snow_amount',
                      DSN_T_ISBA = 'thickness_of_snowfall_amount',
                      TS_ISBA    = 'surface_temperature',
                      TALB_ISBA  = 'surface_albedo',
                      RN_ISBA    = 'surface_net_downward_radiative_flux',
                      H_ISBA     = 'surface_upward_sensible_heat_flux',
                      LE_ISBA    = 'surface_upward_latent_heat_flux',
                      RAINF_ISBA = 'rainfall_flux',
                      SWD_ISBA   = 'surface_downwelling_shortwave_flux_in_air',
                      SWU_ISBA   = 'surface_upwelling_shortwave_flux_in_air',
                      LWD_ISBA   = 'surface_downwelling_longwave_flux_in_air',
                      LWU_ISBA   = 'surface_upwelling_longwave_flux_in_air',
                      RUNOFF_ISBA= 'surface_runoff_flux',
                      DRAIN_ISBA = 'subsurface_runoff_flux',
                      EVAP_ISBA  = 'surface_water_evaporation_flux',
                      TG1        = 'soil_temperature',
                      TG4        = 'soil_temperature',
                      WG1        = 'liquid_water_content_of_soil_layer',
                      WGI1       = 'frozen_water_content_of_soil_layer',
                      latitude   = 'latitude',
                      longitude  = 'longitude',
                      )

        dicfather.update(dicson)

        return dicfather
