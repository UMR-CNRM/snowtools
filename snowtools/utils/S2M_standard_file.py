# -*- coding: utf-8 -*-

'''
Created on 20 août 2019

@author: lafaysse
'''

import datetime
import os

import netCDF4
import configparser
import numpy as np

from snowtools.utils.FileException import VarNameException, UnknownGridTypeException, FileNameException,\
    TimeUnitsException
from snowtools.utils.infomassifs import infomassifs
from snowtools.utils.git import git_infos
from snowtools.DATA import SNOWTOOLS_DIR


class _StandardNC(netCDF4.Dataset):
    '''
    abstract class for S2M netcdf file based on ACDD conventions
    '''

    def read_config(self, section):
        config = configparser.RawConfigParser()
        rootdir = SNOWTOOLS_DIR
        confile = os.path.join(rootdir, 'conf', 'S2M_Standard_Metadata.ini')
        if os.path.isfile(confile):
            config.read(confile)
            myconfig = dict(config.items(section))
            # Add conf variables into the global attribute
            for key, value in list(myconfig.items()):
                setattr(self, key, value)
        else:
            raise FileNameException(confile)

    def GlobalAttributesReanalysis(self):
        self.read_config('DEFAULT')
        self.title = 'S2M reanalysis'
        self.summary = 'This file takes part from a 61-years reanalysis of meteorological and snow conditions in the' \
                       ' French Alps, Pyrenees and Corsica from 1958 to 2020. The simulations are performed over' \
                       ' relatively homogeneous units designed to represent the main drivers of the spatial' \
                       ' variability observed in mountain ranges (elevation, slope and aspect). The meteorological' \
                       ' reanalysis is performed by the SAFRAN system, which adjusts a guess from a Numerical Weather' \
                       ' Prediction model (ERA-40 reanalysis from 1958 to 2002, ARPEGE from 2002 to 2018) with the' \
                       ' best possible set of available in-situ meteorological observations. SAFRAN outputs are used' \
                       ' to force the Crocus detailed snowpack model within the land surface scheme SURFEX/ISBA.' \
                       ' This provides the evolution of the snowpack and the associated avalanche hazard accounting' \
                       ' for the main physical processes involved in a multilayer snowpack.'

        self.id = 's2m_reanalysis_2022.1'

        self.references = 'https://doi.org/10.5194/essd-14-1707-2022'

        self.date_created = datetime.datetime.today().replace(second=0, microsecond=0).isoformat()

        self.platform = 'MODELS_ANALYSES.REANALYSIS_MODELS'
        self.platform_vocabulary = 'GCMD,AERIS'

        self.contributor_name = 'Matthieu Vernay ; Matthieu Lafaysse'
        self.contributor_role = 'Matthieu Vernay collected the input data and ran the SAFRAN reanalysis ;' \
                                ' Matthieu Lafaysse ran the SURFEX-ISBA/Crocus simulations.'

    def GlobalAttributesOper(self):
        self.title = 'S2M operational chain'
        self.summary = 'This file takes part from the operation modelling chain of meteorological and snow conditions' \
                       ' in the French Alps, Pyrenees and Corsica. The simulations are performed over relatively' \
                       ' homogeneous units designed to represent the main drivers of the spatial variability observed' \
                       ' in mountain ranges (elevation, slope and aspect). The meteorological analysis and forecast' \
                       ' is produced by the SAFRAN system, which adjusts a guess from a Numerical Weather Prediction' \
                       ' model (ARPEGE or PEARP) with the best possible set of available in-situ meteorological' \
                       ' observations. SAFRAN outputs are used to force the Crocus detailed snowpack model within' \
                       ' the land surface scheme SURFEX/ISBA. This provides the evolution of the snowpack and the' \
                       ' associated avalanche hazard accounting for the main physical processes involved in a' \
                       ' multilayer snowpack.'

        # For reproductibility tests, the hour is fixed in the operational chain
        # In future versions it would be nice to use the operational rundate.
        self.date_created = datetime.datetime.today().replace(hour=12, minute=0, second=0, microsecond=0).isoformat()

        self.contributor_name = 'Matthieu Vernay ; Matthieu Lafaysse; Emmanuel Riggi-Carrolo'
        self.contributor_role = 'Matthieu Vernay develops the SAFRAN analysis and forecast system ; Matthieu Lafaysse' \
                                ' develops the SURFEX-ISBA/Crocus snowpack model ;' \
                                ' Emmanuel Riggi-Carrolo set up the operational environment.'

    def GlobalAttributes(self):

        time = self.readtime()
        reanalysis = (time[-1] - time[0]) > datetime.timedelta(days=360)

        if reanalysis:
            self.GlobalAttributesReanalysis()
        else:
            self.GlobalAttributesOper()

        self.keywords = 'MOUNTAINS'
        self.keywords_vocabulary = 'GCMD'
        self.Conventions = 'ACDD-1.3,CF-1.7'
        self.naming_authority = 'fr.umr-cnrm.cen'
        self.acknowledgement = 'Many contributors have developed and maintained the codes until today. The full list' \
                               ' of contributors can be found in the associated publications. CNRM is a research unit' \
                               ' of Météo-France and CNRS. CNRM/CEN, is part of LabEX OSUG@2020 (ANR10 LABX56).'
        self.standard_name_vocabulary = 'CF Standard Name Table v67'
        self.creator_name = "CNRM / Centre d'Etudes de la Neige"
        self.creator_type = 'institution'
        self.institution = "CNRM / Centre d'Etudes de la Neige"
        self.publisher_name = "CNRM / Centre d'Etudes de la Neige"
        self.publisher_type = 'institution'

        self.project = 'S2M'

        self.processing_level = 'L4'

        self.comment = 'These data are provided on a semi-distributed grid based on relatively homogeneous areas' \
                       ' called massifs where the variability of meteorological and snow conditions are assumed' \
                       ' to depend only on elevation, aspect and slope. A metadata shapefile is associated with' \
                       ' these data to describe the massif names and contours.'

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

        self.time_coverage_start = time[0].isoformat()
        self.time_coverage_end = time[-1].isoformat()
        self.time_coverage_duration = str(time[-1] - time[0])
        if len(time) > 1:
            self.time_coverage_resolution = str(time[1] - time[0])

        self.snowtools_commit = git_infos(os.path.dirname(os.path.abspath(__file__))).get_commit(default='')

    def standard_names(self):
        return dict(ZS = 'surface_altitude',
                    time       = 'time'
                    )

    def readtime(self):
        # Vérification du nom de la variable
        if "time" not in list(self.variables.keys()):
            raise VarNameException("time", self.filepath())

        time = self.variables["time"]

        if not hasattr(time, 'units'):
            raise TimeUnitsException(self.filepath())

        if netCDF4.__version__ >= '1.4.2':
            return np.array(netCDF4.num2date(time[:], time.units, only_use_cftime_datetimes=False))
        elif netCDF4.__version__ >= '1.4.0':
            return np.array(netCDF4.num2date(time[:], time.units, only_use_cftime_datetimes=False,
                                             only_use_python_datetimes=True))
        else:
            return np.array(netCDF4.num2date(time[:], time.units))

    def get_coord(self):

        latname = self.getlatname()
        lonname = self.getlonname()
        altiname = "ZS"
        xname, yname = self.getcoordname()

        if (set([latname, lonname]).issubset(set(self.variables.keys()))):
            lat, lon = self.variables[latname], self.variables[lonname]
        else:
            if (set([xname, yname]).issubset(set(self.variables.keys()))):
                lat, lon = self.xy2latlon(self.variables[xname], self.variables[yname])
            else:
                try:
                    self.addCoord()
                    lat, lon = self.variables[latname], self.variables[lonname]
                except Exception:
                    raise VarNameException(latname, self.filepath())

        if altiname in self.variables.keys():
            alti = self.variables[altiname]
        else:
            alti = np.nan

        return lat, lon, alti

    def xy2latlon(self, x, y):
        # TO BE CHANGED
        from pyproj import Proj, transform
        from bronx.datagrip.namelist import NamelistParser
        n = NamelistParser()
        N = n.parse("OPTIONS.nam")
        gridtype = N['NAM_PGD_GRID'].CGRID
        if gridtype == "IGN":
            projtype = N['NAM_IGN'].CLAMBERT
            if projtype == 'L93':
                epsg = 'epsg:2154'
            else:
                raise UnknownGridTypeException(gridtype, projtype)
        elif gridtype == "CONF PROJ":
            if N['NAM_CONF_PROJ'].XRPK == 0:
                epsg = 'epsg:23032'
            else:
                projtype = 'No Mercator: XRPK != 0 '
                raise UnknownGridTypeException(gridtype, projtype)
        else:
            raise UnknownGridTypeException(gridtype, "")

        inProj = Proj(epsg)
        outProj = Proj('epsg:4326')

        XX, YY = np.meshgrid(np.array(x), np.array(y))

        lon, lat = transform(inProj, outProj, XX, YY)

        return lat, lon

    def addCoord(self):
        '''Routine to add coordinates in the forcing file for the SAFRAN massifs'''
        INFOmassifs = infomassifs()
        dicLonLat = INFOmassifs.getAllMassifLatLon()
        if netCDF4.__version__ >= '1.4.2':
            massifnumber = self.variables['massif_number'][:]
        else:
            massifnumber = self.variables['massif_number']

        lat = np.empty(massifnumber.shape, np.float)
        lon = np.empty(massifnumber.shape, np.float)

        dimension = self.variables['ZS'].dimensions
        varFillValue = -9999999.

        for point in range(0, len(massifnumber)):
            lonlat = dicLonLat[massifnumber[point]]
            lat[point] = lonlat[1]
            lon[point] = lonlat[0]

        var = self.createVariable("LAT", np.float, dimension, fill_value=varFillValue)
        setattr(var, 'long_name', 'latitude')
        setattr(var, 'units', 'degrees_north')
        var[:] = lat
        var = self.createVariable("LON", np.float, dimension, fill_value=varFillValue)
        setattr(var, 'long_name', 'longitude')
        setattr(var, 'units', 'degrees_east')
        var[:] = lon

        return lat, lon

    def special_long_names(self):

        massifname = self.getmassifname()
        longnames = dict()
        longnames[massifname] = 'SAFRAN massif number. Metadata are provided in the associated shapefile.'

        return longnames

    def add_standard_names(self):

        dicstd = self.standard_names()
        diclong = self.special_long_names()
        for varname in self.variables.keys():
            if varname in dicstd.keys():
                self.variables[varname].standard_name = dicstd[varname]
            if varname in diclong.keys():
                self.variables[varname].long_name = diclong[varname]


class StandardSAFRANetMET(_StandardNC):

    def GlobalAttributes(self):
        super(StandardSAFRANetMET, self).GlobalAttributes()
        self.read_config('StandardSAFRANetMET')

    def getlatname(self):
        return 'LAT'

    def getlonname(self):
        return 'LON'

    def getcoordname(self):
        return 'x', 'y'

    def standard_names(self):

        dicfather = super(StandardSAFRANetMET, self).standard_names()

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


class StandardSAFRAN(StandardSAFRANetMET):

    def GlobalAttributes(self):
        super(StandardSAFRAN, self).GlobalAttributes()
        self.read_config('StandardSAFRAN')
        self.title = self.title + ": meteorological variables"
        self.summary = self.summary + ' This file provides the SAFRAN meteorological fields'
        self.keywords = self.keywords + ',INCOMING SOLAR RADIATION,LONGWAVE RADIATION,SHORTWAVE RADIATION,AIR' \
                                        ' TEMPERATURE,SURFACE TEMPERATURE,ABSOLUTE HUMIDITY,RELATIVE HUMIDITY,' \
                                        'WIND DIRECTION,WIND SPEED,SURFACE WINDS,RAIN,LIQUID PRECIPITATION,' \
                                        'HOURLY PRECIPITATION AMOUNT,SOLID PRECIPITATION'

    def getmassifname(self):
        return 'massif_number'


class StandardCDP(StandardSAFRANetMET):

    def GlobalAttributes(self):
        super(StandardCDP, self).GlobalAttributes()
        self.read_config('StandardCDP')
        self.title = self.title + ": meteorological variables"
        self.summary = self.summary + ' This file provides the Col de Porte observed meteorological fields, completed' \
                                      ' with SAFRAN'
        self.keywords = self.keywords + ',INCOMING SOLAR RADIATION,LONGWAVE RADIATION,SHORTWAVE RADIATION,AIR' \
                                        ' TEMPERATURE,SURFACE TEMPERATURE,ABSOLUTE HUMIDITY,RELATIVE HUMIDITY,' \
                                        'WIND DIRECTION,WIND SPEED,SURFACE WINDS,RAIN,LIQUID PRECIPITATION,HOURLY' \
                                        ' PRECIPITATION AMOUNT,SOLID PRECIPITATION'


class StandardPROSNOW(StandardSAFRAN):

    def GlobalAttributes(self):
        super(StandardPROSNOW, self).GlobalAttributes()
        self.read_config('StandardPROSNOW')
        self.title = "SAFRAN: meteorological variables"
        self.summary = 'This file is a meteorological forcing file generated by the SAFRAN system. Is is based on the' \
                       ' 4 days forecasts of the 35 members french Ensemble Forecasting system PEARP with a 6-hours' \
                       ' temporal resolution. It provides at an hourly time step the main meteorological parameters' \
                       ' to force a snowpack model on all of the PROSNOW stations except the french ones.'
        self.platform = 'MODELS_ANALYSES.MODELS'
        self.comment = "This file provides data for various stations of the PROSNOW project, the mapping between the" \
                       " station numbers and the station names is given by the following dictionary :" \
                       " {1: 'ZAMG-11706', 2: 'ZAMG-11803', 3: 'ZAMG-11804', 4: 'ZAMG-14631', 5: 'ZAMG-14701'," \
                       " 6: 'ZAMG-14812', 7: 'ZAMG-17301', 8: 'ZAMG-17315', 9: 'dwd_gar_1995_20'," \
                       " 10: 'dwd_zug_1995_20', 11: 'lwd_ost_1995_20', 12: 'lwd_pla_2012_20', 13: '*CHU'," \
                       " 14: 'CMA1', 15: 'CMA2', 16: '*DAV', 17: 'DAV3', 18: 'DAV4', 19: 'DAV5', 20: 'ELA1'," \
                       " 21: 'ELA2', 22: 'PMA1', 23: 'PMA2', 24: 'ROT2', 25: 'ROT3', 26: 'TAM3', 27: '*WFJ'," \
                       " 28: 'WFJ1', 29: 'WFJ2', 30: 'BER1', 31: 'BER2', 32: 'BER3', 33: 'BEV1', 34: 'BEV2'," \
                       " 35: 'GESS2', 36: 'KES1', 37: 'KES2', 38: 'LAG1', 39: 'LAG2', 40: 'OFE1', 41: 'OFE2'," \
                       " 42: 'VALL2', 43: 'ZNZ1', 44: 'ZNZ2', 45: 'ZNZ3', 46: 'Bruneck', 47: 'Deutschnofen'," \
                       " 48: 'Merbalm', 49: 'PizlaIla', 50: 'Rittnerhorn', 51: 'SterzingFlughaf'}"

    def addCoord(self):
        '''Routine to add coordinates in the forcing file for the SAFRAN massifs'''
        massifnumber = self.variables['massif_number']

        lat = np.empty(massifnumber.shape, np.float)
        lon = np.empty(massifnumber.shape, np.float)

        dimension = self.variables['ZS'].dimensions
        varFillValue = -9999999.

        var = self.createVariable("LAT", np.float, dimension, fill_value=varFillValue)
        setattr(var, 'long_name', 'latitude')
        setattr(var, 'units', 'degrees_north')
        var[:] = lat
        var = self.createVariable("LON", np.float, dimension, fill_value=varFillValue)
        setattr(var, 'long_name', 'longitude')
        setattr(var, 'units', 'degrees_east')
        var[:] = lon

        return lat, lon


class StandardCROCUS(_StandardNC):

    def GlobalAttributes(self, surfex_commit=None):
        super(StandardCROCUS, self).GlobalAttributes()
        self.read_config('StandardCROCUS')
        self.title = self.title + ": snow variables"
        self.summary = self.summary + ' This file provides the snowpack properties of the Crocus model.'
        self.keywords = self.keywords + ',SNOW WATER EQUIVALENT,SNOW,ALBEDO,AVALANCHE,FREEZE/THAW,SNOW COVER,' \
                                        'SNOW DENSITY,SNOW DEPTH,SNOW ENERGY BALANCE,SNOW MELT,SNOW WATER EQUIVALENT,' \
                                        'SNOW/ICE TEMPERATURE'

        if surfex_commit:
            self.surfex_commit = surfex_commit

    def getlatname(self):
        return 'latitude'

    def getlonname(self):
        return 'longitude'

    def getcoordname(self):
        return 'xx', 'yy'

    def getmassifname(self):
        return 'massif_num'

    def getsoilgrid(self):
        from bronx.datagrip.namelist import NamelistParser
        n = NamelistParser()
        N = n.parse("OPTIONS.nam")
        if 'XSOILGRID' in N['NAM_ISBA']:
            bottom = list(map(float, N['NAM_ISBA'].XSOILGRID))
            top = [0] + bottom[:-1]
            self.soilgrid = (np.array(top) + np.array(bottom)) / 2.
        else:
            from snowtools.utils.prosimu import prosimu
            if os.path.isfile("PGD.nc"):
                pgd = prosimu("PGD.nc")
                nlayers = pgd.read("GROUND_LAYER")
                bottom = []
                for layer in range(1, nlayers[0] + 1):
                    bottom.append(pgd.read('SOILGRID' + str(layer))[0])
                top = [0] + bottom[:-1]
                self.soilgrid = (np.array(top) + np.array(bottom)) / 2.

                pgd.close()

    def soil_long_names(self, varname):
        import re
        layer = int(re.search(r'\d+', varname).group()) - 1

        if not hasattr(self, 'soilgrid'):
            self.getsoilgrid()

        if hasattr(self, 'soilgrid'):
            return '(depth %.4f m)' % self.soilgrid[layer]
        else:
            return ''

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

    def add_standard_names(self):
        super(StandardCROCUS, self).add_standard_names()
        if os.path.isfile("OPTIONS.nam"):
            for varname in self.variables.keys():
                if varname[0:2] in ['TG', 'WG']:
                    if hasattr(self.variables[varname], 'long_name'):
                        self.variables[varname].long_name = self.variables[varname].long_name + \
                                                            self.soil_long_names(varname)
