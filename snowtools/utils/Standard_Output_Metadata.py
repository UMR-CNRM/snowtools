# -*- coding: utf-8 -*-
"""
Xarray accessorsused to set standard metadata attributes
--------------------------------------------------------

Following the xarray project's recomandations, it is based on the use of accessor :
https://tutorial.xarray.dev/advanced/accessors/01_accessor_examples.html

This accessor is automatically made available when you import ``snowtools.utils.xarray_snowtools``.

Usage examples
^^^^^^^^^^^^^^

Adding standard Crocus attributes the 'PRO_gdesRousses_2019-2020.nc' file of the snowtools testbase :

.. code-block:: python

    import xarray as xr
    from snowtools.utils import xarray_snowtool

    ds = xr.open_dataset('PRO_gdesRousses_2019-2020.nc', engine='snowtools')
    ds.crocus.GlobalAttributes()
    ds.crocus.add_coord()  # Optional

"""

import os
import sys
import re
import datetime
import configparser
import importlib

from pyproj import Transformer
import xarray as xr
import numpy as np

from bronx.datagrip.namelist import NamelistParser
from bronx.stdtypes.date import Date
from snowtools.utils.infomassifs import infomassifs


@xr.register_dataset_accessor("standard_nc")
class StandardNC:

    def __init__(self, xarray_ds):
        self.ds = xarray_ds

    def load_conf(self, inifile="Standard_Output_Metadata.ini"):
        """
        Load standard configuraiton file.
        """
        self.conf = configparser.ConfigParser()

        # Load from vortex distribution
        with importlib.resources.open_text(
            "snowtools.utils",
            "Standard_Output_Metadata.ini",
        ) as fh:
            self.conf.read_file(fh)

    def read_constant_attributes(self, section: str):
        """Load attributes of section "section" from the standard configuration file."""

        self.load_conf()
        for key, value in self.conf.items(section):
            if isinstance(value, str):
                value = " ".join(value.split())
            # TODO : check what to do in case of an already set atrtibute (overwrite or extend ?)
            if key in self.ds.attrs.keys():
                self.ds.attrs[key] + value
            else:
                self.ds.attrs[key] = value

    def GlobalAttributes(self, product=None, **additionnal_attributes):
        """
        Global attributes following ACDD conventions.
        The dataset must have a "time" attribute.

        TODO : deal with the possible list of "product" values.
        Currently :
        * S2MReanalysis
        * S2MOper
        """
        time = self.ds.time

        self.ds.attrs["date_created"] = datetime.datetime.now().replace(
            second=0, microsecond=0
        ).isoformat()

        # TODO : find a better way to set the preoduct-dependent constant_attributes to take into account
        # the edelweiss products
        if product is not None:
            self.read_constant_attributes(product)

        try:
            login = os.getlogin()
            self.ds.attrs['contributor_name'] = login
            self.ds.attrs['contributor_role'] = f" {login} ran this simulation\n"
        except OSError:
            pass

        self.get_coord()

        # temporal coverage
        self.ds.attrs["time_coverage_start"] = time.data[0].astype(str)
        self.ds.attrs["time_coverage_end"] = time.data[-1].astype(str)
        duration = Date(time.data[-1].astype(str)) - Date(time.data[0].astype(str))
        self.ds.attrs["time_coverage_duration"] = duration.hms
        if len(time) > 1:
            resolution = Date(time.data[1].astype(str)) - Date(time.data[0].astype(str))
            self.ds.attrs["time_coverage_resolution"] = resolution.hms

        # system env
        self.ds.attrs["python_version"] = sys.version
        self.ds.attrs["python_binary"] = os.path.realpath(sys.executable)

        # Standard attributs
        self.add_standard_names()

        # additional attributs
        for name, value in additionnal_attributes.items():
            self.ds.attrs[name] = value

        return self.ds

    @staticmethod
    def standard_names():
        return dict(ZS="surface_altitude", time="time")

    def special_long_names(self, product=None):
        # TODO : trouver une solution plus élégante pour gérer ça
        if product is not None:
            if "S2M" in product:
                massifname = self.getmassifname
                return StandardS2M.special_long_names(massifname)
        else:
            return dict()

    def add_standard_names(self):
        """
        Apply "standard_name" and "long_name" dictionaries to variables
        """
        std = self.standard_names()
        long = self.special_long_names()

        for var_name, da in self.ds.variables.items():
            if var_name in std:
                da.attrs["standard_name"] = std[var_name]
            if var_name in long:
                da.attrs["long_name"] = long[var_name]

        return self.ds

    def apply_to_all(self, func, **kwargs):
        """
        Optional but usefull method to apply a function `func` to all variables
        of the dataset.

        Parameters
        ----------
        :param func : Function to apply to all variables (its arguments can be provided as `kwargs`)
        :type func: callable
        """
        new_vars = {
            name: func(da, **kwargs) for name, da in self.ds.data_vars.items()
        }
        self.ds.assign(**new_vars)

    def get_coord(self):
        pass


class StandardS2M:

    def special_long_names(self, massifname):
        """
        Return a massif-specific {variable_name: long_name} dictionary
        """
        longnames = dict()
        longnames[massifname] = 'SAFRAN massif number. Metadata are provided in the associated shapefile.'

        return longnames

    def xy2latlon(self, xvar, yvar):
        """
        Convert from an x/y projection to lat/lon by reading the info in the SURFEX namelist
        """
        # Read grid type in the namelist
        n = NamelistParser()
        N = n.parse("OPTIONS.nam")
        gridtype = N["NAM_PGD_GRID"].CGRID

        if gridtype == "IGN":
            projtype = N["NAM_IGN"].CLAMBERT
            if projtype == "L93":
                epsg_src = "epsg:2154"
            else:
                raise ValueError(f"IGN grid type not supported: {projtype}")
        elif gridtype == "CONF PROJ":
            if N["NAM_CONF_PROJ"].XRPK == 0:
                epsg_src = "epsg:23032"
            else:
                raise ValueError("CONF PROJ with XRPK != 0 not supported")
        else:
            raise ValueError(f"Unknown grid type: {gridtype}")

        transformer = Transformer.from_crs(epsg_src, "epsg:4326", always_xy=True)

        #  xvar/yvar must be 2D DataArrays
        x = np.asarray(xvar)
        y = np.asarray(yvar)

        XX, YY = np.meshgrid(x, y, indexing="ij")
        lon2d, lat2d = transformer.transform(XX, YY)

        lat = xr.DataArray(lat2d, dims=xvar.dims, attrs={"standard_name": "latitude"})
        lon = xr.DataArray(lon2d, dims=yvar.dims, attrs={"standard_name": "longitude"})
        return lat, lon

    def get_coord(self):
        # Noms attendus (à définir dans les méthodes getlatname/getlonname/getcoordname des accesseurs spécifiques)
        latname = getattr(self, "getlatname", None)
        lonname = getattr(self, "getlonname", None)
        xname, yname = getattr(self, "getcoordname", (None, None))
        altiname = "ZS"

        if latname is None or lonname is None:
            raise AttributeError(
                "The Dataset must have 'getlatname' and 'getlonname' attributes."
            )

        # lat/lon already present
        if set([latname, lonname]).issubset(set(self.ds.variables)):
            lat = self.ds[latname]
            lon = self.ds[lonname]

        elif set([xname, yname]).issubset(set(self.ds.variables)):
            lat, lon = self.xy2latlon(self.ds[xname], self.ds[yname])
            self.ds["LAT"] = (lat.dims, lat.values)
            self.ds["LON"] = (lon.dims, lon.values)

        else:
            try:
                # Try to add coordinates
                self.addCoord()
                lat = self.ds[latname]
                lon = self.ds[lonname]
            except Exception as exc:
                raise KeyError(f"Impossible to get lat/lon coordinates: {exc}")

        alti = self.ds[altiname] if altiname in self.ds.variables else np.nan

        # Update geospatil attributes
        self.ds.attrs["geospatial_lat_min"] = float(lat.min())
        self.ds.attrs["geospatial_lat_max"] = float(lat.max())
        self.ds.attrs["geospatial_lon_min"] = float(lon.min())
        self.ds.attrs["geospatial_lon_max"] = float(lon.max())
        self.ds.attrs["geospatial_lat_units"] = "degree_north"
        self.ds.attrs["geospatial_lon_units"] = "degree_east"
        self.ds.attrs["geospatial_vertical_min"] = (
            float(alti.min()) if np.isfinite(alti).any() else np.nan
        )
        self.ds.attrs["geospatial_vertical_max"] = (
            float(alti.max()) if np.isfinite(alti).any() else np.nan
        )
        self.ds.attrs["geospatial_vertical_units"] = "m"
        self.ds.attrs["geospatial_vertical_positive"] = "up"

    def addCoord(self):

        INFOmassifs = infomassifs()
        dicLonLat = INFOmassifs.getAllMassifLatLon()

        massif_number = self.ds["massif_number"].values

        lat = np.empty(massif_number.shape, np.float64)
        lon = np.empty(massif_number.shape, np.float64)

        for i, num in enumerate(massif_number.flat):
            lonlat = dicLonLat[int(num)]
            lat[i] = lonlat[1]
            lon[i] = lonlat[0]

        dim = self.ds["ZS"].dims
        fill_value = -9999999.0

        da_lat = xr.DataArray(
            lat,
            dims=dim,
            attrs=dict(
                long_name="latitude",
                units="degrees_north",
                _FillValue=fill_value,
            ),
        )
        da_lon = xr.DataArray(
            lon,
            dims=dim,
            attrs=dict(
                long_name="longitude",
                units="degrees_east",
                _FillValue=fill_value,
            ),
        )

        self.ds["LAT"] = da_lat
        self.ds["LON"] = da_lon

        return da_lat, da_lon


@xr.register_dataset_accessor("crocus")
class StandardCROCUS(StandardNC):

    def GlobalAttributes(self, product=None, **additionnal_attributes):
        super(StandardCROCUS, self).GlobalAttributes(product=product, **additionnal_attributes)
        self.read_constant_attributes('StandardCROCUS')
        if 'title' in self.ds.attrs.keys():
            self.ds.attrs['title'] = self.ds.attrs['title'] + ": snow variables"
        else:
            self.ds.attrs['title'] = "SURFEX/Crocus snow variables"
        if 'summary' in self.ds.attrs.keys():
            self.ds.attrs['summary'] = self.ds.attrs['summary'] + ' This file provides the snowpack properties of the '\
                'Crocus model.'
        else:
            self.ds.attrs['summary'] = 'This file provides the snowpack properties of the Crocus model.'
        keywords = ',SNOW WATER EQUIVALENT,SNOW,ALBEDO,AVALANCHE,' \
            'FREEZE/THAW,SNOW COVER,SNOW DENSITY,SNOW DEPTH,SNOW ENERGY BALANCE,SNOW MELT,SNOW WATER EQUIVALENT,' \
            'SNOW/ICE TEMPERATURE'
        if 'keywords' in self.ds.attrs.keys():
            self.ds.attrs['keywords'] = self.ds.attrs['keywords'] + keywords
        else:
            self.ds.attrs['keywords'] = keywords

        return self.ds

    def getsoilgrid(self):
        if os.path.isfile("OPTIONS.nam"):
            n = NamelistParser()
            N = n.parse("OPTIONS.nam")
            if 'XSOILGRID' in N['NAM_ISBA']:
                bottom = list(map(float, N['NAM_ISBA'].XSOILGRID))
                top = [0] + bottom[:-1]
                self.soilgrid = (np.array(top) + np.array(bottom)) / 2.

        if not hasattr(self, 'soilgrid') and os.path.isfile("PGD.nc"):
            pgd = xr.open_dataset("PGD.nc", engine="snowtools")
            nlayers = pgd["GROUND_LAYER"].data
            bottom = []
            for layer in range(1, nlayers[0] + 1):
                bottom.append(pgd["SOILGRID"] + str(layer)[0])
            top = [0] + bottom[:-1]
            self.soilgrid = (np.array(top) + np.array(bottom)) / 2.
            pgd.close()

    def soil_long_names(self, varname):
        r = re.search(r'\d+', varname)
        # r is None for varname without number: 'WGTOT_ISBA'
        if r is None:
            return ''
        else:
            layer = int(r.group()) - 1

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
            for varname in self.ds.keys():
                if varname[0:2] in ['TG', 'WG']:
                    if hasattr(self.ds[varname], 'long_name'):
                        self.ds[varname].attrs['long_name'] = self.ds[varname].attrs['long_name'] + \
                            self.soil_long_names(varname)

        return self.ds

    @property
    def getlatname(self):
        return 'latitude'

    @property
    def getlonname(self):
        return 'longitude'

    @property
    def getcoordname(self):
        return 'xx', 'yy'

    @property
    def getmassifname(self):
        return 'massif_num'


@xr.register_dataset_accessor("safran")
class StandardSAFRAN(StandardNC):

    def GlobalAttributes(self, product=None, **additionnal_attributes):
        super(StandardSAFRAN, self).GlobalAttributes(product=product, **additionnal_attributes)
        self.read_constant_attributes('StandardSAFRAN')
        if hasattr(self, 'title'):
            self.title = self.title + ": meteorological variables"
        else:
            self.title = "SAFRAN meteorological variables"
        if hasattr(self, 'summary'):
            self.summary = self.summary + ' This file provides the SAFRAN meteorological fields'
        else:
            self.summary = 'This file provides the SAFRAN meteorological fields'
        keywords = ',INCOMING SOLAR RADIATION,LONGWAVE RADIATION,SHORTWAVE RADIATION,AIR' \
            ' TEMPERATURE,SURFACE TEMPERATURE,ABSOLUTE HUMIDITY,RELATIVE HUMIDITY,' \
            'WIND DIRECTION,WIND SPEED,SURFACE WINDS,RAIN,LIQUID PRECIPITATION,' \
            'HOURLY PRECIPITATION AMOUNT,SOLID PRECIPITATION'
        if hasattr(self, 'keywords'):
            self.keywords = self.keywords + keywords
        else:
            self.keywords = keywords

        return self.ds

    @property
    def getlatname(self):
        return 'LAT'

    @property
    def getlonname(self):
        return 'LON'

    @property
    def getcoordname(self):
        return 'x', 'y'

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

    @property
    def getmassifname(self):
        return 'massif_number'
