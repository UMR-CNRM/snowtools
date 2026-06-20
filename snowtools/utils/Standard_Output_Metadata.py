# -*- coding: utf-8 -*-

#===============
#Exemple d'usage
#===============

#import xarray as xr
#from standard_nc_accessor import StandardNC   # le fichier ci‑dessus
#
## Chargement du NetCDF (xarray s’occupe du décodage)
#ds = xr.open_dataset("S2M_example.nc")
#
## Accès aux nouvelles méthodes via l’accessor "standard_nc"
#ds.standard_nc.GlobalAttributes()               # crée les attributs ACDD
#ds.standard_nc.add_standard_names()             # ajoute standard_name / long_name
#ds.standard_nc.add_coord()                      # (si besoin) crée LAT/LON
#
## Exemple d’opération sur toutes les variables
#ds_means = ds.standard_nc.apply_to_all(lambda da: da.mean(dim="time"))
#
#print(ds.standard_nc)                           # <StandardNC accessor …>
#print(ds.attrs)                                 # attributs globaux remplis
#print(ds_means)                                 # nouveau Dataset avec les moyennes


import os
import sys
import datetime
import configparser
import importlib

import xarray as xr
import numpy as np


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
            self.ds.attrs[key] = value

    def GlobalAttributes(self, product='reanalysis', **additionnal_attributes):
        """
        Global attributes following ACDD conventions.
        The dataset must have a "time" attribute.
        """
        time = self.ds.time

        if product == 'reanalysis':
            self.read_constant_attributes("GlobalAttributesReanalysis")
            self.ds.attrs["date_created"] = datetime.datetime.now().replace(
                second=0, microsecond=0
            ).isoformat()
        else:
            self.read_constant_attributes("GlobalAttributesOper")
            self.ds.attrs["date_created"] = datetime.datetime.now().replace(
                hour=12, minute=0, second=0, microsecond=0
            ).isoformat()

        try:
            login = os.getlogin()
            self.ds.attrs['contributor_name'] = self.ds.attrs['contributor_name'] + f" {login}"
            self.ds.attrs['contributor_role'] = self.ds.attrs['contributor_role'] + f" {login} ran this simulation"
        except OSError:
            pass

        # self.get_coord()

        # temporal coverage
        self.ds.attrs["time_coverage_start"] = time.data[0].astype(str)
        self.ds.attrs["time_coverage_end"] = time.data[-1].astype(str)
        self.ds.attrs["time_coverage_duration"] = str(time[-1] - time[0])
        if len(time) > 1:
            self.ds.attrs["time_coverage_resolution"] = str(time[1] - time[0])

        # system env
        self.ds.attrs["python_version"] = sys.version
        self.ds.attrs["python_binary"] = os.path.realpath(sys.executable)

        # additional attributs
        for name, value in additionnal_attributes.items():
            self.ds.attrs[name] = value

    @staticmethod
    def standard_names():
        return dict(ZS="surface_altitude", time="time")

    def addCoord(self):
        from .massif import infomassifs

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

    def special_long_names(self):
        """
        Retourne un dictionnaire {nom_variable: long_name}
        spécifique aux massifs (extrait de la version originale).
        """
        massif_name = getattr(self.ds, "getmassifname", None)
        if massif_name is None:
            raise AttributeError("Le Dataset doit posséder l’attribut 'getmassifname'.")
        return {
            massif_name: "SAFRAN massif number. Metadata are provided in the associated shapefile."
        }

    def add_standard_names(self):
        """Applique les dictionnaires standard_name et long_name aux variables."""
        std = self.standard_names()
        long = self.special_long_names()

        for var_name, da in self.ds.variables.items():
            if var_name in std:
                da.attrs["standard_name"] = std[var_name]
            if var_name in long:
                da.attrs["long_name"] = long[var_name]

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


@xr.register_dataset_accessor("crocus")
class StandardCROCUS(StandardNC):

    def GlobalAttributes(self, product='reanalysis', **additionnal_attributes):
        super(StandardCROCUS, self).GlobalAttributes(product=product, **additionnal_attributes)
        self.read_constant_attributes('StandardCROCUS')
        self.ds.attrs['title'] = self.ds.attrs['title'] + ": snow variables"
        self.ds.attrs['summary'] = self.ds.attrs['summary'] + ' This file provides the snowpack properties of the ' \
            'Crocus model.'
        self.ds.attrs['keywords'] = self.ds.attrs['keywords'] + ',SNOW WATER EQUIVALENT,SNOW,ALBEDO,AVALANCHE,' \
            'FREEZE/THAW,SNOW COVER,SNOW DENSITY,SNOW DEPTH,SNOW ENERGY BALANCE,SNOW MELT,SNOW WATER EQUIVALENT,' \
            'SNOW/ICE TEMPERATURE'

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

#    def getsoilgrid(self):
#        from bronx.datagrip.namelist import NamelistParser
#        n = NamelistParser()
#        N = n.parse("OPTIONS.nam")
#        if 'XSOILGRID' in N['NAM_ISBA']:
#            bottom = list(map(float, N['NAM_ISBA'].XSOILGRID))
#            top = [0] + bottom[:-1]
#            self.soilgrid = (np.array(top) + np.array(bottom)) / 2.
#        else:
#            from snowtools.utils.prosimu import prosimu
#            if os.path.isfile("PGD.nc"):
#                pgd = prosimu("PGD.nc")
#                nlayers = pgd.read("GROUND_LAYER")
#                bottom = []
#                for layer in range(1, nlayers[0] + 1):
#                    bottom.append(pgd.read('SOILGRID' + str(layer))[0])
#                top = [0] + bottom[:-1]
#                self.soilgrid = (np.array(top) + np.array(bottom)) / 2.
#
#                pgd.close()

    def soil_long_names(self, varname):
        import re
        r = re.search(r'\d+', varname)
        # r is None for varname without number: 'WGTOT_ISBA'
        if r is None:
            return ''
        else:
            layer = int(r.group()) - 1

        if not hasattr(self, 'soilgrid'):
            pass
            # self.getsoilgrid()

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

    def xy2latlon(self, xvar, yvar):
        """
        Convert from an x/y projection to lat/lon by reading the info in the SURFEX namelist
        """
        from pyproj import Transformer
        from bronx.datagrip.namelist import NamelistParser

        # ----- Lecture du type de grille dans le namelist -----
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

        # xvar / yvar sont des DataArray 1‑D (ou déjà 2‑D) → on assure 2‑D
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
                "Le Dataset doit posséder les attributs 'getlatname' et 'getlonname'."
            )

        # lat/lon déjà présentes
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




