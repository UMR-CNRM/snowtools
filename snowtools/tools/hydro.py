# -*- coding: utf-8 -*-

"""
snowtools contains hydrological routines designed to agregate S2M outputs into hydrological basins.

Starting from a shapefile of hydrological basins and a digital elevation model including massif numbers, two
preliminary steps are necessary:

1. Rasterize the shapefile of basin on the same grid as the DEM using the following:

.. code-block:: python

    from snowtools.tools.hydro import rasterized_shapefile
    from snowtools.DATA import TESTBASE_DIR
    import os

    dem = os.path.join(TESTBASE_DIR, 'hydro', '/home/lafaysse/hydroMF/DEM_FRANCE_L93_250m_bilinear.nc')
    shapefilebasin = os.path.join(TESTBASE_DIR, 'hydro', 'BVTopo_Topage2024_inmassifs.shp')

    # Rasterize shapefilebasin on the same grid as dem using the value of the 'gid' attribute in the shapefile
    # Note that the attribute should be an integer in the shapefile.
    # If not please use QGIS to previously add an integer id attribute in your basins shapefile.

    rasterized_shapefile(shapefilebasin, 'gid', dem, output='raster_basins.nc')

2. Compute the areas of S2M simulation units inside each basin using the following example where rasterbasin can be
replaced by the file obtained at the previous step:

.. code-block:: python

    from snowtools.tools.hydro import basin_areas_file
    from snowtools.DATA import TESTBASE_DIR
    import os

    dem = os.path.join(TESTBASE_DIR, 'hydro', '/home/lafaysse/hydroMF/DEM_FRANCE_L93_250m_bilinear.nc')
    rasterbasin = os.path.join(TESTBASE_DIR, 'hydro', 'BNBV_SCHAPI_FRANCE250m.nc')
    listpro = [os.path.join(TESTBASE_DIR, "PRO", "pro_testhydro_alp_oper_2025031906_2025032306.nc")]
    outputdir = '.'

    # Compute the areas of basins in rasterbasin corresponding to the geometry of the pro file
    # and store them in outputdir
    b = basin_areas_file(dem, rasterbasin, listpro, outputdir)

Once this areas file is available, the computation of hydrological diagnostics can be done using the following example
where a list of variables are agregated and saved in the HYDRO.nc file

.. code-block:: python

        from snowtools.tools.hydro import hydro
        from snowtools.DATA import TESTBASE_DIR
        import os
        forcing = os.path.join(TESTBASE_DIR, "FORCING", "forcing_testhydro_alp_oper_2025031806_2025032306.nc")
        pro = os.path.join(TESTBASE_DIR, "PRO", "pro_testhydro_alp_oper_2025031906_2025032306.nc")
        areas = os.path.join(TESTBASE_DIR, "hydro", "areas_alp27_allslopes.nc")
        with hydro([forcing, pro], areas, 'HYDRO.nc') as h:
            h.integration(['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'], var_sca='WSN_T_ISBA')

@author: Matthieu Lafaysse, March 2025
"""
import shutil

import numpy as np
import os

from snowtools.utils.S2M_standard_file import StandardHYDRO
from snowtools.utils.prosimu import prosimu
from snowtools.utils.FileException import (TimeListException, VarNameException, FileNameException, FileExistsException,
                                           FileExtensionException)
from snowtools.utils.infomassifs import infomassifs
from snowtools.tools.execute import printandcallSystemOrDie


class hydro(object):
    """
    This class is an object designed to produce hydrological diagnostics of snow simulations.
    It includes the diagnostic file and the methods to read the associated metadata.

    :param profilename: FORCING or PRO file or list of addresses for FORCING + PRO
    :type profilename: str or list of str
    :param areasfile: Address of the file describing the areas of PRO units for different catchments
    :type areasfile: str
    :param hydrofile: Address of the outputfile
    :type hydrofile: str
    """

    def __init__(self, profilename, areasfile, hydrofile):

        # Convert profilename arg to list
        if type(profilename) is str:
            profilename = [profilename]

        # Check number of points between pro and areas file
        self.checkpoints(profilename, areasfile)

        # Open all PRO files, read and store metadata
        self.metaprolist = list()
        for onepro in profilename:
            self.metaprolist.append(metaprosimu(onepro))

        # Here, should check that all pro files share the same temporal axis or crash otherwise
        # To be implemented
        self.time, self.timeunits = self.checktime(profilename)

        # Read basins description
        self.get_basins(areasfile)

        # Open output file
        self.createhydrofile(hydrofile)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    def close(self):
        """
        Write global attributes and close the PRO and diagnostic files associated with this class
        """
        for onepro in self.metaprolist:
            onepro.close()
        self.hydrofile.GlobalAttributes()
        self.hydrofile.close()

    def checktime(self, profilename):
        """
        Check if time vectors are compatible between the different input files and return time to copy in output file

        :param profilename: list of pro file names (for error message)
        :type profilename: str
        :return:
        """

        arrayntime = np.array([metapro.ntime for metapro in self.metaprolist])
        uniquentime = np.unique(arrayntime)

        if len(uniquentime) == 1:
            for metapro in self.metaprolist:
                metapro.removefirsttime = False
            return self.metaprolist[0].readtime_for_copy()
        elif len(uniquentime) == 2:
            if abs(uniquentime[1] - uniquentime[0]) == 1:
                # Case with PRO and FORCING files with same time step but with initial value in FORCING file that must
                # be removed
                ntime = np.min(uniquentime)
                for metapro in self.metaprolist:
                    metapro.removefirsttime = metapro.ntime > ntime
                return self.metaprolist[np.argmin(arrayntime)].readtime_for_copy()
            else:
                ntime = np.min(uniquentime)
                timeshort = self.metaprolist[np.argmin(arrayntime)].readtime()
                timelong = self.metaprolist[np.argmax(arrayntime)].readtime()

                # Check if all dates of timeshort (e.g pro file) are included in timelong (e.g. forcing file)
                if np.all(np.isin(timeshort, timelong)):
                    timestepshort = timeshort[1] - timeshort[0]
                    timesteplong = timelong[1] - timelong[0]
                    for metapro in self.metaprolist:
                        metapro.removefirsttime = False
                        if metapro.ntime > ntime:
                            metapro.ntimestep_integration = int(timestepshort / timesteplong)
                            metapro.firsttimestep_integration = (np.where(timelong == timeshort[0])[0][0]
                                                                 - metapro.ntimestep_integration + 1)
                            print(metapro.ntimestep_integration)
                            print(metapro.firsttimestep_integration)
                    return self.metaprolist[np.argmin(arrayntime)].readtime_for_copy()

                else:
                    raise TimeListException(profilename, [m.ntime for m in self.metaprolist])
        else:
            raise TimeListException(profilename, [m.ntime for m in self.metaprolist])

    @staticmethod
    def checkpoints(listprofilename, areasfile):
        """
        This method raise an exception in case of inconsistent number of points between pro file and areas file

        :param listprofilename: list of pro file name
        :type listprofilename: list of str
        :param areasfile: areas file name
        :type areasfile: str
        :return:
        """

        with prosimu(areasfile) as p:
            npoints_area = p.getlendim('Number_of_points')

        for pro in listprofilename:
            with prosimu(pro) as p:
                npoints_pro = p.getlendim('Number_of_points')

            if npoints_pro != npoints_area:
                raise Exception('Inconsistent number of points between files')

    def get_basins(self, areasfile):
        """
        This method reads the areas file describing the basins

        :param areasfile: Path of the shapefile
        :type areasfile: str
        """

        with prosimu(areasfile) as p:
            self.nbasins = p.getlendim('basin')
            self.areas = p.read('areas')
            listvar = p.listvar()
            self.basin_descriptors = list()
            self.attr_descriptors = dict()
            for varname in listvar:
                if 'basin' in p.getdimvar(varname) and p.getrankvar(varname) == 1:
                    self.basin_descriptors.append(varname)
                    setattr(self, varname, p.read(varname))
                    self.attr_descriptors[varname] = dict()
                    for attrname in p.listattr(varname):
                        self.attr_descriptors[varname][attrname] = p.getattr(varname, attrname)

    def createhydrofile(self, hydrofilename):
        """
        Create a catchment scale diagnostic file
        """

        self.hydrofile = StandardHYDRO(hydrofilename, 'w', format='NETCDF4')
        # Create time dimension and coordinate
        self.hydrofile.createDimension('time', size=None)
        timeout = self.hydrofile.createVariable('time', 'f8', dimensions=('time',))
        timeout[:] = self.time[:]
        timeout.units = self.timeunits

        # Create basin dimension and descriptors
        self.hydrofile.createDimension('basin', size=self.nbasins)

        for varname in self.basin_descriptors:
            basinid = self.hydrofile.createVariable(varname, 'i4', dimensions=('basin',))
            basinid[:] = getattr(self, varname)[:]

            for attrname, value in self.attr_descriptors[varname].items():
                setattr(basinid, attrname, value)

    def integration(self, listvarname, agg_method = 'average', var_sca=None):
        """
        Loop over a list of output variables to compute and write spatially aggregated diagnostics

        :param listvarname: List of variables required in output
        :type listvarname: list
        :param agg_method: Method used for spatial aggregation: average (default), cumul
        :type agg_method: str
        :param var_sca: Variable to compute snow cover area (e.g. DSN_T_ISBA, WSN_T_ISBA)
        :type var_sca: str
        """
        # Check availability of aggregation method
        _dict_methods = dict(average = self.average, cumul = self.cumul)
        if agg_method not in _dict_methods:
            msg_error = ('Incorrect method ' + aggregation_method +
                         ' should be chosen between:' + ','.join(_dict_methods.keys()))
            raise ValueError(msg_error)
        else:
            integre = _dict_methods[agg_method]

        # Loop over all output variables
        for varname in listvarname:
            varfound = False
            varintegr = np.empty((self.metaprolist[0].ntime, self.nbasins))

            if var_sca:
                if varname == var_sca:
                    sca = np.empty((self.metaprolist[0].ntime, self.nbasins))

            # Look for a file containing this variable to initialize fillvalue
            for m, metapro in enumerate(self.metaprolist):
                if varname in metapro.listvar():
                    fillvalue = metapro.getfillvalue(varname)
                    varfound = True
                    break

            # Exit if variable not available in any file
            if not varfound:
                print(varname + " is not available in output files")
                continue

            # Initialize
            varintegr[...] = fillvalue

            # Loop over available FORCING or PRO files
            for m, metapro in enumerate(self.metaprolist):
                # Read only once each variable
                if varname in metapro.listvar():
                    print('Aggregate variable ' + varname + " in " + metapro.filepath())
                    if metapro.removefirsttime:
                        varin = metapro.read(varname)[1:, ...]
                    else:
                        varin = metapro.read(varname)

                    if hasattr(metapro, 'ntimestep_integration'):
                        if metapro.ntimestep_integration > 1:
                            varin = metapro.moytempo(varin, metapro.ntimestep_integration,
                                                     metapro.firsttimestep_integration)

                    basinvalue = integre(varin)
                    # Replace nan values by fillvalue
                    varintegr = np.where(np.isnan(basinvalue), fillvalue, basinvalue)

                    # Compute snow covered area
                    if var_sca:
                        if varname == var_sca:
                            sca = self.sca(varin, 0)

            # Create variable in output file, write values and copy attributes from input file
            self.hydrofile.createVariable(varname, 'f4', dimensions=('time', 'basin'),
                                          fill_value=fillvalue)
            self.hydrofile.variables[varname][...] = varintegr
            self.copyvarattrs(varname)

            # Create and write SCA variable
            if var_sca:
                if varname == var_sca:
                    self.hydrofile.createVariable('SCA', 'f4', dimensions=('time', 'basin'),
                                                  fill_value=fillvalue)
                    self.hydrofile.variables['SCA'][...] = sca
                    self.hydrofile.variables['SCA'].long_name = 'Snow Covered Area'
                    self.hydrofile.variables['SCA'].units = '0-1'

    def cumul(self, varin):
        """
        This method cumulates the diagnostic varname in pro file across the catchment

        :param varin: Variable to cumulate spatially (time, Number_of_points)
        :type varin: 2d array of any type with dimensions (time, Number_of_points) as read from a PRO file
        :return: array of same type as varin with dimension (time, basin)
        """

        # varin (time, Number_of_points), self.areas (Number_of_points, basin)
        return np.matmul(varin, self.areas)

    def average(self, varin):
        """
        This method averages the diagnostic varname in pro file across the catchment

        :param varin: Variable to average spatially (time, Number_of_points)
        :type varin: 2d array of any type with dimensions (time, Number_of_points) as read from a PRO file
        :return: array of same type as varin with dimension (time, basin)
        """

        # cumul (time, basin) self.total_area (basin)
        # Divide each value along the last axis
        return self.cumul(varin) / self.total_area

    def sca(self, varin, threshold):
        """
        This methods return the surface of the catchment for which varin > threshold

        :param varin: Variable for which a surface of exceedance should be computed (snow depth, snow water equivalent)
        :type varin: array of any type with time as first dimension as read from a PRO file
        :param threshold: Threhold
        :type threshold: int or float
        :return: array of same type as varin with time dimension length
        """
        return np.matmul((varin > threshold) * 1, self.areas) / self.total_area

    def copyvarattrs(self, varname):
        """
        Copy attributes from a variable in input file to a variable in output file

        :param varname: variable name in input and output files
        :type varname: str
        """

        # Look for a file containing this variable to get attributes
        for m, metapro in enumerate(self.metaprolist):
            if varname in metapro.listvar():
                for attrname in metapro.listattr(varname):
                    if attrname not in "_FillValue":
                        setattr(self.hydrofile.variables[varname], attrname, metapro.getattr(varname, attrname))
                break


class basin_areas_file(object):
    """
    Class to provide a file describing the areas of S2M units for hydrological basins
    :param demfilename: Digital elevation model including elevation, aspect, slope and massifs
    :type demfilename: str
    :param basinraster: Rasterized file of hydrological basins on the same grid as the dem file
    :type basinraster: str
    :param listprofilename: List of reference PRO files describing S2M geometry
    :type listprofilename: list of str
    :param outputdir: Directory where save output files are saved
    :type outputdir: str
    """
    def __init__(self, demfilename, basinraster, listprofilename, outputdir):
        # 1. Read topography and massifs in DEM
        self.read_dem(demfilename)
        # 2. Read basin raster file on the same grid
        self.read_rasterbasin(basinraster)
        # 3. Check consistency of the grids
        self.check_coordinates()
        # 4. Get list of basins id
        basins_list = self.get_list_basins()
        # 5. Read available S2M units
        listmetapro = self.read_s2m_units(listprofilename)
        # 6. Compute areas
        self.compute_areas(basins_list, listmetapro)
        # 7. Write areas in a file with the same geometry as profilename
        self.write_areas(listmetapro, outputdir)

    def read_dem(self, demfilename):
        """
        This method reads the dem file

        :param demfilename: Digital elevation model file
        :type demfilename: str
        """

        # Read grid file, massif and topography
        with prosimu(demfilename) as dem:
            self.massif_dem = dem.read('massif_number', fill2zero=True)  # missing integers can not be np.nan
            self.zs_dem = dem.read('ZS')
            self.aspect_dem = dem.read('aspect')
            self.slope_dem = dem.read('slope')
            self.x_dem = dem.read('x')
            self.y_dem = dem.read('y')

    def read_rasterbasin(self, basinraster):
        """
        This method reads the raster file of basins

        :param basinraster: Raster file of basins in the same grid as the dem
        :type basinraster: str
        """
        with prosimu(basinraster) as br:
            self.basins_br = br.read('basin', fill2zero=True)  # missing integers can not be np.nan
            self.x_br = br.read('x')
            self.y_br = br.read('y')

    def check_coordinates(self):
        """
        This method checks if grids are identical between dem and raster of basins
        """
        assert np.array_equal(self.x_dem, self.x_br)
        assert np.array_equal(self.y_dem, self.y_br)

    def get_list_basins(self):
        """List of unique available basin identifiers"""
        # 0 is used to identify missing values
        return np.setdiff1d(np.unique(self.basins_br), 0)

    def read_s2m_units(self, listprofilename):
        """
        This method reads available S2M spatial units

        :param listprofilename: List of PRO reference files to describe S2M geometry
        :type listprofilename: list of str
        """
        if type(listprofilename) is list:
            l = list()
            for p in listprofilename:
                l.append(self.read_s2m_units(p))
            return l
        else:
            return metaprosimu(listprofilename)

    def compute_areas(self, basins_list, metaprolist):
        """
        This method computes the areas of S2M units for each basin of the list.

        :param basins_list: List of basin identifiers
        :type basins_list: list of int
        :param metaprolist: List of PRO reference files for S2M units
        :type metaprolist: list of str
        """

        # Apply a threshold on maximum slope to include all pixels even if > 50°
        slopemax = np.max(metaprolist[0].slope)
        slope_dem_threshold = np.where(self.slope_dem > slopemax, slopemax, self.slope_dem)

        # Compute area of a DEM pixel
        area_pixel_dem = abs(self.x_dem[1] - self.x_dem[0]) * abs(self.y_dem[1] - self.y_dem[0])

        # Definition
        for attr in 'basin', 'areas', 'total_area', 'mean_elevation', 'min_elevation', 'max_elevation', 'nbasins':
            setattr(self, attr, dict())
        indexbasin = dict()
        self.indexmetapro = dict()
        self.nmetapro = len(metaprolist)
        nmaxbasins = len(basins_list)
        for m, metapro in enumerate(metaprolist):
            indexbasin[m] = -1
            self.areas[m] = np.zeros((metapro.nbpoints, nmaxbasins), 'float')
            self.basin[m] = np.zeros(nmaxbasins, 'int')
            self.total_area[m] = np.zeros(nmaxbasins, 'float')
            self.mean_elevation[m] = np.zeros(nmaxbasins, 'float')
            self.min_elevation[m] = np.zeros(nmaxbasins, 'float')
            self.max_elevation[m] = np.zeros(nmaxbasins, 'float')

        for basin in basins_list:

            # Extract dem for this basin to reduce computing time
            print('Basin ' + str(basin))
            isinsidebasin = self.basins_br == basin
            rmin, rmax, cmin, cmax = bbox(isinsidebasin)

            zs_dem_basin = self.zs_dem[rmin:rmax+1, cmin:cmax+1]
            aspect_dem_basin = self.aspect_dem[rmin:rmax+1, cmin:cmax+1]
            slope_dem_basin = slope_dem_threshold[rmin:rmax+1, cmin:cmax+1]
            massif_dem_basin = self.massif_dem[rmin:rmax+1, cmin:cmax+1]
            isinsidebasin_zoom = isinsidebasin[rmin:rmax + 1, cmin:cmax + 1]

            count = np.zeros_like(zs_dem_basin)

            # List of massifs belonging to this basin
            massifs_basin = np.setdiff1d(np.unique(massif_dem_basin[isinsidebasin_zoom]), 0)

            # Case for which the basin shape does not include any defined massif
            if len(massifs_basin) == 0:
                continue

            # Identify metapro covering this basin
            availmassifs = False
            for m, metapro in enumerate(metaprolist):
                if np.all(np.isin(massifs_basin, metapro.avail_massifs)):
                    self.indexmetapro[basin] = m
                    indexbasin[m] += 1
                    availmassifs = True
                    break

            if not availmassifs:
                print('WARNING : missing metapro for basin ' + str(basin))
                print('Massifs :' + str(massifs_basin))
                continue

            area = np.zeros_like(metaprolist[self.indexmetapro[basin]].zs)

            # Loop over S2M points and count the number of points associated with the topographic class
            for point in range(1, metapro.nbpoints):
                # print ('Point ' + str(point) + '/' + str(metapro.nbpoints))
                issamemassif = massif_dem_basin == metapro.massif[point]
                issameelevation = abs(zs_dem_basin - metapro.zs[point]) < metapro.resol_zs / 2
                issameaspect = (diff_aspect(aspect_dem_basin, metapro.aspect[point])
                                < metapro.resol_aspect / 2) | (metapro.slope[point] <
                                                               metapro.resol_slope / 2)
                issameslope = abs(slope_dem_basin - metapro.slope[point]) < metapro.resol_slope / 2

                issameunit = isinsidebasin_zoom & issamemassif & issameelevation & issameaspect & issameslope

                count[issameunit] += 1

                area[point] = np.sum(issameunit) * area_pixel_dem

            if np.sum(area) == 0:
                print(np.unique(massif_dem_basin))
                print(np.min(zs_dem_basin), np.max(zs_dem_basin))
                print(np.min(aspect_dem_basin), np.max(aspect_dem_basin))
                print(np.min(slope_dem_basin), np.max(slope_dem_basin))

                raise Exception("A zero area is obtained for this basin, this should not happen.")

            m = self.indexmetapro[basin]
            b = indexbasin[m]
            self.areas[m][:, b] = area
            self.basin[m][b] = basin
            self.total_area[m][b] = np.sum(area)
            self.mean_elevation[m][b] = np.mean(zs_dem_basin[isinsidebasin_zoom])
            self.min_elevation[m][b] = np.min(zs_dem_basin[isinsidebasin_zoom])
            self.max_elevation[m][b] = np.max(zs_dem_basin[isinsidebasin_zoom])

        for m in range(0, self.nmetapro):
            self.nbasins[m] = indexbasin[m] + 1

        return True

    def write_areas(self, metaprolist, outputdir):
        """
        This method writes the computed areas in as many output files as available PRO reference files

        :param metaprolist: List of PRO reference files for S2M units
        :type metaprolist: list of str
        :param outputdir: Directory where save output files are saved
        :type outputdir: str
        """

        i = infomassifs()

        for m, metapro in enumerate(metaprolist):
            region = i.operregionofmassifs[metapro.massif[0]]
            auxfile = os.path.join(outputdir, 'areas_' + region + '.nc')
            metapro.create_auxiliary_file(auxfile)
            nb = self.nbasins[m]
            with prosimu(auxfile, openmode='a') as surffile:
                # Create basin dimension
                surffile.dataset.createDimension('basin', nb)
                surffile.dataset.createVariable('basin', 'i', dimensions='basin')
                # Create output variables
                surffile.dataset.createVariable('areas', 'f', ('Number_of_points', 'basin'))
                for varname in 'total_area', 'mean_elevation', 'min_elevation', 'max_elevation':
                    surffile.dataset.createVariable(varname, 'f', dimensions='basin')

                # Define variables attributes
                setattr(surffile.dataset.variables['areas'], 'long_name', 'Area of the topographic class in the basin')
                setattr(surffile.dataset.variables['total_area'], 'long_name', 'Total area of the basin')
                setattr(surffile.dataset.variables['mean_elevation'], 'long_name', 'Mean elevation of the basin '
                                                                                   'from the provided DEM')
                setattr(surffile.dataset.variables['min_elevation'], 'long_name', 'Minimum elevation of the basin '
                                                                                  'from the provided DEM')
                setattr(surffile.dataset.variables['max_elevation'],  'long_name', 'Maximum elevation of the basin '
                                                                                   'from the provided DEM')
                for varname in 'areas', 'total_area':
                    setattr(surffile.dataset.variables[varname], 'units', 'm2')
                for varname in 'mean_elevation', 'min_elevation', 'max_elevation':
                    setattr(surffile.dataset.variables[varname], 'units', 'm')

                # Fill variables values
                surffile.dataset.variables['basin'][...] = self.basin[m][0:nb]
                surffile.dataset.variables['areas'][...] = self.areas[m][:, 0:nb]
                surffile.dataset.variables['total_area'][...] = self.total_area[m][0:nb]
                surffile.dataset.variables['mean_elevation'][...] = self.mean_elevation[m][0:nb]
                surffile.dataset.variables['min_elevation'][...] = self.min_elevation[m][0:nb]
                surffile.dataset.variables['max_elevation'][...] = self.max_elevation[m][0:nb]


def rasterized_shapefile(shapefilepath, attribute, refraster, output=None):
    """
    Rasterize the attribute value of the shapefile shapefilepath on the same grid as the refraster file
    Dependencies on gdal and nco for this function

    :param shapefilepath: path of the shapefile to rasterize (allowed format: shp)
    :type shapefilepath: str
    :param attribute: name of the attribute of polygons for which the value will be rasterized
    :type attribute: str
    :param refraster: reference file describing the grid  (allowed format: tif)
    :type refraster: str
    :param output: output file address (optional, if not available just change the extension of shapefilepath)
    :type output: str
    :return: True if all commands succeed
    """
    # Check availability of input files
    for ficin in shapefilepath, refraster:
        if not os.path.isfile(ficin):
            raise FileNameException(ficin)

    # Get extension of reference raster and root name of shapefile
    rasterfileName, rasterfileExtension = os.path.splitext(refraster)
    shapefileName, shapefileExtension = os.path.splitext(shapefilepath)

    # Check extensions:
    if rasterfileExtension not in ['.tif', '.nc']:
        raise FileExtensionException(refraster)
    if shapefileExtension not in ['.shp']:
        raise FileExtensionException(shapefilepath)

    # NetCDF case: check if a ZS variable is available
    if rasterfileExtension == '.nc':
        with prosimu(refraster) as p:
            if 'ZS' not in p.listvar():
                raise VarNameException('ZS', refraster)
            varnamefromgdal = 'ZS'
    elif rasterfileExtension == '.tif':
        varnamefromgdal = 'Band1'

    tmpraster = 'temp.tif'

    outputfile = output if output else f'{shapefileName}.nc'

    # Check if outputfile does not already exists to not overwrite
    if os.path.isfile(outputfile):
        raise FileExistsException(outputfile)

    # Copy reference raster if tif or convert it to tif
    if rasterfileExtension in ['.nc']:
        netcdf2tif = f'gdal_translate NETCDF:{refraster}:ZS {tmpraster}'
        printandcallSystemOrDie(netcdf2tif)
    elif rasterfileExtension == '.tif':
        shutil.copy(refraster, tmpraster)

    # Initialize to 0 all values outside the shapefile contours
    burncommand = f'gdal_rasterize -i -burn 0 {shapefilepath} {tmpraster}'
    # Rasterize the shapefile attribute on the grid
    rasterizecommand = f'gdal_rasterize -a {attribute} {shapefilepath} {tmpraster}'
    printandcallSystemOrDie(burncommand)
    printandcallSystemOrDie(rasterizecommand)

    # Convert tif file into netcdf, rename variable, convert to integer and set long_name attribute
    tif2netcdf = f'gdal_translate -of NetCDF {tmpraster} {outputfile}'
    printandcallSystemOrDie(tif2netcdf)
    renamevar = f'ncrename -v {varnamefromgdal},basin {outputfile}'
    floattoint = f'ncap2 -O -s basin=int(basin) {outputfile} {outputfile}'
    # Due to the spaces in long_name attribute, we need to give directly a list to callSystemOrDie
    setlongname = f'ncatted -O -a long_name,basin,o,c,"basin identifier {attribute}" {outputfile}'
    printandcallSystemOrDie(renamevar)
    printandcallSystemOrDie(floattoint)
    printandcallSystemOrDie(setlongname)

    # Clean
    os.remove(tmpraster)

    return True


class metaprosimu(prosimu):
    """
    This class adds topographic and time metadata to a prosimu instance
    :param profilename: Path of the PRO.nc filename
    :type profilename: str
    """

    # Correspondance between topo attributes and allowed varnames in PRO files
    allowedtopovarnames = dict(zs='ZS', aspect='aspect', slope='slope', massif=['massif_num', 'massif_number'])
    reftopovarnames = dict()

    def __init__(self, profilename):
        self.filename = profilename
        super(metaprosimu, self).__init__(profilename)
        # Read metadata of S2M file and store them in attributes
        for topovar, topovarnames in self.allowedtopovarnames.items():
            if type(topovarnames) is not list:
                topovarnames = [topovarnames]
            availtopovar = False
            for varname in topovarnames:
                # Test if variable is available in the file
                if varname in self.listvar():
                    availtopovar = True
                    # Actual variable names available in PRO file
                    self.reftopovarnames[topovar] = varname
                    # Read the variable and store as attribute of the class
                    setattr(self, topovar, self.read(varname))
                    break
            # Exception if topographic variable unavailable in the file
            if not availtopovar:
                raise VarNameException(varname, self.filename)

        # Compute dimension lengths
        self.nbpoints = len(self.zs)
        self.ntime = len(self.timedim)
        self.time, self.timeunits = self.readtime_for_copy()

        # Compute resolution of topographic classes
        list_zs = np.unique(self.zs)
        list_aspects = np.unique(self.aspect)
        list_slopes = np.unique(self.slope)
        self.resol_zs = list_zs[1] - list_zs[0]
        self.resol_slope = list_slopes[1] - list_slopes[0]
        self.resol_aspect = list_aspects[2] - list_aspects[1]

        # Available massifs
        self.avail_massifs = np.unique(self.massif)

    def filepath(self):
        # Future method of netCDF4.Dataset with netcdf >= 4.1.2
        return self.filename

    def create_auxiliary_file(self, filename):
        with prosimu(filename, openmode = 'w') as auxil:
            auxil.dataset.createDimension('Number_of_points', self.nbpoints)

            for var in ['zs', 'aspect', 'slope', 'massif']:
                auxil.dataset.createVariable(var, self.gettypevar(self.reftopovarnames[var]),
                                             dimensions='Number_of_points')
                auxil.dataset.variables[var][...] = getattr(self, var)[...]


def diff_aspect(aspect1, aspect2):
    """
    Compute the angle absolute difference between two aspects (0-360 degrees) or two arrays of aspects

    :param aspect1: Aspect or array of aspect (0-360 degrees)
    :type aspect1: int, float or array of float
    :param aspect2: Aspect or array of aspect (0-360 degrees)
    :type aspect2: int, float or array of float of same type as aspect1
    :return: array of type of aspect1 and aspect2
    """

    # Compute aspect angle absolute difference
    diffabs = abs(aspect1 - aspect2)
    return np.where(diffabs > 180, 360 - diffabs, diffabs)


def bbox(img):
    """
    This method provides the bounding box of True components of a given field

    :param img: Field of logical values
    :type img: 2d np.array
    """
    rows = np.any(img, axis=1)
    cols = np.any(img, axis=0)
    rmin, rmax = np.where(rows)[0][[0, -1]]
    cmin, cmax = np.where(cols)[0][[0, -1]]
    return rmin, rmax, cmin, cmax


if __name__ == '__main__':

    # Demonstrator for test purposes.
    # Do not modify this example. This module is not designed to be called directly as a script
    # Use s2m --hydro file.nc --hydrovar var1,var2... command or import this module in your own script.

    testrasterize = False
    testareas = False
    testhydro = True

    dem = '/home/lafaysse/hydroMF/DEM_FRANCE_L93_250m_bilinear.nc'
    shapefilebasin = '/home/lafaysse/hydroMF/BVTopo_Topage2024_inmassifs.shp'

    if testrasterize:
        rasterized_shapefile(shapefilebasin, 'gid', dem)

    rasterbasin = '/home/lafaysse/hydroMF/BVTopo_Topage2024_inmassifs.nc'

    cache = '/home/lafaysse/cache/cache/vortex/s2m'
    xpid = 'reanalysis_era5.2024.1@lafaysse'
    reffile = 'pro/PRO_2020080106_2021080106.nc'
    forcing = 'meteo/dailyFORCING_2020080106_2021080106.nc'
    listregions = ['alp27_allslopes', 'pyr24_allslopes', 'cor2_allslopes', 'mac11_allslopes', 'jur4_allslopes',
                   'vog3_allslopes']

    listpro = [os.path.join(cache, vconf, xpid, reffile) for vconf in listregions]
    listmeteo = [os.path.join(cache, vconf, xpid, forcing) for vconf in listregions]

    outputdir = '/home/lafaysse/hydroMF'

    if testareas:
        b = basin_areas_file(dem, rasterbasin, listpro, outputdir)

    listareas = ['/home/lafaysse/hydroMF/areas_' + vconf + '.nc' for vconf in listregions]

    if testhydro:
        with hydro([listmeteo[0], listpro[0]], listareas[0], 'HYDRO.nc') as h:
            h.integration(['Tair', 'Rainf', 'Snowf', 'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'],
                          var_sca='WSN_T_ISBA')

    # Below is obsolete call of this module (previous version). To be removed by Dec 2025.
    # dir_dem_bassins = os.path.join('/rd/cenfic3/cenmod/home/lafaysse', 'schapi2020.2', 'bassins_shp')
    #
    # shapefilebasin = os.path.join(dir_dem_bassins, "BNBV_all.shp")
    #
    # pro = ['/rd/cenfic3/cenmod/era40/vortex/s2m/alp_allslopes/schapi2020.2/pro/PRO_2017080106_2018080106.nc',
    #        '/rd/cenfic3/cenmod/era40/vortex/s2m/pyr_allslopes/schapi2020.2/pro/PRO_2017080106_2018080106.nc',
    #        '/rd/cenfic3/cenmod/era40/vortex/s2m/alp_allslopes/reanalysis_2020.2/meteo/FORCING_2017080106_2018080106.nc',
    #        '/rd/cenfic3/cenmod/era40/vortex/s2m/pyr_allslopes/reanalysis_2020.2/meteo/FORCING_2017080106_2018080106.nc',
    #        ]
    #
    # list_var = ['Rainf', 'SNOMLT_ISBA']
    #
    # with hydro(shapefilebasin, dir_dem_bassins, pro, 'HYDRO.nc', 'w') as h:
    #     h.integration(list_var)
