# -*- coding: utf-8 -*-
"""
Skyline Analysis Tool - Optimized Version
----------------------------------------------------------
This script calculates skylines from weather stations using a DEM.
Optimizations: Improved performance, readability and English comments without changing results.
"""

from osgeo import gdal, ogr, osr
import csv
import time
import os
import sys
import math
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

def main():
    start_time = time.time()
    mnt = sys.argv[1]  # DEM file path
    listing = sys.argv[2]  # Weather stations file path

    # Open the NetCDF DEM file
    img = gdal.Open(f"NETCDF:{mnt}:ZS")
    if img is None:
        raise FileNotFoundError(f"Cannot open 'ZS' subdataset in {mnt}")

    print(f"Raster dimensions: {img.RasterXSize} x {img.RasterYSize}")
    print(f"Coordinate system: {img.GetProjection()}")
    print(f"Geotransform: {img.GetGeoTransform()}")

    # Get raster band and geotransform
    band1 = img.GetRasterBand(1)
    rastinit = img.GetGeoTransform()
    step = int((rastinit[1] + (-rastinit[5])) / 2)  # Step size for interpolation

    # Create geographic reference matrices
    imgx = np.zeros((1, img.RasterXSize)).astype(float)
    imgy = np.zeros((img.RasterYSize, 1)).astype(float)
    for i in range(imgx.shape[1]):
        imgx[0, i] = rastinit[0] + (i * rastinit[1])
    for i in range(imgy.shape[0]):
        imgy[i, 0] = rastinit[3] + (i * rastinit[5])

    # Create output directory and files
    if not os.path.isdir("output"):
        os.mkdir("output")

    csv_out = "output/sta_skylines.csv"
    csv_ctr = "output/altitude_check.csv"

    # Remove existing files if they exist
    for file in [csv_out, csv_ctr]:
        if os.path.isfile(file):
            os.remove(file)

    viewmax = 20000  # Maximum view distance in meters (20 km)

    with open(listing, 'r') as file_in:
        next(file_in)  # Sauter l'en-tête
        in_file = np.loadtxt(
            file_in,
            dtype={
                'names': ('numposte', 'alt', 'massif', 'nom', 'lat', 'lon', 'dateouvr','datferm', 'exposition', 'pente', 'type_nivo'),
                'formats': (int, int, int, '|S24', float, float, '|S12', '|S12', int, int, int)
            },
            delimiter=' '
        )

    print(f"Number of stations: {np.size(in_file)}")

    # Define coordinate transformation from WGS84 to Lambert 93
    source = osr.SpatialReference()
    source.ImportFromEPSG(4326)  # WGS84
    target = osr.SpatialReference()
    target.ImportFromEPSG(2154)  # Lambert 93
    transform = osr.CoordinateTransformation(source, target)

    # Open CSV files for writing
    with open(csv_out, "w", newline='', encoding='utf-8') as csvfile, \
         open(csv_ctr, "w", newline='', encoding='utf-8') as ctrfile:

        stawriter = csv.writer(csvfile, delimiter=" ")
        ctrwriter = csv.writer(ctrfile, delimiter=" ")

        # Process each weather station
        for k in range(len(in_file)):
            in_stat = in_file[k] if len(in_file) > 1 else np.reshape(in_file, (-1,))[0]
            
            in_stat[6] = in_stat[6].decode('iso-8859-1').strip('"')            
            in_stat[7] = in_stat[7].decode('iso-8859-1').strip('"')

            # Transform coordinates from WGS84 to Lambert 93
            point = ogr.Geometry(ogr.wkbPoint)
            point.AddPoint(in_stat[4], in_stat[5])  # lat, lon
            point.Transform(transform)
            xx = math.floor(point.GetX())
            yy = math.floor(point.GetY())

            print(f"Station {in_stat[0]}: WGS84 coords: {in_stat[5]}, {in_stat[4]} -> Lambert 93: {xx}, {yy}")

            # Calculate window boundaries
            xmin = rastinit[0] + ((math.floor(((xx - viewmax) - rastinit[0]) / rastinit[1])) * rastinit[1])
            xmax = rastinit[0] + ((math.floor(((xx + viewmax) - rastinit[0]) / rastinit[1])) * rastinit[1])
            ymin = rastinit[3] - ((math.ceil((rastinit[3] - (yy - viewmax)) / rastinit[5])) * rastinit[5])
            ymax = rastinit[3] - ((math.ceil((rastinit[3] - (yy + viewmax)) / rastinit[5])) * rastinit[5])

            print(f"xmin: {xmin}, xmax: {xmax}, ymin: {ymin}, ymax: {ymax}")

            # Find station position in raster coordinates
            stax = rastinit[0] + ((math.floor((xx - rastinit[0]) / rastinit[1])) * rastinit[1])
            stay = rastinit[3] - (math.ceil((rastinit[3] - yy) / rastinit[5]) * rastinit[5])

            # Find closest indices in raster
            if ymax >= max(imgy):
                minrow = 0
            else:
                minrow = np.argmin(np.abs(imgy[:, 0] - ymax))
            if ymin <= min(imgy):
                maxrow = imgy.shape[0] - 1
            else:
                maxrow = np.argmin(np.abs(imgy[:, 0] - ymin))
            if xmin <= min(imgx[0, :]):
                mincol = 0
            else:
                mincol = np.argmin(np.abs(imgx[0, :] - xmin))
            if xmax >= max(imgx[0, :]):
                maxcol = imgx.shape[1] - 1
            else:
                maxcol = np.argmin(np.abs(imgx[0, :] - xmax))

            # Find station position indices in the extracted window
            starow = maxrow - np.argmin(np.abs(imgy[:, 0] - stay))
            stacol = np.argmin(np.abs(imgx[0, :] - stax)) - mincol

            # Extract array from raster
            print(f"mincol: {mincol}, minrow: {minrow}, maxcol - mincol: {maxcol - mincol}, maxrow - minrow: {maxrow - minrow}")
            height = band1.ReadAsArray(int(mincol), int(minrow), int(maxcol - mincol), int(maxrow - minrow))
            height = height.astype('int64')
            w, h = height.shape
            print(f"Raster extracted: {w} x {h}")

            # Find station position in the extracted raster
            sta_rc = (starow, stacol)
            z_alt = height[sta_rc]
            print(f"Station altitude: {z_alt}")

            final_data = []
            az = []
            anglee = []

            # Calculate skylines for each azimuth
            for azimut in range(0, 360, 5):
                num_steps = len(range(step, viewmax, step))
                angle = np.zeros((1, num_steps)).astype(float)
                points = []
                pt_dist = []

                for i, dist in enumerate(range(step, viewmax, step)):
                    ptx = xx + (dist * math.sin(math.radians(azimut)))
                    pty = yy + (dist * math.cos(math.radians(azimut)))
                    pt = (ptx, pty)
                    points.append(pt)
                    pt_dist.append(dist)

                    if ptx < xmax and ptx > xmin:
                        x = rastinit[0] + ((math.floor((ptx - rastinit[0]) / rastinit[1])) * rastinit[1])
                        ptcol = np.argmin(np.abs(imgx[0, :] - x)) - mincol
                    if pty < ymax and pty > ymin:
                        y = rastinit[3] - ((math.ceil((rastinit[3] - pty) / rastinit[5])) * rastinit[5])
                        ptrow = np.argmin(np.abs(imgy[:, 0] - y)) - minrow
                        ptrc = (int(ptrow), int(ptcol))

                    if 'ptrc' in locals() and ptrow < w and ptcol < h:
                        b = height[ptrc] - height[sta_rc]
                        if b > 0:
                            angle[0, i] = math.ceil((math.degrees(math.atan(b / dist))) * 100) / 100
                        else:
                            angle[0, i] = 0

                if len(angle[0]) > 0:
                    max_angle = max(angle[0])
                    max_index = np.argmax(angle[0])
                    data = (in_stat[0], azimut, max_angle, points[max_index][0], points[max_index][1])
                    final_data.append(data)
                    az.append(azimut)
                    anglee.append(max_angle)

            az = np.array(az, dtype='float')
            anglee = np.array(anglee, dtype='float')
            for values in final_data:
                stawriter.writerow([values[0], values[1], values[2]])

            station_name = in_stat[3].decode('iso-8859-1').strip('"')
            print(f"Station {station_name} processed")

            # Create polar plot
            fig = plt.figure()
            a = fig.add_subplot(111, polar=True)
            rmax = max(40., max(anglee)) if len(anglee) > 0 else 40.
            a.fill(az * math.pi / 180., rmax - anglee, '-ob', alpha=0.5, edgecolor='b')
            a.set_rmax(rmax)
            a.set_rgrids([0.01, 10., 20., 30., float(int(rmax))], [str(int(rmax)), '30', '20', '10', '0'])
            a.set_thetagrids([0., 45., 90., 135., 180., 225., 270., 315.], ["N", "NE", "E", "SE", "S", "SW", "W", "NW"])
            a.set_title(f"{station_name} alt mnt:{z_alt} m alt poste:{in_stat[1]}")
            a.set_theta_zero_location('N')
            a.set_theta_direction(-1)
            plt.savefig(f'output/{in_stat[0]}_skyline.png')
            plt.close(fig)

            # Check altitude consistency
            diff = z_alt - in_stat[1]
            if abs(diff) > 100.:
                data_bis = (in_stat[0], station_name, in_stat[10], in_stat[7], in_stat[1], z_alt, diff, 'Warning diff altitude mnt/poste >100m')
            else:
                data_bis = (in_stat[0], station_name, in_stat[10], in_stat[7], in_stat[1], z_alt, diff)
            ctrwriter.writerow(data_bis)

    print(f"Completed in {time.time() - start_time:.2f} seconds")

if __name__ == "__main__":
    main()
