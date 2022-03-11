# -*- coding: utf-8 -*-
import gdal
import csv
import time
import os
import sys
import math
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from osgeo import ogr, osr

# Code Hugue François / Marie Dumont automatiquement normalisé pep8 mais qui gagnerait à être pythonisé.

start_time = time.time()

mnt = sys.argv[1]
listing = sys.argv[2]


# load complete raster
img = gdal.Open(mnt)  # 50 m Lambert 93
band1 = img.GetRasterBand(1)
rastinit = img.GetGeoTransform()
step = int((rastinit[1] + (-rastinit[5])) / 2)  # for further use in line interpolation

# x,y geographic reference matrix
imgx = np.zeros((1, img.RasterXSize)).astype(np.float)
imgy = np.zeros((img.RasterYSize, 1)).astype(np.float)
for i in range(0, imgx.shape[1]):
    imgx[0, i] = rastinit[0] + (i * rastinit[1])
for i in range(0, imgy.shape[0]):
    imgy[i, 0] = rastinit[3] + (i * rastinit[5])

# output to csv file
if not os.path.isdir("output"):
    os.mkdir("output")
csv_out = "output/sta_skylines.csv"
if os.path.isfile(csv_out):
    os.remove(csv_out)
csvfile = open(csv_out, "wb")
stawriter = csv.writer(csvfile, delimiter=" ")


# check de la cohérence de l'altitude
csv_ctr = "output/altitude_check.csv"
if os.path.isfile(csv_ctr):
    os.remove(csv_ctr)
ctrfile = open(csv_ctr, "wb")
ctrwriter = csv.writer(ctrfile, delimiter=" ")


viewmax = 20000  # 20 km

# construction du vecteur d'entree
file_in = open(listing, 'r')
in_file = np.loadtxt(file_in, dtype={'names': ('numposte', 'alt', 'massif', 'nom', 'lat', 'lon'), 'formats': (int, int, int, '|S24', float, float)})
print(np.size(in_file))
file_in.close()

n = int(np.size(in_file))

# wsta=[["Tignes",1004464.1,6490813.1],
# ["Autrans-Prairie",900929.9,6460461.4],
# ["Autrans-Retenue d'eau",901261.0,6460502.4],
# ["2Alpes-Coolidge",946370.1,6439200.3],
# ["2Alpes-Lutins",946600.5,6439619.2],
# ["Chamrousse-Gabourreaux",926945.6,6451060.4],
# ["Chamrousse-Variante",927029.1,6450992.8],
# ["Chamrousse-Perche",926694.0,6450415.3]]

# reprojection to L93
source = osr.SpatialReference()
source.ImportFromEPSG(4326)
target = osr.SpatialReference()
target.ImportFromEPSG(2154)

transform = osr.CoordinateTransformation(source, target)


# extract from original raster
for k in range(n):

    if n > 1:
        in_stat = in_file[k]
    else:
        # va savoir pourquoi...
        in_stat = np.reshape(in_file, (-1,))[0]

    print('hello', in_stat[0], in_stat[3])
    # tranformation des coordonnées geo en L93
    point = ogr.Geometry(ogr.wkbPoint)
    # print in_stat[4], in_stat[5]
    point.AddPoint(in_stat[5], in_stat[4])  # coord lat lon
    point.Transform(transform)
    coord = point.ExportToWkt()
    # print coord
    xx = math.floor(point.GetX())
    yy = math.floor(point.GetY())
    print(xx, yy)
    ####

    final_data = []
    az = []
    anglee = []
    # Find row/col information et xy normalization
    xmin = rastinit[0] + ((math.floor(((xx - viewmax) - rastinit[0]) / rastinit[1])) * rastinit[1])
    xmax = rastinit[0] + ((math.floor(((xx + viewmax) - rastinit[0]) / rastinit[1])) * rastinit[1])
    ymin = rastinit[3] - ((math.ceil((rastinit[3] - (yy - viewmax)) / rastinit[5])) * rastinit[5])
    ymax = rastinit[3] - ((math.ceil((rastinit[3] - (yy + viewmax)) / rastinit[5])) * rastinit[5])
    stax = rastinit[0] + ((math.floor((xx - rastinit[0]) / rastinit[1])) * rastinit[1])
    stay = rastinit[3] - (math.ceil((rastinit[3] - yy) / rastinit[5]) * rastinit[5])
    if ymax >= max(imgy):
        minrow = 0
    else:
        minrow = np.unique(np.argwhere(imgy == ymax))[1]
    if ymin <= min(imgy):
        maxrow = imgy.shape[0]
    else:
        maxrow = np.unique(np.argwhere(imgy == ymin))[1]
    if xmin <= min(imgx[0, ]):
        mincol = 0
    else:
        mincol = np.unique(np.argwhere(imgx == xmin))[1]
    if xmax >= max(imgx[0, ]):
        maxcol = imgx.shape[1]
    else:
        maxcol = np.unique(np.argwhere(imgx == xmax))[1]
        starow = maxrow - np.unique(np.argwhere(imgy == stay))[1]
        stacol = np.unique(np.argwhere(imgx == stax))[1] - mincol
        starow = starow.astype('int64')
        stacol = stacol.astype('int64')
        sta_xy = (stax + (rastinit[1] / 2), stay + (rastinit[5] / 2))
        sta_rc = (starow, stacol)
    # Extract array from raster
    print(mincol, minrow, maxcol - mincol, maxrow - minrow)
    height = band1.ReadAsArray(int(mincol), int(minrow), int(maxcol - mincol), int(maxrow - minrow))
    height = height.astype('int64')
    # get width and heigth of image
    w, h = height.shape
    print("raster extracted", w, h)
    z_alt = height[sta_rc]
    print(height[sta_rc])
    # Get all intersected cells on azimuth
    for azimut in range(0, 360, 5):
        i = 0
        angle = np.zeros((1, (viewmax / step) - 1)).astype(np.float)  # initialize container for angles
        points = []  # initialize container for points
        pt_dist = []
        for dist in range(step, viewmax, step):
            ptx = xx + (dist * math.sin(math.radians(azimut)))
            pty = yy + (dist * math.cos(math.radians(azimut)))
            pt = (ptx, pty)
            points.append(pt)
            pt_dist.append(dist)
        # get row col information
            if ptx < xmax and ptx > xmin:
                x = rastinit[0] + ((math.floor((ptx - rastinit[0]) / rastinit[1])) * rastinit[1])
                ptcol = np.unique(np.argwhere(imgx == x))[1] - mincol
            if pty < ymax and pty > ymin:
                y = rastinit[3] - ((math.ceil((rastinit[3] - pty) / rastinit[5])) * rastinit[5])
                ptrow = np.unique(np.argwhere(imgy == y))[1] - minrow
                ptrc = (ptrow, ptcol)
            # print dist, height[ptrc]-height[sta_rc], x,y, stax, stay, ptrc, sta_rc
            # calculate corresponding angle to reach the height of pt
            if ptrow < w and ptcol < h:
                b = height[ptrc] - height[sta_rc]  # sta[7]
                b = b.astype('float')
            # print b, b/dist, type(b), type(dist), type(b/dist)#)))*100)/100
                if b > 0:
                    angle[0, i] = math.ceil((math.degrees(math.atan(b / dist))) * 100) / 100
                else:
                    angle[0, i] = 0
        # print angle[0,i], max(angle[0,])
        # raw_input()
            i = i + 1
        # print in_stat[0], azimut, max(angle[0,])
        # append each azimut to final data for weather station
        data = (in_stat[0], azimut, max(angle[0, ]), points[np.argwhere(angle == max(angle[0, ]))[0][1]][0], points[np.argwhere(angle == max(angle[0, ]))[0][1]][1])
        final_data.append(data)
        # print(data)
        az = az + [azimut]
        anglee = anglee + [max(angle[0, ])]

    # final_data.append((final_data[0][0], 360, final_data[0][2], final_data[0][3], final_data[0][4]))

# insert values into new table for the given weather station
    az = np.array(az, 'float')
    anglee = np.array(anglee, 'float')
    for values in final_data:
        stawriter.writerow([values[0], values[1], values[2]])
    final_data = None
    print(in_stat[3], "done")
    # print az, anglee
    fig = plt.figure()
    a = fig.add_subplot(111, polar=True)
    rmax = max(40., max(anglee))
    # print rmax-anglee
    a.fill(az * math.pi / 180., rmax - anglee, '-ob', alpha=0.5, edgecolor='b')
    a.set_rmax(rmax)
    a.set_rgrids([0.01, 10., 20., 30., float(int(rmax))], [str(int(rmax)), '30', '20', '10', '0'])
    a.set_thetagrids([0., 45., 90., 135., 180., 225., 270., 315.], ["N", "NE", "E", "SE", "S", "SW", "W", "NW"])
    a.set_title(in_stat[3] + ' alt mnt:' + str(z_alt) + ' m alt poste:' + str(in_stat[1]))
    a.set_theta_zero_location('N')
    a.set_theta_direction(-1)
    plt.savefig('output/' + str(in_stat[0]) + '_skyline.png')

    # show()
    # check de latitude
    diff = z_alt - in_stat[1]
    if abs(diff) > 100.:
        data_bis = (in_stat[0], in_stat[3], in_stat[1], z_alt, diff, 'Warning diff altitude mnt/poste >100m')
    else:
        data_bis = (in_stat[0], in_stat[3], in_stat[1], z_alt, diff)
    ctrwriter.writerow(data_bis)

csvfile.close()
ctrfile.close()
print("done in", time.time() - start_time, "seconds")
