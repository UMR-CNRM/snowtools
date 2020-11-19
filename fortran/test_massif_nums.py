#!/usr/bin/env python3


import numpy as np
import netCDF4 as nc
from netCDF4 import Dataset
import matplotlib.pyplot as plt
from matplotlib import cm
import cartopy.crs as ccrs

ftot = Dataset("alpha_massifs.nc")
massif = ftot.variables['massif_num'][:, :]
lats = ftot.variables['latitude'][:]
lons = ftot.variables['longitude'][:]
ZS = ftot.variables['ZS'][:,:]
print(lons)

print(np.unique(massif))

# fig = plt.figure(figsize=(110,100))
ax = plt.axes(projection=ccrs.PlateCarree())
plt.pcolormesh(lons, lats, ZS, transform=ccrs.PlateCarree(), cmap=cm.terrain) #gist_earth
plt.pcolormesh(lons, lats, massif, vmin=1,
             transform=ccrs.PlateCarree(), cmap=cm.prism)

ax.gridlines(draw_labels=True)
# plt.imshow(massif);
m = plt.cm.ScalarMappable(cmap=cm.prism)
m.set_array(massif)
m.set_clim(1., 91.)
m.cmap.set_under(color='w', alpha=0)
plt.colorbar(m, boundaries=np.linspace(1, 92, 92), orientation="horizontal")
#
plt.savefig('massif_alpha.png')
