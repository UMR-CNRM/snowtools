#!/usr/bin/env python3


import numpy as np
import netCDF4 as nc
from netCDF4 import Dataset
import matplotlib.pyplot as plt
from matplotlib import cm
import cartopy.crs as ccrs

class output_test():
    def __init__(self, outputfile, outputreffile, lat_bnds, lon_bnds, figname, flip=True):
        self.ftot = Dataset(outputfile)
        self.fcomp = Dataset(outputreffile)
        self.lat_bnds = lat_bnds
        self.lon_bnds = lon_bnds
        self.figname = figname
        self.flip = flip

    def test(self):
        massif = self.ftot.variables['massif_num'][:, :]
        lats = self.ftot.variables['LAT'][:]
        lons = self.ftot.variables['LON'][:]
        ZS = self.ftot.variables['ZS'][:,:]
        snow = self.ftot.variables['SD_1DY_ISBA'][7,:,:]

        snowcomp = self.fcomp.variables['SD_1DY_ISBA'][7,:,:]
        complats = self.fcomp.variables['LAT'][:]
        complons = self.fcomp.variables['LON'][:]
        compZS = self.fcomp.variables['ZS'][:,:]
        compmassif = self.fcomp.variables['massif_num'][:, :]

        # print(complons)
        print(complats.min(), complats.max())
        print(complons.min(), complons.max())


        lat_inds = np.where((lats > self.lat_bnds[0]) & (lats < self.lat_bnds[1]))
        lon_inds = np.where((lons > self.lon_bnds[0]) & (lons < self.lon_bnds[1]))
        snow_region = self.ftot.variables['SD_1DY_ISBA'][7, np.min(lat_inds):np.max(lat_inds)+1,np.min(lon_inds):np.max(lon_inds)+1]
        massif_region = massif[np.min(lat_inds):np.max(lat_inds)+1,np.min(lon_inds):np.max(lon_inds)+1]
        # print(lons[np.min(lon_inds):np.max(lon_inds)+1])
        print(snow_region.shape)
        print(snowcomp.shape)
        if self.flip:
            diff = snow_region - np.flipud(snowcomp)
            massif_diff = massif_region - np.flipud(compmassif)
        else:
            diff = snow_region - snowcomp
            massif_diff = massif_region - compmassif

        print(massif_diff.min(), massif_diff.max())
        print(len(diff[diff!=0]))
        # print(diff[diff < 999999].max())
        # print(np.unique(massif))


        # ax = plt.axes(projection=ccrs.PlateCarree())
        # plt.pcolormesh(lons, lats, ZS, transform=ccrs.PlateCarree(), cmap=cm.terrain) #gist_earth
        # plt.pcolormesh(lons, lats, snow, vmin=0,
        #              transform=ccrs.PlateCarree(), cmap=cm.bone)

        # ax.gridlines(draw_labels=True)
        # # plt.imshow(massif);
        # m = plt.cm.ScalarMappable(cmap=cm.bone)
        # m.set_array(snow)
        # m.set_clim(0., snow.max())
        # m.cmap.set_under(color='w', alpha=0)
        # plt.colorbar(m, orientation="horizontal")
        # #
        # plt.savefig('snow_test_alps_to_alpha.png')

        ax = plt.axes(projection=ccrs.PlateCarree())
        plt.pcolormesh(complons, complats, compZS, transform=ccrs.PlateCarree(), cmap=cm.terrain) #gist_earth
        plt.pcolormesh(complons, complats, diff, vmin=-0.0001,
                     transform=ccrs.PlateCarree(), cmap=cm.seismic)

        ax.gridlines(draw_labels=True)
        # plt.imshow(massif);
        m = plt.cm.ScalarMappable(cmap=cm.seismic)
        m.set_array(diff)
        m.set_clim(-0.0001, snowcomp.max())
        m.cmap.set_under(color='w', alpha=0)
        plt.colorbar(m, orientation="horizontal")
        #
        plt.savefig(self.figname)


        # ax = plt.axes(projection=ccrs.PlateCarree())
        # plt.pcolormesh(complons, complats, np.flipud(compZS), transform=ccrs.PlateCarree(), cmap=cm.terrain) #gist_earth
        # plt.pcolormesh(complons, complats, np.flipud(compmassif), #alpha=0.4, # vmin=-1, vmax=1,
        #              transform=ccrs.PlateCarree(), cmap=cm.seismic)
        #
        # ax.gridlines(draw_labels=True)
        # # plt.imshow(massif);
        # m = plt.cm.ScalarMappable(cmap=cm.seismic)
        # m.set_array(compmassif)
        # # m.set_clim(-1.000, 1)
        # # m.cmap.set_over(color='w', alpha=0)
        # plt.colorbar(m, orientation="horizontal")
        # #
        # plt.savefig(self.figname)

        # ax = plt.axes(projection=ccrs.PlateCarree())
        # plt.pcolormesh(complons, complats, compZS, transform=ccrs.PlateCarree(), cmap=cm.terrain) #gist_eart
        #
        # ax.gridlines(draw_labels=True)
        # # plt.imshow(massif);
        # # m = plt.cm.ScalarMappable(cmap=cm.seismic)
        # # m.set_array(diff)
        # #m.set_clim(-0.0001, snowcomp.max())
        # # m.cmap.set_under(color='w', alpha=0)
        # # plt.colorbar(m, orientation="horizontal")
        # #
        # plt.savefig('test_alps_terrain.png')

    def snowtest(self):
        lats = self.ftot.variables['LAT'][:]
        lons = self.ftot.variables['LON'][:]
        snowcomp = self.fcomp.variables['SD_1DY_ISBA'][:, :, :]
        complats = self.fcomp.variables['LAT'][:]
        complons = self.fcomp.variables['LON'][:]
        compmassif = self.fcomp.variables['massif_num'][:, :]

        lat_inds = np.where((lats > self.lat_bnds[0]) & (lats < self.lat_bnds[1]))
        lon_inds = np.where((lons > self.lon_bnds[0]) & (lons < self.lon_bnds[1]))
        snow_region = self.ftot.variables['SD_1DY_ISBA'][:, np.min(lat_inds):np.max(lat_inds) + 1,
                      np.min(lon_inds):np.max(lon_inds) + 1]
        massif_region = self.ftot.variables['massif_num'][np.min(lat_inds):np.max(lat_inds) + 1,
                        np.min(lon_inds):np.max(lon_inds) + 1]
        if self.flip:
            massif_diff = massif_region - np.flipud(compmassif)
        else:
            massif_diff = massif_region - compmassif
        for i in range(snowcomp.shape[0]):
            if self.flip:
                snowdiff = snow_region[i,:,:] - np.flipud(snowcomp[i,:,:])
            else:
                snowdiff = snow_region[i, :, :] - snowcomp[i, :, :]
            # print(snowdiff.shape)
            print(i, np.sum(snowdiff), sum(snowdiff[massif_diff==0]),
                  np.min(snow_region[i, :, :]), np.min(snowcomp[i,:,:]),
                  np.max(snow_region[i,:,:]), np.max(snowcomp[i,:,:]))



# def test_pyr():

    # ALPS
  #  lat_bnds, lon_bnds = [43.909, 46.42], [5.19, 7.77]
# PYRENNEES
# lat_bnds, lon_bnds = [42.07, 43.18], [-1.63, 2.71]
# CORSE
# lat_bnds, lon_bnds = [41.69, 42.56], [8.779, 9.279]
# alp_test = output_test("dev_multiin_singleout_test_single_in_single_out_alps.nc",
#                        "output_mulitin_zeroslope_input_alps.nc",
#                        lat_bnds=[43.909, 46.42], lon_bnds = [5.19, 7.77],
#                        figname='diff_test_alps_to_alps_dev_multiin_singleout_alps.png',
#                        flip=False)
# alp_test.snowtest()
# alp_test.test()
pyr_test = output_test("dev_multiin_singleout_test_single_in_single_out_pyr.nc",
                       "output_mulitin_zeroslope_input_pyr.nc",
                       lat_bnds=[42.07, 43.18], lon_bnds = [-1.63, 2.71],
                       figname='diff_test_pyr_to_pyr_dev_multiin_singleout_pyr.png',
                       flip=False)
pyr_test.test()
pyr_test.snowtest()

# corse_test = output_test("test_v2_multi_in_single_out_alpha.nc",
#                          "output_mulitin_zeroslope_input_cor.nc",
#                        lat_bnds=[41.69, 42.56], lon_bnds = [8.779, 9.279],
#                        figname='diff_test_all_to_alpha_dev_multiin_singleout_cor.png', flip=True)
# corse_test.test()
# corse_test.snowtest()
# corse_massiftest = output_test("/home/radanovicss/Interpol_hauteur_neige/Results/Alpha_output_file_test/output_cor_alpha.nc", "output_original_version_zeroslope_input_cor.nc",
#                        lat_bnds=[41.69, 42.56], lon_bnds = [8.779, 9.279],
#                        figname='massif_comp_test_cor_to_alpha_flipped.png')
# #corse_massiftest.test()
# corse_massiftest.snowtest()
