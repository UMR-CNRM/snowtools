#!/usr/bin/env python3


import numpy as np
import netCDF4 as nc
from netCDF4 import Dataset
import matplotlib.pyplot as plt
from matplotlib import cm
import cartopy.crs as ccrs
import matplotlib
print(matplotlib.__version__)


class test_medianfile():
    def __init__(self, outputfile, outputreffile):
        self.ftest = Dataset(outputfile)
        self.fref = Dataset(outputreffile)

    def test(self):
        snowcomp = self.ftest.variables['SD_1DY_ISBA'][:, :]
        snowref = self.fref.variables['SD_1DY_ISBA'][:, :]
        #points = self.fref.variables['']

        diff = snowcomp - snowref
        for time in range(7):
            print(time, snowcomp[time,:].min(), snowcomp[time,:].max())
            print(time, snowref[time,:].min(), snowref[time,:].max())
            print(time, diff[time,:].min(), diff[time,:].max())
            plt.plot(diff[time,:])
            plt.show()


class test_pro():
    def __init__(self, inputzeros, inputnonzeros):
        self.bigfile = Dataset(inputzeros)
        self.smallfile = Dataset(inputnonzeros)

    def test(self):
        snowbig = self.bigfile.variables['SD_1DY_ISBA'][:, :]
        snowsmall = self.smallfile.variables['SD_1DY_ISBA'][:, :]
        slope = self.bigfile.variables['slope'][:]
        slopesmall = self.smallfile.variables['slope'][:]
        massif_small = self.smallfile.variables['massif_num'][:]
        massif_big = self.bigfile.variables['massif_num'][:]
        zs_small = self.smallfile.variables['ZS'][:]
        zs_big = self.bigfile.variables['ZS'][:]
        snowbig_sel = snowbig[:, ::17]
        massif_big_sel = massif_big[::17]
        print(sum(massif_small-massif_big_sel))
        timebig = self.bigfile.variables['time'][:]
        timesmall = self.smallfile.variables['time'][:]
        print(snowsmall.shape)
        print(snowbig.shape)
        # snowtrans = np.transpose(snowbig_sel)
        # snowtrans = snowtrans.reshape(snowsmall.shape)
        # snowtrans = snowtrans[:, ::17]
        diff = snowsmall - snowbig_sel
        for time in range(7):
            print(time, snowsmall[time, :].min(), snowsmall[time, :].max())
            print(time, snowbig_sel[time, :].min(), snowbig_sel[time, :].max())
            print(time, diff[time, :].min(), diff[time, :].max())
            plt.plot(diff[time, :])
            plt.show()
        # for ind, ms, zss, testval in zip(range(len(massif_small)), massif_small, zs_small,snowsmall[0, :]):
        #     if testval == 0:
        #         print(ind, ms, zss)
        #     for it in range(7):
        #         [print(ind, i, it, ms, massif_big[i], zss, zs_big[i], slope[i], testval, val) for i, val in enumerate(snowbig[it,:])
        #             if ((testval != 0.) & (abs(val - testval) < 0.00000001))]



class output_test():
    def __init__(self, outputfile, outputreffile, lat_bnds, lon_bnds, figname, flip=True, doubleflip=False):
        self.ftot = Dataset(outputfile)
        self.fcomp = Dataset(outputreffile)
        self.lat_bnds = lat_bnds
        self.lon_bnds = lon_bnds
        self.figname = figname
        self.flip = flip
        self.doubleflip = doubleflip

    def slope_visu(self, figname):
        lats = self.ftot.variables['LAT'][:]
        lons = self.ftot.variables['LON'][:]
        ZS = self.ftot.variables['ZS'][:, :]
        slope = self.ftot.variables['slope'][:, :]
        if self.flip:
            ZS = np.flipud(ZS)
            slope = np.flipud(slope)

        ax = plt.axes(projection=ccrs.PlateCarree())
        plt.pcolormesh(lons, lats, ZS, transform=ccrs.PlateCarree(), cmap=cm.terrain)  # gist_earth
        plt.pcolormesh(lons, lats, slope, # vmin=-0.0001,
                       transform=ccrs.PlateCarree(), cmap=cm.Reds)
        plt.title("Slopes", y=1.09)

        ax.gridlines(draw_labels=True)
        # plt.imshow(massif);
        m = plt.cm.ScalarMappable(cmap=cm.Reds)
        m.set_array(slope)
        # m.set_clim(-0.0001, snowcomp.max())
        m.cmap.set_under(color='w', alpha=0)
        plt.colorbar(m, orientation="horizontal")
        #
        plt.savefig(figname)

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

        if self.doubleflip:
            diff = np.flipud(diff)
            massif_diff = np.flipud(massif_diff)
            compZS = np.flipud(compZS)

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
        lim = max(diff.max(), abs(diff.min()))
        print(diff.max(), diff.min())
        ax = plt.axes(projection=ccrs.PlateCarree())
        # X,Y = np.meshgrid(complons, complats)
        # print(complons.shape)
        plt.pcolormesh(complons, complats, compZS, transform=ccrs.PlateCarree(), cmap=cm.terrain) #gist_earth ,
        plt.pcolormesh(complons, complats, diff, vmin=-lim, vmax=lim,
                     transform=ccrs.PlateCarree(), cmap=cm.seismic)

        ax.gridlines(draw_labels=True)
        m = plt.cm.ScalarMappable(cmap=cm.seismic)
        m.set_array(diff)
        #
        # m.set_clim(-lim, lim)
        # m.cmap.set_under(color='w', alpha=0)
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


# test_median = test_medianfile("/home/radanovicss/Hauteur_neige_median/Out_Belenos/postproc_2020092706_2020092806.nc",
#                               "/home/radanovicss/Hauteur_neige_median/Out_Belenos/cdo_median_numpy_2020092706_2020092806.nc")
# test_median.test()
# test_mb000 = test_medianfile("/home/radanovicss/Hauteur_neige_median/Out_Belenos/pro_2020092706_2020092806.nc",
#                              "/home/radanovicss/Hauteur_neige_median/PRO_2020092706_2020092806_mb000.nc")
# test_mb000.test()
# test_input = test_pro("PRO_2020092706_2020092806_mb035_alps.nc",
#                       "PRO_2020092706_2020092806_mb035_zeroslope_alps.nc")
# test_input.test()

# def test_pyr():

    # ALPS
# lat_bnds, lon_bnds = [43.909, 46.42], [5.19, 7.77]
# PYRENNEES
# lat_bnds, lon_bnds = [42.07, 43.18], [-1.63, 2.71]
# CORSE
# lat_bnds, lon_bnds = [41.69, 42.56], [8.779, 9.279]
alp_test = output_test("test_ignore_nonzeroslopes_allslopes_alps.nc",
                       "test_ignore_nonzeroslopes_zeroslopes_alps.nc",
                       lat_bnds=[43.909, 46.42], lon_bnds = [5.19, 7.77],
                       figname='diff_test_ignore_nonzeroslopes_alps.png',
                       flip=False, doubleflip=True)
# alp_test.snowtest()
alp_test.test()
# alp_test.slope_visu('resolved_slopes_alps.png')
# pyr_test = output_test("dev_multiin_singleout_test_single_in_single_out_pyr.nc",
#                        "output_mulitin_zeroslope_input_pyr.nc",
#                        lat_bnds=[42.07, 43.18], lon_bnds = [-1.63, 2.71],
#                        figname='diff_test_pyr_to_pyr_dev_multiin_singleout_pyr.png',
#                        flip=False)
# pyr_test.test()
# pyr_test.snowtest()

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
