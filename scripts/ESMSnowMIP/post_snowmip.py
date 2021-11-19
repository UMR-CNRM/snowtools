#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Auteur : M lafaysse
# Date : July 2017


import numpy as np
import netCDF4

from snowtools.utils.prosimu import prosimu


class ESMSnowMIP_output:
    def __init__(self, filein, fileout):

        nsoillayer = 20
        nsnowlayer = 50

        DataSim = prosimu(filein)
        availVar = DataSim.listvar()

        dicvar_direct = {"hfls": "LE_ISBA", "hfsbl": "LEI_ISBA", "hfss": "H_ISBA", "rlus": "LWU_ISBA", "rsus": "SWU_ISBA", "evspsbl": "EVAP_ISBA",
                         "mrrob": "DRAIN_ISBA", "mrros": "RUNOFF_ISBA", "snmsl": "SNOMLT_ISBA", "albs": "TALB_ISBA", "albsn": "ASN_ISBA", "cw": "WR",
                         "snc": "PSN_ISBA", "snd": "DSN_T_ISBA", "snw": "WSN_T_ISBA", "snwc": "WRVN", "tcs": "TV", "ts": "TS_ISBA"}

        dicvar_xlvtt = {"esn": "LESL_ISBA", "evspsblveg": "LEV_ISBA", "tran": "LETR_ISBA"}
        dicvar_xlstt = {"sbl": "LES_ISBA"}

        # dicvarsnow_direct = {"tsn": "SNOWTEMP", "SNOWDZ": "SNOWDZ"}

        # dicvar_= {"hfls": "LE_ISBA", "hfsbl": "LEI_ISBA", "hfss": "H_ISBA", "rlus": "LWU_ISBA", "rsus": "SWU_ISBA", "evspsbl": "EVAP_ISBA",
        #           "mrrob": "DRAIN_ISBA", "mrros": "RUNOFF_ISBA", "snmsl": "SNOMLT_ISBA", "albs": "TALB_ISBA", "albsn": "ASN_ISBA", "cw": "WR",
        #           "snc": "PSN_ISBA", "snd": "DSN_T_ISBA", "snw": "WSN_T_ISBA", "snwc": "WRVN", "tcs": "TV", "ts": "TS_ISBA"}

        dicvar_units = {"hfls": "W.m-2", "hfsbl": "W.m-2", "hfss": "W.m-2", "rlus": "W.m-2", "rsus": "W.m-2", "evspsbl": "kg.m-2.s-1",
                        "mrrob": "kg.m-2.s-1", "mrros": "kg.m-2.s-1", "snmsl": "kg.m-2.s-1", "albs": "", "albsn": "", "cw": "kg.m-2",
                        "snc": "", "snd": "m", "snw": "kg.m-2", "snwc": "kg.m-2", "tcs": "K", "ts": "K"}

        newFile = netCDF4.Dataset(fileout, "w")
        newFile.createDimension("time", None)
        newFile.createDimension("Number_of_points", 1)
        newFile.createDimension("soil_layer", nsoillayer)
        newFile.createDimension("snow_layer", nsnowlayer)

        time, units = DataSim.readtime_for_copy()
        var = newFile.createVariable("time", 'd', ("time"), fill_value=1.e20)
        var[:] = time[:]
        var.units = units

        for varout, varin in dicvar_direct.iteritems():

            if varin in availVar:
                print(varout)
                varData = DataSim.read(varin, keepfillvalue=True)

                var = newFile.createVariable(varout, 'd', ("time"), fill_value=1.e20)
                var[:] = varData[:]
                var.units = dicvar_units[varout]

        for varout, varin in dicvar_xlvtt.iteritems():

            if varin in availVar:
                print(varout)
                varData = DataSim.read(varin, keepfillvalue=True)

                var = newFile.createVariable(varout, 'd', ("time"), fill_value=1.e20)
                var[:] = varData[:] / 2500800.
                var.units = "kg.m-2.s-1"

        for varout, varin in dicvar_xlstt.iteritems():

            if varin in availVar:
                print(varout)
                varData = DataSim.read(varin, keepfillvalue=True)

                var = newFile.createVariable(varout, 'd', ("time"), fill_value=1.e20)
                var[:] = varData[:] / 2834500.
                var.units = "kg.m-2.s-1"

        print("evspsblsoi")
        leg = DataSim.read("LEG_ISBA", keepfillvalue=True)
        legi = DataSim.read("LEGI_ISBA", keepfillvalue=True)
        var = newFile.createVariable("evspsblsoi", 'd', ("time"), fill_value=1.e20)
        var[:] = leg / 2500800. + legi / 2834500.

        print("tsl", "mrlsl", "mrfsofr")
        var = newFile.createVariable("tsl", 'd', ("time", "soil_layer"), fill_value=1.e20)

        tg = np.empty_like(var)
        wg = np.empty_like(var)
        fracice = np.empty_like(var)

        for soillayer in range(1, nsoillayer + 1):
            varData = DataSim.read("TG" + str(soillayer), keepfillvalue=True, selectpoint=0)
            tg[:, soillayer - 1] = varData[:]
            liq = DataSim.read("WG" + str(soillayer), keepfillvalue=True, selectpoint=0)
            ice = DataSim.read("WGI" + str(soillayer), keepfillvalue=True, selectpoint=0)
            wg[:, soillayer - 1] = liq[:] + ice[:]
            fracice[:, soillayer - 1] = ice[:] / wg[:, soillayer - 1]

        var[:, :] = tg[:, :]
        var.units = "K"
        var = newFile.createVariable("mrlsl", 'd', ("time", "soil_layer"), fill_value=1.e20)
        var[:, :] = wg[:, :]
        var.units = "kg.m-2"
        var = newFile.createVariable("mrfsofr", 'd', ("time", "soil_layer"), fill_value=1.e20)
        var[:, :] = fracice[:, :]
        var.units = ""

        print("tsn")
        var = newFile.createVariable("tsn", 'd', ("time"), fill_value=1.e20)
        snowtemp = DataSim.read("SNOWTEMP", keepfillvalue=True, selectpoint=0)
        snowswe = DataSim.read("WSN_VEG", fill2zero=True, selectpoint=0)
        var[:] = np.sum(snowtemp[:, :] * snowswe[:, :], axis=1) / np.sum(snowswe[:, :], axis=1)
        var.units = 'K'
        print("tsns")
        var = newFile.createVariable("tsns", 'd', ("time"), fill_value=1.e20)
        var[:] = snowtemp[:, 0]
        var.units = 'K'

        print("lwsnl")
        var = newFile.createVariable("lwsnl", 'd', ("time"), fill_value=1.e20)
        snowliq = DataSim.read("SNOWLIQ", keepfillvalue=True, selectpoint=0)
        var[:] = np.sum(snowliq, axis=1)
        var.units = 'kg.m-2'

        DataSim.close()
        newFile.close()
