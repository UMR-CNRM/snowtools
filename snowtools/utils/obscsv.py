# -*- coding: utf-8 -*-


"""
Created on 29 oct. 2012

@author: lafaysse
"""

import os
import csv
import re
import datetime

import numpy as np

from snowtools.utils.FileException import FileOpenException, FileNameException


class multiplecsv(object):
    def __init__(self, listpath):
        """
        class for multiple csv files
        :param listpath: list of paths to csv files
        :type listpath: list
        """

        self.__listpath = listpath
        self._data = {}
        self.listcsv = []
        for path in listpath:
            self.listcsv.append(obscsv(path))

    def read(self):
        """
        read csv files
        :return:
        """

        for csvobj in self.listcsv:
            csvfile = csvobj.theFile
            try:
                cr = csv.reader(csvfile, delimiter=";")
            except Exception:
                raise Exception("cant't read correctly the file " + csvobj.path)

            for row in cr:

                # Dans les extractions SIM-BDCLIM de François, il manque le 0 du numéro de station
                if re.match("^\d{7}$", row[0]):
                    row[0] = "0" + row[0]

                if re.match("^\d{8}\d?$", row[0]):
                    if not row[0] in self._data.keys():
                        self._data[row[0]] = {}
                        self._data[row[0]]["time"] = []
                        self._data[row[0]]["SNOWDEPTH"] = []
                        self._data[row[0]]["SNOWSWE"] = []

                    listdate = row[1].split("-")
                    date = datetime.datetime(int(listdate[0]), int(listdate[1]), int(listdate[2]),
                                             int(listdate[3]), int(listdate[4]))
                    self._data[row[0]]["time"].append(date)
                    self._data[row[0]]["SNOWDEPTH"].append(float(row[2].replace(",", ".")))
                    if len(row) >= 4:
                        self._data[row[0]]["SNOWSWE"].append(float(row[3].replace(",", ".")))
                else:
                    print("station ignorée :" + row[0])

        return True

    def get(self, station, varname):
        if station in self._data.keys():
            if varname in self._data[station].keys():
                return np.array(self._data[station][varname])
            else:
                raise Exception(varname + "is not a valid observed variable")
        else:
            raise Exception(station + "is not a valid station")

    def getListStations(self):
        #        return self._data.keys()
        return sorted(self._data)

    def close(self):
        for csvobj in self.listcsv:
            csvfile = csvobj.theFile
            csvfile.close()


class obscsv(object):
    def __init__(self, path):
        """
        class for observation data in csv format
        :param path: path to observation file
        """
        self.path = path
        # Vérification du nom du fichier
        if os.path.isfile(path):
            try:
                self.theFile = open(path, "r")
            except IOError:
                raise FileOpenException(path)
        else:
            raise FileNameException(path)
        self._data = {}

    def read(self, keysfromheader=False):
        """

        :param keysfromheader:
        :return:
        """

        try:
            cr = csv.reader(self.theFile, delimiter=";")
        except Exception:
            raise Exception("cant't read correctly the file " + self.path)

        firstline = True
        for row in cr:

            if keysfromheader and firstline:
                headers = row[:]
                firstline = False
                continue

            # Dans les extractions SIM-BDCLIM de François, il manque le 0 du numéro de station
            if re.match("^\d{7}$", row[0]):
                row[0] = "0" + row[0]

            if re.match("^\d{8}\d?$", row[0]):
                if not row[0] in self._data.keys():
                    self._data[row[0]] = {}
                    self._data[row[0]]["time"] = []
                    if keysfromheader:
                        for header in headers[2:]:
                            self._data[row[0]][header] = []
                    else:
                        self._data[row[0]]["SNOWDEPTH"] = []
                        self._data[row[0]]["SNOWSWE"] = []

                listdate = row[1].split("-")
                date = datetime.datetime(int(listdate[0]), int(listdate[1]), int(listdate[2]),
                                         int(listdate[3]), int(listdate[4]))
                self._data[row[0]]["time"].append(date)
                if keysfromheader:
                    for h, header in enumerate(headers[2:]):
                        self._data[row[0]][header].append(float(row[h+2].replace(",", ".")))
                else:
                    self._data[row[0]]["SNOWDEPTH"].append(float(row[2].replace(",", ".")))
                    if len(row) >= 4:
                        self._data[row[0]]["SNOWSWE"].append(float(row[3].replace(",", ".")))
            else:
                print("station ignorée :" + row[0])

        for station in self._data.keys():
            for varname in self._data[station].keys():
                self._data[station][varname] = np.array(self._data[station][varname])
        return True

    def get(self, station, varname):
        """
        get the data for a specific station and variable name
        :param station: station number
        :param varname: variable name
        :return: data array for the given station and variable
        """
        if station in self._data.keys():
            if varname in self._data[station].keys():
                return self._data[station][varname]
            else:
                raise Exception(varname + "is not a valid observed variable")
        else:
            raise Exception(station + "is not a valid station")

    def getListStations(self):
        #        return self._data.keys()
        return sorted(self._data)

    def close(self):
        """
        close the file
        """
        self.theFile.close()
