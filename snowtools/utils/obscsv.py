# -*- coding: utf-8 -*-


'''
Created on 29 oct. 2012

@author: lafaysse
'''

import os
import csv
import re
import datetime

import numpy as np

from snowtools.utils.FileException import FileOpenException, FileNameException


class multiplecsv(object):
    def __init__(self, listpath):

        self.__listpath = listpath
        self.listcsv = []
        for path in listpath:
            self.listcsv.append(obscsv(path))

    def read(self):

        self._data = {}
        for csvobj in self.listcsv:
            csvfile = csvobj.theFile
            try:
                cr = csv.reader(csvfile, delimiter=";")
            except Exception:
                raise BaseException("cant't read correctly the file " + self.__path)

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
                    date = datetime.datetime(int(listdate[0]), int(listdate[1]), int(listdate[2]), int(listdate[3]), int(listdate[4]))
                    self._data[row[0]]["time"].append(date)
                    self._data[row[0]]["SNOWDEPTH"].append(float(row[2].replace(",", ".")))
                    if len(row) >= 4:
                        self._data[row[0]]["SNOWSWE"].append(float(row[3].replace(",", ".")))
                else:
                    print ("station ignorée :" + row[0])

        return True

    def get(self, station, varname):
        if station in self._data.keys():
            if varname in self._data[station].keys():
                return np.array(self._data[station][varname])
            else:
                raise BaseException(varname + "is not a valid observed variable")
        else:
            raise BaseException(station + "is not a valid station")

    def getListStations(self):
        #        return self._data.keys()
        return sorted(self._data)

    def close(self):
        for csvobj in self.listcsv:
            csvfile = csvobj.theFile
            csvfile.close()


class obscsv(object):
    def __init__(self, path):
        # Vérification du nom du fichier
        if os.path.isfile(path):
            try:
                self.theFile = open(path, "r")
                self.__path = path
            except IOError:
                raise FileOpenException(path)
        else:
            raise FileNameException(path)

    def read(self, keysfromheader=False):

        try:
            cr = csv.reader(self.theFile, delimiter=";")
        except Exception:
            raise BaseException("cant't read correctly the file " + self.__path)

        self._data = {}

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
                date = datetime.datetime(int(listdate[0]), int(listdate[1]), int(listdate[2]), int(listdate[3]), int(listdate[4]))
                self._data[row[0]]["time"].append(date)
                if keysfromheader:
                    for h, header in enumerate(headers[2:]):
                        self._data[row[0]][header].append(float(row[h+2].replace(",", ".")))
                else:
                    self._data[row[0]]["SNOWDEPTH"].append(float(row[2].replace(",", ".")))
                    if len(row) >= 4:
                        self._data[row[0]]["SNOWSWE"].append(float(row[3].replace(",", ".")))
            else:
                print ("station ignorée :" + row[0])

        for station in self._data.keys():
            for varname in self._data[station].keys():
                self._data[station][varname] = np.array(self._data[station][varname])
        return True

    def get(self, station, varname):
        if station in self._data.keys():
            if varname in self._data[station].keys():
                return self._data[station][varname]
            else:
                raise BaseException(varname + "is not a valid observed variable")
        else:
            raise BaseException(station + "is not a valid station")

    def getListStations(self):
        #        return self._data.keys()
        return sorted(self._data)

    def close(self):
        self.theFile.close()
