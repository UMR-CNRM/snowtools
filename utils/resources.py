#! /usr/bin/python
# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse

This module contains all file manipulations.
'''

import os
import shutil
from utils.FileException import FileNameException, DirNameException


def absolute_path(pathin):
    '''Convert a local path in a absolute path'''
    if pathin:
        if pathin[0] != "/":
            pathin = os.getcwd() + "/" + pathin

    return pathin


def smart_copy(pathin, nameout):
    '''If pathin includes /home do a symbolic link because we probably are on the disk.
    Otherwise, do a hard copy of the file to improve computing times.'''

    if pathin[0:5] == '/home':
        os.symlink(pathin, nameout)
    else:
        shutil.copy(pathin, nameout)


def check_surfex_exe(path):
    if not path:
        if "EXESURFEX" in os.environ.keys():
            path = os.environ["EXESURFEX"]
        else:
            raise BaseException("A directory for SURFEX executables must be defined either with -s option or with $EXESURFEX")

    for program in ["PGD", "PREP", "OFFLINE"]:
        if not os.path.isfile(path + "/" + program):
            raise FileNameException(path + "/" + program)

    return path


class InstallException(Exception):
    def __init__(self, issue):
        self.issue = issue

    def __str__(self):
        return self.issue + "Not a valid snowtools install !"


def check_snowtools_install():
    # Check installation of snowtools package
    ValidInstall = True
    issue = ""
    if 'SNOWTOOLS_CEN' not in os.environ.keys():
        issue += "SNOWTOOLS_CEN environment variable is not defined.\n"
        ValidInstall = False

    if 'PYTHONPATH' not in os.environ.keys():
        issue += "PYTHONPATH environment variable is not defined.\n"
        ValidInstall = False

    if ValidInstall:
        for rep in ["DATA", "tasks", "tools", "utils"]:
            if not os.path.isdir(os.environ["SNOWTOOLS_CEN"] + "/" + rep):
                issue += "There is not a correct install of snowtools_git in directory " + os.environ["SNOWTOOLS_CEN"] + "\n"
                issue += "missing directory:" + rep + "\n"
                ValidInstall = False
    if ValidInstall:
        if not os.environ["SNOWTOOLS_CEN"] in os.environ["PYTHONPATH"]:
            issue += "PYTHONPATH environment variable must contain" + os.environ["SNOWTOOLS_CEN"] + "\n"
            ValidInstall = False

    if not ValidInstall:
        raise InstallException(issue)


def save_file_const(path, name, newname=None, copy=False):
    if os.path.isdir(path):
        if newname:
            savename = path + "/" + newname
        else:
            savename = path + "/" + name

        if copy:
            shutil.copy(name, savename)
        else:
            os.rename(name, savename)

    else:
        raise DirNameException(path)


def save_file_date(path, prefix, datefile, newprefix=None, copy=False):
    if newprefix:
        savename = newprefix + "_" + datefile.strftime('%Y%m%d%H') + ".nc"
    else:
        savename = prefix + "_" + datefile.strftime('%Y%m%d%H') + ".nc"
    save_file_const(path, prefix + ".nc", savename, copy=copy)


def save_file_period(path, prefix, datebegin, dateend, newprefix=None):

    if newprefix:
        savename = newprefix + "_" + datebegin.strftime('%Y%m%d%H') + "_" + dateend.strftime('%Y%m%d%H') + ".nc"
    else:
        savename = prefix + "_" + datebegin.strftime('%Y%m%d%H') + "_" + dateend.strftime('%Y%m%d%H') + ".nc"

    save_file_const(path, prefix + ".nc", savename)


def get_file_const_or_crash(pathin, nameout):
    if not get_file_const(pathin, nameout):
        raise FileNameException(pathin)


def get_file_const(pathin, nameout):
    if os.path.isfile(pathin):
        shutil.copy(pathin, nameout)
        return True
    else:
        return False


def get_file_date(prefix, path, datefile):

    namefile = prefix + "_" + datefile.strftime('%Y%m%d%H') + ".nc"
    return get_file_const(path + "/" + namefile, prefix + ".nc")


def get_file_period(prefix, path, datebegin, dateend):
    if os.path.isfile(path):
        smart_copy(path, prefix + ".nc")
        return datebegin, dateend
    elif os.path.isdir(path):
        # Attempt to find the full file
        fullperiodfile = path + "/" + prefix + "_" + datebegin.strftime('%Y%m%d%H') + "_" + dateend.strftime('%Y%m%d%H') + ".nc"
        if os.path.isfile(fullperiodfile):
            smart_copy(fullperiodfile, prefix + ".nc")
            return datebegin, dateend

        # Attempt to find a yearly file
        if datebegin.month >= 8:
            dateyear_beg = datebegin.replace(month=8, day=1, hour=6)
            dateyear_end = datebegin.replace(year=datebegin.year + 1, month=8, day=1, hour=6)
        else:
            dateyear_beg = datebegin.replace(year=datebegin.year - 1, month=8, day=1, hour=6)
            dateyear_end = datebegin.replace(month=8, day=1, hour=6)
        yearlyfile1 = path + "/" + prefix + "_" + dateyear_beg.strftime('%Y%m%d%H') + "_" + dateyear_end.strftime('%Y%m%d%H') + ".nc"
        yearlyfile2 = path + "/" + prefix + "_" + datebegin.strftime('%Y%m%d%H') + "_" + dateyear_end.strftime('%Y%m%d%H') + ".nc"
        yearlyfile3 = path + "/" + prefix + "_" + dateyear_beg.strftime('%Y%m%d%H') + "_" + dateend.strftime('%Y%m%d%H') + ".nc"

        if os.path.isfile(yearlyfile1):
            smart_copy(yearlyfile1, prefix + ".nc")
            return dateyear_beg, dateyear_end

        if os.path.isfile(yearlyfile2):
            smart_copy(yearlyfile1, prefix + ".nc")
            return datebegin, dateyear_end

        if os.path.isfile(yearlyfile3):
            smart_copy(yearlyfile1, prefix + ".nc")
            return dateyear_beg, dateend

        # Attempt to find a monthly file
        datemonth_beg = datebegin.replace(day=1, hour=6)
        if datebegin.month < 12:
            datemonth_end = datemonth_beg.replace(month=datemonth_beg.month + 1)
        else:
            datemonth_end = datemonth_beg.replace(year=datemonth_beg.year + 1, month=1)

        monthlyfile1 = path + "/" + prefix + "_" + datemonth_beg.strftime('%Y%m%d%H') + "_" + datemonth_end.strftime('%Y%m%d%H') + ".nc"
        monthlyfile2 = path + "/" + prefix + "_" + datebegin.strftime('%Y%m%d%H') + "_" + datemonth_end.strftime('%Y%m%d%H') + ".nc"
        monthlyfile3 = path + "/" + prefix + "_" + datemonth_beg.strftime('%Y%m%d%H') + "_" + dateend.strftime('%Y%m%d%H') + ".nc"

        if os.path.isfile(monthlyfile1):
            smart_copy(monthlyfile1, prefix + ".nc")
            return datemonth_beg, datemonth_end

        if os.path.isfile(monthlyfile2):
            smart_copy(monthlyfile2, prefix + ".nc")
            return datebegin, datemonth_end

        if os.path.isfile(monthlyfile3):
            smart_copy(monthlyfile3, prefix + ".nc")
            return datemonth_beg, dateend

        raise FileNameException(fullperiodfile)

    else:
        raise FileNameException(path)

# python-chroot-builder
# Copyright (C) 2012 Ji-hoon Kim
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


def ldd(filename):
    import subprocess

    libs = []
    for x in filename:
        p = subprocess.Popen(["ldd", x],
                             stdout = subprocess.PIPE,
                             stderr = subprocess.PIPE)

        result = p.stdout.readlines()

        for x in result:
            s = x.split()
            if "=>" in x:
                if len(s) == 3:  # virtual library
                    pass
                else:
                    libs.append(s[2])
            else:
                if len(s) == 2:
                    libs.append(s[0])

    return libs