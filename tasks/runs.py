#! /usr/bin/python
# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse
'''

# Python general modules
import os
import datetime

# Snowtools modules
from tools.change_forcing import forcinput_select, forcinput_applymask,\
    forcinput_tomerge
from tools.change_prep import prep_tomodify
from tools.update_namelist import update_surfex_namelist_file
from tools.execute import callSurfexOrDie
from utils.resources import get_file_period, get_file_date, get_file_const, save_file_period, save_file_date, save_file_const,\
    get_file_const_or_crash, ldd
from utils.prosimu import prosimu
from utils.FileException import DirFileException


class surfexrun(object):

    """Class for any SURFEX run"""

    def __init__(self, datebegin, dateend, forcingpath, diroutput,
                 namelist=os.environ['SNOWTOOLS_CEN'] + '/DATA/OPTIONS_V8_NEW_OUTPUTS_NC.nam',
                 execdir=".",
                 threshold=-999, dirwork=None, datespinup=None, geolist=[], addmask=False):

        # Convert arguments in attributes
        for var in "datebegin", "dateend", "forcingpath", "diroutput", "namelist", "execdir", "threshold", "geolist", "addmask":
            setattr(self, var, locals()[var])

        mavariable = 12
        mavariable = [mavariable, 8, 10, 12]

        self.dateforcbegin = datebegin
        self.dateforcend = dateend
        self.updateloc = True
        self.onlyextractforcing = False

        self.defaults_from_env()

        self.dirmeteo = self.diroutput + "/meteo"
        self.dirprep = self.diroutput + "/prep"
        self.dirpro = self.diroutput + "/pro"
        if dirwork:
            self.dirwork = dirwork + "/workSurfex" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")
        else:
            self.dirwork = self.diroutput + "/workSurfex" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")

        if datespinup:
            self.dateinit = datespinup
        else:
            self.dateinit = self.datebegin

    def defaults_from_env(self, moderun="NORMAL"):
        machine = os.uname()[1]

        if "beaufix" in machine or "prolix" in machine:
            self.nproc = 40
            self.moderun = "MPI"
        else:
            if "MPIAUTO" in os.readlink(self.execdir + "/OFFLINE"):
                self.moderun = "MPIRUN"
            else:
                self.moderun = moderun

            if self.moderun == "MPIRUN":
                if "NOFFLINE" in os.environ.keys():
                    self.nproc = int(os.environ["NOFFLINE"])
                else:
                    self.nproc = 4
            else:
                self.nproc = 1

        print "Type of run: " + self.moderun + " Number of processes " + str(self.nproc)

    def create_env(self):
        """Create working directory and directories to save outputs"""

        # Note that it is not necessary to remove any existing working directory as the working directory is always new
        # (date in microseconds in the directory name)

        if os.path.isfile(self.diroutput):
            raise DirFileException(self.diroutput)

        # Create all directories
        for directory in [self.dirmeteo, self.dirprep, self.dirpro, self.dirwork]:
            if not os.path.isdir(directory):
                os.makedirs(directory)

        # Change current directory to working directory
        os.chdir(self.dirwork)

    def run(self, firstrun=True):

        """Describe the sequence of instructions to run SURFEX"""

        if firstrun:
            # 1.1 Create the working environment
            self.create_env()
            # 1.2 Get all constant files
            self.get_all_consts()
        else:
            self.updateloc = False

        # 2. Get the forcing
        self.get_forcing()

        need_other_run = self.dateforcend < self.dateend
        self.dateend_run = min(self.dateend, self.dateforcend)

        # 3. Preprocessing
        # 3.1 Modify the forcing if required
        self.modify_forcing(*self.geolist)
        self.merge_forcing(self.addmask)

        if not self.onlyextractforcing:
            # 3.2 Build the appropriate namelist. At second run, only temporal modif
            update_surfex_namelist_file(self.datebegin, dateend=self.dateend_run, updateloc=self.updateloc)

            if firstrun:
                # 3.3 Get the PGD file or generate it
                self.get_or_run_pgd()

            # 3.4 Get the PREP file or generate it
            self.get_or_run_prep()

            # 3.5 Modify the initial conditions if required
            self.modify_prep()

            # 4. Run OFFLINE
            callSurfexOrDie(self.execdir + "/OFFLINE", moderun=self.moderun, nproc=self.nproc)

        # 5. Save outputs
        self.save_output()

        if need_other_run:
            # Recursive call to this routine while an other run is required, the next simulation starts at the end of the previous one.
            self.datebegin = self.dateforcend
            self.dateinit = self.dateforcend
            self.run(firstrun=False)

    def get_all_consts(self):
        get_file_const_or_crash(self.namelist, "OPTIONS.nam")

        for ecoclimap_file in ["ecoclimapI_covers_param.bin", "ecoclimapII_eu_covers_param.bin"]:
            get_file_const_or_crash(self.execdir + "/../MY_RUN/ECOCLIMAP/" + ecoclimap_file, ecoclimap_file)

        get_file_const_or_crash(self.execdir + "/../MY_RUN/DATA/CROCUS/drdt_bst_fit_60.nc", "drdt_bst_fit_60.nc")

    def get_forcing(self):
        ''' Look for a FORCING file including the starting date'''
        self.dateforcbegin, self.dateforcend = get_file_period("FORCING", self.forcingpath, self.datebegin, self.dateend)

        f = prosimu("FORCING.nc")
        print "FORMAT OF FORCING NETCDF FILE: " + f.format()
        if f.format() != "NETCDF3_CLASSIC":
            print "Check consistency with your SURFEX compilation (netcdf4 library required)."
            print ldd(self.execdir + "/OFFLINE")
        f.close()

    def get_or_run_pgd(self):
        ''' Look for a PGD file to configure the simulation or run PGD and save it'''
        findpgd = get_file_const(self.dirprep + "/PGD.nc", "PGD.nc")
        if not findpgd:
            callSurfexOrDie(self.execdir + "/PGD", moderun=self.moderun, nproc=self.nproc)
            save_file_const(self.dirprep, "PGD.nc", copy=True)

    def get_or_run_prep(self):
        ''' Look for a PREP file to restart the simulation or run PREP and save it'''
        findprep = get_file_date("PREP", self.dirprep, self.dateinit)

        print "findprep=", findprep

        if not findprep:
            get_file_const_or_crash(self.dirprep + "/init_TG.nc", "init_TG.nc")
            callSurfexOrDie(self.execdir + "/PREP", moderun=self.moderun, nproc=self.nproc)
            save_file_date(self.dirprep, "PREP", self.dateinit, copy=True)

    def save_output(self):
        ''' Save outputs of 1 OFFLINE run'''
        save_file_date(self.dirprep, "SURFOUT", self.dateend_run, newprefix="PREP")
        save_file_period(self.dirpro, "ISBA_PROGNOSTIC.OUT", self.datebegin, self.dateend_run, newprefix="PRO")

    def modify_forcing(self, *args, **kwargs):
        ''' In the general case, the forcing file is not modified.'''
        pass

    def merge_forcing(self, *args, **kwargs):
        ''' In the general case, the forcing file is not modified.'''
        pass

    def modify_prep(self):
        ''' The PREP file needs to be modified if the init date differs from the starting date
         or if a threshold needs to be applied on snow water equivalent.'''

        modif = (self.threshold > 0 and self.datebegin.month == 8 and self.datebegin.day == 1) or self.datebegin != self.dateinit

        if modif:
            prep = prep_tomodify("PREP.nc")

            if self.datebegin.month == 8 and self.datebegin.day == 1:
                if self.threshold > 0:
                    prep.apply_swe_threshold(self.threshold)

            if self.datebegin != self.dateinit:
                prep.change_date(self.datebegin)

            prep.close()


class massifrun(surfexrun):
    """Class for a PC massif SAFRAN-SURFEX run for which the geometry needs to be modified"""
    def modify_forcing(self, list_massif_number, min_alt, max_alt, liste_pentes, list_exp):
        ''' Extract the simulation points in the forcing file.'''
        os.rename("FORCING.nc", "FORCING_base.nc")
        forcinput_select("FORCING_base.nc", "FORCING.nc", list_massif_number, min_alt, max_alt, liste_pentes, list_exp)

    def save_output(self):
        super(massifrun, self).save_output()
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)


class postesrun(surfexrun):
    """Class for a PC SAFRAN-SURFEX run on stations for which shadows must be applied"""

    def get_forcing(self):
        ''' Look for a FORCING file including the starting date'''
        for i, path in enumerate(self.forcingpath):
            self.dateforcbegin, self.dateforcend = get_file_period("FORCING", path, self.datebegin, self.dateend)
            os.rename("FORCING.nc", "FORCING_" + str(i) + ".nc")

    def merge_forcing(self, addmask):
        ''' Extract the simulation points in the forcing file.'''
        list_forcing = []
        for i in range(0, len(self.forcingpath)):
            list_forcing.append("FORCING_" + str(i) + ".nc")
        if addmask:
            forcinput_applymask(list_forcing, "FORCING.nc")
        else:
            forcinput_tomerge(list_forcing, "FORCING.nc")

    def save_output(self):
        super(postesrun, self).save_output()
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)


class massifextractforcing(massifrun):
    def __init__(self, datebegin, dateend, forcingpath, diroutput, dirwork=None, geolist=[]):
        super(massifextractforcing, self).__init__(datebegin, dateend, forcingpath, diroutput, dirwork= dirwork, geolist= geolist)
        self.onlyextractforcing = True

    def save_output(self):
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)

    def defaults_from_env(self):
        pass

    def get_all_consts(self):
        pass


class griddedrun(surfexrun):
    """Class for a PC gridded SURFEX run for which the geometry is defined in the namelist"""
    def __init__(self, datebegin, dateend, forcingpath, threshold=-999):
        super(griddedrun, self).__init__(datebegin, dateend, forcingpath, threshold=threshold)
        self.updateloc = False
