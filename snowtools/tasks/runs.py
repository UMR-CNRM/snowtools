# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse
'''

# Python general modules
import os
import datetime

# Snowtools modules
from snowtools.tools.change_forcing import forcinput_select, forcinput_applymask,\
    forcinput_tomerge, proselect
from snowtools.tools.change_prep import prep_tomodify
from snowtools.tools.update_namelist import update_surfex_namelist_file
from snowtools.tools.execute import callSurfexOrDie
from snowtools.tools.massif_diags import massif_simu
from snowtools.utils.resources import get_file_period, get_file_date, get_file_const, save_file_period, \
        save_file_date, save_file_const, get_file_const_or_crash, ldd
from snowtools.utils.prosimu import prosimu
from snowtools.utils.FileException import DirFileException
from snowtools.utils.git import get_summary_git
from snowtools.DATA import SNOWTOOLS_DIR, DIRDATAPGD


class surfexrun(object):
    """Class for any SURFEX run"""
    addmask = False

    def __init__(self, datebegin, dateend, forcingpath, diroutput,
                 namelist=os.path.join(SNOWTOOLS_DIR, 'DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC.nam'),
                 execdir=".", onlyextractforcing = False,
                 threshold=-999, workdir=None, datespinup=None, geolist=[], addmask=None, s2mcommand=None):

        # Convert arguments in attributes
        for var in ("datebegin", "dateend", "forcingpath", "diroutput", "namelist", "execdir", "onlyextractforcing",
                    "threshold", "geolist"):
            setattr(self, var, locals()[var])

        # Depending on the child classes, addmask can be provided in arguments of __init__ or in class attributes
        if addmask is not None:
            self.addmask = addmask

        self.dateforcbegin = datebegin
        self.dateforcend = dateend
        self.updateloc = True

        self.defaults_from_env()

        self.dirmeteo = self.diroutput + "/meteo"
        self.dirprep = self.diroutput + "/prep"
        self.dirpro = self.diroutput + "/pro"
        if workdir:
            self.workdir = workdir + "/workSurfex" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")
        else:
            self.workdir = self.diroutput + "/workSurfex" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")

        if datespinup:
            self.dateinit = datespinup
        else:
            self.dateinit = self.datebegin

        if s2mcommand:
            self.s2mcommand = s2mcommand
        else:
            self.s2mcommand = 'unknown'

    def defaults_from_env(self, moderun="NORMAL"):
        machine = os.uname()[1]

        if "taranis" in machine or "belenos" in machine:
            self.nproc = 128
            self.moderun = "MPI"
            self.modeinterpol = "MPI"
        else:
            if not self.onlyextractforcing:
                if os.path.islink(self.execdir + "/OFFLINE"):
                    if "MPIAUTO" in os.readlink(self.execdir + "/OFFLINE"):
                        self.moderun = "MPIRUN"
                    else:
                        self.moderun = moderun
                else:
                    self.moderun = moderun
            else:
                self.moderun = moderun
            if os.path.islink(os.path.join(SNOWTOOLS_DIR, "interpolation/interpol")):
                if "MPIAUTO" in os.readlink(os.path.join(SNOWTOOLS_DIR, "interpolation/interpol")):
                    self.modeinterpol = "MPIRUN"
                else:
                    self.modeinterpol = moderun
            else:
                self.modeinterpol = "NOTCOMPILED"

            if self.moderun == "MPIRUN":
                if "NOFFLINE" in list(os.environ.keys()):
                    self.nproc = int(os.environ["NOFFLINE"])
                else:
                    self.nproc = 4
            else:
                self.nproc = 1

            if self.modeinterpol == "MPIRUN":
                if "NINTERPOL" in list(os.environ.keys()):
                    self.ninterpol = int(os.environ["NINTERPOL"])
                else:
                    self.ninterpol = 4
            else:
                self.ninterpol = 1

        print("Type of run: " + self.moderun + " Number of processes " + str(self.nproc))

    def create_env(self):
        """Create working directory and directories to save outputs"""

        # Note that it is not necessary to remove any existing working directory as the working directory is always new
        # (date in microseconds in the directory name)

        if os.path.isfile(self.diroutput):
            raise DirFileException(self.diroutput)

        # Create all directories
        for directory in [self.dirmeteo, self.dirprep, self.dirpro, self.workdir]:
            if not os.path.isdir(directory):
                os.makedirs(directory)

        # Save initial directory to go back at the end
        self.initcurrentdirectory = os.getcwd()

        # Change current directory to working directory
        os.chdir(self.workdir)

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

            # 5. Post-processing if required
            self.postprocess()

        # 5. Save outputs
        self.save_output()

        if need_other_run:
            # Recursive call to this routine while an other run is required, the next simulation starts at the end of the previous one.
            self.datebegin = self.dateforcend
            self.dateinit = self.dateforcend
            self.run(firstrun=False)
        else:
            os.chdir(self.initcurrentdirectory)

    def get_all_consts(self):
        get_file_const_or_crash(self.namelist, "OPTIONS.nam")

        if not self.onlyextractforcing:
            for ecoclimap_file in ["ecoclimapI_covers_param.bin", "ecoclimapII_eu_covers_param.bin"]:
                get_file_const_or_crash(self.execdir + "/../MY_RUN/ECOCLIMAP/" + ecoclimap_file, ecoclimap_file)

            get_file_const_or_crash(self.execdir + "/../MY_RUN/DATA/CROCUS/drdt_bst_fit_60.nc", "drdt_bst_fit_60.nc")
            #line below is necessary for Tartes Optimisation Option
            #get_file_const_or_crash(self.execdir + "/../MY_RUN/DATA/CROCUS/refice_etotref.nc", "refice_etotref.nc")

    def get_extra_files(self, path):
        import glob
        for extra_file in glob.glob(os.path.join(path, '*')):
            get_file_const_or_crash(extra_file, os.path.basename(extra_file))

    def get_forcing(self):
        ''' Look for a FORCING file including the starting date'''
        self.dateforcbegin, self.dateforcend = get_file_period("FORCING", self.forcingpath, self.datebegin, self.dateend)

        f = prosimu("FORCING.nc")
        print("FORMAT OF FORCING NETCDF FILE: " + f.format())
        #if f.format() != "NETCDF3_CLASSIC":
        #    print("Check consistency with your SURFEX compilation (netcdf4 library required).")
        #    print(ldd(self.execdir + "/OFFLINE"))
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

        print("findprep=", findprep)

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

    def postprocess(self):

        surfex_commit = get_summary_git(os.path.dirname(os.path.normpath(self.execdir)))

        profile = massif_simu("ISBA_PROGNOSTIC.OUT.nc", openmode='a')
        profile.massif_natural_risk()
        profile.dataset.GlobalAttributes(surfex_commit=surfex_commit, snowtools_command=self.s2mcommand)
        profile.dataset.add_standard_names()
        profile.close()


class massifrun(surfexrun):
    """Class for a PC massif SAFRAN-SURFEX run for which the geometry needs to be modified"""
    def modify_forcing(self, list_massif_number, min_alt, max_alt, liste_pentes, list_exp):
        ''' Extract the simulation points in the forcing file.'''
        os.rename("FORCING.nc", "FORCING_base.nc")
        forcinput_select("FORCING_base.nc", "FORCING.nc", list_massif_number, min_alt, max_alt,
                         liste_pentes, list_exp, snowtools_command=self.s2mcommand)

    def save_output(self):
        super(massifrun, self).save_output()
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)


class postesrun(surfexrun):
    """Class for a PC SAFRAN-SURFEX run on stations for which shadows must be applied"""

    @property
    def forcingpathlist(self):
        if type(self.forcingpath) is list:
            return self.forcingpath
        else:
            return [self.forcingpath]

    def get_forcing(self):
        ''' Look for a FORCING file including the starting date'''
        for i, path in enumerate(self.forcingpathlist):
            self.dateforcbegin, self.dateforcend = get_file_period("FORCING", path, self.datebegin, self.dateend)
            os.rename("FORCING.nc", "FORCING_" + str(i) + ".nc")

    def merge_forcing(self, addmask):
        ''' Extract the simulation points in the forcing file.'''
        list_forcing = []
        for i in range(0, len(self.forcingpathlist)):
            list_forcing.append("FORCING_" + str(i) + ".nc")
        if addmask:
            forcinput_applymask(list_forcing, "FORCING.nc",  snowtools_command=self.s2mcommand)
        else:
            forcinput_tomerge(list_forcing, "FORCING.nc", snowtools_command=self.s2mcommand)

    def save_output(self):
        if not self.onlyextractforcing:
            super(postesrun, self).save_output()
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)


class interpolrun(surfexrun):
    addmask = True

    def rename_input(self):
        os.rename("FORCING.nc", "input.nc")

    def rename_output(self):
        os.rename("output.nc", "FORCING.nc")

    def modify_forcing(self, *args):
        self.rename_input()
        print(args)
        if not os.path.islink('GRID.nc'):
            os.symlink(args[0], "GRID.nc")
        callSurfexOrDie(os.path.join(SNOWTOOLS_DIR, "interpolation/interpol"), moderun=self.modeinterpol, nproc=self.ninterpol)
        self.rename_output()

    def save_output(self):
        if not self.onlyextractforcing:
            super(interpolrun, self).save_output()
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)


class interpolpro(interpolrun):
    addmask = False

    def rename_input(self):
        os.rename("PRO.nc", "input.nc")

    def rename_output(self):
        os.rename("output.nc", "PRO.nc")

    def get_forcing(self):
        ''' Look for a PRO file including the starting date'''
        self.dateforcbegin, self.dateforcend = get_file_period("PRO", self.forcingpath, self.datebegin, self.dateend)

    def save_output(self):
        save_file_period(self.dirpro, "PRO", self.dateforcbegin, self.dateforcend)


class massifextractforcing(massifrun):
    def __init__(self, datebegin, dateend, forcingpath, diroutput, workdir=None, geolist=[], s2mcommand=None):
        super(massifextractforcing, self).__init__(datebegin, dateend, forcingpath, diroutput, workdir= workdir,
                                                   geolist=geolist, s2mcommand=s2mcommand)
        self.onlyextractforcing = True

    def save_output(self):
        save_file_period(self.dirmeteo, "FORCING", self.dateforcbegin, self.dateforcend)

    def defaults_from_env(self):
        pass

    def get_all_consts(self):
        pass


class massifextractpro(massifrun):
    def __init__(self, datebegin, dateend, forcingpath, diroutput, workdir=None, geolist=[], s2mcommand=None):
        super(massifextractpro, self).__init__(datebegin, dateend, forcingpath, diroutput, workdir= workdir,
                                               geolist=geolist, s2mcommand=s2mcommand)
        self.onlyextractforcing = True

    def get_forcing(self):
        ''' Look for a PRO file including the starting date'''
        self.dateforcbegin, self.dateforcend = get_file_period("PRO", self.forcingpath, self.datebegin, self.dateend)

    def modify_forcing(self, list_massif_number, min_alt, max_alt, liste_pentes, list_exp):
        ''' Extract the simulation points in the forcing file.'''
        os.rename("PRO.nc", "PRO_base.nc")
        proselect("PRO_base.nc", "PRO.nc", list_massif_number, min_alt, max_alt, liste_pentes, list_exp)

    def save_output(self):
        save_file_period(self.dirpro, "PRO", self.dateforcbegin, self.dateforcend)

    def defaults_from_env(self):
        pass

    def get_all_consts(self):
        pass


class ecoclimaprun(surfexrun):
    """Class for a PC SURFEX run for which vegetation is taken from ECOCLIMAP"""

    def get_all_consts(self):
        super(ecoclimaprun, self).get_all_consts()
        if not os.path.isfile(self.dirprep + "/PGD.nc"):
            if "DIRDATAPGD" in list(os.environ.keys()):
                dirdatapgd = os.environ["DIRDATAPGD"]
            else:
                dirdatapgd = DIRDATAPGD

            print(os.listdir(dirdatapgd))
            for fic in os.listdir(dirdatapgd):
                get_file_const_or_crash(dirdatapgd + "/" + fic, fic, preferlink=True)


class griddedrun(ecoclimaprun):
    """Class for a PC gridded SURFEX run for which the geometry is defined in the namelist"""
    def __init__(self, *args, **kwargs):
        super(griddedrun, self).__init__(*args, **kwargs)
        self.updateloc = False


class interpolgriddedrun(interpolrun, griddedrun):
    addmask = False

    """Class for a PC gridded SURFEX run for which the geometry is defined in the namelist
    and the forcing requires a preliminary interpolation"""
    pass
