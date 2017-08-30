'''
Created on 30 Aug. 2017

@author: lafaysse
'''

from tools.change_forcing import forcinput_select
from tools.change_prep import prep_tomodify
from tools.update_namelist import update_surfex_namelist
from tools.execute import callSurfexOrDie
import os

class surfexrun(object):
    
    """Class for any SURFEX run"""
    
    def __init__(self,datebegin,dateend,forcingpath,threshold=-999):

        self.datebegin=datebegin
        self.dateend=dateend
        self.forcinpath=forcingpath
        self.dateforcbegin=datebegin
        self.dateforcend=dateend        
        self.threshold=threshold
        self.updateloc=True
        self.moderun="NORMAL"
        self.nproc=1
        
    def create_env(self):
        pass
        
    def run(self):
        
        """Describe the sequence of instructions to run SURFEX"""

        # 1. Create the working environment
        self.create_env()
        # 2. Get the resources
#         forcing="FORCING_base.nc"
#         refnamelist="OPTIONS.nam"
        
        # 3. Preprocessing
        # 3.1 Modify the forcing if required
        self.modify_forcing()
        
        # 3.2 Build the appropriate namelist
        update_surfex_namelist(self.datebegin,updateloc=self.updateloc,dateforcbegin=self.dateforcbegin,dateforcend=self.dateforcend)
        
        # 3.3 Modify the initial conditions if required
        self.modify_prep()        
        
        # 4. Run OFFLINE
        callSurfexOrDie("OFFLINE",moderun=self.moderun,nproc=self.nproc)
        
        # 5. Save outputs 
        
    def modify_forcing(self):
        # In the general case, the forcing file is not modified
        os.symlink("FORCING_base.nc","FORCING.nc")
        
    def modify_prep(self):
        # The PREP file needs to be modified if the init date differs from the starting date
        # or if a threshold needs to be modified
        
        modif= (self.threshold>0 and self.datedeb.month==8 and self.datedeb.day==1) or self.datedeb != self.dateinit
        
        if modif:
            prep=prep_tomodify("PREP.nc")
            
            if self.datedeb.month==8 and self.datedeb.day==1:
                if self.threshold>0:
                    prep.apply_swe_threshold(400)
            
            if self.datedeb != self.dateinit:
                prep.change_date(self.datedeb)
            
            prep.close()

class massifrun(surfexrun):
    """Class for a PC massif SAFRAN-SURFEX run for which the geometry needs to be modified"""
    def modify_forcing(self,list_massif_number,min_alt,max_alt,liste_pentes,list_exp):
        forcinput_select("FORCING_base.nc","FORCING.nc",list_massif_number,min_alt,max_alt,liste_pentes,list_exp)
        
class griddedrun(surfexrun):
    """Class for a PC gridded SURFEX run for which the geometry is defined in the namelist"""
    def __init__(self,datebegin,dateend,forcingpath,threshold=-999):
        super(griddedrun,self).__init__(datebegin,dateend,forcingpath,threshold=threshold)
        self.updateloc=False
        
class beaufixmassifrun(massifrun):
    """Class for a beaufix massif SAFRAN-SURFEX run for which the geometry needs to be modified"""    
    def __init__(self,datebegin,dateend,forcingpath,threshold=-999):
        super(beaufixmassifrun,self).__init__(datebegin,dateend,forcingpath,threshold=threshold)
        self.moderun="MPI"
        self.nproc=40
 
class beaufixgriddedrun(griddedrun):
    """Class for a beaufix gridded SURFEX run for which the geometry is defined in the namelist"""    
    def __init__(self,datebegin,dateend,forcingpath,threshold=-999):
        super(beaufixgriddedrun,self).__init__(datebegin,dateend,forcingpath,threshold=threshold)
        self.moderun="MPI"
        self.nproc=40   
