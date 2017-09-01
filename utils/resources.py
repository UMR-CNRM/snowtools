#! /usr/bin/python
# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse

This module contains all file manipulations.
'''

import os
import shutil
from utils.FileException import FileNameException,DirNameException

def absolute_path(pathin):
    '''Convert a local path in a absolute path'''
    if pathin:
        if pathin[0]!="/" :
            pathin=os.getcwd()+"/"+pathin
    
    return pathin

def smart_copy(pathin,nameout):
    '''If pathin includes /home do a symbolic link because we probably are on the disk.
    Otherwise, do a hard copy of the file to improve computing times.'''
    
    if pathin[0:5]=='/home':
        os.symlink(pathin, nameout)
    else:
        shutil.copy(pathin,nameout)

def check_surfex_exe(path):
    
    if not path:
        if "EXESURFEX" in os.environ.keys():
            path=os.environ["EXESURFEX"]
        else:
            raise BaseException("A directory for SURFEX executables must be defined either with -s option or with $EXESURFEX")
    
    for program in ["PGD","PREP","OFFLINE"]:
        if not os.path.isfile(path+"/"+program):
            raise FileNameException(path+"/"+program)

    return path

def save_file_const(path,name,newname=None,copy=False):
    
    if os.path.isdir(path):
    
        if newname:
            savename=path+"/"+newname
        else:
            savename=path+"/"+name
    
        if copy:
            shutil.copy(name,savename)
        else:
            os.rename(name,savename)            
    
    else:
        raise DirNameException(path)    
    
def save_file_date(path,prefix,datefile,newprefix=None):
    
    if newprefix:
        savename=newprefix+"_"+datefile.strftime('%Y%m%d%H')+".nc"
    else:
        savename=prefix+"_"+datefile.strftime('%Y%m%d%H')+".nc"
        
    save_file_const(path,prefix+".nc",savename)

    
def save_file_period(path,prefix,datebegin,dateend,newprefix=None):
    
    if newprefix:
        savename=newprefix+"_"+datebegin.strftime('%Y%m%d%H')+"_"+dateend.strftime('%Y%m%d%H')+".nc"
    else:
        savename=prefix+"_"+datebegin.strftime('%Y%m%d%H')+"_"+dateend.strftime('%Y%m%d%H')+".nc"
        
    save_file_const(path,prefix+".nc",savename)   
    
def get_file_const(pathin,nameout):
    if os.path.isfile(pathin):
        shutil.copy(pathin,nameout)
        return True
    else:
        return False

def get_file_date(path,prefix,datefile):
    
    namefile=prefix+"_"+datefile.strftime('%Y%m%d%H')+".nc"
    return get_file_const(path+"/"+namefile,prefix+".nc")        
    

def get_file_period(prefix,path,datebegin,dateend):
    if os.path.isfile(path):
        smart_copy(path, prefix+".nc")
        return datebegin,dateend
    elif os.path.isdir(path):
        
        # Attempt to find the full file
        fullperiodfile=path+"/"+prefix+"_"+datebegin.strftime('%Y%m%d%H')+"_"+dateend.strftime('%Y%m%d%H')+".nc"        
        if os.path.isfile(fullperiodfile):
            smart_copy(fullperiodfile, prefix+".nc")
            return datebegin,dateend
        
        # Attempt to find a yearly file
        if datebegin.month>=8:
            dateyear_beg=datebegin.replace(month=8,day=1,hour=6)
            dateyear_end=datebegin.replace(year=datebegin.year+1,month=8,day=1,hour=6)
        else:
            dateyear_beg=datebegin.replace(year=datebegin.year-1,month=8,day=1,hour=6)
            dateyear_end=datebegin.replace(month=8,day=1,hour=6)                               
        yearlyfile1=path+"/"+prefix+"_"+dateyear_beg.strftime('%Y%m%d%H')+"_"+dateyear_end.strftime('%Y%m%d%H')+".nc"
        yearlyfile2=path+"/"+prefix+"_"+datebegin.strftime('%Y%m%d%H')+"_"+dateyear_end.strftime('%Y%m%d%H')+".nc"
        yearlyfile3=path+"/"+prefix+"_"+dateyear_beg.strftime('%Y%m%d%H')+"_"+dateend.strftime('%Y%m%d%H')+".nc"     
                  
        if os.path.isfile(yearlyfile1):
            smart_copy(yearlyfile1, prefix+".nc")
            return dateyear_beg,dateyear_end

        if os.path.isfile(yearlyfile2):
            smart_copy(yearlyfile1, prefix+".nc")
            return datebegin,dateyear_end
        
        if os.path.isfile(yearlyfile3):
            smart_copy(yearlyfile1, prefix+".nc")
            return dateyear_beg,dateend

        # Attempt to find a monthly file
        datemonth_beg=datebegin.replace(day=1,hour=6)
        if datebegin.month<12:
            datemonth_end=datemonth_beg.replace(month=datemonth_beg.month+1)
        else:
            datemonth_end=datemonth_beg.replace(year=datemonth_beg.year+1,month=1)
              
        monthlyfile1= path+"/"+prefix+"_"+datemonth_beg.strftime('%Y%m%d%H')+"_"+datemonth_end.strftime('%Y%m%d%H')+".nc"
        monthlyfile2= path+"/"+prefix+"_"+datebegin.strftime('%Y%m%d%H')+"_"+datemonth_end.strftime('%Y%m%d%H')+".nc"
        monthlyfile3= path+"/"+prefix+"_"+datemonth_beg.strftime('%Y%m%d%H')+"_"+dateend.strftime('%Y%m%d%H')+".nc"
        
        if os.path.isfile(monthlyfile1):
            smart_copy(monthlyfile1, prefix+".nc")
            return datemonth_beg,datemonth_end              

        if os.path.isfile(monthlyfile2):
            smart_copy(monthlyfile2, prefix+".nc")
            return datebegin,datemonth_end              
        
        if os.path.isfile(monthlyfile3):
            smart_copy(monthlyfile3, prefix+".nc")
            return datemonth_beg,dateend                      
        
        raise FileNameException(fullperiodfile)        
                
    else:
        raise FileNameException(path)
    
    