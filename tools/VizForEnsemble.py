#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on Wed Oct 24 17:53:27 2018

@author: deschampsbc
"""
import netCDF4
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats.stats import pearsonr
import csv
import argparse
import os
#inFORref='FORCING_2013080106_2014080106_00.nc'
#inFORlist=['FORCING_2013080106_2014080106_07.nc', 'FORCING_2013080106_2014080106_12.nc', 'FORCING_2013080106_2014080106_14.nc', 'FORCING_2013080106_2014080106_15.nc', 'FORCING_2013080106_2014080106_18.nc', 'FORCING_2013080106_2014080106_09.nc', 'FORCING_2013080106_2014080106_19.nc', 'FORCING_2013080106_2014080106_06.nc', 'FORCING_2013080106_2014080106_04.nc', 'FORCING_2013080106_2014080106_10.nc', 'FORCING_2013080106_2014080106_02.nc', 'FORCING_2013080106_2014080106_08.nc', 'FORCING_2013080106_2014080106_03.nc', 'FORCING_2013080106_2014080106_16.nc', 'FORCING_2013080106_2014080106_13.nc', 'FORCING_2013080106_2014080106_17.nc', 'FORCING_2013080106_2014080106_11.nc', 'FORCING_2013080106_2014080106_05.nc']

def VizForEnsemble(rootDir, outDir):
    
    if outDir.endswith('/'):
        outDir=outDir[:-1]
        
    
    #inFORlist=[]
    # with open(inFORcsv,mode='r') as csv_file:
    #     csv_reader = csv.reader(csv_file)
    #     for row in csv_reader:
    #         inFORlist.append(row[0])
    # Nens=len(inFORlist)
    
    
    ### Read data at the midle point
    
    varList = [ 'Tair' , 'Snowf' , 'DIR_SWdown' , 'Wind' , 'LWdown', 'IMPWET1', 'IMPWET2', 'IMPDRY1', 'IMPDRY2' ]
    inFORlist = []
    for dirpath, dirnames, filenames in os.walk(rootDir):
        for filename in [f for f in filenames if f.endswith(".nc")]:
            inFORlist.append(os.path.join(dirpath, filename))
    inFORlist = sorted(inFORlist)
    print(inFORlist)
    Nens = len(inFORlist)
    FORCINGref=netCDF4.Dataset(inFORlist[0],'r')
    semiDistrib = len(np.shape(FORCINGref.variables['Tair'])) == 2
    Nt=FORCINGref.variables['Tair'][:].shape[0]
    var_dist=np.zeros((Nens,Nt))
    for varName in varList:
        print(varName)
        plt.figure(varName, figsize = (6,9 ))
        if varName in list(FORCINGref.variables.keys()):
            var=FORCINGref.variables[varName]
            if semiDistrib:
                N_mid = int(round(var.shape[1]/2))
                vart_ref =  var[ : , N_mid]
            else:
                Y_mid = int(round(var.shape[1]/2))
                X_mid = int(round(var.shape[2]/2))
                vart_ref =  var[ : , Y_mid , X_mid ]
            

            
            for idx,inFORdist in enumerate(inFORlist):
                inFORdist=netCDF4.Dataset(inFORdist,'r')
                var=inFORdist.variables[varName]
                if semiDistrib:
                    var_dist[idx,:] = var[:,N_mid]
                else:
                    var_dist[idx,:]=var[ : , Y_mid , X_mid ]
            
            ## Figure FOR =f(t) reference and perturbed
            plt.subplot(311)
            for ii in range(0, Nens):
                if 'IMP' in varName:
                    plt.scatter(ii+1, np.sum(var_dist[ii,:]))
                    # print(np.sum(var_dist[ii,:]))
                    plt.scatter(1, np.sum(vart_ref), color = 'k')
                    plt.yscale('log')
                
                
                
                else:
                    
                    plt.plot(np.transpose(var_dist),color=(0.4,0.4,0.4),linewidth=0.1,alpha=0.1)
                    plt.plot(vart_ref,color=(0,0,0),linewidth=1)
            
                    plt.title('Time serie for middle pixel')
                    if 'Snow' in varName:
                        plt.xlim(4000,5000)
                    else:
                        plt.xlim((0,100))
                    plt.xlabel('Time')
                    plt.ylabel(varName)

                
            ### Figure histogram of the mean and SD of residual between reference and perturbed
            dvar=var_dist-vart_ref
            dvar_mean=np.mean(dvar,1)
            dvar_std=np.std(dvar,1)
            
            plt.subplot(312)
            plt.suptitle('Distribution of mean and std of perturbation at middle point of the ensemble')
            plt.boxplot((dvar_mean,dvar_std),labels=('Mean','Std'))
            
            ### Temporal corelation overlay all corel in one plot
            cor_coef=np.zeros((Nens,1000))
            for jj in range(Nens):
                data=dvar[jj,:]
                for ii in range(1,1000):
                    cor_coef[jj,ii-1]=pearsonr(data[:-ii],data[ii:])[0]
                
                
            plt.subplot(313)
            plt.plot(np.transpose(cor_coef))
            plt.title('Auto-correlation')
            plt.xlabel('time lag')
            
            plt.savefig(outDir+'/'+varName+'_dist.png')
            
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description = "Vizualise ensemble weather forcing.")
    parser.add_argument("-r", dest = "r")
    parser.add_argument("-o", dest = "o")
    args = parser.parse_args()
    
    VizForEnsemble( args.r, args.o)

    
