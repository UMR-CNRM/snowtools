#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 30 12:42:44 2023

@author: Jari-Pekka Nousu
"""

"""
Functions to plot surface energy budget of SURFEX simulation
Intended for outputs of SURFEX/ISBA-Crocus and SURFEX/ISBA-MEB-Crocus
Additional function to plot energy budget of observation pandas dataframe
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def proplot_SEB(profile, period, resample=None, 
                snow='DSN_T_ISBA', title='TEST', fluxlims=None, 
                bar_width=0.2, save_path=None):
    
    '''
    Plots total surface energy budget (H, LE, RN, G) of a given profile and a period.
    Sign convention relative to the surface (i.e. positive means that surface is gaining energy)
    To plot snow: snow='WSN_T_ISBA' or snow='DSN_T_ISBA'
    '''
        
    eb = pd.DataFrame()
    eb['time'] = period
    
    if snow:
        sn = pd.DataFrame()
        sn.index = period
        sn['snow'] = np.nan
        sn['snow'] = profile[snow].sel(time=period).values
    
    fluxes = ['GFLUX_ISBA', 'LE_ISBA', 'H_ISBA', 'RN_ISBA']
    
    for flux in fluxes:
        if flux != 'RN_ISBA':
            eb[flux] = profile[flux].sel(time=period)*-1
        else:
            eb[flux] = profile[flux].sel(time=period)

    eb.index = eb['time']
    eb = eb.drop('time', axis=1)

    if resample:
        eb = eb.resample(resample).mean()
        if snow:
            sn = sn.resample(resample).mean()

    eb_pos = eb.copy()
    eb_neg = eb.copy()

    for col in eb_pos.columns:
        eb_pos[col] = eb.loc[eb[col] > 0, col]
        eb_pos.loc[eb_pos[col] < 0, col] = 0
        eb_neg[col] = eb.loc[eb[col] < 0, col]
        eb_neg.loc[eb_neg[col] > 0, col] = 0

    for i in eb_pos.columns:
        eb_pos[i] = eb_pos[i].fillna(0)
        eb_neg[i] = eb_neg[i].fillna(0)
    
    width = bar_width
    
    fig, ax = plt.subplots(figsize=(8,4))
    
    if snow:
        axb = ax.twinx()
        axb.plot(sn['snow'], color='black', alpha=0.7, label=f'{snow}')
        axb.legend(loc='lower left')
        axb.set_ylabel(f'{snow} [{profile[snow].units}]')
        max_snow = profile[snow].sel(time=period).max()
        axb.set_ylim([0,max_snow*1.05])

    ax.bar(eb_pos.index, eb_pos['RN_ISBA'], width=width, color='tab:orange', label='RN')
    ax.bar(eb_pos.index, eb_pos['H_ISBA'],  bottom=eb_pos['RN_ISBA'], width=width, color='tab:blue', label='H')
    ax.bar(eb_pos.index, eb_pos['LE_ISBA'],   bottom=(eb_pos['RN_ISBA']+eb_pos['H_ISBA']), width=width,color='tab:green', label='LE')
    ax.bar(eb_pos.index, eb_pos['GFLUX_ISBA'],   bottom=(eb_pos['RN_ISBA']+eb_pos['H_ISBA']+eb_pos['LE_ISBA']), width=width,color='tab:red', label='G')

    ax.bar(eb_neg.index, eb_neg['RN_ISBA'], width=width, color='tab:orange')
    ax.bar(eb_neg.index, eb_neg['H_ISBA'],  bottom=eb_neg['RN_ISBA'], width=width, color='tab:blue')
    ax.bar(eb_neg.index, eb_neg['LE_ISBA'],   bottom=(eb_neg['RN_ISBA']+eb_neg['H_ISBA']), width=width, color='tab:green')
    ax.bar(eb_neg.index, eb_neg['GFLUX_ISBA'],   bottom=(eb_neg['RN_ISBA']+eb_neg['H_ISBA']+eb_neg['LE_ISBA']), width=width,color='tab:red')
    ax.set_ylabel('Flux [W/m2]')
    ax.legend(loc='upper left')

    if fluxlims:
        ax.set_ylim(fluxlims)
    else:
        ylims = [(eb_neg['GFLUX_ISBA'] + eb_neg['RN_ISBA'] + eb_neg['H_ISBA'] + eb_neg['LE_ISBA']).min() -10, 
                 (eb_pos['GFLUX_ISBA'] + eb_pos['RN_ISBA'] + eb_pos['H_ISBA'] + eb_pos['LE_ISBA']).max() +10]
        ax.set_ylim(ylims)
        
    ax.grid()
    ax.set_title(title)
    ax.xaxis.set_tick_params(rotation=30)
    
    if save_path:
        plt.savefig(save_path, bbox_inches='tight', dpi=300)


    
def proplot_SEB_MEB_SNOW(profile, period, resample=None, 
                        snow='DSN_T_ISBA', title='TEST', 
                        fluxlims=None, bar_width=0.2, save_path=None):
    
    '''
    Plots below-canopy snowpack surface energy budget (H, LE, RN, G) of a given profile and a period.
    Sign convention relative to the surface (i.e. positive means that surface is gaining energy)
    To plot snow: snow='WSN_T_ISBA' or snow='DSN_T_ISBA'
    '''
        
    eb = pd.DataFrame()
    eb['time'] = period
    
    if snow:
        sn = pd.DataFrame()
        sn.index = period
        sn['snow'] = np.nan
        sn['snow'] = profile[snow].sel(time=period).values
    
    fluxes = ['GFLUX_ISBA', 'LE_GN_ISBA', 'H_GN_ISBA', 'SWNT_NS_ISBA', 'LWNT_N_ISBA']
    
    
    for flux in fluxes:
        if (flux != 'SWNT_NS_ISBA') & (flux != 'LWNT_N_ISBA'):
            eb[flux] = profile[flux].sel(time=period)*-1
        else:
            eb[flux] = profile[flux].sel(time=period)

    eb.index = eb['time']
    eb = eb.drop('time', axis=1)
    eb['RN_ISBA'] = eb['SWNT_NS_ISBA'] + eb['LWNT_N_ISBA']
    
    
    if resample:
        eb = eb.resample(resample).mean()
        if snow:
            sn = sn.resample(resample).mean()
        
    eb['GFLUX_ISBA'] = eb[['H_GN_ISBA', 'LE_GN_ISBA', 'RN_ISBA']].sum(axis=1)*-1

    eb_pos = eb.copy()
    eb_neg = eb.copy()

    for col in eb_pos.columns:
        eb_pos[col] = eb.loc[eb[col] > 0, col]
        eb_pos.loc[eb_pos[col] < 0, col] = 0
        eb_neg[col] = eb.loc[eb[col] < 0, col]
        eb_neg.loc[eb_neg[col] > 0, col] = 0

    for i in eb_pos.columns:
        eb_pos[i] = eb_pos[i].fillna(0)
        eb_neg[i] = eb_neg[i].fillna(0)
    
    width = bar_width
    
    fig, ax = plt.subplots(figsize=(8,4))
    
    if snow:
        axb = ax.twinx()
        axb.plot(sn['snow'], color='black', alpha=0.7, label=f'{snow}')
        axb.legend(loc='lower left')
        axb.set_ylabel(f'{snow} [{profile[snow].units}]')
        max_snow = profile[snow].sel(time=period).max()
        axb.set_ylim([0,max_snow*1.05])

    ax.bar(eb_pos.index, eb_pos['RN_ISBA'], width=width, color='tab:orange', label='RN')
    ax.bar(eb_pos.index, eb_pos['H_GN_ISBA'],  bottom=eb_pos['RN_ISBA'], width=width, color='tab:blue', label='H')
    ax.bar(eb_pos.index, eb_pos['LE_GN_ISBA'],   bottom=(eb_pos['RN_ISBA']+eb_pos['H_GN_ISBA']), width=width,color='tab:green', label='LE')
    ax.bar(eb_pos.index, eb_pos['GFLUX_ISBA'],   bottom=(eb_pos['RN_ISBA']+eb_pos['H_GN_ISBA']+eb_pos['LE_GN_ISBA']), width=width,color='tab:red', label='G')

    ax.bar(eb_neg.index, eb_neg['RN_ISBA'], width=width, color='tab:orange')
    ax.bar(eb_neg.index, eb_neg['H_GN_ISBA'],  bottom=eb_neg['RN_ISBA'], width=width, color='tab:blue')
    ax.bar(eb_neg.index, eb_neg['LE_GN_ISBA'],   bottom=(eb_neg['RN_ISBA']+eb_neg['H_GN_ISBA']), width=width, color='tab:green')
    ax.bar(eb_neg.index, eb_neg['GFLUX_ISBA'],   bottom=(eb_neg['RN_ISBA']+eb_neg['H_GN_ISBA']+eb_neg['LE_GN_ISBA']), width=width,color='tab:red')
    ax.set_ylabel('Flux [W/m2]')
    ax.legend(loc='upper left')
    
    if fluxlims:
        ax.set_ylim(fluxlims)
    else:
        ylims = [(eb_neg['GFLUX_ISBA'] + eb_neg['RN_ISBA'] + eb_neg['H_ISBA'] + eb_neg['LE_ISBA']).min() -10, 
                 (eb_pos['GFLUX_ISBA'] + eb_pos['RN_ISBA'] + eb_pos['H_ISBA'] + eb_pos['LE_ISBA']).max() +10]
        ax.set_ylim(ylims)
        
    ax.grid()
    ax.set_title(title)
    ax.xaxis.set_tick_params(rotation=30)
    
    if save_path:
        plt.savefig(save_path, bbox_inches='tight', dpi=300)


def pdplot_SEB(obs, period, resample=None, fluxlims=None, 
               snow='SWE', G='G', G_method='residual', LE='LE', H='H', RN='RN', 
               title='test', bar_width=0.2, save_path=None):
    
    '''
    Plots surface energy budget of a given pandas dataframe and a period.
    Define each energy flux columns name: e.g. LE='LE_obs'
    To plot snow: e.g. snow='SWE', where 'SWE' is the column name.
    '''
    
    #eb = pd.DataFrame()
    #eb['time'] = period
    
    fluxes = [G, LE, H, RN]
    
    eb = obs.loc[period][fluxes].copy()
    if G_method == 'residual':
        eb[G] = eb[[H, LE, RN]].sum(axis=1)*-1
    if snow:
        sn = obs.loc[period][snow].copy()
    #eb.index = eb['time']
    #eb = eb.drop('time', axis=1)
    
    if resample:
        eb = eb.resample(resample).mean()
        if snow:
            sn = sn.resample(resample).mean()

    eb_pos = eb.copy()
    eb_neg = eb.copy()

    for col in eb_pos.columns:
        eb_pos[col] = eb.loc[eb[col] > 0, col]
        eb_pos.loc[eb_pos[col] < 0, col] = 0
        eb_neg[col] = eb.loc[eb[col] < 0, col]
        eb_neg.loc[eb_neg[col] > 0, col] = 0

    for i in eb_pos.columns:
        eb_pos[i] = eb_pos[i].fillna(0)
        eb_neg[i] = eb_neg[i].fillna(0)
    
    width = bar_width
    
    fig, ax = plt.subplots(figsize=(8,4))

    if snow:
        axb = ax.twinx()
        axb.scatter(sn.index, sn, color='black', s=5, alpha=0.7, label=f'{snow}')
        axb.legend()
        axb.set_ylabel(f'{snow}')

    ax.bar(eb_pos.index, eb_pos[RN], width=width, color='tab:orange', label='RN')
    ax.bar(eb_pos.index, eb_pos[H],  bottom=eb_pos[RN], width=width, color='tab:blue', label='H')
    ax.bar(eb_pos.index, eb_pos[LE],   bottom=(eb_pos[RN]+eb_pos[H]), width=width,color='tab:green', label='LE')
    ax.bar(eb_pos.index, eb_pos[G],   bottom=(eb_pos[RN]+eb_pos[H]+eb_pos[LE]), width=width,color='tab:red', label='G')

    ax.bar(eb_neg.index, eb_neg[RN], width=width, color='tab:orange')
    ax.bar(eb_neg.index, eb_neg[H],  bottom=eb_neg[RN], width=width, color='tab:blue')
    ax.bar(eb_neg.index, eb_neg[LE],   bottom=(eb_neg[RN]+eb_neg[H]), width=width, color='tab:green')
    ax.bar(eb_neg.index, eb_neg[G],   bottom=(eb_neg[RN]+eb_neg[H]+eb_neg[LE]), width=width,color='tab:red')
    ax.set_ylabel('Flux [W/m2]')
    
    if fluxlims:
        ylims = fluxlims
    else:
        ylims = [(eb_neg[G] + eb_neg[RN] + eb_neg[H] + eb_neg[LE]).min() * 1.05, 
                 (eb_pos[G] + eb_pos[RN] + eb_pos[H] + eb_pos[LE]).max() * 1.05]
    
    ax.set_ylim(ylims)
    ax.grid()
    ax.set_title(title)

    ax.legend()
    
    if save_path:
        plt.savefig(save_path, bbox_inches='tight', dpi=300)
    