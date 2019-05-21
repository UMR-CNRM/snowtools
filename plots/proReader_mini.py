#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 6 apr. 2017

@author viallon
'''

from utils.prosimu import prosimu
import numpy as np
import datetime as dt
import os
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from matplotlib import collections
from matplotlib.colors import BoundaryNorm
from EvoProfilPlot import plot_profil
import Dictionnaries

constante_sampling = 1000
zero_C=273.15
MIN_temp=220

dico = {'WSN_VEG':(0,40),
            'SNOWRO':(0,500),
            'SNOWTEMP':(238,274),
            'SNOWLIQ':(0,35),
            'SNOWDZ':(0,0.2),
            'SNOWDEND':(-0.1,1),
            'SNOWSPHER':(-0.1,1),
            'SNOWSIZE':(0,0.1),
            'SNOWSSA':(-1,60),
            'SNOWSHEAR':(0,30),
            'RSN_VEG':(0,600),
            'ASN_VEG':(0,300),
            'ACC_RAT':(0,50),
            'NAT_RAT':(0,200),
            'SNOWIMP1':(-10,0),
            'SNOWIMP2':(-10,0)
            }

class ProReader_mini:
    """
    Basé sur une variation du logiciel Proreader.ry qui sert à tracer des graphes de fichier PRO issus du modèle de neige Crocus.
    Permet d'être utilisé par GUI_Proreader.py pour tracé interactif.
    Pour plus d'information sur le code de ProReader_mini, aller directement voir l'aide de Proreader.py
    """

    def __init__(self, ncfile=None, var=None, point=None, var_sup=[], liste_points=[]):
        """
         - ProReader_GUI(ncfile='path/to/ncfile', var=None, point=None) : A partir d'un fichier PRO
                !! si le fichier contient plusieurs points de simulation, selection par point sous forme d'un entier
                !! si le fichier contient plusieurs variables, selection par variable sous forme d'une chaine de caractère
        """
        self.initFromFile(ncfile, var=var, point=point, var_sup=var_sup, liste_points=liste_points)

    def initFromFile(self, ncfile, var=None, point=None, var_sup=[], liste_points=[]):
        ff = prosimu(ncfile)

        listvariables = ff.listvar()
        if('slope' in listvariables):
            slopetab = ff.read('slope')[:]
        else:
            slopetab = np.array([0])
        if('aspect' in listvariables):
            aspecttab = ff.read('aspect')[:]
        else:
            aspecttab = np.array([0])
        if('ZS' in listvariables):
            alttab = ff.read('ZS')[:]
        else:
            alttab = np.array([0])
        if('latitude' in listvariables):
            lattab = ff.read('latitude')[:]
        else:
            lattab = np.array([0])
        if('longitude' in listvariables):
            lontab = ff.read('longitude')[:]
        else:
            lontab = np.array([0])
        self.date = ff.readtime()
        
        if(isinstance(var, str)):
            var = var
        else:
            var = 'SNOWDZ'

        # Selection du point d interet
        if(isinstance(point, int)):
            point = point
        else:
            point = 0
            
        print ("Lecture fichier %s" % ncfile)
        print ("Variable %s selectionnee\n" % var)

        print ("Point %i selectionne\n" % point)

        self.slope = slopetab[point]
        self.aspect = aspecttab[point]
        self.alt = alttab[point]
        self.lat = lattab[point]
        self.lon = lontab[point]
        
        if('station' in ff.listvar()):
            nrstationtab = ff.read('station')[:]
            self.nrstation = nrstationtab[point]
        elif('massif_number' in ff.listvar()):
            nrstationtab = ff.read('massif_number')[:]
            self.nrstation = nrstationtab[point]
        else:
            self.nrstation = 0

        # Extraction des data
        self.var = {}
        self.var1D = {}
        list_var_necessaire = list(set(['SNOWDZ','SNOWTEMP',var]).union(set(var_sup)))
        if (('SNOWDZ' or 'SNOWTEMP' or var) not in ff.listvar()):
            print("Une variable (SNOWDZ, SNOWTEMP ou "+var+") est absente de ce fichier PRO.\n Sa présence est nécessaire. Plantage assuré")
    
        for i in range(len(ff.listvar())):
            if(ff.listvar()[i]==(ff.listvar()[i].upper()) and ff.listvar()[i]!='ZS' and (ff.listvar()[i] in list_var_necessaire)):
                if ('snow_layer' in ff.getdimvar(ff.listvar()[i])):
                    if(ff.listvar()[i] == 'WSN_VEG'):
                        self.var['WSN_VEG'] = ff.read(ff.listvar()[i], selectpoint=point)
                    else:
                        self.var[ff.listvar()[i]] = ff.read(ff.listvar()[i], selectpoint=point, fill2zero=True)  # Fill2zero necessaire pour le plot
                else:
                    self.var1D[ff.listvar()[i]] = ff.read(ff.listvar()[i], selectpoint=point, fill2zero=True)  # Fill2zero necessaire pour le plot

        self.ntime = np.shape(self.var['SNOWDZ'])[0]
        self.nsnowlayer = np.shape(self.var['SNOWDZ'])[1]
    
    def get_choix(self, ncfile):
        ff = prosimu(ncfile)

        listvariables = ff.listvar()
        if('massif_num' in listvariables):
            massiftab = ff.read('massif_num')[:]
        else:
            massiftab = np.array([-10.])
        if('ZS' in listvariables):
            alttab = ff.read('ZS')[:]
        else:
            alttab = np.array([-10.])
        if('slope' in listvariables):
            slopetab = ff.read('slope')[:]
        else:
            slopetab = np.array([-10.])
        if('aspect' in listvariables):
            aspecttab = ff.read('aspect')[:]
        else:
            aspecttab = np.array([-10.])
            
        return np.vstack((massiftab,alttab,slopetab,aspecttab))
    
    def get_choix_ss_massif(self, ncfile):
        ff = prosimu(ncfile)

        listvariables = ff.listvar()
        if('ZS' in listvariables):
            alttab = ff.read('ZS')[:]
        else:
            alttab = np.array([-10.])
        if('slope' in listvariables):
            slopetab = ff.read('slope')[:]
        else:
            slopetab = np.array([-10.])
        if('aspect' in listvariables):
            aspecttab = ff.read('aspect')[:]
        else:
            aspecttab = np.array([-10.])
            
        return np.vstack((alttab,slopetab,aspecttab))
    
    def get_topplot(self,var, b=None, e=None):
        b = self.parsedate(b, self.date, self.date[0])
        e = self.parsedate(e, self.date, self.date[self.ntime - 1])
        
        intime = (self.date >= b) * (self.date <= e)
        ep = self.var['SNOWDZ'][intime]
        return np.max(np.nansum(ep, axis=1))
    
    def parsedate(self,date, datetab, default):
        if isinstance(date, str):
            if len(date) == 8:
                pdate = dt.datetime.strptime(date, "%Y%m%d")
                date = datetab[(datetab >= pdate)][0]
            elif len(date) == 10:
                pdate = dt.datetime.strptime(date, "%Y%m%d%H")
                date = datetab[(datetab >= pdate)][0]
            else:
                date = None
        if date is None:
            date = default
        return date

    def plot(self, axe, var, b=None, e=None, xlabel=True, legend=None, colormap='viridis', real_layers=True):
        '''
        Trace la variable demandee sur la hauteur du manteau neigeux en fonction du temps
            axe : matplotlib.Axe
            var : string, nom variable a afficher
            b : date debut affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            e : date fin affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            xlabel : True (default) ou False, affichage xlabel
            legend : legende colorbar, legende automatique par defaut
            colormap : string, colormap name
            real_layers : True (default) (couches epaisseurs reelles) ou False (couches numeriques)
        '''
        b = self.parsedate(b, self.date, self.date[0])
        e = self.parsedate(e, self.date, self.date[self.ntime - 1])

        if legend is None:
            legend = var

        if var == 'SNOWTYPE':
            colormap = 'grains'
        elif var == 'SNOWTEMP':
            colormap = 'RdBu_r'
        elif 'SNOWIMP1' in var:
            colormap = 'echelle_log'
        elif 'SNOWIMP2' in var:
            colormap = 'echelle_log_sahara'
        else:
            colormap = colormap

        intime = (self.date >= b) * (self.date <= e)
        
        if len(np.where(intime)[0]) > constante_sampling:
            sampling = int(len(np.where(intime)[0])/constante_sampling)+1
            intime_t=np.zeros(len(intime),dtype=bool)
            intime_t[np.where(intime)[0][0]:np.where(intime)[0][len(np.where(intime)[0])-1]:sampling]=True
            intime = intime_t
        
        # Trace par appel a plot_profil
        ep = self.var['SNOWDZ'][intime]
        toplot = self.var[var][intime]
        if(real_layers):
            plot_profil(axe, ep, toplot, colormap=colormap, legend=legend)
            axe.set_ylabel('Hauteur (m)')
            axe.set_ylim(0, np.max(np.nansum(ep, axis=1)))
        else:
            ret = axe.pcolormesh(np.swapaxes(toplot, 0, 1), cmap=colormap)
            cbar = plt.colorbar(ret, ax=axe)
            cbar.set_label(legend)
            axe.set_ylabel('Couche numerique')
            axe.set_ylim(0, self.nsnowlayer)
            
            
            

        axe.set_xlim(0, toplot.shape[0])
        if(xlabel):
            def format_ticks(x, pos):
                x = int(x)
                if(x >= 0 and x < toplot.shape[0]):
                    return self.date[intime][x].strftime('%Y-%m-%d')
                else:
                    return 'E'
            #formatter = ticker.FuncFormatter(lambda x, pos: (int(x) >= 0 and int(x) < toplot.shape[0]) and \
            #                                 self.date[intime][int(x)].strftime('%Y-%m-%d'))
            formatter = ticker.FuncFormatter(format_ticks)
            axe.xaxis.set_major_formatter(formatter)
            axe.xaxis.set_major_locator(ticker.MaxNLocator(5))
            plt.setp(axe.xaxis.get_majorticklabels(),size='small')
    
        return intime

    def plot1D(self, axe, var, b=None, e=None, legend=None, color='b.'):
        '''
        Trace la variable demandee en fonction du temps
            axe : matplotlib.Axe
            var : string, nom variable 1D a afficher (cf. ProReader.dico1D)
            b : date debut affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            e : date fin affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            legend : legende colorbar, legende automatique par defaut
            color : string, color name
        '''

        b = self.parsedate(b, self.date, self.date[0])
        e = self.parsedate(e, self.date, self.date[self.ntime - 1])

        if legend is None:
            legend = var

        intime = (self.date >= b) * (self.date <= e)
        
        if len(np.where(intime)[0]) > constante_sampling:
            sampling = int(len(np.where(intime)[0])/constante_sampling)+1
            intime_t=np.zeros(len(intime),dtype=bool)
            intime_t[np.where(intime)[0][0]:np.where(intime)[0][len(np.where(intime)[0])-1]:sampling]=True
            intime = intime_t
        
        toplot = self.var1D[var][intime]
        xplot = range(toplot.shape[0])
        axe.plot(xplot, toplot, color)
        axe.set_xlim(0, toplot.shape[0])
        
        def format_ticks(x, pos):
            x = int(x)
            if(x >= 0 and x < toplot.shape[0]):
                return self.date[intime][x].strftime('%Y-%m-%d')
            else:
                return 'E'
        formatter = ticker.FuncFormatter(format_ticks)
        axe.xaxis.set_major_formatter(formatter)
        axe.xaxis.set_major_locator(ticker.MaxNLocator(5))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')

        axe.set_ylabel(legend)
        
        return intime
    
    def plot1D_bande(self, axe, var, date=None, legend=None, color='b.'):
        '''
        Trace la variable demandee en fonction de la longueur d'onde pour une date donnée
        RQ: dimension bands necessaire dans le fichier PRO.nc
            axe : matplotlib.Axe
            var : string, nom variable 1D a afficher (cf. ProReader.dico1D)
            date : date affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            legend : legende colorbar, legende automatique par defaut
            color : string, color name
        '''

        date = self.parsedate(date, self.date, self.date[self.ntime - 1])
        date = self.date[self.date >= date][0]

        if legend is None:
            legend = var

        toplot = self.var1D[var][self.date == date][0]
        xplot = [300+20*i for i in range(toplot.shape[0])]
        axe.plot(xplot, toplot, color)
        axe.set_xlim(300, 300+20*toplot.shape[0])
        
        axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))

        axe.set_ylabel(legend)
        

    def plot_date(self, axe, var, date=None, hauteur=None, legend=None, color='b', cbar_show=True, top=None, bool_layer=False):
    
        if legend is None:
            legend = var

        date = self.parsedate(date, self.date, self.date[self.ntime - 1])
        date = self.date[self.date >= date][0]
        ep = self.var['SNOWDZ'][self.date == date]
        epc = np.cumsum(ep)

        pointsx = np.zeros(2 * self.nsnowlayer + 2)
        pointsx[2:2 * self.nsnowlayer + 2:2] = epc
        pointsx[3:2 * self.nsnowlayer + 2:2] = epc
        pointsx[0:2] = 0
        pointsy = np.zeros(2 * self.nsnowlayer + 2)
        pointsy[1:2 * self.nsnowlayer:2] = self.var[var][self.date == date]
        pointsy[2:2 * self.nsnowlayer + 1:2] = self.var[var][self.date == date]
            
        pointsy=np.delete(pointsy,0)
        pointsx=np.delete(pointsx,0)
            
        if (var == 'SNOWTEMP'):
            pointsy=np.where(pointsy > MIN_temp ,pointsy,zero_C)
        if ('SNOWIMP' in var):
            pointsy = np.where(pointsy > 10**(-10) , pointsy, 10**(-10))
            pointsy = np.where(pointsy > 0 , np.log10(pointsy) , -10)
            
        axe.plot(pointsy[::-1], np.subtract(pointsx[-1],pointsx[::-1]), color=color)
        #axe.set(title=date.strftime('%Y-%m-%d %Hh'))
        if ('SNOWIMP' in var):
            axe.set_xlabel('log10 for x    ' + date.strftime('%Y-%m-%d %Hh'))
        else:
            axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))

        axe.xaxis.set_major_locator(ticker.MaxNLocator(5))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')
            
        Max = np.nanmax(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) > 0 else 0
        Min = np.nanmin(self.var[var][self.date == date]) if np.nanmin(self.var[var][self.date == date]) < 0 else 0
            
        if (var in dico.keys()):
            Max = np.nanmax(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) > dico[var][1] else dico[var][1]
            Min = np.nanmin(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) < dico[var][0] else dico[var][0]
        axe.set_xlim(Min, Max)
        
        if bool_layer:
            axe.axhline(y=hauteur, color='black', linestyle='-')
            
        if top is None:
            Max_y = ProReader_mini.get_topplot(self,var)
        else:
            Max_y = top
        axe.set_ylim(0,Max_y)
        
    def plot_date_complet(self, axe, axe2, var, date=None, hauteur=None, legend=None, color='b', cbar_show=False, top=None, bool_layer=False):
    
        if legend is None:
            legend = var

        date = self.parsedate(date, self.date, self.date[self.ntime - 1])
        date = self.date[self.date >= date][0]
        ep = self.var['SNOWDZ'][self.date == date]
        epc = np.cumsum(ep)

        # Tracé du profil    
        pointsx = np.zeros(2 * self.nsnowlayer + 2)
        pointsx[2:2 * self.nsnowlayer + 2:2] = epc
        pointsx[3:2 * self.nsnowlayer + 2:2] = epc
        pointsx[0:2] = 0
        pointsy = np.zeros(2 * self.nsnowlayer + 2)
        pointsy[1:2 * self.nsnowlayer:2] = self.var[var][self.date == date]
        pointsy[2:2 * self.nsnowlayer + 1:2] = self.var[var][self.date == date]
            
        pointsy=np.delete(pointsy,0)
        pointsx=np.delete(pointsx,0)

        if (var == 'SNOWTEMP'):
            pointsy=np.where(pointsy > MIN_temp ,pointsy,zero_C)
        if ('SNOWIMP' in var):
            pointsy = np.where(pointsy > 10**(-10) , pointsy, 10**(-10))
            pointsy = np.where(pointsy > 0 , np.log10(pointsy) , -10)
        axe.plot(pointsy[::-1], np.subtract(pointsx[-1],pointsx[::-1]), color=color)
        axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))
        axe.set_title('RAM - Snowgrain', y=1.04)

        axe.xaxis.set_major_locator(ticker.MaxNLocator(6))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')

        Max = np.nanmax(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) > 0 else 0
        Min = np.nanmin(self.var[var][self.date == date]) if np.nanmin(self.var[var][self.date == date]) < 0 else 0

        if (var in dico.keys()):
            Max = np.nanmax(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) > dico[var][1] else dico[var][1]
            Min = np.nanmin(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) < dico[var][0] else dico[var][0]
        axe.set_xlim(Min, Max)
        
        if bool_layer:
            axe.axhline(y=hauteur, color='black', linestyle='-')

        if top is None:
            Max_y = ProReader_mini.get_topplot(self,var)
        else:
            Max_y = top
        axe.set_ylim(0,Max_y)

        #Tracé du graphe SNOWTYPE / SNOWRAM
        epc_inv=epc[::-1].ravel()
        bottom_y = np.subtract(np.array(epc_inv[0]),np.array(epc_inv))
        bottom_y = bottom_y[(ep > 0).ravel()[::-1]]
        top_y=np.append(bottom_y[1:],epc_inv[0])

        left_x = self.var['SNOWRAM'][self.date == date].ravel()[::-1]
        right_x = np.zeros(shape=bottom_y.shape[0], dtype='int')
        left_x=left_x[(ep > 0).ravel()[::-1]]
        left_x=np.where(left_x > 0.5, left_x, 0.5)

        vertices = np.zeros(shape=(bottom_y.shape[0], 4, 2))
        vertices[:, 0, 0] = right_x
        vertices[:, 0, 1] = bottom_y
        vertices[:, 1, 0] = right_x
        vertices[:, 1, 1] = top_y
        vertices[:, 2, 0] = left_x
        vertices[:, 2, 1] = top_y
        vertices[:, 3, 0] = left_x
        vertices[:, 3, 1] = bottom_y

        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5

        rect = collections.PolyCollection(vertices[::-1], array=self.var['SNOWTYPE'][self.date == date][(ep > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none', alpha=0.7)

        rect.set_clim(vmin, vmax)
        axe2.add_collection(rect)
        axe2.xaxis.set_major_locator(ticker.MaxNLocator(5))
        axe2.set_xlim(30, 0)
        axe2.set_zorder(2)

        #Tracé éventuel de la colorbar
        if(cbar_show):
            cbar = plt.colorbar(rect, ax=[axe,axe2])
            labels = Dictionnaries.MEPRA_labels
            cbar.set_ticks(np.arange(np.shape(labels)[0]))
            cbar.ax.set_yticklabels(labels)
            
    
            
class ProReader_massif:
    """
    Basé sur une variation du logiciel Proreader.ry qui sert à tracer des graphes de fichier PRO issus du modèle de neige Crocus.
    Permet d'être utilisé par GUI_Proreader.py pour tracé interactif.
    Pour plus d'information sur le code de ProReader_mini, aller directement voir l'aide de Proreader.py
    """

    def __init__(self, ncfile=None, var=None, point=None, var_sup=[], liste_points=[]):
        """
         - ProReader_GUI(ncfile='path/to/ncfile', var=None, point=None) : A partir d'un fichier PRO
                !! si le fichier contient plusieurs points de simulation, selection par point sous forme d'un entier
                !! si le fichier contient plusieurs variables, selection par variable sous forme d'une chaine de caractère
        """
        self.initFromFile(ncfile, var=var, point=point, var_sup=var_sup, liste_points=liste_points)

    def initFromFile(self, ncfile, var=None, point=None, var_sup=[], liste_points=[]):
        ff = prosimu(ncfile)

        listvariables = ff.listvar()
        if('slope' in listvariables):
            slopetab = ff.read('slope')[:]
        else:
            slopetab = np.array([0])
        if('aspect' in listvariables):
            aspecttab = ff.read('aspect')[:]
        else:
            aspecttab = np.array([0])
        if('ZS' in listvariables):
            alttab = ff.read('ZS')[:]
        else:
            alttab = np.array([0])
        if('latitude' in listvariables):
            lattab = ff.read('latitude')[:]
        else:
            lattab = np.array([0])
        if('longitude' in listvariables):
            lontab = ff.read('longitude')[:]
        else:
            lontab = np.array([0])
        self.date = ff.readtime()
        
        if(isinstance(var, str)):
            var = var
        else:
            var = 'SNOWDZ'

        # Selection du point d interet
        if(isinstance(point, int)):
            point = point
        else:
            point = 0
            
        print ("Lecture fichier %s" % ncfile)
        print ("Variable %s selectionnee\n" % var)
        
        # Extraction des data
        self.var_massif = {}
        self.var1D_massif = {}
        list_var_necessaire_massif = list(set(['SNOWDZ','SNOWTEMP',var]).union(set(var_sup)))
        if (('SNOWDZ' or 'SNOWTEMP' or var) not in ff.listvar()):
            print("Une variable (SNOWDZ, SNOWTEMP ou "+var+") est absente de ce fichier PRO.\n Sa présence est nécessaire. Plantage assuré")

        for i in range(len(ff.listvar())):
            if(ff.listvar()[i]==(ff.listvar()[i].upper()) and ff.listvar()[i]!='ZS' and (ff.listvar()[i] in list_var_necessaire_massif)):
                if ('snow_layer' in ff.getdimvar(ff.listvar()[i])):
                    if(ff.listvar()[i] == 'WSN_VEG'):
                        self.var_massif['WSN_VEG'] = ff.read_var(ff.listvar()[i], Number_of_points=liste_points)
                    else:
                        self.var_massif[ff.listvar()[i]] = ff.read_var(ff.listvar()[i], Number_of_points=liste_points, fill2zero=True)
                else:
                    self.var1D_massif[ff.listvar()[i]] = ff.read_var(ff.listvar()[i], Number_of_points=liste_points, fill2zero=True)

        self.ntime = np.shape(self.var_massif['SNOWDZ'])[0]
        self.nsnowlayer = np.shape(self.var_massif['SNOWDZ'])[1]
        self.nmassif = np.shape(self.var_massif['SNOWDZ'])[2]             
    
    def get_topplot_massif(self,var):
        ep = self.var_massif['SNOWDZ'][:,:,:]
        return np.max(np.nansum(ep, axis=1))
    
    def get_topplot_massif_date(self,var,date, b=None, e=None):
        ep = self.var_massif['SNOWDZ'][self.date == date,:,:]
        return np.max(np.nansum(ep, axis=1))
    
    def parsedate(self,date, datetab, default):
        if isinstance(date, str):
            if len(date) == 8:
                pdate = dt.datetime.strptime(date, "%Y%m%d")
                date = datetab[(datetab >= pdate)][0]
            elif len(date) == 10:
                pdate = dt.datetime.strptime(date, "%Y%m%d%H")
                date = datetab[(datetab >= pdate)][0]
            else:
                date = None
        if date is None:
            date = default
        return date
    
    
    
    '''def plot_ligne_date_massif(self, axe):
        b = self.parsedate(None, self.date, self.date[0])
        e = self.parsedate(None, self.date, self.date[self.ntime - 1])
        intime = (self.date >= b) * (self.date <= e)
        
        toplot = np.zeros(len(list(intime)))
        xplot = range(len(list(intime)))
        axe.plot(xplot, toplot, 'black')
        axe.set_xlim(0, len(list(intime)))
        axe.set_ylim(0, 1)
        axe.yaxis.set_major_locator(ticker.MaxNLocator(2))
        
        def format_ticks(x, pos):
            x = int(x)
            if(x >= 0 and x < len(list(intime))):
                return self.date[intime][x].strftime('%Y-%m-%d')
            else:
                return 'E'
        formatter = ticker.FuncFormatter(format_ticks)
        axe.xaxis.set_major_formatter(formatter)
        axe.xaxis.set_major_locator(ticker.MaxNLocator(5))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')'''

    def plot_massif(self, axe, var, date=None, xlabel=True, legend=None, colormap='viridis', real_layers=True, legend_x=[], cbar_show=False, top_zoom=False):
        '''
        Trace la variable demandee sur la hauteur du manteau neigeux en fonction du massif pour une date donnée.
            axe : matplotlib.Axe
            var : string, nom variable a afficher
            xlabel : True (default) ou False, affichage xlabel
            legend : legende colorbar, legende automatique par defaut
            colormap : string, colormap name
            real_layers : True (default) (couches epaisseurs reelles) ou False (couches numeriques)
        '''

        if legend is None:
            legend = var

        if var == 'SNOWTYPE':
            colormap = 'grains'
        elif var == 'SNOWTEMP':
            colormap = 'RdBu_r'
        else:
            colormap = colormap
            
        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)

        ep = self.var_massif['SNOWDZ'][intime,:,:].transpose()
        toplot = self.var_massif[var][intime,:,:].transpose()
        
        if top_zoom:
            eptop = self.var_massif['SNOWDZ'][intime,:,:].transpose()
        else:
            eptop = self.var_massif['SNOWDZ'][:,:,:].transpose()
        
        if(real_layers):
            plot_profil(axe, ep, toplot, colormap=colormap, legend=legend, cbar_show=cbar_show)
            axe.set_ylabel('Hauteur (m)')
            axe.set_ylim(0, np.max(np.nansum(eptop,axis=1)))
        else:
            if(cbar_show):
                ret = axe.pcolormesh(np.swapaxes(toplot, 0, 1), cmap=colormap)
                cbar = plt.colorbar(ret, ax=axe)
                cbar.set_label(legend)
            axe.set_ylabel('Couche numerique')
            axe.set_ylim(0, self.nsnowlayer)
            

        axe.set_xlim(0, toplot.shape[0])
        axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))
        
        if(len(legend_x)>0):
            def format_ticks(x, pos):
                x = int(x)
                if(x >= 0 and x < len(legend_x)):
                   return legend_x[x]
                else:
                   return 'E'
            formatter = ticker.FuncFormatter(format_ticks)
            axe.xaxis.set_major_formatter(formatter)
            axe.xaxis.set_major_locator(ticker.MaxNLocator(min(len(legend_x),5)))
            plt.setp(axe.xaxis.get_majorticklabels(),size='small')
        
        else:
            axe.xaxis.set_major_locator(ticker.MaxNLocator(5))

    def plot1D_massif(self, axe, var, date=None, legend=None, color='b.', legend_x=[]):
        '''
        Trace la variable demandee en fonction du temps
            axe : matplotlib.Axe
            var : string, nom variable 1D a afficher (cf. ProReader.dico1D)
            b : date debut affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            e : date fin affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            legend : legende colorbar, legende automatique par defaut
            color : string, color name
        '''

        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)

        if legend is None:
            legend = var

        toplot = self.var1D_massif[var][intime,:]
        xplot = range(toplot.shape[0])
        axe.plot(xplot, toplot, color)
        axe.set_xlim(0, toplot.shape[0])
        axe.set_ylim(0, np.nanmax(self.var1D_massif[var][:,:]))
        
        if(len(legend_x)>0):
            def format_ticks(x, pos):
                x = int(x)
                if(x >= 0 and x < len(legend_x)):
                   return legend_x[x]
                else:
                   return 'E'
            formatter = ticker.FuncFormatter(format_ticks)
            axe.xaxis.set_major_formatter(formatter)
            axe.xaxis.set_major_locator(ticker.MaxNLocator(min(len(legend_x),5)))
            plt.setp(axe.xaxis.get_majorticklabels(),size='small')
        
        else:
            axe.xaxis.set_major_locator(ticker.MaxNLocator(5))

        axe.set_ylabel(legend)

    def plot_profil_massif(self, axe, var, date=None, massif=0, hauteur=None, legend=None, color='b', cbar_show=True, top=None, bool_layer=False, liste_nom=None):
    
        if legend is None:
            legend = var

        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)
        
        
        ep = self.var_massif['SNOWDZ'][intime,:,massif]
        epc = np.cumsum(ep)

        pointsx = np.zeros(2 * self.nsnowlayer + 2)
        pointsx[2:2 * self.nsnowlayer + 2:2] = epc
        pointsx[3:2 * self.nsnowlayer + 2:2] = epc
        pointsx[0:2] = 0
        pointsy = np.zeros(2 * self.nsnowlayer + 2)
        pointsy[1:2 * self.nsnowlayer:2] = self.var_massif[var][intime,:,massif]
        pointsy[2:2 * self.nsnowlayer + 1:2] = self.var_massif[var][intime,:,massif]
            
        pointsy=np.delete(pointsy,0)
        pointsx=np.delete(pointsx,0)
        
        pointsy=np.delete(pointsy,1)
        pointsx=np.delete(pointsx,1)
            
        if (var == 'SNOWTEMP'):
            pointsy=np.where(pointsy > MIN_temp ,pointsy,zero_C)
        axe.plot(pointsy[::-1], np.subtract(pointsx[-1],pointsx[::-1]), color=color)
        #axe.set(title=date.strftime('%Y-%m-%d %Hh'))
        #axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))
        axe.set_xlabel(liste_nom[massif])
        axe.set_title(var, y=1.04)

        axe.xaxis.set_major_locator(ticker.MaxNLocator(6))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')
            
        Max = np.nanmax(self.var_massif[var][intime,:,massif]) if np.nanmax(self.var_massif[var][intime,:,massif]) > 0 else 0
        Min = np.nanmin(self.var_massif[var][intime,:,massif]) if np.nanmin(self.var_massif[var][intime,:,massif]) < 0 else 0
            
        if (var in dico.keys()):
            Max = np.nanmax(self.var_massif[var][intime,:,massif]) if np.nanmax(self.var_massif[var][intime,:,massif]) > dico[var][1] else dico[var][1]
            Min = np.nanmin(self.var_massif[var][intime,:,massif]) if np.nanmax(self.var_massif[var][intime,:,massif]) < dico[var][0] else dico[var][0]
        axe.set_xlim(Min, Max)
        
        if bool_layer:
            axe.axhline(y=hauteur, color='black', linestyle='-')
            
        if top is None:
            Max_y = ProReader_massif.get_topplot_massif(self,var)
        elif top is False:
            Max_y = ProReader_massif.get_topplot_massif(self,var)
        elif top is True:
            Max_y = ProReader_massif.get_topplot_massif_date(self,var,date)
        else:
            Max_y = top
        axe.set_ylim(0,Max_y)
        
    def plot_profil_complet_massif(self, axe, axe2, var, date=None, massif=0, hauteur=None, legend=None, color='b', cbar_show=False, top=None, bool_layer=False, liste_nom=None):
    
        if legend is None:
            legend = var

        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)
        
        ep = self.var_massif['SNOWDZ'][intime,:,massif].filled(fill_value=0)
        epc = np.cumsum(ep)
       
        # Tracé du profil    
        pointsx = np.zeros(2 * self.nsnowlayer + 2)
        pointsx[2:2 * self.nsnowlayer + 2:2] = epc
        pointsx[3:2 * self.nsnowlayer + 2:2] = epc
        pointsx[0:2] = 0
        pointsy = np.zeros(2 * self.nsnowlayer + 2)
        pointsy[1:2 * self.nsnowlayer:2] = self.var_massif[var][intime,:,massif]
        pointsy[2:2 * self.nsnowlayer + 1:2] = self.var_massif[var][intime,:,massif]
            
        pointsy=np.delete(pointsy,0)
        pointsx=np.delete(pointsx,0)
        
        pointsy=np.delete(pointsy,1)
        pointsx=np.delete(pointsx,1)

        if (var == 'SNOWTEMP'):
            pointsy=np.where(pointsy > MIN_temp ,pointsy,zero_C)
        axe.plot(pointsy[::-1], np.subtract(pointsx[-1],pointsx[::-1]), color=color)
        axe.set_xlabel(liste_nom[massif])
        axe.set_title('RAM - Snowgrain', y=1.04)

        axe.xaxis.set_major_locator(ticker.MaxNLocator(6))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')

        Max = np.nanmax(self.var_massif[var][intime,:,massif]) if np.nanmax(self.var_massif[var][intime,:,massif]) > 0 else 0
        Min = np.nanmin(self.var_massif[var][intime,:,massif]) if np.nanmin(self.var_massif[var][intime,:,massif]) < 0 else 0

        if (var in dico.keys()):
            Max = np.nanmax(self.var_massif[var][intime,:,massif]) if np.nanmax(self.var_massif[var][intime,:,massif]) > dico[var][1] else dico[var][1]
            Min = np.nanmin(self.var_massif[var][intime,:,massif]) if np.nanmax(self.var_massif[var][intime,:,massif]) < dico[var][0] else dico[var][0]
        axe.set_xlim(Min, Max)
        
        if bool_layer:
            axe.axhline(y=hauteur, color='black', linestyle='-')

        if top is None:
            Max_y = ProReader_massif.get_topplot_massif(self,var)
        elif top is False:
            Max_y = ProReader_massif.get_topplot_massif(self,var)
        elif top is True:
            Max_y = ProReader_massif.get_topplot_massif_date(self,var,date)
        else:
            Max_y = top
        axe.set_ylim(0,Max_y)

        #Tracé du graphe SNOWTYPE / SNOWRAM
        epc_inv=epc[::-1].ravel()
        bottom_y = np.subtract(np.array(epc_inv[0]),np.array(epc_inv))
        bottom_y = bottom_y[(ep > 0).ravel()[::-1]]
        top_y=np.append(bottom_y[1:],epc_inv[0])

        left_x = self.var_massif['SNOWRAM'][intime,:,massif].ravel()[::-1]
        right_x = np.zeros(shape=bottom_y.shape[0], dtype='int')
        left_x=left_x[(ep > 0).ravel()[::-1]]
        left_x=np.where(left_x > 0.5, left_x, 0.5)

        vertices = np.zeros(shape=(bottom_y.shape[0], 4, 2))
        vertices[:, 0, 0] = right_x
        vertices[:, 0, 1] = bottom_y
        vertices[:, 1, 0] = right_x
        vertices[:, 1, 1] = top_y
        vertices[:, 2, 0] = left_x
        vertices[:, 2, 1] = top_y
        vertices[:, 3, 0] = left_x
        vertices[:, 3, 1] = bottom_y

        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5

        rect = collections.PolyCollection(vertices[::-1], array=self.var_massif['SNOWTYPE'][intime,:,massif][(ep > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none', alpha=0.7)

        rect.set_clim(vmin, vmax)
        axe2.add_collection(rect)
        axe2.xaxis.set_major_locator(ticker.MaxNLocator(5))
        axe2.set_xlim(30, 0)
        axe2.set_zorder(2)

        #Tracé éventuel de la colorbar
        if(cbar_show):
            cbar = plt.colorbar(rect, ax=[axe,axe2])
            labels = Dictionnaries.MEPRA_labels
            cbar.set_ticks(np.arange(np.shape(labels)[0]))
            cbar.ax.set_yticklabels(labels)

class ProReader_membre:
    """
    Basé sur une variation du logiciel Proreader.ry qui sert à tracer des graphes de fichier PRO issus du modèle de neige Crocus.
    Permet d'être utilisé par GUI_Proreader.py pour tracé interactif.
    Pour plus d'information sur le code de ProReader_mini, aller directement voir l'aide de Proreader.py
    """

    def __init__(self, ncfile=None, var=None, point=None, var_sup=[]):
        """
         - ProReader_GUI(ncfile='path/to/ncfile', var=None, point=None) : A partir d'un fichier PRO
                !! si le fichier contient plusieurs points de simulation, selection par point sous forme d'un entier
                !! si le fichier contient plusieurs variables, selection par variable sous forme d'une chaine de caractère
        """
        self.nb_membre = self.initFromFile(ncfile, var=var, point=point, var_sup=var_sup)

    def initFromFile(self, ncfile, var=None, point=None, var_sup=[]):
        ff = prosimu(ncfile)
        arborescence = os.path.dirname(ncfile)
        separateur = ncfile.replace(os.path.dirname(ncfile),'')[0] # suivant les OS, '/' ou '\' => devrait éviter des soucis
        place_mb0 = arborescence.find('mb0')
        nom_fichier = ncfile.replace(arborescence + separateur,'')
        
        if len(arborescence)==place_mb0+5 or arborescence[place_mb0+5] == separateur:
                nb_chiffre = 3
        elif len(arborescence)==place_mb0+6 or arborescence[place_mb0+6] == separateur:
                nb_chiffre = 4
        
        nmembre = 0
        if nb_chiffre == 3:
            while ('mb'+'%03d' %nmembre) in os.listdir(arborescence[:place_mb0]):
                nmembre = nmembre + 1
                if nmembre > 100:
                    break
                    print('more than 100 members. Pb of directory possible. If not, must change indexes in proReader_mini')
        elif nb_chiffre == 4:
            while ('mb'+'%04d' %nmembre) in os.listdir(arborescence[:place_mb0]):
                nmembre = nmembre + 1
                if nmembre > 100:
                    break
                    print('more than 100 members. Pb of directory possible. If not, must change indexes in proReader_mini')
                    
        # Preparation des data: mise en place des dictionnaires
        self.var_membre = {}
        self.var1D_membre = {}
        list_var_necessaire_membre = list(set(['SNOWDZ','SNOWTEMP',var]).union(set(var_sup)))
        if (('SNOWDZ' or 'SNOWTEMP' or var) not in ff.listvar()):
            print("Une variable (SNOWDZ, SNOWTEMP ou "+var+") est absente de ce fichier PRO.\n Sa présence est nécessaire. Plantage assuré")

        a = ff.read('SNOWDZ')
        liste = [nmembre] + list(a.shape)
        self.ntime = a.shape[0]
        self.nsnowlayer = a.shape[1]

        for i in range(len(ff.listvar())):
            if(ff.listvar()[i]==(ff.listvar()[i].upper()) and ff.listvar()[i]!='ZS' and (ff.listvar()[i] in list_var_necessaire_membre)):
                if ('snow_layer' in ff.getdimvar(ff.listvar()[i])):
                    if(ff.listvar()[i] == 'WSN_VEG'):
                        self.var_membre['WSN_VEG'] = np.ma.masked_where(np.zeros(tuple(liste[0:3])) < -1, np.zeros(tuple(liste[0:3])))
                    else:
                        self.var_membre[ff.listvar()[i]] = np.ma.masked_where(np.zeros(tuple(liste[0:3])) < -1, np.zeros(tuple(liste[0:3])))
                else:
                    self.var1D_membre[ff.listvar()[i]] = np.ma.masked_where(np.zeros(tuple(liste[0:2])) < -1, np.zeros(tuple(liste[0:2])))

        self.date = ff.readtime()

        for nb_m in range(nmembre):
            if nb_chiffre == 3:
                chaine = 'mb'+'%03d' %nb_m
                path_for_nc = arborescence[:place_mb0] + chaine + arborescence[place_mb0+5:]+separateur+nom_fichier
            elif nb_chiffre == 4:
                chaine = 'mb'+'%04d' %nb_m
                path_for_nc = arborescence[:place_mb0] + chaine + arborescence[place_mb0+6:]+separateur+nom_fichier            
            #path pour nc = arborescence jusqu'à mb + les 3 chiffres + le séparateur ('/' ou '\')  + le nom du fichier supposé tjrs le même
        
            ff = prosimu(path_for_nc)  
        
            if(isinstance(var, str)):
                var = var
            else:
                var = 'SNOWDZ'

            # Selection du point d interet
            if(isinstance(point, int)):
                point = point
            else:
                point = 0

            print ("Lecture fichier %s" % path_for_nc)
            if nb_m ==0:
                print ("Variable %s selectionnee\n" % var)

            # Extraction des data
            for i in range(len(ff.listvar())):
                if(ff.listvar()[i]==(ff.listvar()[i].upper()) and ff.listvar()[i]!='ZS' and (ff.listvar()[i] in list_var_necessaire_membre)):
                    if ('snow_layer' in ff.getdimvar(ff.listvar()[i])):
                        if(ff.listvar()[i] == 'WSN_VEG'):
                            self.var_membre['WSN_VEG'][nb_m,:,:] = ff.read_var(ff.listvar()[i], Number_of_points = point)
                        else:
                            self.var_membre[ff.listvar()[i]][nb_m,:,:] = ff.read_var(ff.listvar()[i], Number_of_points = point, fill2zero=True)
                    else:
                        self.var1D_membre[ff.listvar()[i]][nb_m,:] = ff.read_var(ff.listvar()[i], Number_of_points = point, fill2zero=True)
                        
        return nmembre

        # SE SOUVENIR DE LA DIFFERENCE ENTRE MASSIF ET MEMBRE: pour MEMBRE, variable en premier !!
        '''self.ntime = np.shape(self.var_membre['SNOWDZ'])[0]
        self.nsnowlayer = np.shape(self.var_membre['SNOWDZ'])[1]
        self.nmassif = np.shape(self.var_membre['SNOWDZ'])[2]'''

    def parsedate(self,date, datetab, default):
        if isinstance(date, str):
            if len(date) == 8:
                pdate = dt.datetime.strptime(date, "%Y%m%d")
                date = datetab[(datetab >= pdate)][0]
            elif len(date) == 10:
                pdate = dt.datetime.strptime(date, "%Y%m%d%H")
                date = datetab[(datetab >= pdate)][0]
            else:
                date = None
        if date is None:
            date = default
        return date

    def get_choix(self, ncfile):
        ff = prosimu(ncfile)

        listvariables = ff.listvar()
        if('massif_num' in listvariables):
            massiftab = ff.read('massif_num')[:]
        else:
            massiftab = np.array([-10.])
        if('ZS' in listvariables):
            alttab = ff.read('ZS')[:]
        else:
            alttab = np.array([-10.])
        if('slope' in listvariables):
            slopetab = ff.read('slope')[:]
        else:
            slopetab = np.array([-10.])
        if('aspect' in listvariables):
            aspecttab = ff.read('aspect')[:]
        else:
            aspecttab = np.array([-10.])
            
        return np.vstack((massiftab,alttab,slopetab,aspecttab))
    
    def get_topplot_membre(self,var):
        ep = self.var_membre['SNOWDZ'][:,:,:]
        return np.max(np.nansum(ep, axis=2))
    
    def get_topplot_membre_date(self,var,date, b=None, e=None):
        ep = self.var_membre['SNOWDZ'][:,self.date == date,:]
        return np.max(np.nansum(ep, axis=2))
    
    def plot_membre(self, axe, var, date=None, xlabel=True, legend=None, colormap='viridis', real_layers=True, cbar_show=False, top_zoom=False):
        '''
        Trace la variable demandee sur la hauteur du manteau neigeux en fonction du massif pour une date donnée.
            axe : matplotlib.Axe
            var : string, nom variable a afficher
            xlabel : True (default) ou False, affichage xlabel
            legend : legende colorbar, legende automatique par defaut
            colormap : string, colormap name
            real_layers : True (default) (couches epaisseurs reelles) ou False (couches numeriques)
        '''

        if legend is None:
            legend = var

        if var == 'SNOWTYPE':
            colormap = 'grains'
        elif var == 'SNOWTEMP':
            colormap = 'RdBu_r'
        else:
            colormap = colormap
            
        if date is None:
            date=self.date[0]

        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)

        ep = self.var_membre['SNOWDZ'][:,intime,:]
        toplot = self.var_membre[var][:,intime,:]
        
        if top_zoom:
            eptop = self.var_membre['SNOWDZ'][:,intime,:]
        else:
            eptop = self.var_membre['SNOWDZ'][:,:,:]
        
        if(real_layers):
            plot_profil(axe, ep, toplot, colormap=colormap, legend=legend, cbar_show=cbar_show)
            axe.set_ylabel('Hauteur (m)')
            axe.set_ylim(0, np.max(np.nansum(eptop,axis=1)))
        else:
            if(cbar_show):
                ret = axe.pcolormesh(np.swapaxes(toplot, 0, 1), cmap=colormap)
                cbar = plt.colorbar(ret, ax=axe)
                cbar.set_label(legend)
            axe.set_ylabel('Couche numerique')
            axe.set_ylim(0, self.nsnowlayer)
            

        axe.set_xlim(0, toplot.shape[0])
        axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))
        
        axe.xaxis.set_major_locator(ticker.MaxNLocator(5))

    def plot1D_membre(self, axe, var, date=None, legend=None, color='b.'):
        '''
        Trace la variable demandee en fonction du temps
            axe : matplotlib.Axe
            var : string, nom variable 1D a afficher (cf. ProReader.dico1D)
            b : date debut affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            e : date fin affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            legend : legende colorbar, legende automatique par defaut
            color : string, color name
        '''

        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)

        if legend is None:
            legend = var

        toplot = self.var1D_membre[var][:,intime]
        xplot = range(toplot.shape[0])
        axe.plot(xplot, toplot, color)
        axe.set_xlim(0, toplot.shape[0])
        axe.set_ylim(0, np.nanmax(self.var1D_membre[var][:,:]))

        axe.xaxis.set_major_locator(ticker.MaxNLocator(5))

        axe.set_ylabel(legend)

    def plot_profil_membre(self, axe, var, date=None, membre=0, hauteur=None, legend=None, color='b', cbar_show=True, top=None, bool_layer=False):
    
        if legend is None:
            legend = var

        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)
        
        
        ep = self.var_membre['SNOWDZ'][membre,intime,:]
        epc = np.cumsum(ep)

        pointsx = np.zeros(2 * self.nsnowlayer + 2)
        pointsx[2:2 * self.nsnowlayer + 2:2] = epc
        pointsx[3:2 * self.nsnowlayer + 2:2] = epc
        pointsx[0:2] = 0
        pointsy = np.zeros(2 * self.nsnowlayer + 2)
        pointsy[1:2 * self.nsnowlayer:2] = self.var_membre[var][membre,intime,:]
        pointsy[2:2 * self.nsnowlayer + 1:2] = self.var_membre[var][membre,intime,:]
            
        pointsy=np.delete(pointsy,0)
        pointsx=np.delete(pointsx,0)
        
        pointsy=np.delete(pointsy,1)
        pointsx=np.delete(pointsx,1)
            
        if (var == 'SNOWTEMP'):
            pointsy=np.where(pointsy > MIN_temp ,pointsy,zero_C)
        axe.plot(pointsy[::-1], np.subtract(pointsx[-1],pointsx[::-1]), color=color)
        #axe.set(title=date.strftime('%Y-%m-%d %Hh'))
        #axe.set_xlabel(date.strftime('%Y-%m-%d %Hh'))
        axe.set_xlabel('membre: '+str(membre))
        axe.set_title(var, y=1.04)

        axe.xaxis.set_major_locator(ticker.MaxNLocator(6))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')
            
        Max = np.nanmax(self.var_membre[var][membre,intime,:]) if np.nanmax(self.var_membre[var][membre,intime,:]) > 0 else 0
        Min = np.nanmin(self.var_membre[var][membre,intime,:]) if np.nanmin(self.var_membre[var][membre,intime,:]) < 0 else 0
            
        if (var in dico.keys()):
            Max = np.nanmax(self.var_membre[var][membre,intime,:]) if np.nanmax(self.var_membre[var][membre,intime,:]) > dico[var][1] else dico[var][1]
            Min = np.nanmin(self.var_membre[var][membre,intime,:]) if np.nanmax(self.var_membre[var][membre,intime,:]) < dico[var][0] else dico[var][0]
        axe.set_xlim(Min, Max)
        
        if bool_layer:
            axe.axhline(y=hauteur, color='black', linestyle='-')
            
        if top is None:
            Max_y = ProReader_membre.get_topplot_membre(self,var)
        elif top is False:
            Max_y = ProReader_membre.get_topplot_membre(self,var)
        elif top is True:
            Max_y = ProReader_membre.get_topplot_membre_date(self,var,date)
        else:
            Max_y = top
        axe.set_ylim(0,Max_y)
        
    def plot_profil_complet_membre(self, axe, axe2, var, date=None, membre=0, hauteur=None, legend=None, color='b', cbar_show=False, top=None, bool_layer=False):
    
        if legend is None:
            legend = var

        if date is None:
            date=self.date[0]
            
        b = self.parsedate(date, self.date, self.date[0])
        intime=list(self.date == b).index(True)
        
        ep = self.var_membre['SNOWDZ'][membre,intime,:].filled(fill_value=0)
        epc = np.cumsum(ep)
       
        # Tracé du profil    
        pointsx = np.zeros(2 * self.nsnowlayer + 2)
        pointsx[2:2 * self.nsnowlayer + 2:2] = epc
        pointsx[3:2 * self.nsnowlayer + 2:2] = epc
        pointsx[0:2] = 0
        pointsy = np.zeros(2 * self.nsnowlayer + 2)
        pointsy[1:2 * self.nsnowlayer:2] = self.var_membre[var][membre,intime,:]
        pointsy[2:2 * self.nsnowlayer + 1:2] = self.var_membre[var][membre,intime,:]
            
        pointsy=np.delete(pointsy,0)
        pointsx=np.delete(pointsx,0)
        
        pointsy=np.delete(pointsy,1)
        pointsx=np.delete(pointsx,1)

        if (var == 'SNOWTEMP'):
            pointsy=np.where(pointsy > MIN_temp ,pointsy,zero_C)
        axe.plot(pointsy[::-1], np.subtract(pointsx[-1],pointsx[::-1]), color=color)
        axe.set_xlabel('membre: '+str(membre))
        axe.set_title('RAM - Snowgrain', y=1.04)

        axe.xaxis.set_major_locator(ticker.MaxNLocator(6))
        plt.setp(axe.xaxis.get_majorticklabels(),size='small')

        Max = np.nanmax(self.var_membre[var][membre,intime,:]) if np.nanmax(self.var_membre[var][membre,intime,:]) > 0 else 0
        Min = np.nanmin(self.var_membre[var][membre,intime,:]) if np.nanmin(self.var_membre[var][membre,intime,:]) < 0 else 0

        if (var in dico.keys()):
            Max = np.nanmax(self.var_membre[var][membre,intime,:]) if np.nanmax(self.var_membre[var][membre,intime,:]) > dico[var][1] else dico[var][1]
            Min = np.nanmin(self.var_membre[var][membre,intime,:]) if np.nanmax(self.var_membre[var][membre,intime,:]) < dico[var][0] else dico[var][0]
        axe.set_xlim(Min, Max)
        
        if bool_layer:
            axe.axhline(y=hauteur, color='black', linestyle='-')

        if top is None:
            Max_y = ProReader_membre.get_topplot_membre(self,var)
        elif top is False:
            Max_y = ProReader_membre.get_topplot_membre(self,var)
        elif top is True:
            Max_y = ProReader_membre.get_topplot_membre_date(self,var,date)
        else:
            Max_y = top
        axe.set_ylim(0,Max_y)

        #Tracé du graphe SNOWTYPE / SNOWRAM
        epc_inv=epc[::-1].ravel()
        bottom_y = np.subtract(np.array(epc_inv[0]),np.array(epc_inv))
        bottom_y = bottom_y[(ep > 0).ravel()[::-1]]
        top_y=np.append(bottom_y[1:],epc_inv[0])

        left_x = self.var_membre['SNOWRAM'][membre,intime,:].ravel()[::-1]
        right_x = np.zeros(shape=bottom_y.shape[0], dtype='int')
        left_x=left_x[(ep > 0).ravel()[::-1]]
        left_x=np.where(left_x > 0.5, left_x, 0.5)

        vertices = np.zeros(shape=(bottom_y.shape[0], 4, 2))
        vertices[:, 0, 0] = right_x
        vertices[:, 0, 1] = bottom_y
        vertices[:, 1, 0] = right_x
        vertices[:, 1, 1] = top_y
        vertices[:, 2, 0] = left_x
        vertices[:, 2, 1] = top_y
        vertices[:, 3, 0] = left_x
        vertices[:, 3, 1] = bottom_y

        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5

        rect = collections.PolyCollection(vertices[::-1], array=self.var_membre['SNOWTYPE'][membre,intime,:][(ep > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none', alpha=0.7)

        rect.set_clim(vmin, vmax)
        axe2.add_collection(rect)
        axe2.xaxis.set_major_locator(ticker.MaxNLocator(5))
        axe2.set_xlim(30, 0)
        axe2.set_zorder(2)

        #Tracé éventuel de la colorbar
        if(cbar_show):
            cbar = plt.colorbar(rect, ax=[axe,axe2])
            labels = Dictionnaries.MEPRA_labels
            cbar.set_ticks(np.arange(np.shape(labels)[0]))
            cbar.ax.set_yticklabels(labels)
