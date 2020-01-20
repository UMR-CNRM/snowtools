#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 6 apr. 2017

@author viallon
'''

from utils.prosimu import prosimu
import numpy as np
import datetime as dt
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from .EvoProfilPlot import plot_profil, plot_grains1D


class ProReader:
    """
    Definit un objet contenant :
     * date : liste des dates
     * lat, lon, alt, slope, aspect, nrstation (no station ou massif)
     * var : dictionnaire contenant toutes les variables 2D du fichier PRO
       pour le point selectionne. La liste des variables disponibles est
       stockee dan ProReader.dico (avec correspondance noms SURFEX)
     * var1D : dictionnaire contenant toutes les variables 1D du fichier PRO
       pour le point selectionne. La liste des variables disponibles est
       stockee dan ProReader.dico1D (avec correspondance noms SURFEX)
    """

    dico = {'swe': ('WSN_VEG', 'Snow Water Equivalent (m)'),
            'rho': ('SNOWRO', 'Density (kg/m3)'),
            'pheat': ('SNOWHEAT', 'Enthalpie (J/m2)'),
            'age': ('SNOWAGE', 'Age (days)'),
            'g1': ('SNOWGRAN1', 'Diametre optique (m)'),
            'g2': ('SNOWGRAN2', 'Spericite'),
            'hist': ('SNOWHIST', 'History'),
            'temp': ('SNOWTEMP', 'Temperature (K)'),
            'tel': ('SNOWLIQ', 'Liquid Water Content (kg/m3)'),
            'ep': ('SNOWDZ', 'Thickness (m)'),               # ne pas changer le nom
            'dend': ('SNOWDEND', 'Dendricity'),
            'spher': ('SNOWSPHER', 'Sphericity'),
            'gs': ('SNOWSIZE', 'Grain size (m)'),
            'ssa': ('SNOWSSA', 'SSA (m2/kg)'),
            'grain': ('SNOWTYPE', 'Grain type (EN)'),        # ne pas changer le nom
            'ram': ('SNOWRAM', 'RAM Resistance (daN)'),
            'shear': ('SNOWSHEAR', 'Shear Resistance (kPa)')
            }

    dico1D = {'albedo': ('TALB_ISBA', 'Albedo'),
              'tg1': ('TG1', 'Soil Temperature l1 (K)'),
              'tg4': ('TG4', 'Soil Temperature l4 (K)'),
              }

    dico2 = {'slope': 'slope',
             'aspect': 'aspect',
             'alt': 'ZS',
             'lat': 'lat',
             'lon': 'lon',
             'time': 'time'
             }

    def __init__(self, ncfile=None, pro=None, ntime=1460, nsnowlayer=50, point=None ):
        """
        ProReader(ncfile=None, pro=None, ntime=1460, nsnowlayer=50, point=None ):
        Initialise une classe contenant les donnees neige du fichier PRO
         - ProReader(ncfile='path/to/ncfile', point=None) : A partir d'un fichier PRO
                !! si le fichier contient plusieurs points de simulation, selection par point
                    - sous forme d'un entier
                    - sous forme d'une recherche en passant un dictionnaire pouvant contenir les entrees :
                        * lat, latinf, latsup
                        * lon, loninf, lonsup
                        * alt, altinf, altsup
                        * aspect, aspectinf, aspectsup
                        * slope, slopeinf, slopesup
                        * station (numero de station, si renseigne)
                        * massif (numero de massif, si renseigne)
         - ProReader(pro=(ProReader1, ..., ProReaderN)) : Par fusion d'une liste d'objets ProReader
         - ProReader(ntime=1460, nsnowlayer=50) : Vide a partir d'un temps donne et d un nombre de couches
        """
        if(ncfile):
            self.initFromFile(ncfile, point=point)
        elif(pro):
            self.initFromMultiple(pro)
        else:
            self.initVoid(ntime, nsnowlayer)

    def initVoid(self, ntime, nsnowlayer):
        self.ntime = ntime
        self.nsnowlayer = nsnowlayer
        self.slope = 0
        self.aspect = 0
        self.alt = 0
        self.lat = 0
        self.lon = 0
        self.nrstation = 0
        self.date = np.empty(ntime)

        self.var = {}
        for key in list(ProReader.dico.keys()):
            self.var[key] = np.zeros((ntime, nsnowlayer))

        self.var1D = {}
        for key in list(ProReader.dico1D.keys()):
            self.var1D[key] = np.zeros(ntime)

    def initFromMultiple(self, pro):
        self.ntime = 0
        for proi in pro:
            self.ntime += proi.ntime
        self.initVoid(self.ntime, pro[0].nsnowlayer)
        self.slope = pro[0].slope
        self.aspect = pro[0].aspect
        self.alt = pro[0].alt
        self.lat = pro[0].alt
        self.lon = pro[0].alt
        self.nrstation = pro[0].alt
        datelist = [pro[0].date]
        for i in range(1, len(pro)):
            datelist.append(pro[i].date)
        self.date = np.concatenate(datelist)

        for key in list(ProReader.dico.keys()):
            cursor = 0
            for i in range(len(pro)):
                self.var[key][cursor:cursor + pro[i].ntime] = pro[i].var[key]
                cursor += pro[i].ntime
        for key in list(ProReader.dico1D.keys()):
            cursor = 0
            for i in range(len(pro)):
                self.var1D[key][cursor:cursor + pro[i].ntime] = pro[i].var1D[key]
                cursor += pro[i].ntime

    def initFromFile(self, ncfile, point=None):
        ff = prosimu(ncfile)

        listvariables = ff.listvar()
        if(ProReader.dico2['slope'] in listvariables):
            slopetab = ff.read(ProReader.dico2['slope'])[:]
        else:
            slopetab = np.array([0])
        if(ProReader.dico2['aspect'] in listvariables):
            aspecttab = ff.read(ProReader.dico2['aspect'])[:]
        else:
            aspecttab = np.array([0])
        if(ProReader.dico2['alt'] in listvariables):
            alttab = ff.read(ProReader.dico2['alt'])[:]
        else:
            alttab = np.array([0])
        if(ProReader.dico2['lat'] in listvariables):
            lattab = ff.read(ProReader.dico2['lat'])[:]
        else:
            lattab = np.array([0])
        if(ProReader.dico2['lon'] in listvariables):
            lontab = ff.read(ProReader.dico2['lon'])[:]
        else:
            lontab = np.array([0])
        self.date = ff.readtime()
        # Selection du point d interet
        if(isinstance(point, int)):
            point = point
        elif(isinstance(point, dict)):
            cond = True
            if('lat' in list(point.keys())):
                cond *= (lattab == point['lat'])
            if('latinf' in list(point.keys())):
                cond *= (lattab >= point['latinf'])
            if('latsup' in list(point.keys())):
                cond *= (lattab <= point['latsup'])
            if('lon' in list(point.keys())):
                cond *= (lontab == point['lon'])
            if('loninf' in list(point.keys())):
                cond *= (lontab >= point['loninf'])
            if('lonsup' in list(point.keys())):
                cond *= (lontab <= point['lonsup'])
            if('alt' in list(point.keys())):
                cond *= (alttab == point['alt'])
            if('altinf' in list(point.keys())):
                cond *= (alttab >= point['altinf'])
            if('altsup' in list(point.keys())):
                cond *= (alttab <= point['altsup'])
            if('aspect' in list(point.keys())):
                cond *= (aspecttab == point['aspect'])
            if('aspectinf' in list(point.keys())):
                cond *= (aspecttab >= point['aspectinf'])
            if('aspectsup' in list(point.keys())):
                cond *= (aspecttab <= point['aspectsup'])
            if('slope' in list(point.keys())):
                cond *= (slopetab == point['slope'])
            if('slopeinf' in list(point.keys())):
                cond *= (slopetab >= point['slopeinf'])
            if('slopesup' in list(point.keys())):
                cond *= (slopetab <= point['slopesup'])
            if('station' in list(point.keys())):
                nrstationtab = ff.read('station')[:]
                cond *= (nrstationtab == point['station'])
            if('massif' in list(point.keys())):
                nrmassiftab = ff.read('massif_number')[:]
                cond *= (nrmassiftab == point['massif'])

            result = cond.nonzero()
            if np.shape(result)[1] > 1:
                print("Warning : more than 1 simulation point for your query. 1st matching selected")
                point = result[0][0]
            elif np.shape(result)[1] == 1:
                point = result[0][0]
            else:
                print("ERROR : no simulation point for your query. 1st point selected")
                point = 0
        else:
            point = 0

        print("Lecture fichier %s" % ncfile)
        print("Point %i selectionne\n" % point)

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
        for key, val in list(ProReader.dico.items()):
            if(val[0] in listvariables):
                if(key == "swe"):
                    self.var['swe'] = ff.read(val[0], selectpoint=point)
                else:
                    self.var[key] = ff.read(val[0], selectpoint=point, fill2zero=True)  # Fill2zero necessaire pour le plot
            else:
                print("WARNING : Variable %s not present in your PRO.nc file, %s (%s) not provided" % (val[0], key, val[1]))

        self.var1D = {}
        for key, val in list(ProReader.dico1D.items()):
            if(val[0] in listvariables):
                self.var1D[key] = ff.read(val[0], selectpoint=point, fill2zero=True)  # Fill2zero necessaire pour le plot
            else:
                print("WARNING : Variable %s not present in your PRO.nc file, %s (%s) not provided" % (val[0], key, val[1]))

        self.ntime = np.shape(self.var['swe'])[0]
        self.nsnowlayer = np.shape(self.var['swe'])[1]

    def get_htot(self):
        """
        Retourne la hauteur totale du manteau neigeux en fonction du temps
        """
        return np.nansum(self.var['ep'], axis=1)

    def plot(self, axe, var, b=None, e=None, xlabel=True, legend=None, colormap='jet', real_layers=True):
        '''
        Trace la variable demandee sur la hauteur du manteau neigeux en fonction du temps
            axe : matplotlib.Axe
            var : string, nom variable a afficher (cf. ProReader.dico)
            b : date debut affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            e : date fin affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            xlabel : True (default) ou False, affichage xlabel
            legend : legende colorbar, legende automatique par defaut
            colormap : string, colormap name
            real_layers : True (default) (couches epaisseurs reelles) ou False (couches numeriques)
        '''
        # Interpretation des entrees
        var = ProReader.varname(var)

        def parsedate(date, datetab, default):
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

        b = parsedate(b, self.date, self.date[0])
        e = parsedate(e, self.date, self.date[self.ntime - 1])

        if legend is None:
            legend = ProReader.dico[var][1]

        if var == 'grain':
            colormap = 'grains'
        else:
            colormap = colormap

        intime = (self.date >= b) * (self.date <= e)
        # Trace par appel a plot_profil
        ep = self.var['ep'][intime]
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
                    return self.date[intime][x].strftime('%Y-%m-%d %Hh')
                else:
                    return 'E'
            formatter = ticker.FuncFormatter(format_ticks)
            axe.xaxis.set_major_formatter(formatter)
            plt.setp(axe.xaxis.get_majorticklabels(), rotation=60)
            plt.tight_layout()

    def plot1D(self, axe, var, b=None, e=None, legend=None, color='bo'):
        '''
        Trace la variable demandee en fonction du temps
            axe : matplotlib.Axe
            var : string, nom variable 1D a afficher (cf. ProReader.dico1D)
            b : date debut affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            e : date fin affichage, datetime format or string YYYYMMDD ou YYYYMMDDHH
            legend : legende colorbar, legende automatique par defaut
            color : string, color name
        '''
        # Interpretation des entrees
        var1D = ProReader.varname1D(var)

        def parsedate(date, datetab, default):
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

        b = parsedate(b, self.date, self.date[0])
        e = parsedate(e, self.date, self.date[self.ntime - 1])

        if legend is None:
            legend = ProReader.dico1D[var1D][1]

        intime = (self.date >= b) * (self.date <= e)
        toplot = self.var1D[var1D][intime]
        xplot = self.date[intime]
        axe.plot_date(xplot, toplot, color)
        axe.set_ylabel(legend)

    def plot_date(self, axe, var, date=None, legend=None, color='b', cbar_show=True):
        '''
        Trace la variable demandee sur la hauteur du manteau neigeux a une date donnee
            axe : matplotlib.Axe
            var : string, nom variable a afficher
            date : date a afficher, datetime format or string YYYYMMDD ou YYYYMMDDHH (defaut : derniere date du fichier)
            legend : legende colorbar, legende automatique par defaut
            color : couleur du trace
            cbar_show : Lors du plot des grains, affiche ou pas la colorbar
        '''
        var = ProReader.varname(var)
        if legend is None:
            legend = ProReader.dico[var][1]

        def parsedate(date, datetab, default):
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
        date = parsedate(date, self.date, self.date[self.ntime - 1])
        date = self.date[self.date >= date][0]
        ep = self.var['ep'][self.date == date]
        epc = np.cumsum(ep)

        if(var == 'grain'):
            plot_grains1D(axe, ep, self.var['grain'][self.date == date], legend=legend, cbar_show=cbar_show)
            locator0 = ticker.NullLocator()
            axe.xaxis.set_major_locator(locator0)
            axe.set_xlim(0, 1)
        else:
            pointsx = np.zeros(2 * self.nsnowlayer + 2)
            pointsx[2:2 * self.nsnowlayer + 2:2] = epc
            pointsx[3:2 * self.nsnowlayer + 2:2] = epc
            pointsx[0:2] = 0
            pointsy = np.zeros(2 * self.nsnowlayer + 2)
            pointsy[1:2 * self.nsnowlayer:2] = self.var[var][self.date == date]
            pointsy[2:2 * self.nsnowlayer + 1:2] = self.var[var][self.date == date]
            axe.plot(pointsy, pointsx, color=color)
            axe.set_xlabel(legend, color=color)
            Max = np.nanmax(self.var[var][self.date == date]) if np.nanmax(self.var[var][self.date == date]) > 0 else 0
            Min = np.nanmin(self.var[var][self.date == date]) if np.nanmin(self.var[var][self.date == date]) < 0 else 0
            axe.set_xlim(Min, Max)
        axe.set_ylim(np.nansum(ep), 0)

    def varname(nom):
        '''
        Permet de retrouver le nom de la variable recherchee
        si l utilisateur entre le nom SURFEX
        Valable pour les variables 2D
        '''
        if nom in list(ProReader.dico.keys()):
            return nom
        else:
            for key, val in list(ProReader.dico.items()):
                if nom == val[0]:
                    return key
            return 'err'
    varname = staticmethod(varname)

    def varname1D(nom):
        '''
        Permet de retrouver le nom de la variable recherchee
        si l utilisateur entre le nom SURFEX
        Valable pour les variables 1D
        '''
        if nom in list(ProReader.dico1D.keys()):
            return nom
        else:
            for key, val in list(ProReader.dico1D.items()):
                if nom == val[0]:
                    return key
            return 'err'
    varname1D = staticmethod(varname1D)


if __name__ == "__main__":
    from argparse import ArgumentParser
    parser = ArgumentParser(description = """
                                            Programme permettant de lire un fichier PRO (ou plusieurs en les concatenant dans l'ordre)
                                            et trace l evolution temporelle des variables 2D (--plot) ou 1D (--plot1D) souhaitees ou
                                            l'etat du manteau a une date donnee (--dateplot).
                                            """)
    parser.add_argument('ncfile', help="Fichier PRO.nc a lire, si plusieurs, concatenation de ces fichiers", nargs='+')
    parser.add_argument('--point', help="Numero du point a lire dans le cas d'un fichier avec plusieurs points de simulation. Par defaut, le premier point sera lu.", type=int, dest='point')
    parser.add_argument('-b', '--begin', help="Date de debut, format YYYYMMDD ou YYYYMMDDHH", type=str, dest='begin')
    parser.add_argument('-e', '--end', help="Date de fin, format YYYYMMDD ou YYYYMMDDHH", type=str, dest='end')
    parser.add_argument('-p', '--plot', help="liste d'au plus 6 elemnts a afficher\n Variables SURFEX ou dans la liste suivante : \n %s" % list(ProReader.dico.keys()), type=str, nargs='*', dest='plot')
    parser.add_argument('--plot1D', help="liste d'au plus 6 elemnts a afficher\n Variables SURFEX ou dans la liste suivante : \n %s" % list(ProReader.dico1D.keys()), type=str, nargs='*', dest='plot1D')
    parser.add_argument('-d', '--date', help="Date pour l'option --dateplot, format YYYYMMDD ou YYYYMMDDHH", type=str, dest='date')
    parser.add_argument('--dateplot', help="Liste des elements to plot (meme liste que --plot).\nLa date correspond a la date --date ou a defaut, la derniere date du fichier.", type=str, nargs='+', dest='dateplot')
    parser.add_argument('--title', help="Titre pour --plot", dest='title')
    parser.add_argument('--datetitle', help="Titre pour --dateplot", dest='datetitle')
    parser.add_argument('--numeric', help="Trace sur les couches numeriques plutot que sur les couches reelles pour --plot (incompatible avec le trace du type de grains", dest='numeric', action="store_true")
    args = parser.parse_args()
    real_layers = not args.numeric

    # Lecture des fichiers PRO
    if(len(args.ncfile) == 1):
        pro = ProReader(ncfile=args.ncfile[0], point=args.point)
    else:
        pro = []
        for elem in args.ncfile:
            pro.append(ProReader(ncfile=elem, point=args.point))
        pro = ProReader(pro=pro)

    # Trace plot
    if(args.plot):
        for elem in args.plot:
            if ProReader.varname(elem) == 'err':
                print("Erreur dans le nom de variable %s" % elem)
                exit(3)
            if ProReader.varname(elem) == 'grain':
                real_layers = True
        length = len(args.plot)
        if(length == 0):
            fig1, ax1 = plt.subplots(1, 1, sharex=True, sharey=True)
            pro.plot(ax1, list(ProReader.dico.keys())[0], b=args.begin, e=args.end, real_layers=real_layers)
        elif(length == 1):
            fig1, ax1 = plt.subplots(1, 1, sharex=True, sharey=True)
            pro.plot(ax1, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
        elif(length == 2):
            fig1, (ax11, ax12) = plt.subplots(2, 1, sharex=True, sharey=True)
            pro.plot(ax11, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax12, args.plot[1], b=args.begin, e=args.end, real_layers=real_layers)
        elif(length == 3):
            fig1, ((ax11, ax12), (ax13, ax14)) = plt.subplots(2, 2, sharex=True, sharey=True)
            pro.plot(ax11, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax12, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax13, args.plot[1], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax14, args.plot[2], b=args.begin, e=args.end, real_layers=real_layers)
        elif(length == 4):
            fig1, ((ax11, ax12), (ax13, ax14)) = plt.subplots(2, 2, sharex=True, sharey=True)
            pro.plot(ax11, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax12, args.plot[1], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax13, args.plot[2], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax14, args.plot[3], b=args.begin, e=args.end, real_layers=real_layers)
        elif(length == 5):
            fig1, ((ax11, ax12, ax13), (ax14, ax15, ax16)) = plt.subplots(2, 3, sharex=True, sharey=True)
            pro.plot(ax11, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax12, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax13, args.plot[1], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax14, args.plot[2], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax15, args.plot[3], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax16, args.plot[4], b=args.begin, e=args.end, real_layers=real_layers)
        elif(length >= 6):
            fig1, ((ax11, ax12, ax13), (ax14, ax15, ax16)) = plt.subplots(2, 3, sharex=True, sharey=True)
            pro.plot(ax11, args.plot[0], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax12, args.plot[1], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax13, args.plot[2], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax14, args.plot[3], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax15, args.plot[4], b=args.begin, e=args.end, real_layers=real_layers)
            pro.plot(ax16, args.plot[5], b=args.begin, e=args.end, real_layers=real_layers)

        if(args.title):
            fig1.suptitle(args.title, fontsize=14)

    # Trace plot 1D
    if(args.plot1D):
        for elem in args.plot1D:
            if ProReader.varname1D(elem) == 'err':
                print("Erreur dans le nom de variable %s" % elem)
                exit(3)
        length = len(args.plot1D)
        if(length == 0):
            fig1, ax1 = plt.subplots(1, 1, sharex=True, sharey=False)
            pro.plot1D(ax1, list(ProReader.dico1D.keys())[0], b=args.begin, e=args.end)
        elif(length == 1):
            fig1, ax1 = plt.subplots(1, 1, sharex=True, sharey=False)
            pro.plot1D(ax1, args.plot1D[0], b=args.begin, e=args.end)
        elif(length == 2):
            fig1, (ax11, ax12) = plt.subplots(2, 1, sharex=True, sharey=False)
            pro.plot1D(ax11, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax12, args.plot1D[1], b=args.begin, e=args.end)
        elif(length == 3):
            fig1, ((ax11, ax12), (ax13, ax14)) = plt.subplots(2, 2, sharex=True, sharey=False)
            pro.plot1D(ax11, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax12, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax13, args.plot1D[1], b=args.begin, e=args.end)
            pro.plot1D(ax14, args.plot1D[2], b=args.begin, e=args.end)
        elif(length == 4):
            fig1, ((ax11, ax12), (ax13, ax14)) = plt.subplots(2, 2, sharex=True, sharey=False)
            pro.plot1D(ax11, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax12, args.plot1D[1], b=args.begin, e=args.end)
            pro.plot1D(ax13, args.plot1D[2], b=args.begin, e=args.end)
            pro.plot1D(ax14, args.plot1D[3], b=args.begin, e=args.end)
        elif(length == 5):
            fig1, ((ax11, ax12, ax13), (ax14, ax15, ax16)) = plt.subplots(2, 3, sharex=True, sharey=False)
            pro.plot1D(ax11, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax12, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax13, args.plot1D[1], b=args.begin, e=args.end)
            pro.plot1D(ax14, args.plot1D[2], b=args.begin, e=args.end)
            pro.plot1D(ax15, args.plot1D[3], b=args.begin, e=args.end)
            pro.plot1D(ax16, args.plot1D[4], b=args.begin, e=args.end)
        elif(length >= 6):
            fig1, ((ax11, ax12, ax13), (ax14, ax15, ax16)) = plt.subplots(2, 3, sharex=True, sharey=False)
            pro.plot1D(ax11, args.plot1D[0], b=args.begin, e=args.end)
            pro.plot1D(ax12, args.plot1D[1], b=args.begin, e=args.end)
            pro.plot1D(ax13, args.plot1D[2], b=args.begin, e=args.end)
            pro.plot1D(ax14, args.plot1D[3], b=args.begin, e=args.end)
            pro.plot1D(ax15, args.plot1D[4], b=args.begin, e=args.end)
            pro.plot1D(ax16, args.plot1D[5], b=args.begin, e=args.end)

        if(args.title):
            fig1.suptitle(args.title, fontsize=14)

    # Trace dateplot
    if(args.dateplot):
        for elem in args.dateplot:
            if ProReader.varname(elem) == 'err':
                print("Erreur dans le nom de variable %s" % elem)
                exit(3)
        length = len(args.dateplot)
        if(length == 0):
            fig2, ax2 = plt.subplots(1, 1, sharex=False, sharey=True)
            pro.plot_date(ax2, list(ProReader.dico.keys())[0], date=args.date)
            ax2.set_ylabel('Depth (m)')
        elif(length == 1):
            fig2, ax2 = plt.subplots(1, 1, sharex=False, sharey=True)
            pro.plot_date(ax2, args.dateplot[0], date=args.date)
            ax2.set_ylabel('Depth (m)')
        elif(length == 2):
            fig2, (ax21, ax22) = plt.subplots(1, 2, sharex=False, sharey=True)
            pro.plot_date(ax21, args.dateplot[0], date=args.date)
            pro.plot_date(ax22, args.dateplot[1], date=args.date)
            ax21.set_ylabel('Depth (m)')
        elif(length == 3):
            fig2, (ax21, ax22, ax23) = plt.subplots(1, 3, sharex=False, sharey=True)
            pro.plot_date(ax21, args.dateplot[0], date=args.date)
            pro.plot_date(ax22, args.dateplot[1], date=args.date)
            pro.plot_date(ax23, args.dateplot[2], date=args.date)
            ax21.set_ylabel('Depth (m)')
        elif(length == 4):
            fig2, (ax21, ax22, ax23, ax24) = plt.subplots(1, 4, sharex=False, sharey=True)
            pro.plot_date(ax21, args.dateplot[0], date=args.date)
            pro.plot_date(ax22, args.dateplot[1], date=args.date)
            pro.plot_date(ax23, args.dateplot[2], date=args.date)
            pro.plot_date(ax24, args.dateplot[3], date=args.date)
            ax21.set_ylabel('Depth (m)')
        elif(length == 5):
            fig2, ((ax21, ax22, ax23), (ax24, ax25, ax26)) = plt.subplots(2, 3, sharex=False, sharey=True)
            pro.plot_date(ax21, args.dateplot[0], date=args.date)
            pro.plot_date(ax22, args.dateplot[0], date=args.date)
            pro.plot_date(ax23, args.dateplot[1], date=args.date)
            pro.plot_date(ax24, args.dateplot[2], date=args.date)
            pro.plot_date(ax25, args.dateplot[3], date=args.date)
            pro.plot_date(ax26, args.dateplot[4], date=args.date)
            ax21.set_ylabel('Depth (m)')
            ax24.set_ylabel('Depth (m)')
        elif(length >= 6):
            fig2, ((ax21, ax22, ax23), (ax24, ax25, ax26)) = plt.subplots(2, 3, sharex=False, sharey=True)
            pro.plot_date(ax21, args.dateplot[0], date=args.date)
            pro.plot_date(ax22, args.dateplot[1], date=args.date)
            pro.plot_date(ax23, args.dateplot[2], date=args.date)
            pro.plot_date(ax24, args.dateplot[3], date=args.date)
            pro.plot_date(ax25, args.dateplot[4], date=args.date)
            pro.plot_date(ax26, args.dateplot[5], date=args.date)
            ax21.set_ylabel('Depth (m)')
            ax24.set_ylabel('Depth (m)')

        if(args.datetitle):
            fig2.suptitle(args.datetitle, fontsize=14)

    if(args.plot or args.plot1D or args.dateplot):
        plt.show()
