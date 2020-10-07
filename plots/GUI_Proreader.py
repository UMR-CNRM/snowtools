#! /usr/bin/env python
# -*- coding: utf-8 -*-

#from tkinter import *
from tkinter import Toplevel
from tkinter import Frame
from tkinter import Button
from tkinter import Scale
from tkinter import Menu
from tkinter import Label
from tkinter import IntVar
from tkinter import messagebox
from tkinter import ttk
from tkinter import Tk
import tkinter.filedialog

import logging
logger = logging.getLogger()
logger.setLevel(logging.WARNING)
console_handler = logging.StreamHandler()
console_handler.setFormatter(logging.Formatter('%(levelname)s :: %(message)s'))
console_handler.setLevel(logging.DEBUG)
logger.addHandler(console_handler)

import math
import numpy as np
import argparse
import sys
import os.path

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

from utils.prosimu import prosimu
from utils.infomassifs import infomassifs
from utils.dates import check_and_convert_date
from utils.FileException import FileNameException, FileParseException
import proReader_mini

import pickle

constante_sampling = proReader_mini.constante_sampling

########################################################
# Modele de fenetre graphique dont les autres heritent #
########################################################
class Graph(Toplevel):
    def __init__(self, parent, **Arguments):
        Toplevel.__init__(self, parent)
        self.transient(parent)
        self.grab_set()

        # Initialisation variables
        self.title('GUI PROreader CEN')        
        self.protocol("WM_DELETE_WINDOW", quit)
        self.taille_x = 900
        self.taille_y = 700
        self.geometry('900x700')
        self.x = ''
        self.y = ''
        self.test = ''
        self.variable = ''
        self.variable_souris = ''
        self.date = ''
        self.datedeb = ''
        self.datefin = ''
        self.date1_zoom = ''
        self.date1_zoom_old = ''
        self.bool_profil = False
        self.bool_layer = False
        self.figclear = True
        self.first_profil = True
        self.width_rect = 0.01
        self.rectangle_choix = ''
        self.filename = ''
        self.list_massif_num = []
        self.list_massif_nom = []
        self.liste_variable = []
        self.liste_variable_for_pres = []
        self.pro = ''
        self.Tableau = ''
        self.var_choix1 = ''
        self.var_choix2 = ''
        self.var_sup = []
        self.valeur_choisie1 = ''
        self.valeur_choisie2 = ''
        self.message_filedialog = 'Importer un fichier PRO'
        self.direction_coupe = 'up'
        self.hauteur_coupe = 10
        self.stop_right_click = False
        self.bool_ligne_commande = False
        self.clik_zoom = False
        self.largeur = self.winfo_width()/self.taille_x
        self.hauteur = self.winfo_height()/self.taille_y

        # Boutons communs a toutes les fenetres
        self.buttonQuit = Button(self, text = 'Quitter', command = self.close_window)
        self.buttonPlot = Button(self, text = 'Tracer graphe', state = 'disabled')
        self.buttonRaz = Button(self, text = 'Remise à zéro', state = 'disabled')
        self.buttonSave1 = Button(self, text = 'Sauver graphe', state = 'disabled')
        self.buttonSave2 = Button(self, text = 'Sauver profil', state = 'disabled')
        self.buttonSave3 = Button(self, text = 'Pickle graphe', state = 'disabled')
        self.buttonSave4 = Button(self, text = 'Pickle profil', state = 'disabled')

        self.buttonOpenFile = Button(self,  text='1: Ouvrir un fichier', command = self.recup)

        # Combobox communs a toutes les fenetres
        style = ttk.Style()
        style.configure('TCombobox', postoffset = (0,0,200,0))
        self.combobox = ttk.Combobox(self, state = 'disabled', values = '')
        self.combobox_choix_profil = ttk.Combobox(self, state = 'disabled', values = '')
        self.combobox_reduce2 = ttk.Combobox(self, state = 'disabled', values = '')
        self.combobox_reduce3 = ttk.Combobox(self, state = 'disabled', values = '')
        self.combobox_reduce4 = ttk.Combobox(self, state = 'disabled', values = '')
        self.scale_date = Scale(self, orient = 'horizontal', state = 'disabled', label = 'Echelle de dates')

        # Menu
        self.menubar = Menu()
        self.filemenu = Menu(self.menubar, tearoff = 0)
        self.filemenu.add_command(label = 'French', command=self.toFrench)
        self.filemenu.add_command(label = 'English', command=self.toEnglish)
        self.menubar.add_cascade(label = 'Change Language', menu=self.filemenu)
        self.config(menu = self.menubar)

        # Figures: graphe et profil
        self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        self.Canevas = FigureCanvasTkAgg(self.fig1, self)
        self.fig2, self.ax2 = plt.subplots(1, 1, sharex = True, sharey = True)
        self.ax3=self.ax2.twiny()
        self.Canevas2 = FigureCanvasTkAgg(self.fig2, self)

        self.make_list_massif()
        self.bind('<Configure>', lambda x: Graph.onsize_test(self, x))
        self.bind('<Control-o>', self.recup)
        self.bind('<Escape>', self.close_window)
        self.bind('<Control-q>', self.close_window)
        if len(Arguments) > 0:
            self.ini_ligne_commande(**Arguments)

    def close_window(self, *args):
        self.destroy()

    # POUR APPEL EN LIGNE DE COMMANDE
    def ini_ligne_commande(self, **Arguments):
        self.filename = Arguments.get('filename')
        self.variable = Arguments.get('variable')
        self.var_sup = [Arguments.get('profil')]
        self.variable_souris = Arguments.get('profil')

        self.ff = prosimu(self.filename)
        self.date = self.ff.readtime()
        self.datedeb = self.date[0]
        self.datefin = self.date[len(self.date)-1]
        listvariables = self.ff.listvar()

        # Choix du point:
        if 'SNOWDZ' in listvariables:
            self.type_fichier = 'PRO'
            self.point_choisi = 0
        elif 'Dsnw' or 'ts' or 'tsns' in listvariables:
            self.type_fichier = 'FSM'
            self.point_choisi = -1
        else:
            logger.warn('We suppose this is a PRO file without SNOWDZ')
            self.type_fichier = 'PRO'
            self.point_choisi = 0
        if Arguments.get('point') != 0:
            self.point_choisi = Arguments.get('point')

        self.combobox_reduce2.config(state = "readonly")
        self.combobox_reduce3.config(state = "readonly")
        self.combobox_reduce4.config(state = "readonly")
        self.combobox_choix_profil.config(state = "readonly")
        self.combobox.config(state = "readonly")
        self.buttonSave2.config(state = 'normal', command = self.Save_profil)
        self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
        self.buttonRaz.config(state = 'normal', command = self.raz)
        self.buttonSave1.config(state = 'normal', command = self.Save_plot)
        #self.buttonSave3.config(state = 'normal', command = self.Pickle_plot)
        self.buttonPlot.config(state = 'normal', command = self.Plotage)

        if len(self.date) > constante_sampling: 
            logger.info("Init with automatic sampling")
            messagebox.showinfo('Time > '+str(constante_sampling), 'Information: automatic sampling to avoid too long treatment')

    # CREER LISTE DES VARIABLES POUR COMBOBOX
    def creer_liste_variable(self):
        listvariables = self.ff.listvar()
        list_var_non_plot = ['time', 'slope', 'aspect', 'ZS', 'massif_num', 'latitude', 'longitude', 'lat', 'lon',
                             'Projection_Type', 'station', 'massif', 'naturalIndex']
        for i in range(len(listvariables)):
            if (listvariables[i] not in list_var_non_plot):
                if 'long_name' in self.ff.listattr(listvariables[i]) and self.ff.getattr(listvariables[i],'long_name') != '':
                    self.liste_variable_for_pres.append('{} ({})'.format(self.ff.getattr(listvariables[i],'long_name'), listvariables[i]))
                    self.liste_variable.append(listvariables[i])
                else:
                    self.liste_variable_for_pres.append(listvariables[i])
                    self.liste_variable.append(listvariables[i])
        self.combobox.config(state = 'readonly', values = self.liste_variable_for_pres)
        self.combobox.bind('<<ComboboxSelected>>', self.liste_profil)

        self.profil_complet = False
        # MODIFIER POUR FSM ???
        if ({'SNOWTYPE', 'SNOWRAM'}.issubset(set(self.ff.listvar()))):
        #if ({'SNOWTYPE'}.issubset(set(self.ff.listvar()))):
            self.profil_complet = True
            self.var_sup.extend([self.variable_souris, 'SNOWTYPE', 'SNOWRAM'])

    # INITIALISER LA LISTE DES MASSIFS
    def make_list_massif(self):
        self.list_massif_num = []
        self.list_massif_nom = []
        try:
            IM = infomassifs()
            listmassif = IM.getListMassif_of_region('all')
            for massif in listmassif:
                self.list_massif_num.append(massif)
                self.list_massif_nom.append(str(IM.getMassifName(massif).decode('UTF-8')))
        except FileNameException:
            logger.warning('Could not find massif metadata')
            self.list_massif_num = list(range(100))
            self.list_massif_nom = list(range(100))
        except FileParseException:
            logger.warning('Could not parse massif metadata')
            self.list_massif_num = list(range(100))
            self.list_massif_nom = list(range(100))
        except:
            logger.warning('Error while loading massif metadata')
            self.list_massif_num = list(range(100))
            self.list_massif_nom = list(range(100))

    # PLACEMENT BOUTON - COMBOBOX - ETC... COMMUN
    def onsize_test(self, event):
        self.largeur = self.winfo_width()/self.taille_x
        self.hauteur = self.winfo_height()/self.taille_y
        self.buttonQuit.place(x = 760*self.largeur, y = 660*self.hauteur)
        self.buttonOpenFile.place(x = 5*self.largeur, y = 5*self.hauteur)
        self.buttonPlot.place(x = 760*self.largeur, y = 100*self.hauteur)
        self.buttonRaz.place(x = 760*self.largeur, y = 5*self.hauteur)
        self.buttonSave1.place(x = 5*self.largeur, y = 660*self.hauteur)
        self.buttonSave2.place(x = 155*self.largeur, y = 660*self.hauteur)
        self.buttonSave3.place(x = 305*self.largeur, y = 660*self.hauteur)
        self.buttonSave4.place(x = 455*self.largeur, y = 660*self.hauteur)
        self.Canevas.get_tk_widget().place(x = 3*self.largeur, y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
        self.Canevas2.get_tk_widget().place(x = 504*self.largeur, y = 150*self.hauteur, width = 390*self.largeur, height = 500*self.hauteur)
        self.scale_date.place(x = 30*self.largeur, y = 100*self.hauteur)
        self.scale_date.config(length = 380*self.largeur)

    # TRADUCTION
    def toFrench(self):
        self.buttonQuit.config(text = 'Quitter')
        self.buttonOpenFile.config(text = '1: Ouvrir un fichier')
        self.buttonPlot.config(text = 'Tracer graphe')
        self.buttonRaz.config(text = 'Remise à zéro')
        self.buttonSave1.config(text = 'Sauver graphe')
        self.buttonSave2.config(text = 'Sauver profil')
        self.buttonSave3.config(text = 'Pickle graphe')
        self.buttonSave4.config(text = 'Pickle profil')
        self.message_filedialog='Importer un fichier PRO'
        self.scale_date.config(label='Echelle de dates')

    def toEnglish(self):
        self.buttonQuit.config(text='Exit')
        self.buttonOpenFile.config(text='1: Open File')
        self.buttonPlot.config(text='Plot')
        self.buttonRaz.config(text='Reset')
        self.buttonSave1.config(text='Save graph')
        self.buttonSave2.config(text='Save profile')
        self.buttonSave3.config(text='Pickle graph')
        self.buttonSave4.config(text='Pickle profile')
        self.message_filedialog='Import a PRO File'
        self.scale_date.config(label='Dates scale')

    # REMISE A ZERO 
    def refresh(self):
        # Combobox
        self.combobox.config(state = 'disabled',values='')
        self.combobox.set('')
        self.combobox_choix_profil.config(state = 'disabled',values='')
        self.combobox_choix_profil.set('')
        self.combobox_reduce2.config(state = 'disabled', values = '')
        self.combobox_reduce2.set('')
        self.combobox_reduce3.config(state = 'disabled', values = '')
        self.combobox_reduce3.set('')
        self.combobox_reduce4.config(state = 'disabled', values = '')
        self.combobox_reduce4.set('')
        # Variables
        self.var_sup = []
        self.bool_profil = False
        self.bool_ligne_commande = False
        self.liste_variable=[]
        self.liste_variable_for_pres=[]
        # Graphes
        if (self.figclear == False):
            self.fig1.clear()
            self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
            self.Canevas.get_tk_widget().destroy()
            self.Canevas = FigureCanvasTkAgg(self.fig1,self)
            self.Canevas.get_tk_widget().place(x = 3*self.largeur, y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
            self.fig2.clear()
            self.fig2, self.ax2 = plt.subplots(1, 1, sharex = True, sharey = True)
            self.ax3 = self.ax2.twiny()
            self.Canevas2.get_tk_widget().destroy()
            self.Canevas2 = FigureCanvasTkAgg(self.fig2,self)
            self.Canevas2.get_tk_widget().place(x = 504*self.largeur, y = 150*self.hauteur, width = 390*self.largeur, height = 500*self.hauteur)
        # Boutons
        self.buttonPlot.config(state = 'disabled')
        self.buttonSave1.config(state = 'disabled')
        self.buttonSave2.config(state = 'disabled')
        self.buttonSave3.config(state = 'disabled')
        self.buttonSave4.config(state = 'disabled')
        self.scale_date.config(from_ = 0, to = 0, state = 'disabled')
        self.figclear = True
        self.first_profil = True

    # RECUPERATION FICHIER
    def Ouvrir(self):
        selectedfilename = tkinter.filedialog.askopenfilename(title = self.message_filedialog, filetypes = [('PRO files','.nc'),('all files','.*')])
        if len(selectedfilename)==0:
            logger.info('No file selected. Ignore.')
            return None
        self.filename = selectedfilename
        self.raz()

        self.ff = prosimu(self.filename)
        self.date = self.ff.readtime()
        self.datedeb = self.date[0]
        self.datefin = self.date[len(self.date)-1]
        if len(self.date) > constante_sampling: 
            messagebox.showinfo('Time > '+str(constante_sampling), 'Information: automatic sampling to avoid too long treatment')

        listvariables = self.ff.listvar()
        list_var_non_plot = ['time', 'slope', 'aspect', 'ZS', 'massif_num', 'latitude', 'longitude', 'lat', 'lon',
                             'Projection_Type', 'station', 'massif', 'naturalIndex']

        if 'SNOWDZ' in listvariables:
            self.type_fichier = 'PRO'
        elif 'Dsnw' or 'ts' or 'tsns' in listvariables:
            self.type_fichier = 'FSM'
                    
        for i in range(len(listvariables)):
            if(self.ff.listvar()[i] not in list_var_non_plot):
                if 'long_name' in self.ff.listattr(listvariables[i]) and self.ff.getattr(listvariables[i],'long_name') != '':
                    self.liste_variable_for_pres.append('{} ({})'.format(self.ff.getattr(listvariables[i],'long_name'), listvariables[i]))
                    self.liste_variable.append(listvariables[i])
                else:
                    self.liste_variable_for_pres.append(listvariables[i])
                    self.liste_variable.append(listvariables[i])

        self.pro = proReader_mini.ProReader_standard(ncfile=self.filename)
        return self.filename

    # CHOIX VARIABLE PROFIL
    def choix_profil(self, event):
        if self.bool_no_snowlayer == False:
            variable_souris_for_pres = self.combobox_choix_profil.get()
            self.variable_souris = self.liste_variable[self.liste_variable_for_pres.index(variable_souris_for_pres)]
            if self.profil_complet:
                self.var_sup.extend([self.variable_souris,'SNOWTYPE','SNOWRAM'])
            else:
                self.var_sup.append(self.variable_souris)
            self.bool_profil = True
        else: 
            self.bool_profil = False
        self.test_presence_champs()

    # LISTE VARIABLES DU PROFIL
    def liste_profil(self):
        self.profil_complet = False
        if ({'SNOWTYPE','SNOWRAM'}.issubset(set(self.ff.listvar()))):
                self.profil_complet = True
        liste_pres = []
        liste = list(set(self.ff.listvar())-{'SNOWTYPE','SNOWRAM'})
        for var in liste:
            if 'snow_layer' in list(self.ff.getdimvar(var)):
                liste_pres.append(self.liste_variable_for_pres[self.liste_variable.index(var)])
        if liste_pres != []:
            self.bool_no_snowlayer = False
            self.combobox_choix_profil.config(values = liste_pres)
            self.combobox_choix_profil.config(state = "readonly")
            self.combobox_choix_profil.bind('<<ComboboxSelected>>', self.choix_profil)
        else:
            self.bool_no_snowlayer = True
            self.choix_profil(self)

    # TEST PRESENCE-ABSENCE DE PARAMETRE
    def test_presence_champs_4_params(self):
        # Cas 1 seul point
        if (len(self.Tableau[0]) == 1):
            if self.type_fichier == 'PRO':
                self.point_choisi=0
            if self.type_fichier == 'FSM':
                self.point_choisi=-1
            self.combobox_reduce1.config(state = 'disabled', values = '')
            self.combobox_reduce2.config(state = 'disabled', values = '')
            self.combobox_reduce3.config(state = 'disabled', values = '')
            self.combobox_reduce4.config(state = 'disabled', values = '')
            if self.Tableau[0] == [-10]:
                self.combobox_reduce1.set('inconnu')
            else:
                self.combobox_reduce1.set(self.Tableau[0][0])
            if self.Tableau[1] == [-10]:
                self.combobox_reduce2.set('inconnu')
            else:
                self.combobox_reduce2.set(self.Tableau[1][0])
            if self.Tableau[2] == [-10]:
                self.combobox_reduce3.set('inconnu')
            else:
                self.combobox_reduce3.set(self.Tableau[2][0])
            if self.Tableau[3] == [-10]:
                self.combobox_reduce4.set('inconnu')
            else:
                self.combobox_reduce4.set(self.Tableau[3][0])
            self.buttonPlot.config(state = 'normal', command = self.Plotage)

        # Cas plusieurs points mais des champs absents
        elif -10 in set(self.Tableau[0]).union(set(self.Tableau[1]), set(self.Tableau[2]), set(self.Tableau[3])) :
            if -10 in set(self.Tableau[0]):
                self.combobox_reduce1.config(state = 'disabled', values = '')
                self.combobox_reduce1.set('inconnu')
                self.ChoixPossible[0] = False
            if -10 in set(self.Tableau[1]):
                self.combobox_reduce2.config(state = 'disabled', values = '')
                self.combobox_reduce2.set('inconnu')
                self.ChoixPossible[1] = False
            if -10 in set(self.Tableau[2]):
                self.combobox_reduce3.config(state = 'disabled', values = '')
                self.combobox_reduce3.set('inconnu')
                self.ChoixPossible[2] = False
            if -10 in set(self.Tableau[3]):
                self.combobox_reduce4.config(state = 'disabled', values = '')
                self.combobox_reduce4.set('inconnu')
                self.ChoixPossible[3] = False
            self.reduce1(self)
            if self.bool_ligne_commande:
                self.Plotage()

        # Cas plusieurs points sans pb
        else:
            self.reduce1(self)
        if self.bool_ligne_commande:
            self.Plotage()

    # CHOIX POINT D'INTERET PAR REDUCTIONS SUCCESSIVES (4 CHOIX)
    def reduce1(self, event):
        if self.ChoixPossible[0]:
            self.combobox_reduce1.config(state = "readonly")
            liste = []
            for it_massif in list(set(self.Tableau[0])):
                indice = self.list_massif_num.index(it_massif)
                liste.append(self.list_massif_nom[indice])
            self.combobox_reduce1.config(values = liste)
            self.combobox_reduce1.bind('<<ComboboxSelected>>', self.reduce2)
            if self.bool_profil:
                self.choix_point()
        else:
            self.reduce2(self)


    def reduce2(self, event):
        if self.ChoixPossible[1]:
            self.combobox_reduce2.config(state = "readonly")
            n = len(self.Tableau[0])
            if self.ChoixPossible[0]:
                nom_massif = self.combobox_reduce1.get()
                indice = self.list_massif_nom.index(nom_massif)
                num_massif = self.list_massif_num[indice]
                self.list_choix[0] = float(num_massif)
                A = ( self.Tableau[0] == ([self.list_choix[0]]*n) )
                indices = A
            else:
                indices = np.asarray([True]*n)

            liste = list(set(self.Tableau[1,indices]))
            self.combobox_reduce2.config(values = liste)
            self.combobox_reduce2.bind('<<ComboboxSelected>>', self.reduce3)
            if self.bool_profil:       
                self.choix_point()
        else:
            self.reduce3(self)
        
    def reduce3(self, event):
        if self.ChoixPossible[2]:
            self.combobox_reduce3.config(state = "readonly")
            n = len(self.Tableau[0])
            if self.ChoixPossible[1]:
                altitude = self.combobox_reduce2.get()
                self.list_choix[1] = float(altitude)
                B = ( self.Tableau[1] == ([self.list_choix[1]]*n) )
            else:
                B = np.asarray([True]*n)
            if self.ChoixPossible[0]:
                A = ( self.Tableau[0] == ([self.list_choix[0]]*n) )
            else:
                A = np.asarray([True]*n)
            indices = A & B

            liste = list(set(self.Tableau[2,indices]))
            self.combobox_reduce3.config(values = liste)
            self.combobox_reduce3.bind('<<ComboboxSelected>>', self.reduce4)
            if self.bool_profil:
                self.choix_point()
        else:
            self.reduce4(self)

    def reduce4(self, event):
        if self.ChoixPossible[3]:
            self.combobox_reduce4.config(state = "readonly")
            n = len(self.Tableau[0])
            if self.ChoixPossible[2]:
                pente = self.combobox_reduce3.get()
                self.list_choix[2] = float(pente)
                C = self.Tableau[2,:]==[self.list_choix[2]]*n
            else:
                C = np.asarray([True]*n)
            if self.ChoixPossible[1]:
                B = ( self.Tableau[1] == ([self.list_choix[1]]*n) )
            else:
                B = np.asarray([True]*n)
            if self.ChoixPossible[0]:
                A = ( self.Tableau[0] == ([self.list_choix[0]]*n) )
            else:
                A = np.asarray([True]*n)
            indices = A & B & C

            liste = list(set(self.Tableau[3,indices]))
            self.combobox_reduce4.config(values = liste)
            self.combobox_reduce4.bind('<<ComboboxSelected>>', self.finalisation_reduce)
            if self.bool_profil:
                self.choix_point()
        else:
            self.finalisation_reduce(self)

    def choix_point(self):
        n = len(self.Tableau[0])
        if self.ChoixPossible[0]:
            A = ( self.Tableau[0] == ([self.list_choix[0]]*n) )
        else:
            A = np.asarray([True]*n)
        if self.ChoixPossible[1]:
            B = ( self.Tableau[1] == ([self.list_choix[1]]*n) )
        else:
            B = np.asarray([True]*n)
        if self.ChoixPossible[2]:
            C = ( self.Tableau[2] == ([self.list_choix[2]]*n) )
        else:
            C = np.asarray([True]*n)
        if self.ChoixPossible[3]:
            D = ( self.Tableau[3] == ([self.list_choix[3]]*n) )
        else:
            D = np.asarray([True]*n)

        indices = A & B & C & D

        if True not in list(indices):
            self.buttonPlot.config(state = 'disabled')
        else:
            self.point_choisi = list(indices).index(True)
            if self.bool_profil:
                self.buttonPlot.config(state = 'normal', command = self.Plotage)

    def finalisation_reduce(self, event):
        if self.ChoixPossible[3]:
            orientation = self.combobox_reduce4.get()
            self.list_choix[3] = float(orientation)
        self.choix_point()

    # PROFIL INTERACTIF
    def motion(self, event):     
        if self.bool_desactive_motion or self.stop_right_click:
            return
        if (event.inaxes == self.ax1):
            date_souris = self.date[min(np.where(self.intime)[0][int(math.floor(event.xdata))], len(self.date)-1)]
            hauteur_souris = event.ydata
            self.ax2.clear()
            if self.profil_complet:
                self.ax3.clear()
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, date_souris, hauteur_souris, cbar_show = self.first_profil, bool_layer = self.bool_layer)
            else:
                self.pro.plot_profil(self.ax2, self.variable_souris, date_souris, hauteur_souris, bool_layer = self.bool_layer)
            self.Canevas2.draw()
            self.buttonSave2.config(state = 'normal', command = self.Save_profil)
            self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
            plt.close(self.fig2)
            self.first_profil = False

    # PROFIL INTERACTIF PENDANT ZOOM
    def motion_zoom(self, event):
        if self.bool_desactive_motion or self.stop_right_click:
            return
        if (event.inaxes == self.ax1):
            hauteur_souris = event.ydata
            date_souris = self.date[min(np.where(self.intime)[0][int(math.floor(event.xdata))], len(self.date)-1)]
            self.ax2.clear()
            top_zoom = self.pro.get_topplot(self.variable, self.date1_zoom, self.date2_zoom)

            if self.profil_complet:
                self.ax3.clear()
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, date_souris, hauteur_souris, cbar_show = self.first_profil, top = top_zoom, bool_layer=self.bool_layer)
            else:    
                self.pro.plot_profil(self.ax2, self.variable_souris, date_souris, hauteur_souris, top = top_zoom, bool_layer = self.bool_layer)

            self.Canevas2.draw()
            plt.close(self.fig2)
            self.first_profil = False

    # ZOOM
    def on_button_press(self, event):
        if (event.button > 1):
            self.stop_right_click = not self.stop_right_click
            return
        if self.bool_desactive_motion or self.stop_right_click:
            return
        if (event.inaxes == self.ax1):
            self.boolzoom = True
            self.x_date1_zoom = event.xdata
            self.date1_zoom = self.date[min(np.where(self.intime)[0][int(math.floor(event.xdata))], len(self.date)-1)]
            bottom, top = self.ax1.get_ylim()
            self.rectangle_choix = self.ax1.add_patch(matplotlib.patches.Rectangle((self.x_date1_zoom, bottom), self.width_rect, top - bottom, alpha = 0.1))
            self.Canevas.draw()

    def on_move_press(self, event):
        if self.bool_desactive_motion or self.stop_right_click:
            return
        if self.boolzoom is False:
            return
        if (event.inaxes == self.ax1):
            hauteur = self.winfo_height()/self.taille_y
            self.width_rect = abs(event.xdata-self.x_date1_zoom)
            height_rect = 500*hauteur
            self.rectangle_choix.set_width(self.width_rect)
            if event.xdata-self.x_date1_zoom  < 0:
                self.rectangle_choix.set_x(event.xdata)
            self.Canevas.draw()
            self.Canevas.flush_events()

    def on_button_release(self, event):
        if self.bool_desactive_motion or self.stop_right_click:
            return
        if (event.button > 1):
            return
        if (event.inaxes == self.ax1):
            self.width_rect = 0.01
            
            self.date2_zoom = self.date[min(np.where(self.intime)[0][int(math.floor(event.xdata))], len(self.date)-1)]
            
            self.fig1.clear()
            self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
            self.Canevas.get_tk_widget().destroy()
            self.Canevas = FigureCanvasTkAgg(self.fig1,self)
            self.Canevas.get_tk_widget().place(x = 3*self.largeur,y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
            
            self.fig2.clear()
            self.fig2, self.ax2 = plt.subplots(1, 1, sharex = True, sharey = True)
            self.ax3 = self.ax2.twiny()
            self.first_profil = True
            # Chez Neige ( + sur le Mac de Pascal), le Canevas2.destroy amene un segmentation fault 11 => on a commente et c'est bon...
            # Ce serait quand même bien de savoir ce qu'il se passe... 
            #self.Canevas2.get_tk_widget().destroy()
            self.Canevas2 = FigureCanvasTkAgg(self.fig2,self)
            self.Canevas2.get_tk_widget().place(x = 504*self.largeur,y = 150*self.hauteur,width = 390*self.largeur, height = 500*self.hauteur)
        
            if self.date1_zoom>self.date2_zoom:
                self.date1_zoom,self.date2_zoom = self.date2_zoom,self.date1_zoom

            if ('snow_layer' in self.ff.getdimvar(self.variable)):
                self.intime = self.pro.plot(self.ax1, self.variable, self.date1_zoom, self.date2_zoom,
                                               legend = self.variable, direction_cut = self.direction_coupe, height_cut = self.hauteur_coupe)
            else:
                self.intime = self.pro.plot1D(self.ax1, self.variable, self.date1_zoom, self.date2_zoom, legend = self.variable)
            self.Canevas.draw()
            self.boolzoom = False
            self.Canevas.mpl_connect('motion_notify_event', self.motion_zoom)
            self.Canevas.mpl_connect('button_press_event', self.on_button_press)
            self.Canevas.mpl_connect('motion_notify_event', self.on_move_press)
            self.Canevas.mpl_connect('button_release_event', self.on_button_release)
            
            plt.close(self.fig1)

    # SAUVEGARDE
    def Export_data(self):
        self.file_opt = options = {}
        options['filetypes'] = [('all files', '.*'),
                                ('csv files', '.csv')]

        options['initialfile'] = 'proreader_data.csv'
        filename = tkinter.filedialog.asksaveasfilename(**self.file_opt)

        X_data = self.ax1.lines[0].get_xdata()
        Y_data = self.ax1.lines[0].get_ydata()
        data = np.vstack((self.date[self.intime][X_data],Y_data))

        if filename:
            return np.savetxt(filename, np.transpose(data), delimiter=',', fmt="%s")

    def Save_plot(self):
        self.file_opt = options = {}
        options['filetypes'] = [('all files', '.*'),
                                ('jpeg image', '.jpg'), ('png image', '.png'),
                                ('tiff image', '.tiff'), ('bmp image', '.bmp')]

        options['initialfile'] = 'graph_proreader.png'
        filename = tkinter.filedialog.asksaveasfilename(**self.file_opt)

        if filename:
            return self.fig1.savefig(filename, bbox_inches='tight')
        
    def Save_profil(self):
        self.file_opt = options = {}
        options['filetypes'] = [('all files', '.*'),
                                ('jpeg image', '.jpg'), ('png image', '.png'),
                                ('tiff image', '.tiff'), ('bmp image', '.bmp')]

        options['initialfile'] = 'profil_proreader.png'
        filename = tkinter.filedialog.asksaveasfilename(**self.file_opt)

        if filename:
            return self.fig2.savefig(filename, bbox_inches='tight')
        
    def Pickle_plot(self):
        self.file_opt = options = {}
        options['filetypes'] = [('all files', '.*')]

        options['initialfile'] = 'mypicklefile'
        filename = tkinter.filedialog.asksaveasfilename(**self.file_opt)

        if filename:
            return pickle.dump(self.fig1, open(filename, 'wb'))
        
    def Pickle_profil(self):
        self.file_opt = options = {}
        options['filetypes'] = [('all files', '.*')]

        options['initialfile'] = 'mypicklefile'
        filename = tkinter.filedialog.asksaveasfilename(**self.file_opt)

        if filename:
            return pickle.dump(self.fig2, open(filename, 'wb'))


################################################################################################
################################################################################################
#                                                                                              #
#                                                                                              #
#                             Standard graph                                                   #
#                                                                                              #
#                                                                                              #
################################################################################################
################################################################################################

class GraphStandard(Graph):
    def __init__(self, parent, **Arguments):
        Graph.__init__(self, parent, **Arguments)
        self.boolzoom = False
        self.list_choix = [None, None, None, None]
        self.point_choisi = 0
        self.ChoixPossible = [True, True, True, True]
        self.type_fichier = ''

        self.label_var = Label(self, text = '2: Variable à tracer')
        self.label_choix_profil = Label(self, text = '3: Choix variable profil')
        self.label_reduce1 = Label(self, text = '4: Choix massif')
        self.label_reduce2 = Label(self, text = '5: Choix altitude')
        self.label_reduce3 = Label(self, text = '6: Choix angle de pente')
        self.label_reduce4 = Label(self, text = '7: Choix orientation')
        self.combobox_reduce1 = ttk.Combobox(self, state = 'disabled', values = '')
        self.bind('<Configure>', self.onsize_test)

        if len(Arguments) > 0:
            self.ini_ligne_commande_interne(**Arguments)

    def ini_ligne_commande_interne(self, **Arguments):
        #Graph.ini_ligne_commande(self,**Arguments)
        self.pro = proReader_mini.ProReader_standard(ncfile = self.filename, var = self.variable, point = int(self.point_choisi), var_sup = self.var_sup)
        self.Tableau = self.pro.get_choix(self.filename)
        if self.Tableau[0][self.point_choisi] >0: 
            self.combobox_reduce1.set(self.list_massif_nom[self.list_massif_num.index(self.Tableau[0][self.point_choisi])])
        else:
            self.combobox_reduce1.set('inconnu')
        self.combobox_reduce2.set(str(self.pro.alt))
        self.combobox_reduce3.set(str(self.pro.slope))
        self.combobox_reduce4.set(str(self.pro.aspect))
        self.combobox_choix_profil.set(self.var_sup)
        self.combobox.set(self.variable)
        self.combobox_reduce1.config(state = "readonly")

        Graph.creer_liste_variable(self)
        self.bool_ligne_commande = True
        self.test_presence_champs()
    ##########################################################
    # PLACEMENT BOUTONS, LISTES DEFILANTES, ETC...
    ##########################################################
    def onsize_test(self, event):
        Graph.onsize_test(self, event)
        self.label_var.place(x = 200*self.largeur, y = 5*self.hauteur)
        self.combobox.place(x = 200*self.largeur, y = 20*self.hauteur)
        self.label_choix_profil.place(x = 400*self.largeur, y = 5*self.hauteur)
        self.combobox_choix_profil.place(x = 400*self.largeur, y = 20*self.hauteur)
        self.label_reduce1.place(x = 75*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce1.place(x = 75*self.largeur, y = 60*self.hauteur)
        self.label_reduce2.place(x = 270*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce2.place(x = 270*self.largeur, y = 60*self.hauteur)
        self.label_reduce3.place(x = 470*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce3.place(x = 470*self.largeur, y = 60*self.hauteur)
        self.label_reduce4.place(x = 670*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce4.place(x = 670*self.largeur, y = 60*self.hauteur)

    ##########################################################
    # TRADUCTION
    ##########################################################
    def toFrench(self):
        Graph.toFrench(self)
        self.label_var.config(text = '2: Variable à tracer')
        self.label_choix_profil.config(text = '3: Choix variable profil')
        self.label_reduce1.config(text = '4: Choix massif')
        self.label_reduce2.config(text = '5: Choix altitude')
        self.label_reduce3.config(text = '6: Choix angle de pente')
        self.label_reduce4.config(text = '7: Choix orientation')

    def toEnglish(self):
        Graph.toEnglish(self)
        self.label_var.config(text = '2: Variable to plot')
        self.label_choix_profil.config(text = '3: Variable for Profile')
        self.label_reduce1.config(text = '4: Choose massif')
        self.label_reduce2.config(text = '5: Choose altitude')
        self.label_reduce3.config(text = '6: Choose slope')
        self.label_reduce4.config(text = '7: Choose orientation')

    ##########################################################
    # GUI: GESTION BARRE DES DATES
    ##########################################################
    def update_plot(self,value):
        if self.bool_desactive_motion:
            self.date_motion = self.date[int(value)]
            self.ax1.clear()
            self.pro.plot1D_bande(self.ax1, self.variable, date = self.date_motion, legend = self.variable)
            self.Canevas.draw()
            plt.close(self.fig1)
            self.ax2.clear()
            if self.profil_complet:
                self.ax3.clear()
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, date = self.date_motion, cbar_show = self.first_profil, bool_layer = self.bool_layer)    
            else:
                self.pro.plot_profil(self.ax2, self.variable_souris, date = self.date_motion, bool_layer = self.bool_layer)
            self.Canevas2.draw()
            self.buttonSave2.config(state = 'normal', command = self.Save_profil)
            self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
            plt.close(self.fig2)
            self.clik_zoom = False
    
    ##########################################################
    # REMISE À ZERO
    ##########################################################
    def raz(self):
        Graph.refresh(self)
        self.combobox_reduce1.config(state = 'disabled', values = '')
        self.combobox_reduce1.set('')
        self.list_choix = [None, None, None, None]
        self.ChoixPossible = [True, True, True, True]

    ##########################################################
    # CHOIX VARIABLE
    ##########################################################
    def liste_profil(self, event):
        variable_for_pres=self.combobox.get()
        self.variable=self.liste_variable[self.liste_variable_for_pres.index(variable_for_pres)]
        self.var_sup.append(self.variable)
        logger.info('Variable {} selected'.format(self.variable))
        if self.bool_profil:
            self.pro = proReader_mini.ProReader_standard(ncfile=self.filename, var=self.variable, point=int(self.point_choisi),var_sup=self.var_sup)
        Graph.liste_profil(self)

    def recup(self, *args):
        if Graph.Ouvrir(self) is not None:
            self.Tableau=self.pro.get_choix(self.filename)
            self.combobox.config(state ='readonly', values=self.liste_variable_for_pres)
            self.combobox.bind('<<ComboboxSelected>>', self.liste_profil)

    ##########################################################
    # TEST PRESENCE-ABSENCE DE PARAMETRE
    ##########################################################
    def test_presence_champs(self):
        Graph.test_presence_champs_4_params(self)

    ##########################################################
    # TRACE
    ##########################################################
    def Plotage(self):
        self.boolzoom = False
        self.pro = proReader_mini.ProReader_standard(ncfile = self.filename, var = self.variable, point = int(self.point_choisi),var_sup = self.var_sup)
        self.fig1.clear()
        self.ax1.clear()
        self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        self.Canevas.get_tk_widget().destroy()
        self.Canevas = FigureCanvasTkAgg(self.fig1,self)
        self.Canevas.get_tk_widget().place(x = 3*self.largeur, y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
        logger.info('Variable {} selected'.format(self.variable))
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.intime = self.pro.plot(self.ax1, self.variable, self.datedeb, self.datefin, real_layers = True, legend = self.variable)
            self.bool_layer = True
            self.scale_date.config(from_ = 0, to = 0,state = 'disabled')
            self.bool_desactive_motion = False
        elif('bands' in self.ff.getdimvar(self.variable)):
            self.pro.plot1D_bande(self.ax1, self.variable, self.datedeb, legend = self.variable)
            self.bool_layer = False
            if self.profil_complet:
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, self.datedeb, cbar_show = self.first_profil, bool_layer = self.bool_layer)    
            else:
                self.pro.plot_profil(self.ax2, self.variable_souris, self.datedeb, bool_layer = self.bool_layer)
            self.Canevas2.draw()
            self.buttonSave2.config(state = 'normal', command = self.Save_profil)
            self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
            plt.close(self.fig2)
            self.scale_date.config(from_ = 0, to = (len(self.date)-1), state = 'normal', showvalue = 0, command = self.update_plot, variable = IntVar)
            self.bool_desactive_motion = True
        else:
            self.intime = self.pro.plot1D(self.ax1, self.variable, self.datedeb, self.datefin, legend = self.variable)
            self.bool_layer = False
            self.scale_date.config(from_ = 0, to = 0, state = 'disabled')
            self.bool_desactive_motion = False
        self.Canevas.draw()
        self.Canevas.mpl_connect('motion_notify_event', self.motion)
        self.Canevas.mpl_connect('button_press_event', self.on_button_press)
        self.Canevas.mpl_connect('motion_notify_event', self.on_move_press)
        self.Canevas.mpl_connect('button_release_event', self.on_button_release)
        plt.close(self.fig1)
        self.buttonRaz.config(state = 'normal', command = self.raz)
        self.buttonSave1.config(state = 'normal', command = self.Save_plot)
        #self.buttonSave3.config(state = 'normal', command = self.Pickle_plot)
        self.figclear = False

        
################################################################################################
################################################################################################
#                                                                                              #
#                                                                                              #
#                             Height graph                                                     #
#                                                                                              #
#                                                                                              #
################################################################################################
################################################################################################

class GraphHeight(Graph):
    def __init__(self, parent, **Arguments):
        Graph.__init__(self, parent)
        self.boolzoom = False
        self.list_choix = [None, None, None, None]
        self.point_choisi = 0
        self.ChoixPossible = [True, True, True, True]
        
        self.buttonExportData = Button(self,  text = 'Export data', state = 'disabled')
        self.combobox_reduce1 = ttk.Combobox(self, state = 'disabled', values = '')
        self.combobox_choix_direction = ttk.Combobox(self, state = 'disabled', values = ['up', 'down'])
        self.combobox_choix_hauteur = ttk.Combobox(self, state = 'disabled', values = [x for x in range(5,600,5)])

        self.label_var=Label(self,text='2: Variable à tracer')
        self.label_choix_direction = Label(self,text='3: Choix direction')
        self.label_choix_hauteur = Label(self,text='4: Choix hauteur')
        self.label_choix_profil=Label(self,text='5: Choix variable profil')
        self.label_reduce1 = Label(self,text='6: Choix massif')
        self.label_reduce2 = Label(self,text='7: Choix altitude')
        self.label_reduce3 = Label(self,text='8: Choix angle de pente')
        self.label_reduce4 = Label(self,text='9: Choix orientation')

        self.bind('<Configure>', self.onsize_test)

        if len(Arguments) > 0:
            self.ini_ligne_commande_interne(**Arguments)

    def ini_ligne_commande_interne(self, **Arguments):
        Graph.ini_ligne_commande(self, **Arguments)
        self.pro = proReader_mini.ProReader_height(ncfile = self.filename, var = self.variable, point = int(self.point_choisi), var_sup = self.var_sup)
        self.Tableau = self.pro.get_choix(self.filename)
        if 'direction' in Arguments.keys():
            self.direction_coupe = Arguments.get('direction')
        if 'height' in Arguments.keys():
            self.hauteur_coupe = Arguments.get('height')
		
        if self.Tableau[0][self.point_choisi] >0: 
            self.combobox_reduce1.set(self.list_massif_nom[self.list_massif_num.index(self.Tableau[0][self.point_choisi])])
        else:
            self.combobox_reduce1.set('inconnu')
        self.combobox_reduce2.set(str(self.pro.alt))
        self.combobox_reduce3.set(str(self.pro.slope))
        self.combobox_reduce4.set(str(self.pro.aspect))
        self.combobox_choix_profil.set(self.var_sup)
        self.combobox.set(self.variable)
        self.combobox_choix_direction.set(str(self.direction_coupe))
        self.combobox_choix_hauteur.set(str(self.hauteur_coupe))
        self.combobox_reduce1.config(state = "readonly")
        self.combobox_choix_direction.config(state = "readonly")
        self.combobox_choix_hauteur.config(state = "readonly")
        self.buttonExportData.config(state = 'normal', command = self.Export_data)

        Graph.creer_liste_variable(self)
        self.bool_ligne_commande = True
        self.test_presence_champs()

    ##########################################################
    # PLACEMENT BOUTONS, LISTES DEFILANTES, ETC...
    ##########################################################
     
    def onsize_test(self, event):
        Graph.onsize_test(self, event)
        self.buttonExportData.place(x = 605*self.largeur, y = 660*self.hauteur)
        self.label_var.place(x = 170*self.largeur, y = 5*self.hauteur)
        self.combobox.place(x = 170*self.largeur, y = 20*self.hauteur)
        self.label_choix_direction.place(x = 370*self.largeur, y = 5*self.hauteur)
        self.combobox_choix_direction.place(x = 370*self.largeur, y = 20*self.hauteur)
        self.label_choix_hauteur.place(x = 570*self.largeur, y = 5*self.hauteur)
        self.combobox_choix_hauteur.place(x = 570*self.largeur, y = 20*self.hauteur)
        self.label_choix_profil.place(x = 70*self.largeur, y = 50*self.hauteur)
        self.combobox_choix_profil.place(x = 70*self.largeur, y = 65*self.hauteur)
        self.label_reduce1.place(x = 270*self.largeur, y = 50*self.hauteur)
        self.combobox_reduce1.place(x = 270*self.largeur, y = 65*self.hauteur)
        self.label_reduce2.place(x = 470*self.largeur, y = 50*self.hauteur)
        self.combobox_reduce2.place(x = 470*self.largeur, y = 65*self.hauteur)
        self.label_reduce3.place(x = 670*self.largeur, y = 50*self.hauteur)
        self.combobox_reduce3.place(x = 670*self.largeur, y = 65*self.hauteur)
        self.label_reduce4.place(x = 555*self.largeur, y = 85*self.hauteur)
        self.combobox_reduce4.place(x = 555*self.largeur, y = 100*self.hauteur)


    ##########################################################
    # TRADUCTION
    ##########################################################
    def toFrench(self):
        Graph.toFrench(self)
        self.label_var.config(text='2: Variable à tracer')
        self.label_choix_direction.config(text='3: Choix direction')
        self.label_choix_hauteur.config(text='4: Choix hauteur')
        self.label_choix_profil.config(text='5: Choix variable profil')
        self.label_reduce1.config(text='6: Choix massif')
        self.label_reduce2.config(text='7: Choix altitude')
        self.label_reduce3.config(text='8: Choix angle de pente')
        self.label_reduce4.config(text='9: Choix orientation')

    def toEnglish(self):
        Graph.toEnglish(self)
        self.label_var.config(text='2: Variable to plot')
        self.label_choix_direction.config(text='3: Choose direction')
        self.label_choix_hauteur.config(text='4: Choose height')
        self.label_choix_profil.config(text='5: Variable for Profile')
        self.label_reduce1.config(text='6: Choose massif')
        self.label_reduce2.config(text='7: Choose altitude')
        self.label_reduce3.config(text='8: Choose slope')
        self.label_reduce4.config(text='9: Choose orientation')

    ##########################################################
    # GUI: MISE A JOUR BARRE DES DATES
    ##########################################################
    def update_plot(self,value):
        if self.bool_desactive_motion:
            self.date_motion=self.date[int(value)]
            self.ax1.clear()
            self.pro.plot1D_bande(self.ax1, self.variable, date=self.date_motion, legend=self.variable)
            self.Canevas.draw()
            plt.close(self.fig1)
            self.ax2.clear()
            if self.profil_complet:
                self.ax3.clear()
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, date = self.date_motion, cbar_show = self.first_profil, bool_layer=self.bool_layer)    
            else:
                self.pro.plot_profil(self.ax2, self.variable_souris, date = self.date_motion, bool_layer = self.bool_layer)
            self.Canevas2.draw()
            self.buttonSave2.config(state = 'normal', command = self.Save_profil)
            self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
            plt.close(self.fig2)
            self.clik_zoom = False
    
    ##########################################################
    # REMISE À ZERO
    ##########################################################
    def raz(self):
        Graph.refresh(self)
        self.combobox_reduce1.config(state = 'disabled', values = '')
        self.combobox_reduce1.set('')
        self.combobox_choix_direction = ttk.Combobox(self, state = 'readonly', values = ['up', 'down'])
        self.combobox_choix_hauteur = ttk.Combobox(self, state = 'readonly', values = [x for x in range(5,600,5)])
        self.list_choix = [None, None, None, None]
        self.ChoixPossible = [True, True, True, True]

    ##########################################################
    # CHOIX VARIABLE
    ##########################################################
    def recup_direction(self, event):
        variable_for_pres = self.combobox.get()
        self.variable = self.liste_variable[self.liste_variable_for_pres.index(variable_for_pres)]
        self.var_sup.append(self.variable)
        logger.info('Variable {} selected'.format(self.variable))
        self.combobox_choix_direction.config(state = "readonly")
        self.combobox_choix_direction.bind('<<ComboboxSelected>>', self.finalisation_dirhauteur)

    def finalisation_dirhauteur(self, event):
        self.direction_coupe = self.combobox_choix_direction.get()
        self.combobox_choix_hauteur.config(state = "readonly")
        self.combobox_choix_hauteur.bind('<<ComboboxSelected>>', self.liste_profil)


    def liste_profil(self, event):
        self.hauteur_coupe = self.combobox_choix_hauteur.get()
        if self.bool_profil:
            self.pro = proReader_mini.ProReader_height(ncfile = self.filename, var = self.variable, point = int(self.point_choisi), var_sup = self.var_sup)

        Graph.liste_profil(self)

    def recup(self):
        if Graph.Ouvrir(self) is not None:
            self.Tableau=self.pro.get_choix(self.filename)
            self.combobox.config(state ='readonly', values = self.liste_variable_for_pres)
            self.combobox.bind('<<ComboboxSelected>>', self.recup_direction)

    ##########################################################
    # TEST PRESENCE-ABSENCE DE PARAMETRE
    ##########################################################
    def test_presence_champs(self):
        Graph.test_presence_champs_4_params(self)

    ##########################################################
    # TRACE
    ##########################################################
    def Plotage(self):
        self.boolzoom = False
        if not self.bool_ligne_commande:
            self.pro = proReader_mini.ProReader_height(ncfile = self.filename, var = self.variable, point = int(self.point_choisi), var_sup = self.var_sup)
        self.fig1.clear()
        self.ax1.clear()
        self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        self.Canevas.get_tk_widget().destroy()
        self.Canevas = FigureCanvasTkAgg(self.fig1,self)
        self.Canevas.get_tk_widget().place(x = 3*self.largeur, y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
        logger.info('Variable {} selected'.format(self.variable))
        self.intime = self.pro.plot(self.ax1, self.variable, self.datedeb, self.datefin, legend = self.variable,
                                           direction_cut = self.direction_coupe, height_cut = self.hauteur_coupe)
        self.bool_layer = False
        self.scale_date.config(from_=0, to = 0,state = 'disabled')
        self.bool_desactive_motion = False
        self.Canevas.draw()
        self.Canevas.mpl_connect('motion_notify_event', self.motion)
        self.Canevas.mpl_connect('button_press_event', self.on_button_press)
        self.Canevas.mpl_connect('motion_notify_event', self.on_move_press)
        self.Canevas.mpl_connect('button_release_event', self.on_button_release)
        plt.close(self.fig1)
        self.buttonRaz.config(state = 'normal', command = self.raz)
        self.buttonSave1.config(state = 'normal', command = self.Save_plot)
        #self.buttonSave3.config(state = 'normal', command = self.Pickle_plot)
        self.buttonExportData.config(state = 'normal', command = self.Export_data)
        self.figclear = False

        
################################################################################################
################################################################################################
#                                                                                              #
#                                                                                              #
#                             Massif graph                                                     #
#                                                                                              #
#                                                                                              #
################################################################################################
################################################################################################

class GraphMassif(Graph):
    def __init__(self, parent, **Arguments):
        Graph.__init__(self, parent)
        self.date_motion = None
        self.list_choix = [None, None, None]
        self.liste_points = ''

        self.label_var = Label(self,text = '2: Variable à tracer')
        self.label_choix_profil = Label(self,text = '3: Choix variable profil')
        self.label_reduce2 = Label(self,text = '4: Choix altitude')
        self.label_reduce3 = Label(self,text = '5: Choix angle de pente')
        self.label_reduce4 = Label(self,text = '6: Choix orientation')

        self.bind('<Configure>', self.onsize_test)
        
    ##########################################################
    # PLACEMENT BOUTONS, LISTES DEFILANTES, ETC...
    ##########################################################
     
    def onsize_test(self, event):
        Graph.onsize_test(self, event)
        self.label_var.place(x = 200*self.largeur, y = 5*self.hauteur)
        self.combobox.place(x = 200*self.largeur, y = 20*self.hauteur)
        self.label_choix_profil.place(x = 400*self.largeur, y = 5*self.hauteur)
        self.combobox_choix_profil.place(x = 400*self.largeur, y = 20*self.hauteur)
        self.label_reduce2.place(x = 75*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce2.place(x = 75*self.largeur, y = 60*self.hauteur)
        self.label_reduce3.place(x = 270*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce3.place(x = 270*self.largeur, y = 60*self.hauteur)
        self.label_reduce4.place(x = 470*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce4.place(x = 470*self.largeur, y = 60*self.hauteur)   

    ##########################################################
    # TRADUCTION
    ##########################################################
    def toFrench(self):
        Graph.toFrench(self)
        self.label_var.config(text = '2: Variable à tracer')
        self.label_choix_profil.config(text = '3: Choix variable profil')
        self.label_reduce2.config(text = '4: Choix altitude')
        self.label_reduce3.config(text = '5: Choix angle de pente')
        self.label_reduce4.config(text = '6: Choix orientation')

    def toEnglish(self):
        Graph.toEnglish(self)
        self.label_var.config(text = '2: Variable to plot')
        self.label_choix_profil.config(text = '3: Variable for Profile')
        self.label_reduce2.config(text = '4: Choose altitude')
        self.label_reduce3.config(text = '5: Choose slope')
        self.label_reduce4.config(text = '6: Choose orientation')

    ##########################################################
    # GESTION DU PROFIL INTERACTIF
    ##########################################################
        
    def click_for_zoom(self, event):
        if (event.button > 1):
            self.stop_right_click = not self.stop_right_click
            return
        self.ax1.clear()
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.pro.plot_massif(self.ax1, self.variable, date = self.date_motion, real_layers = True, legend = self.variable, legend_x = self.liste_massif_pour_legende, cbar_show = False, top_zoom = True)
        else:
            self.pro.plot1D_massif(self.ax1, self.variable, date = self.date_motion, legend = self.variable, legend_x = self.liste_massif_pour_legende)
        self.Canevas.draw()
        plt.close(self.fig1)
        self.clik_zoom = True
    
    def update_plot(self, value):
        self.date_motion = self.date[int(value)]
        self.ax1.clear()
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.pro.plot_massif(self.ax1, self.variable, date = self.date_motion, real_layers = True, legend = self.variable, legend_x = self.liste_massif_pour_legende, cbar_show = False)
        else:
            self.pro.plot1D_massif(self.ax1, self.variable, date = self.date_motion, legend = self.variable, legend_x = self.liste_massif_pour_legende)
        self.Canevas.draw()
        plt.close(self.fig1)
        self.clik_zoom = False
    
    def motion_massif(self, event):
        if self.stop_right_click:
            return
        if (event.inaxes == self.ax1):
            massif_souris = min(math.floor(event.xdata),len(self.liste_points)-1)
            hauteur_souris = event.ydata
            self.ax2.clear()
            if self.profil_complet:
                self.ax3.clear()
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, self.date_motion, massif_souris, hauteur_souris, cbar_show = self.first_profil, bool_layer = self.bool_layer, liste_nom = self.liste_massif_pour_legende, top = self.clik_zoom)    
            else:
                self.pro.plot_profil(self.ax2, self.variable_souris, self.date_motion, massif_souris, hauteur_souris, bool_layer = self.bool_layer, liste_nom = self.liste_massif_pour_legende, top = self.clik_zoom)
            self.Canevas2.draw()
            self.buttonSave2.config(state = 'normal', command = self.Save_profil)
            self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
            plt.close(self.fig2)
            self.first_profil = False
    
    ##########################################################
    # REMISE À ZERO
    ##########################################################
    def raz(self):
        Graph.refresh(self)
        self.list_choix = [None, None, None]
        
    ##########################################################
    # CHOIX VARIABLE
    ##########################################################
    def liste_profil(self, event):
        variable_for_pres = self.combobox.get()
        self.variable = self.liste_variable[self.liste_variable_for_pres.index(variable_for_pres)]
        self.var_sup.append(self.variable)
        logger.info('Variable {} selected'.format(self.variable))
        if self.bool_profil:
            self.pro = proReader_mini.ProReader_massif(ncfile = self.filename, var = self.variable, liste_points = self.liste_points, var_sup = self.var_sup)

        Graph.liste_profil(self)

    def recup(self):
        if Graph.Ouvrir(self) is not None:
            self.Tableau = self.pro.get_choix_ss_massif(self.filename)
            self.combobox.config(state ='readonly', values = self.liste_variable_for_pres)
            self.combobox.bind('<<ComboboxSelected>>', self.liste_profil)

    ##########################################################
    # CHOIX POINT
    ##########################################################

    def reduce2(self, event):
        self.combobox_reduce2.config(state = "readonly")
        liste = list(set(self.Tableau[0,:])) 
        self.combobox_reduce2.config(values = liste)
        self.combobox_reduce2.bind('<<ComboboxSelected>>', self.reduce3)
        if self.bool_profil:       
            self.choix_point_massif()    
        
    def reduce3(self, event):
        self.combobox_reduce3.config(state = "readonly")
        altitude = self.combobox_reduce2.get()
        self.list_choix[0] = float(altitude)
        n = len(self.Tableau[0,:])
        A = self.Tableau[0,:] == [self.list_choix[0]]*n
        indices = A
        
        liste = list(set(self.Tableau[1,indices]))
        self.combobox_reduce3.config(values = liste)
        self.combobox_reduce3.bind('<<ComboboxSelected>>', self.reduce4)
        if self.bool_profil:
            self.choix_point_massif()
        
    def reduce4(self, event):
        self.combobox_reduce4.config(state = "readonly")
        pente = self.combobox_reduce3.get()
        self.list_choix[1] = float(pente)
        n=len(self.Tableau[0,:])
        A=self.Tableau[0,:] == [self.list_choix[0]]*n
        B=self.Tableau[1,:] == [self.list_choix[1]]*n
        indices = A & B

        liste = list(set(self.Tableau[2,indices]))
        self.combobox_reduce4.config(values = liste)
        self.combobox_reduce4.bind('<<ComboboxSelected>>', self.finalisation_reduce)
        if self.bool_profil:
            self.choix_point_massif()

    def choix_point_massif(self):
        n = len(self.Tableau[0,:])
        A = self.Tableau[0,:] == [self.list_choix[0]]*n
        B = self.Tableau[1,:] == [self.list_choix[1]]*n
        C = self.Tableau[2,:] == [self.list_choix[2]]*n
        indices = A & B & C
        if True not in list(indices):
            self.buttonPlot.config(state='disabled')
        else:
            self.liste_points = [i for i, x in enumerate(indices) if x == True]
            if self.bool_profil == True:
                self.buttonPlot.config(state = 'normal', command = self.Plotage)
        
    def finalisation_reduce(self, event):
        orientation = self.combobox_reduce4.get()
        self.list_choix[2] = float(orientation)
        self.choix_point_massif()

    def test_presence_champs(self):
        if (len(self.Tableau[0])+len(self.Tableau[1])+len(self.Tableau[2]) == 3):
            self.combobox_reduce2.config(state = 'disabled', values = '')
            self.combobox_reduce3.config(state = 'disabled', values = '')
            self.combobox_reduce4.config(state = 'disabled', values = '')
            if self.Tableau[0] == [-10]:
                self.combobox_reduce2.set('inconnu')
            else:
                self.combobox_reduce2.set(self.Tableau[0][0])
            if self.Tableau[1] == [-10]:
                self.combobox_reduce3.set('inconnu')
            else:
                self.combobox_reduce3.set(self.Tableau[1][0])
            if self.Tableau[2] == [-10]:
                self.combobox_reduce4.set('inconnu')
            else:
                self.combobox_reduce4.set(self.Tableau[2][0])
            self.buttonPlot.config(state = 'normal', command = self.Plotage)
        else:
            self.reduce2(self)
        
    ##########################################################
    # TRACE
    ##########################################################
    def Plotage(self):
        self.pro = proReader_mini.ProReader_massif(ncfile = self.filename, var = self.variable, liste_points = self.liste_points, var_sup = self.var_sup)
        self.fig1.clear()
        self.ax1.clear()
        self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        self.Canevas.get_tk_widget().destroy()
        self.Canevas = FigureCanvasTkAgg(self.fig1,self)
        self.Canevas.get_tk_widget().place(x = 3*self.largeur, y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
        logger.info('Variable {} selected'.format(self.variable))
        self.liste_massif_pour_legende=[]
        if ('massif_num' in self.ff.listvar()):
            nrstationtab = self.ff.read('massif_num')[:]
            for num in self.liste_points:
                indice=self.list_massif_num.index(nrstationtab[num])
                self.liste_massif_pour_legende.append(self.list_massif_nom[indice])
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.pro.plot_massif(self.ax1, self.variable, date = None, real_layers = True, legend = self.variable, legend_x = self.liste_massif_pour_legende, cbar_show = True)
            self.bool_layer = True
        else:
            self.pro.plot1D_massif(self.ax1, self.variable, date = None, legend = self.variable, legend_x = self.liste_massif_pour_legende)
            self.bool_layer = False
        self.bool_desactive_motion = False
        self.Canevas.draw()
        self.Canevas.mpl_connect('motion_notify_event', self.motion_massif)
        self.scale_date.config(from_ = 0, to = (len(self.date)-1), state='normal', showvalue=0, command = self.update_plot, variable = IntVar)
        self.Canevas.mpl_connect('button_press_event', self.click_for_zoom)
        plt.close(self.fig1)
        self.buttonRaz.config(state = 'normal', command = self.raz)
        self.buttonSave1.config(state = 'normal', command = self.Save_plot)
        #self.buttonSave3.config(state = 'normal', command = self.Pickle_plot)
        self.figclear = False
        self.clik_zoom = False


################################################################################################
################################################################################################
#                                                                                              #
#                                                                                              #
#                             Membre graph                                                     #
#                                                                                              #
#                                                                                              #
################################################################################################
################################################################################################

class GraphMembre(Graph):
    def __init__(self,parent, **Arguments):
        Graph.__init__(self, parent)
        self.date_motion = None
        self.list_choix = [None,None,None,None]
        self.ChoixPossible = [True, True, True, True]
        self.type_fichier = ''
        self.old_date = None

        self.label_var = Label(self,text = '2: Variable à tracer')
        self.label_choix_profil = Label(self,text = '3: Choix variable profil')
        self.label_reduce1 = Label(self,text = '4: Choix massif')
        self.label_reduce2 = Label(self,text = '5: Choix altitude')
        self.label_reduce3 = Label(self,text = '6: Choix angle de pente')
        self.label_reduce4 = Label(self,text = '7: Choix orientation')

        self.combobox_reduce1 = ttk.Combobox(self, state = 'disabled', values = '')
        self.bind('<Configure>', self.onsize_test)
        
    ##########################################################
    # PLACEMENT BOUTONS, LISTES DEFILANTES, ETC...
    ##########################################################
     
    def onsize_test(self, event):
        Graph.onsize_test(self, event)
        self.label_var.place(x = 200*self.largeur, y = 5*self.hauteur)
        self.combobox.place(x = 200*self.largeur, y = 20*self.hauteur)
        self.label_choix_profil.place(x = 400*self.largeur, y = 5*self.hauteur)
        self.combobox_choix_profil.place(x = 400*self.largeur, y = 20*self.hauteur)
        self.label_reduce1.place(x = 75*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce1.place(x = 75*self.largeur, y = 60*self.hauteur)
        self.label_reduce2.place(x = 270*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce2.place(x = 270*self.largeur, y = 60*self.hauteur)
        self.label_reduce3.place(x = 470*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce3.place(x = 470*self.largeur, y = 60*self.hauteur)
        self.label_reduce4.place(x = 670*self.largeur, y = 45*self.hauteur)
        self.combobox_reduce4.place(x = 670*self.largeur, y = 60*self.hauteur)

    ##########################################################
    # TRADUCTION
    ##########################################################
    def toFrench(self):
        Graph.toFrench(self)
        self.label_var.config(text = '2: Variable à tracer')
        self.label_choix_profil.config(text = '3: Choix variable profil')
        self.label_reduce1.config(text = '4: Choix massif')
        self.label_reduce2.config(text = '5: Choix altitude')
        self.label_reduce3.config(text = '6: Choix angle de pente')
        self.label_reduce4.config(text = '7: Choix orientation')

    def toEnglish(self):
        Graph.toEnglish(self)
        self.label_var.config(text = '2: Variable to plot')
        self.label_choix_profil.config(text = '3: Variable for Profile')
        self.label_reduce1.config(text = '4: Choose massif')
        self.label_reduce2.config(text = '5: Choose altitude')
        self.label_reduce3.config(text = '6: Choose slope')
        self.label_reduce4.config(text = '7: Choose orientation')
        
    ##########################################################
    # GESTION DU PROFIL INTERACTIF
    ##########################################################
        
    def click_for_zoom(self, event):
        if (event.button > 1):
            self.stop_right_click = not self.stop_right_click
            return
        self.ax1.clear()
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.pro.plot_membre(self.ax1, self.variable, date = self.date_motion, real_layers = True, legend = self.variable, cbar_show = False, top_zoom = True)
        else:
            self.pro.plot1D_membre(self.ax1, self.variable, date = self.date_motion, legend = self.variable)
        self.Canevas.draw()
        plt.close(self.fig1)
        self.clik_zoom = True
    
    def update_plot(self,value):
        self.date_motion = self.date[int(value)]
        self.ax1.clear()
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.pro.plot_membre(self.ax1, self.variable, date = self.date_motion, real_layers = True, legend = self.variable, cbar_show = False)
        else:
            self.pro.plot1D_membre(self.ax1, self.variable, date = self.date_motion, legend = self.variable)
        self.Canevas.draw()
        plt.close(self.fig1)
        self.clik_zoom = False
    
    def motion_membre(self, event):
        if self.stop_right_click:
            return
        if self.bool_no_snowlayer:
            if self.date_motion != self.old_date:
                logger.info('Date {} selected'.format(self.date_motion))
                self.old_date = self.date_motion
            return
        if (event.inaxes == self.ax1):
            membre_souris = min(math.floor(event.xdata), self.nmembre-1)
            hauteur_souris = event.ydata
            self.ax2.clear()
            if self.profil_complet:
                self.ax3.clear()
                self.pro.plot_profil_complet(self.ax2, self.ax3, self.variable_souris, self.date_motion, membre_souris, hauteur_souris, cbar_show = self.first_profil, bool_layer = self.bool_layer, top = self.clik_zoom)    
            else:
                self.pro.plot_profil(self.ax2, self.variable_souris, self.date_motion, membre_souris, hauteur_souris, bool_layer = self.bool_layer, top = self.clik_zoom)
            self.Canevas2.draw()
            self.buttonSave2.config(state = 'normal', command = self.Save_profil)
            self.buttonSave4.config(state = 'normal', command = self.Pickle_profil)
            plt.close(self.fig2)
            self.first_profil = False
    
    ##########################################################
    # REMISE À ZERO
    ##########################################################
    def raz(self):
        Graph.refresh(self)
        self.combobox_reduce1.config(state = 'disabled', values = '')
        self.combobox_reduce1.set('')
        self.list_choix = [None, None, None, None]
        self.ChoixPossible = [True, True, True, True]
        
    ##########################################################
    # CHOIX VARIABLE
    ##########################################################
    def liste_profil(self, event):
        variable_for_pres = self.combobox.get()
        self.variable = self.liste_variable[self.liste_variable_for_pres.index(variable_for_pres)]
        self.var_sup.append(self.variable)
        self.info('Variable {} selected'.format(self.variable))
        if self.bool_profil:
            self.pro = proReader_mini.ProReader_membre(ncfile = self.filename, var = self.variable, point = int(self.point_choisi), var_sup = self.var_sup)

        Graph.liste_profil(self)

    def recup(self):
        if Graph.Ouvrir(self) is not None:
            self.Tableau = self.pro.get_choix(self.filename)            
            self.combobox.config(state = 'readonly', values = self.liste_variable_for_pres)
            self.combobox.bind('<<ComboboxSelected>>', self.liste_profil)
            
    ##########################################################
    # TEST PRESENCE-ABSENCE DE PARAMETRE
    ##########################################################
    def test_presence_champs(self):
        Graph.test_presence_champs_4_params(self)

    ##########################################################
    # TRACE
    ##########################################################
    def Plotage(self):
        self.pro = proReader_mini.ProReader_membre(ncfile=self.filename, var = self.variable, point = int(self.point_choisi), var_sup = self.var_sup)
        self.nmembre = self.pro.nb_membre
        self.fig1.clear()
        self.ax1.clear()
        self.fig1, self.ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        self.Canevas.get_tk_widget().destroy()
        self.Canevas = FigureCanvasTkAgg(self.fig1,self)
        self.Canevas.get_tk_widget().place(x = 3*self.largeur, y = 150*self.hauteur, width = 502*self.largeur, height = 500*self.hauteur)
        logger.info('Variable {} selected'.format(self.variable))
        if ('snow_layer' in self.ff.getdimvar(self.variable)):
            self.pro.plot_membre(self.ax1, self.variable, date = None, real_layers = True, legend = self.variable, cbar_show = True)
            self.bool_layer = True
        else:
            self.pro.plot1D_membre(self.ax1, self.variable, date=None, legend=self.variable)
            self.bool_layer = False
        self.bool_desactive_motion = False
        self.Canevas.draw()
        self.Canevas.mpl_connect('motion_notify_event', self.motion_membre)
        self.scale_date.config(from_ = 0, to = (len(self.date)-1), state = 'normal', showvalue = 0, command = self.update_plot, variable = IntVar)
        self.Canevas.mpl_connect('button_press_event', self.click_for_zoom)
        plt.close(self.fig1)
        self.buttonRaz.config(state = 'normal', command = self.raz)
        self.buttonSave1.config(state = 'normal', command = self.Save_plot)
        #self.buttonSave3.config(state = 'normal', command = self.Pickle_plot)
        self.figclear = False
        self.clik_zoom = False

################################################################################################
################################################################################################
#                                                                                              #
#                                                                                              #
#                   Generic tools and command line interpretation                              #
#                                                                                              #
#                                                                                              #
################################################################################################
################################################################################################

class GestionFenetre(Frame):
    'In order to choose which graph will be drawn: standard, by massif, by members'
    def __init__(self):
        self.root = Tk()
        Frame.__init__(self, self.root)
        self.master.title('GUI PROreader CEN')
        self.master.protocol("WM_DELETE_WINDOW", self.close)
        self.master.geometry('250x200+200+200')
        self.master.taille_x_master = 250
        self.master.taille_y_master = 280
        self.master.buttongraphe_standard = Button(self.master,text = 'Standard Graph', command = self.graphe1)
        self.master.buttongraphe_massif = Button(self.master,text = 'Massif Graph', command = self.graphe2)
        self.master.buttongraphe_membre = Button(self.master,text = 'Member Graph', command = self.graphe3)
        self.master.buttongraphe_hauteur = Button(self.master,text = 'Height Graph', command = self.graphe4)
        self.master.buttonquit = Button(self.master,text='Quitter', command = quit)
        self.master.bind('<Configure>', self.onsize_master)
        self.master.bind('<Escape>', self.close)
        self.master.bind('<Control-q>', self.close)
        self.master.bind('<Alt-s>', self.graphe1)
        self.master.bind('<Alt-m>', self.graphe2)
        self.master.bind('<Alt-e>', self.graphe3)
        self.master.bind('<Alt-h>', self.graphe4)

    def onsize_master(self, event):
        largeur_master = self.master.winfo_width()/self.master.taille_x_master
        hauteur_master = self.master.winfo_height()/self.master.taille_y_master
        self.master.buttongraphe_standard.place(x = 20*largeur_master, y = 15*hauteur_master)
        self.master.buttongraphe_massif.place(x = 20*largeur_master, y = 65*hauteur_master)
        self.master.buttongraphe_membre.place(x = 20*largeur_master, y = 115*hauteur_master)
        self.master.buttongraphe_hauteur.place(x = 20*largeur_master, y = 165*hauteur_master)
        self.master.buttonquit.place(x = 150*largeur_master, y = 215*hauteur_master)
        
    def graphe1(self, *args):
        self.fen1 = GraphStandard(self)
            
    def graphe2(self, *args):
        self.fen2 = GraphMassif(self)
            
    def graphe3(self, *args):
        self.fen3 = GraphMembre(self)

    def graphe4(self, *args):
        self.fen4 = GraphHeight(self)

    def close(self, *args):
        self.quit()

def parseArguments(args):
    # Create argument parser
    parser = argparse.ArgumentParser()

    # Optional arguments
    #parser.add_argument("-f", "--filename", help = "Name for input file", type=str)
    parser.add_argument('filename', nargs = '?', help = "Name for input file", type = str)

    parser.add_argument("-p", "--profil", help = "Variable for profil plot", type=str, default = 'SNOWTEMP')
    parser.add_argument("-v", "--variable", help = "Variable to plot", type=str, default = 'SNOWSSA')

    parser.add_argument("-n", "--NOGUI", help = "Option to save graph without GUI", action='store_true', default = False)
    parser.add_argument("-a", "--alt", help = "altitude", type = int)
    parser.add_argument("-as", "--aspect", help = "aspect", type = int)
    parser.add_argument("-d", "--date", help = "Date for plot (useful for massif and member plots)", type=str, default = '2001010106')
    parser.add_argument("-m", "--massif", help = "massif", type = int)
    parser.add_argument("-o", "--out", help = "name for graph to save", type=str, default = 'out.png')
    parser.add_argument("-s", "--slope", help = "slope for massif graph", type=int)
    parser.add_argument("-t", "--type", help = "type of graph (standard, massif, membre, height)", type=str, default = 'standard')

    parser.add_argument("-dir", "--direction", help = "direction for plot (up or down, useful for height plots)", type=str, default = 'up')
    parser.add_argument("--hauteur", help = "centimeters for height plots", type = int, default = 10)
    parser.add_argument("--point", help = "point number", type = int)
    parser.add_argument("--sampling", help = "Maximum time length before subsampling (-1 is never)", type = int)

    # Print version
    parser.add_argument("--version", action="version", version='%(prog)s - Version 1.0')
    # Logging options
    parser.add_argument("--debug", action="store_true", help="Set logging to debug.")
    # Parse arguments
    args = parser.parse_args(args)

    return args

def ChoixPointMassif(ff, altitude, aspect, massif, slope, exitonerror=False):
    listvariables = ff.listvar()
    choice = []
    if massif is not None:
        if('massif_num' in listvariables):
            massiftab = ff.read('massif_num')[:]
            choice.append(massiftab==massif)
    if altitude is not None:
        if('ZS' in listvariables):
            alttab = ff.read('ZS')[:]
            choice.append(alttab==altitude)
    if slope is not None :
        if('slope' in listvariables):
            slopetab = ff.read('slope')[:]
            choice.append(slopetab==slope)
    if aspect is not None:
        if('aspect' in listvariables):
            aspecttab = ff.read('aspect')[:]
            choice.append(aspecttab==aspect)
    if len(choice)==0:
        logger.warning('No data to select point. Choose 0 !')
        return [0]
    allcriteria = choice[0]
    for i in range(1,len(choice)):
        allcriteria = np.logical_and(allcriteria, choice[i])
    indices = np.where(allcriteria)[0]
    if len(indices) == 0:
        logger.error('Aucun choix de point correspondant -> PB. Use point 0')
        if exitonerror:
            sys.exit(2)
        return [0]
    else:
        return indices


def Savefig(filename, profil, variable, date_massif_membre, out_name, type_graph, altitude, aspect, massif, slope, direction_coupe, hauteur_coupe):
    ff = prosimu(filename)
    liste_points = ChoixPointMassif(ff, altitude, aspect, massif, slope)

    if type_graph == 'standard':
        date = ff.readtime()
        datedeb = date[0]
        datefin = date[len(date)-1]
        if len(date) > constante_sampling: 
            logger.warn('Time > {}: automatic sampling to avoid too long treatment'.format(constante_sampling))
        pro = proReader_mini.ProReader_standard(ncfile = filename, var = variable, point = int(liste_points[0]), var_sup = [])
        fig1, ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        if ('snow_layer' in ff.getdimvar(variable)):
            intime = pro.plot(ax1, variable, datedeb, datefin, real_layers = True,legend = variable)
        elif('bands' in ff.getdimvar(variable)):
            pro.plot1D_bande(ax1, variable, datedeb, legend = variable)
        else:
            intime = pro.plot1D(ax1, variable, datedeb, datefin, legend = variable)

    if type_graph == 'height':
        date = ff.readtime()
        datedeb = date[0]
        datefin = date[len(date)-1]
        if len(date) > constante_sampling: 
            logger.warn('Time > {}: automatic sampling to avoid too long treatment'.format(constante_sampling))
        pro = proReader_mini.ProReader_height(ncfile = filename, var = variable, point = int(liste_points[0]), var_sup = [])
        fig1, ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        intime = pro.plot(ax1, variable, datedeb, datefin, legend = variable, direction_cut = direction_coupe, height_cut = hauteur_coupe)

    if type_graph == 'massif':
        list_massif_num = []
        list_massif_nom = []
        IM=infomassifs()
        listmassif = IM.getListMassif_of_region('all')
        for massif in listmassif:
            list_massif_num.append(massif)
            list_massif_nom.append(str(IM.getMassifName(massif).decode('UTF-8')))
        pro = proReader_mini.ProReader_massif(ncfile = filename, var = variable, liste_points = liste_points, var_sup = [])
        fig1, ax1 = plt.subplots(1, 1, sharex = True, sharey = True)

        liste_massif_pour_legende=[]
        if ('massif_num' in ff.listvar()):
            nrstationtab = ff.read('massif_num')[:]
            for num in liste_points:
                indice = list_massif_num.index(nrstationtab[num])
                liste_massif_pour_legende.append(list_massif_nom[indice])
        if ('snow_layer' in ff.getdimvar(variable)):
            pro.plot_massif(ax1, variable, date = date_massif_membre, real_layers = True, legend = variable, legend_x = liste_massif_pour_legende, cbar_show = True)
        else:
            pro.plot1D_massif(ax1, variable, date = date_massif_membre, legend = variable, legend_x = liste_massif_pour_legende)

    if type_graph == 'membre':
        pro = proReader_mini.ProReader_membre(ncfile = filename, var = variable, point = int(liste_points[0]), var_sup = [])
        fig1, ax1 = plt.subplots(1, 1, sharex = True, sharey = True)
        if ('snow_layer' in ff.getdimvar(variable)):
            pro.plot_membre(ax1, variable, date = date_massif_membre, real_layers = True, legend = variable, cbar_show = True)
        else:
            pro.plot1D_membre(ax1, variable, date = date_massif_membre, legend = variable)

    plt.savefig(out_name)
    plt.close(fig1)

def main(version='undefined', args=None):
    args = args if args is not None else sys.argv[1:]
    if len(sys.argv) > 1: 
        args = parseArguments(args)

        # argument for command line call
        filename = args.filename
        profil = args.profil
        variable = args.variable

        # argument for saving graph without GUI
        NOGUI = args.NOGUI
        altitude = args.alt
        aspect = args.aspect
        date_massif_membre = check_and_convert_date(str(args.date))
        massif = args.massif
        out_name = args.out
        slope = args.slope
        type_graph = args.type
        direction_coupe = args.direction
        hauteur_coupe = args.hauteur
        point = args.point

        # Logging options
        if args.debug:
            logger.setLevel(logging.DEBUG)
        
        logger.info('GUI_Proreader version {}'.format(version))

        # Sampling options
        if args.sampling:
            constante_sampling = args.sampling if args.sampling > 0 else float("inf")
            proReader_mini.constante_sampling = constante_sampling

        # Is there a filename provided. In case yes, is it a correct filename
        if filename is None:
            GestionFenetre().mainloop()
            sys.exit(0)
        else:
            if not os.path.isfile(filename):
                logger.critical('Provided filename does not exist ({})'.format(filename))
                sys.exit(1)

        # Get point of interest
        if (altitude is not None or aspect is not None or slope is not None or massif is not None) and point is None:
            with prosimu(filename) as ff:
                point = ChoixPointMassif(ff, altitude, aspect, massif, slope, exitonerror=True)[0]
        if point is None:
            point = 0

        # check variables exist
        if variable is not None or profil is not None:
            with prosimu(filename) as ff:
                if variable is not None and variable not in ff.listvar():
                    logger.critical('Variable {} does not exist in {}'.format(variable, filename))
                    sys.exit(3)
                if profil is not None and profil not in ff.listvar():
                    logger.critical('Variable {} does not exist in {}'.format(profil, filename))
                    sys.exit(3)

        # Launch the app or save the figure
        if NOGUI:
            Savefig(filename, profil, variable, date_massif_membre, out_name, type_graph, altitude, aspect, massif, slope, direction_coupe, hauteur_coupe)
        else:
            dic_option = {'filename': filename, 'variable': variable, 'profil': profil, 'point': point}
            mainw = GestionFenetre()
            if type_graph == 'height':
                fenetre = GraphHeight(mainw.root, **dic_option)
            else:
                fenetre = GraphStandard(mainw.root, **dic_option)
            mainw.mainloop()

    else: 
        # Lancement du gestionnaire d'événements
        GestionFenetre().mainloop()

if __name__ == '__main__':
    main(version='git_master')
