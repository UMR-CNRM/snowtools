# -*- coding: utf-8 -*-

"""
GUI Application for proplotter

The GUI is splitted in different parts :

* The Application which is the overall Frame
* The choice bar (left side) to select variables, divided in:
    * Choice of variables (ProPlotterChoicesBarVariables), only linked to the file
    * Point selection (ProPlotterChoicesBarPoint), only linked to the file
    * Parameters specific to the graph plotted (ProPlotterChoicesBarParams), only linked to the graph type selected
* The open bar that give allow to open a file.
* The Plot bar that handle the buttons and give information on what is plotted.
* The ProPlotterController, specific to the graph, that manage the plot function
* The Status bar at bottom
"""

import abc
import logging
import textwrap
import os.path

import tkinter as tk
import tkinter.filedialog
from tkinter import ttk
from tkinter import messagebox

import numpy as np
# import pdb

from snowtools.plots.stratiprofile import proreader
from snowtools.plots.stratiprofile import profilPlot

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk

logger = logging.getLogger()

# App constants:
FILETYPES = [('PRO files', '.nc'), ('all files', '.*')]


class ProPlotterApplication(tk.Frame):
    """
    The base frame for the GUI application

    :param master: The Tk root (tk.Tk())
    :param fileobj: file proreader object if already open
    :param point: Select point, if any
    :param arguments: Other arguments provided as a dict.
                      Accepted keys are: type, begin, end, max_samples,
                      date, variable, variable_profil

    Maintain links to the different parts of the interface:
     * menu: The Menu (File, graph type, change language)
     * openbar: The Tk Frame containing the Open button and the information on currently opened file
     * choices: The Tk lateral frame for selecting:
                 * Parameters specific to representation
                 * Variable selection (specific to opened file)
                 * Point selection (specific to opened file)
                 * Additional parameters (specific to opened file)
     * controls: The Tk Frame with "reset" and "plot" button. Also have a text field for displaying information
                 on what is currently plotted
     * main: The Tk Frame in which are plotted the graphs
     * status: The Tk Frame correspondig to the statusbar at bottom
     * controller : The class which controls interaction between previously defined elements. For each action triggered
                    in one element, it should call a function of the same element (as elements may be modified, it is
                    safer not to link directly to an action of an other one as it may have changed since callback
                    definition) which will call a controller action which will act on desired graphical elements.
    * fileobj:

    Note that only two elements are adapted to graph type: the first part of the choices Frame
    (``choices.params_w``) and the controller.

    .. warning::
        * Actions when clicking buttons or similar should only call functions in the same class
        * Class actions should only call functions in main Application class or fileobj or controller
        * controller can act on every element.
    """
    def __init__(self, master=None, fileobj: proreader.reader = None, point=None, arguments: dict = None):
        super().__init__(master)
        self.master = master
        self.fileobj = fileobj
        self.point = point
        self.pack(fill=tk.BOTH, expand=True)

        self.type = arguments['type'] if arguments is not None and 'type' in arguments else 'standard'

        # Controls
        self.controller = ProPlotterController(self)

        # Graphical elements
        self.menu = ProPlotterMenu(self)
        self.openbar = ProPlotterOpenBar(self)
        self.choices = ProPlotterChoicesBar(self)
        self.controls = ProPlotterControlsBar(self)
        self.main = ProPlotterMain(self)
        self.status = ProPlotterStatus(self)

        # Set the ProPlotterChoicesBar graphical element adapted to graph type
        # Set the ProPlotterController adapted to graph type
        self.to_graph(self.type)
        # Update to take into account an eventual open file
        self.open_update()

        # Keyboard shortcuts
        self.master.bind('<Control-o>', self.open)
        self.master.bind('<Escape>', self.close_window)
        self.master.bind('<Control-q>', self.close_window)

        # Parse additional arguments
        if 'variable' in arguments:
            set_v1 = self.choices.variables_w.set_var_master(arguments['variable'])
        else:
            set_v1 = False
        if 'variable_profil' in arguments:
            set_v2 = self.choices.variables_w.set_var_react(arguments['variable_profil'])
        else:
            set_v2 = False
        if point is not None:
            set_v3 = self.choices.point_w.set_point(point)
        else:
            set_v3 = False

        if set_v1 and set_v2 and set_v3:
            self.plot()

        self.update_idletasks()

    def close_window(self, *args, **kwargs):
        """
        Just to close the application
        """
        self.quit()

    def to_graph(self, typ=None):
        """
        Launch the Controller adapted to the graph type
        :param typ: a string which qualify the graph type
        """
        if typ is None:
            typ = self.type

        if typ == 'standard':
            self.to_graph_standard()
        elif typ == 'multiple profil':
            self.to_graph_multiple_profil()
        elif typ == 'multiple saison':
            self.to_graph_multiple_saison()
        elif typ == 'height':
            self.to_graph_height()
        elif typ == 'member profil':
            self.to_graph_member_profil()
        elif typ == 'member saison':
            self.to_graph_member_saison()
        elif typ == 'compare':
            self.to_graph_compare()
        else:
            raise ValueError('Unknown graph type')
        self.type = typ

    def to_graph_reset(self):
        """
        Graph reset
        """
        self.main.clear()
        # Variable selection reset
        if self.choices.params_w is not None:
            self.choices.params_w.clean_frame()

    def to_graph_standard(self):
        """
        Reset then launch Controller and Choices Bar Parameter for standard graph
        """
        self.to_graph_reset()
        self.controller = ProPlotterControllerStandard(self)
        self.choices.params_w = ProPlotterChoicesBarParamsStandard(self, self.choices.params)

    def to_graph_multiple_profil(self):
        """
        Reset then launch Controller and Choices Bar Parameter for multiple graph with profil on the right
        """
        self.to_graph_reset()
        self.controller = ProPlotterControllerMultipleProfil(self)
        self.choices.params_w = ProPlotterChoicesBarParamsMultiple(self, self.choices.params)

    def to_graph_multiple_saison(self):
        """
        Reset then launch Controller and Choices Bar Parameter for multiple graph with seasonal graph on the right
        """
        self.to_graph_reset()
        self.controller = ProPlotterControllerMultipleSaison(self)
        self.choices.params_w = ProPlotterChoicesBarParamsMultiple(self, self.choices.params)

    def to_graph_height(self):
        """
        Reset then launch Controller and Choices Bar Parameter for height graph
        """
        self.to_graph_reset()
        self.choices.params_w = ProPlotterChoicesBarParamsHeight(self, self.choices.params)
        self.controller = ProPlotterControllerHeight(self)

    def to_graph_member_profil(self):
        """
        Reset then launch Controller and Choices Bar Parameter for member graph with profil on the right
        """
        self.to_graph_reset()
        self.controller = ProPlotterControllerMemberProfil(self)
        self.choices.params_w = ProPlotterChoicesBarParamsMember(self, self.choices.params)

    def to_graph_member_saison(self):
        """
        Reset then launch Controller and Choices Bar Parameter for member graph with seasonal graph on the right
        """
        self.to_graph_reset()
        self.controller = ProPlotterControllerMemberSaison(self)
        self.choices.params_w = ProPlotterChoicesBarParamsMember(self, self.choices.params)

    def to_graph_compare(self):
        """
        Reset then launch Controller and Choices Bar Parameter for comparison of two graphs
        """
        self.to_graph_reset()
        self.controller = ProPlotterControllerCompare(self)
        self.choices.params_w = ProPlotterChoicesBarParamsCompare(self, self.choices.params)

    def open(self, *args):
        """
        Opening a file
        """
        self.status.set_status('Open file...')
        selectedfilename = tkinter.filedialog.askopenfilename(title='Open filename', filetypes=FILETYPES)
        if len(selectedfilename) == 0:
            logger.info('No file selected. Ignore.')
            self.status.set_status('Open file... No file selected.')
            return None
        self.status.set_status('Opening file {}'.format(selectedfilename))
        self.fileobj = proreader.read_file(selectedfilename)
        if self.fileobj is not None:
            self.status.set_status('Successfully opened file {}'.format(selectedfilename))
            self.open_update()

    def open_update(self):
        """
        Do the different update after successfully open a new file
        (i.e. once self.fileobj correctly set).
        """
        if self.fileobj is None:
            return
        self.openbar.update_filename(self.fileobj.get_filename())
        self.choices.variables_w.update()
        self.choices.point_w.update()
        self.choices.addparams.update()
        self.choices.params_w.update()

    def open_user_guide(self, *args):
        """
        Opening the help window with text inside
        """
        messagebox.showinfo('User Guide', 'This software is a visualisation tool for PRO.nc files coming from our snow '
                                          'model CROCUS embedded in the SURFEX model. \n \n'
                                          'There are several types of graphs:\n'
                                          '- usual graph: click the Open File button. Select a PRO file. Choose the '
                                          'variables you want and then the point. \n \n '
                                          '- the height graph is made to follow a parameter in a specified place. For '
                                          'example, 5 cm below the snow surface. \n \n'
                                          '- in the Meteo France system, there is the possibility to have several '
                                          'members for a simulation. In that case, choose member plot. \n \n'
                                          '- if you want to compare several points in your simulation, try the multiple'
                                          ' plot. Just have let free a part of the choices for point selection. \n \n'
                                          '- finally, if you want to compare two PRO files (in the same configuration),'
                                          ' or if you want to plot together two parameters of the same file, please use'
                                          ' the compare plots. \n'
                                          '\n'
                                          'Feel free to contact us at crocus@meteo.fr if you have some questions.\n'
                                          '\n'
                                          'More informations are available on \n'
                                          'https://opensource.umr-cnrm.fr/projects/snowtools_git/wiki'

                            )

    def open_credits(self, *args):
        """
        Opening the help window with text inside
        """
        messagebox.showinfo('CREDITS', 'This software was made by our local geek, the Legendary Visualisation Ghost, '
                                       'or LVG.\n'
                                       '\n'
                                       'Nobody knows who is he, but we know that he inspired some characters in the '
                                       'french serie "Le Bureau des Légendes".\n'
                                       '\n'
                                       'Most of the code was written by Mathieu Fructus.'
                                       '')

    def plot(self):
        self.controls.plot()


class ProPlotterMenu(tk.Menu):
    """ The app menu """
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        # Menu 0
        self.filemenu = tk.Menu(self, tearoff=0)
        self.filemenu.add_command(label='Open', command=self.master.open)
        self.filemenu.add_command(label='Quit', command=self.master.close_window)
        self.add_cascade(label='File', menu=self.filemenu)
        # Menu 1
        self.typemenu = tk.Menu(self, tearoff=0)
        self.typemenu.add_command(label='Standard', command=self.master.to_graph_standard)
        self.typemenu.add_command(label='Multiple Profil', command=self.master.to_graph_multiple_profil)
        self.typemenu.add_command(label='Multiple Saison', command=self.master.to_graph_multiple_saison)
        self.typemenu.add_command(label='Member Profil', command=self.master.to_graph_member_profil)
        self.typemenu.add_command(label='Member Saison', command=self.master.to_graph_member_saison)
        self.typemenu.add_command(label='Height', command=self.master.to_graph_height)
        self.typemenu.add_command(label='Compare', command=self.master.to_graph_compare)
        self.add_cascade(label='Graph type', menu=self.typemenu)
        # Menu 2
        self.infomenu = tk.Menu(self, tearoff=0)
        self.infomenu.add_command(label='User Guide', command=self.master.open_user_guide)
        self.infomenu.add_command(label='Credits', command=self.master.open_credits)
        self.add_cascade(label='Help', menu=self.infomenu)
        # Config
        self.master.master.config(menu=self)


class ProPlotterOpenBar(tk.Frame):
    """The Frame for open button and file path"""

    def __init__(self, master):
        """Initialize open bar """
        self.master = master
        super().__init__(master, height=100)
        self.pack(fill=tk.X)

        self.openbutton = tk.Button(self, text="Open File", command=self.open)
        self.openbutton.pack(side=tk.LEFT, padx=5)

        filename = self.master.fileobj.get_filename() if self.master.fileobj is not None else '--'
        self.filename = tk.Label(self, text=filename)
        self.filename.pack(side=tk.LEFT)

    def open(self):
        """
        If the Open button is clicked, launch the Open application
        """
        self.master.open()

    def update_filename(self, filename):
        """Get the name of opened file"""
        self.filename.configure(text=filename)


class ProPlotterChoicesBar(tk.Frame):
    """
    The lateral bar for choosing:
     * Parameters specific to representation
     * Variable selection
     * Point selection
    """

    WIDTH = 40
    WIDTH_TXT = WIDTH

    def __init__(self, master):
        """Initialization of left bar for selecting parameters specific to representation,
        variables and point of interest for plotting"""
        super().__init__(master, borderwidth=1, relief=tk.RAISED)
        self.master = master
        self.pack(side=tk.LEFT, fill=tk.Y, padx=3, pady=3)
        self.canvas = tk.Canvas(self)
        self.scroll = tk.Scrollbar(self, command=self.canvas.yview)
        self.scroll.pack(side=tk.RIGHT, fill=tk.BOTH)
        self.canvas.pack(fill=tk.BOTH)

        self.bind("<Configure>",
                  lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"), yscrollcommand=self.scroll.set))

        self.variables = tk.Frame(self.canvas)
        self.variables.pack(fill=tk.X)
        self.point = tk.Frame(self.canvas)
        self.point.pack(fill=tk.X)
        self.addparams = tk.Frame(self.canvas)
        self.addparams.pack(fill=tk.X)
        self.params = tk.Frame(self.canvas)
        self.params.pack(fill=tk.X)

        self.variables_w = ProPlotterChoicesBarVariables(self, self.variables)
        self.point_w = ProPlotterChoicesBarPoint(self, self.point)
        self.addparams_w = ProPlotterchoicesBarAdditionalParams(self, self.addparams)
        self.params_w = None


class ProPlotterChoicesBarVariables:
    """
    Choice of variables in the opened file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self._var_master = None
        self._var_react = None
        self.label = None
        self.label1 = None
        self.label2 = None
        self.variables_info = None
        self.choice_var_master = None
        self.choice_var_react = None
        self.exists_snl = True
        self.update()

    def update(self):
        """Clean and fill the Combobox with choices for variables"""
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of variables', relief=tk.RAISED)
        self.label.pack()

        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_desc
            variables_list = [v['full_name'] if 'full_name' in v else k for k, v in self.variables_info.items()]
            list_bool = [v['has_snl'] if 'full_name' in v else k for k, v in self.variables_info.items()]
            variables_with_snl = [variables_list[i] for i in range(len(variables_list)) if list_bool[i]]
            if len(variables_with_snl) == 0:
                self.exists_snl = False

            self.label1 = tk.Label(self.frame, text='Variable:')
            self.label1.pack()
            self.choice_var_master = ttk.Combobox(self.frame, state='readonly', values=variables_list,
                                                  width=self.master.WIDTH, )
            self.choice_var_master.bind('<<ComboboxSelected>>', self.update_var_master)
            self.choice_var_master.pack()
            self.label2 = tk.Label(self.frame, text='Variable profil:')
            self.label2.pack()
            self.choice_var_react = ttk.Combobox(self.frame, state='readonly', values=variables_with_snl,
                                                 width=self.master.WIDTH)
            self.choice_var_react.bind('<<ComboboxSelected>>', self.update_var_react)
            self.choice_var_react.pack()

    def set_var_master(self, var):
        if var in self.choice_var_master['values']:
            i = self.choice_var_master['values'].index(var)
            self.choice_var_master.current(i)
            self.update_var_master()
            return True
        return False

    def update_var_master(self, *args):
        """Update the value for the master graph, is the left graph"""
        value = self.choice_var_master.get()
        if value != self._var_master:
            self._var_master = value
            self.master.master.controls.plot_mark()

    def set_var_react(self, var):
        if var in self.choice_var_react['values']:
            i = self.choice_var_react['values'].index(var)
            self.choice_var_react.current(i)
            self.update_var_react()
            return True
        return False

    def update_var_react(self, *args):
        """Update the value for the servant graph, is the graph on the right"""
        value = self.choice_var_react.get()
        if value != self._var_react:
            self._var_react = value
            self.master.master.controls.plot_mark()

    @property
    def var_master(self):
        return self._var_master

    @property
    def var_react(self):
        return self._var_react

    def clean_frame(self):
        """Clean"""
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterChoicesBarPoint:
    """
    Choice of points in the opened file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self.label = None
        self.variables_info = None
        # self.lf = []
        self.update()

    def update(self):
        """Clean and fill the Combobox with choices for the point"""
        self.lselectors = []
        self.llabels = []
        self.lvariables = []
        self.clean_frame()

        self.label = tk.Label(self.frame, text='Choice of point selectors\n(fill from top to bottom)', relief=tk.RAISED)
        self.label.pack(pady=5)

        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_selection_point
            for v, info in self.variables_info.items():
                label = tk.Label(self.frame, text=textwrap.fill(info['full_name'], width=self.master.WIDTH_TXT))
                label.pack()
                ii = len(self.llabels)
                if info['type'] == 'choices':
                    choices = list(info['choices'])  # Tkinter knows nothing of numpy arrays...
                    selector = ttk.Combobox(self.frame, state='readonly', values=[''] + choices,
                                            width=self.master.WIDTH)
                    selector.bind('<<ComboboxSelected>>', lambda _, i=ii: self.update_var(i))
                elif info['type'] in ['int', 'float']:
                    if 'choices' in info:
                        choices = list(info['choices'])  # Tkinter knows nothing of numpy arrays...
                    else:
                        choices = list(range(info['limits'][0], info['limits'][1] + 1))
                    sv = tk.StringVar()
                    selector = ttk.Spinbox(self.frame, values=choices, textvariable=sv,
                                           width=self.master.WIDTH,
                                           command=lambda *_, i=ii: self.update_var_numeric(i))
                else:
                    selector = ttk.Combobox(self.frame, state='readonly', values=[''],
                                            width=self.master.WIDTH)
                selector.pack()
                self.llabels.append(label)
                self.lselectors.append(selector)
                self.lvariables.append(v)

    def set_point(self, point):
        """Set point number"""
        if 'point' in self.lvariables:
            i = self.lvariables.index('point')
            selector = self.lselectors[i]
            if str(point) in selector['values']:
                selector.set(point)
                return True
        return False

    def get_selector(self):
        """Give the good type for the selected field"""
        selector = {}
        for j in range(len(self.llabels)):
            v = self.lvariables[j]
            val = self.lselectors[j].get()
            if val == '':
                continue
            if val is not None and len(val) > 0:
                if 'type' in self.variables_info[v]:
                    if self.variables_info[v]['type'] == 'int':
                        val = int(val)
                    elif self.variables_info[v]['type'] == 'float':
                        val = float(val)
                    else:
                        val = float(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.inexact):
                    val = float(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.integer):
                    val = int(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.bool_):
                    val = int(val)
                selector[v] = val
        return selector

    def update_var(self, i):
        """
        run at each modification of a widget:

        * Set 1st acceptable value for previous widgets if they have not
          already an acceptable value
        * Reduce the choices for following widgets
        """
        self.master.master.controls.plot_mark()
        selector = {}
        for j in range(i + 1):
            v = self.lvariables[j]
            val = self.lselectors[j].get()
            if val == '':
                continue
            if val is not None and len(val) > 0:
                if np.issubdtype(self.variables_info[v]['choices'].dtype, np.inexact):
                    val = float(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.integer):
                    val = int(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.bool_):
                    val = int(val)
                selector[v] = val
        remaining_options = self.master.master.fileobj.variables_choices(selector=selector)

        # Previous values:  set 1st acceptable value if not already an acceptable value
        for j in range(0, i):
            v = self.lvariables[j]
            ro = list(remaining_options[v])
            if v in selector and str(selector[v]) in ro:
                continue  # Already an acceptable value
            if len(ro) > 0 and v != 'point':
                self.lselectors[j].set(ro[0])

        # For the modified selector

        # For the following ones, modify the options available
        for j in range(i + 1, len(self.llabels)):
            v = self.lvariables[j]
            ro = list(remaining_options[v])
            null_val = [''] if self.variables_info[v]['type'] == 'choices' else []
            self.lselectors[j].configure(values=null_val + ro)

    def update_var_numeric(self, i):
        # TODO: Check value or do it in previous function  <13-09-21, Léo Viallon-Galinier> #
        # We have to put the nearest value (or maybe not)
        self.update_var(i)

    def clean_frame(self):
        """Clean"""
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterchoicesBarAdditionalParams(tk.Frame):
    """
    Choice of additional information to select data in the file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self.label = None
        self.variables_info = None
        # self.lf = []
        self.update()

    def update(self):
        """Clean and fill the Combobox with choices for the point"""
        self.lselectors = []
        self.llabels = []
        self.lvariables = []
        self.clean_frame()

        self.label = tk.Label(self.frame, text='Additional choices', relief=tk.RAISED)
        self.label.pack(pady=5)

        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.additional_choices()
            for v, info in self.variables_info.items():
                label = tk.Label(self.frame, text=textwrap.fill(info['name'], width=self.master.WIDTH_TXT))
                label.pack()
                ii = len(self.llabels)
                if info['type'] == 'choices':
                    choices = list(info['choices'])  # Tkinter knows nothing of numpy arrays...
                    selector = ttk.Combobox(self.frame, state='readonly', values=[''] + choices,
                                            width=self.master.WIDTH)
                    selector.bind('<<ComboboxSelected>>', lambda _, i=ii: self.update_var(i))
                elif info['type'] in ['int', 'float']:
                    choices = list(range(info['limits'][0], info['limits'][1] + 1))
                    sv = tk.StringVar()
                    selector = ttk.Spinbox(self.frame, values=choices, textvariable=sv,
                                           width=self.master.WIDTH,
                                           command=lambda *_, i=ii: self.update_var(i))
                else:
                    selector = ttk.Combobox(self.frame, state='readonly', values=[''],
                                            width=self.master.WIDTH)
                selector.pack()
                self.llabels.append(label)
                self.lselectors.append(selector)
                self.lvariables.append(v)

    def get_selector(self):
        """Give the good type for the selected field"""
        selector = {}
        for j in range(len(self.llabels)):
            v = self.lvariables[j]
            val = self.lselectors[j].get()
            if val == '' or val is None:
                if 'default_value' in self.variables_info[v]:
                    selector[v] = self.variables_info[v]['default_value']
            if val is not None and len(val) > 0:
                if 'type' in self.variables_info[v]:
                    if self.variables_info[v]['type'] == 'int':
                        val = int(val)
                    elif self.variables_info[v]['type'] == 'float':
                        val = float(val)
                    else:
                        val = float(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.inexact):
                    val = float(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.integer):
                    val = int(val)
                elif np.issubdtype(self.variables_info[v]['choices'].dtype, np.bool_):
                    val = int(val)
                selector[v] = val
        return selector

    def update_var(self, i):
        """
        run at each modification of a widget:
        """
        self.master.master.controls.plot_mark()

    def clean_frame(self):
        """Clean"""
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterControlsBar(tk.Frame):
    """The Frame for control buttons and information"""

    def __init__(self, master):
        """Initialize controls bar """
        self.master = master
        super().__init__(master, relief=tk.RAISED, borderwidth=1)
        self.pack(fill=tk.X)

        self.resetbutton = tk.Button(self, text="Reset", command=self.reset)
        self.resetbutton.pack(side=tk.LEFT, padx=5)

        self.plotbutton = tk.Button(self, text="Plot", command=self.plot)
        self.plotbutton.pack(side=tk.LEFT, padx=5)

        text = 'Nothing plotted yet'
        self.infos = tk.Label(self, text=text, anchor='w')
        self.infos.pack(side=tk.LEFT, fill='both')

    def plot(self):
        """
        The plot button action -> Call the controller equivalent method
        """
        self.master.controller.plot()
        self.plotbutton.configure(fg='black')

    def reset(self):
        """
        The reset button action -> Call the controller equivalent method
        """
        self.master.controller.reset()

    def plot_mark(self):
        """
        To mark that a modification have been done somewhere that necessitate a new plot
        """
        self.plotbutton.configure(fg='red')

    def update_text(self, text):
        """
        To update the text of this bar
        """
        self.infos.configure(text=text)


class ProPlotterMain(tk.Frame):
    """The Frame for plotting"""

    def __init__(self, master):
        """Initialize plotting area """
        self.master = master
        super().__init__(master, relief=tk.RAISED, borderwidth=1)
        self.pack(fill=tk.BOTH, expand=True)

        self.fig1 = plt.figure()
        self.ax1 = None
        self.ax2 = None
        self.ax3 = None
        self.cid = None
        self.first_profil = True
        self.first_master = True
        self.first_graph = True
        self.Canevas = FigureCanvasTkAgg(self.fig1, self)
        self.toolbar = NavigationToolbar2Tk(self.Canevas, self)
        self.toolbar.update()

        self.Canevas.get_tk_widget().pack(fill=tk.BOTH, expand=True)
        self.update()

        self.update_idletasks()

    def clear(self):
        """Clean main frame (figure)"""
        for e in self.fig1.axes:
            self.fig1.delaxes(e.axes)
        if self.cid is not None:
            self.Canevas.mpl_disconnect(self.cid)
        self.ax1 = None
        self.ax2 = None
        self.ax3 = None
        self.cid = None
        self.first_profil = True
        self.first_master = True
        self.first_graph = True
        self.update()

    def ready_to_plot(self, same_y, nb_graph, rat1=2, rat2=1):
        """Prepare the main frame for figure"""
        self.clear()
        if nb_graph == 1:
            self.ax1 = self.fig1.add_subplot(1, 1, 1)
        elif nb_graph == 2:
            self.first_profil = True
            self.ax1 = self.fig1.add_subplot(1, rat1 + rat2, (1, rat1))
            if same_y:
                self.ax2 = self.fig1.add_subplot(1, rat1 + rat2, (rat1 + 1, rat1 + rat2), sharey=self.ax1)
            else:
                self.ax2 = self.fig1.add_subplot(1, rat1 + rat2, (rat1 + 1, rat1 + rat2))
            self.fig1.subplots_adjust(left=0.1, bottom=0.08, wspace=0.1)
            self.ax3 = self.ax2.twiny()

    def update(self):
        """
        Update the figure after a change in plot.

        NB: If you do not call this function, the changes on the graph are not shown.

        :return: None
        """
        self.Canevas.draw()


class ProPlotterStatus(tk.Frame):
    """The Frame for plotting"""

    def __init__(self, master):
        """Initialize plotting area """
        self.master = master
        super().__init__(master)
        self.pack(fill=tk.X)

        self.status = tk.Label(self, text='Status bar')
        self.status.pack(side=tk.LEFT)

    def set_status(self, status):
        """
        Update status printed on status bar

        :param status: The status to be printed
        :type status: str
        """
        self.status.configure(text=status)


class ProPlotterChoicesBarParams(abc.ABC):
    """Abstract Class for the Choices for the Parameters. The standard case is described"""
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame

        self.label = tk.Label(self.frame, text='Graph specific options', relief=tk.RAISED)
        self.label.pack(pady=5)

    def clean_frame(self):
        if self.frame is not None:
            for widgets in self.frame.winfo_children():
                widgets.destroy()


class ProPlotterChoicesBarParamsStandard(ProPlotterChoicesBarParams):
    """Standard case, nothing to add"""
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self.update()

    def update(self):
        pass


class ProPlotterChoicesBarParamsHeight(ProPlotterChoicesBarParams):
    """
     Specific choices for direction and height. Add the specific combobox and variables
     """
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self._direction = None
        self._height = 8.3
        self.label = None
        self.label1 = None
        self.label2 = None
        self.choice_direction = None
        self.choice_height = None
        self.update()

    def update(self):
        """Specific choices for variables in Height Graph case"""
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of direction and height', relief=tk.RAISED)
        self.label.pack()
        if self.master.fileobj is not None:
            direction_list = ['from ground to top of snow layer', 'from top of snow layer to ground']
            self.label1 = tk.Label(self.frame, text='Direction:')
            self.label1.pack()
            self.choice_direction = ttk.Combobox(self.frame, state='readonly', values=direction_list,
                                                 width=self.master.choices.WIDTH, )
            self.choice_direction.bind('<<ComboboxSelected>>', self.update_direction)
            self.choice_direction.pack()

            self.label2 = tk.Label(self.frame, text='Height in cm (default = 8.3 cm):')
            self.label2.pack()
            height = tk.DoubleVar()
            self.choice_height = ttk.Entry(self.frame, textvariable=height, width=self.master.choices.WIDTH)
            self.choice_height.pack()

    def update_direction(self, *args):
        """Update the value of the direction choice (from ground to top of snow layer or the contrary)"""
        value = self.choice_direction.get()
        if value == 'from ground to top of snow layer':
            self._direction = 'up'
        elif value == 'from top of snow layer to ground':
            self._direction = 'down'

    def update_height(self, *args):
        """Update the value of the cut in the snow pack (8.3cm) from the direction"""
        if self.choice_height is None:
            value = 8.3
        else:
            value = self.choice_height.get()
        if value is not None:
            self._height = float(value)

    @property
    def var_direction(self):
        return self._direction

    @property
    def var_height(self):
        return self._height


class ProPlotterChoicesBarParamsDateSlicer(ProPlotterChoicesBarParams):
    """
     Specific choice via adding a Date Slicer (for Multiple Plots or Member Plots for example)
     """
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self._dateslice = None
        self.label = None
        self.scale_date = None
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice for Date', relief=tk.RAISED)
        self.label.pack()
        if self.master.fileobj is not None:
            self.scale_date = tk.Scale(self.frame, orient='horizontal', state='disabled',
                                       label='Use the slicer to choose the date')
            self.scale_date.config(from_=0, to=(len(self.master.fileobj.get_time()) - 1), state='normal', showvalue=0,
                                   command=self.update_slice_date, variable=tk.IntVar)
            self.scale_date.pack(expand=1, fill=tk.BOTH)

    def update_slice_date(self, *args):
        value = self.scale_date.get()
        if value != self._dateslice:
            self._dateslice = value

    @property
    def var_dateslice(self):
        return self._dateslice


class ProPlotterChoicesBarParamsMember(ProPlotterChoicesBarParamsDateSlicer):
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self.update()


class ProPlotterChoicesBarParamsMultiple(ProPlotterChoicesBarParamsDateSlicer):
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self.update()


class ProPlotterChoicesBarParamsCompare(ProPlotterChoicesBarParams):
    """
     Specific choice for opening second file for comparison
     """

    WIDTH_TXT = 40

    def __init__(self, master, frame):
        super().__init__(master, frame)
        self.fileobj2 = None
        self.opencompare = tk.Button(self.frame, text="Open Second File", command=self.open2)
        self.opencompare.pack(padx=5, fill=tk.X)
        self.labelcompare = tk.Label(self.frame, text='--')
        self.labelcompare.pack(padx=5, fill=tk.X)
        self.update()

    def update(self):
        pass

    def open2(self):
        self.master.status.set_status('Open second file...')
        selectedfilename = tkinter.filedialog.askopenfilename(title='Open second filename', filetypes=FILETYPES)
        if len(selectedfilename) == 0:
            logger.info('No file selected. Ignore.')
            self.master.status.set_status('Open second file... No file selected.')
            return None
        self.master.status.set_status('Opening second file {}'.format(selectedfilename))
        self.fileobj2 = proreader.read_file(selectedfilename)
        if self.fileobj2 is not None:
            self.master.status.set_status('Successfully second opened file {}'.format(selectedfilename))
            self.labelcompare.config(text=textwrap.fill(selectedfilename, width=self.WIDTH_TXT))


class ProPlotterController(abc.ABC):
    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.dataplot_master = None
        self.vartoplot_master_desc = None
        self.dataplot_react = None
        self.vartoplot_react_desc = None
        self.dztoplot = None
        self.timeplot = None
        self.stop_right_click = False
        self.grain = None
        self.ram = None
        self.same_y = True
        self.x_legend = None
        self.ymax_react = None
        self.hauteur = None
        if self.master.fileobj is not None:
            self.colormap = self.master.fileobj.colorbar_variable('')
        self.ratio = [2, 1]
        """Ratio of the different figures to plot (when several subplots on the graph)"""

    def get_choice(self):
        """
        Check if there is only one point which is selected
        """
        selector = self.master.choices.point_w.get_selector()
        points = self.master.fileobj.get_points(selector=selector)
        if len(points) > 1:
            error_msg = 'Too much points, please refine you selection'
            messagebox.showerror(title='Too much points', message=error_msg)
            return None
        elif len(points) == 0:
            error_msg = 'No points with current selection.'
            messagebox.showerror(title='No point found', message=error_msg)
            return None
        return points[0]

    def get_additional_choices(self):
        selector = self.master.choices.addparams_w.get_selector()
        return selector

    def info_text_bar(self, point, additional_options=None):
        """
        Fill the information bar above the graph
        :param point: the number of the point chosen by the user
        """
        v_master = self.master.choices.variables_w.var_master
        v_react = self.master.choices.variables_w.var_react
        text = ' VARIABLES: {}/{}.\nPOINT: {}'.format(v_master, v_react, point)
        if additional_options is not None:
            for key, value in additional_options.items():
                text += ' {}: {}'.format(key, value)
        self.master.controls.update_text(text)

    def check_var_info(self):
        """
        Check if there is a choice for variable(s) to plot
        """
        self.vartoplot_master_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_master)
        self.vartoplot_react_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_react)
        if self.vartoplot_master_desc is None:
            error_msg = 'Please choose a variable to plot'
            messagebox.showerror(title='Missing variable', message=error_msg)
            return None
        if self.master.choices.variables_w.exists_snl:
            if self.vartoplot_react_desc is None:
                error_msg = 'Please choose a variable for profil plot'
                messagebox.showerror(title='Missing variable', message=error_msg)
                return None
        if self.vartoplot_master_desc['has_snl']:
            self.same_y = True
        else:
            self.same_y = False

        return 'OK'

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        """
        self.dataplot_master = self.master.fileobj.get_data(self.vartoplot_master_desc['name'],
                                                            point, additional_options=additional_options)
        if self.vartoplot_react_desc is not None:
            self.dataplot_react = self.master.fileobj.get_data(self.vartoplot_react_desc['name'],
                                                               point, additional_options=additional_options)
        self.dztoplot = self.master.fileobj.get_data(self.master.fileobj.variable_dz, point, fillnan=0.,
                                                     additional_options=additional_options)
        self.timeplot = self.master.fileobj.get_time()
        self.colormap = self.master.fileobj.colorbar_variable(self.vartoplot_master_desc['name'])

        self.ymax_react = np.max(np.nansum(self.dztoplot, axis=1))

        # usefull for motion
        if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
            self.grain = self.master.fileobj.get_data(self.master.fileobj.variable_grain, point, fillnan=0.,
                                                      additional_options=additional_options)
        if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
            self.ram = self.master.fileobj.get_data(self.master.fileobj.variable_grain, point, fillnan=0.,
                                                    additional_options=additional_options)
        if self.vartoplot_react_desc['name'] in self.master.fileobj.variables_log:
            self.dataplot_react = np.where(self.dataplot_react > 10 ** (-10), self.dataplot_react, 10 ** (-10))
            self.dataplot_react = np.where(self.dataplot_react > 0, np.log10(self.dataplot_react), -10)

    def give_master1d_args(self):
        """
        Returns dictionary for the master figure, depending of the choice for the graph type.
        """
        return dict(ax=self.master.main.ax1, value=self.dataplot_master, time=self.timeplot)

    def give_mastersaison_args(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        return dict(ax=self.master.main.ax1, value=self.dataplot_master, time=self.timeplot, dz=self.dztoplot,
                    colormap=self.colormap)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        limitplot_react = self.master.fileobj.limits_variable(self.vartoplot_react_desc['name'])

        xindex = min(int(x_event), self.dztoplot.shape[0] - 1)
        date = self.timeplot[xindex]
        data_date = self.dataplot_react[xindex, ...]
        dz_date = self.dztoplot[xindex, ...]
        if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
            grain_date = self.grain[xindex, ...]
        else:
            grain_date = None

        if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
            ram_date = self.ram[xindex, ...]
        else:
            ram_date = None

        if self.same_y:
            return dict(axe=self.master.main.ax2, axe2=self.master.main.ax3, cbar_show=self.master.main.first_profil,
                        xlimit=limitplot_react, value=data_date, value_dz=dz_date, value_grain=grain_date,
                        value_ram=ram_date, legend=date, hauteur=self.hauteur)
        else:
            return dict(axe=self.master.main.ax2, axe2=self.master.main.ax3, cbar_show=self.master.main.first_profil,
                        xlimit=limitplot_react, value=data_date, value_dz=dz_date, value_grain=grain_date,
                        value_ram=ram_date, legend=date, hauteur=self.hauteur, ylimit=self.ymax_react)

    def masterfig1d(self, **kwargs):
        return profilPlot.saison1d(**kwargs)

    def masterfigsaison(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def reactfig(self, **kwargs):
        return profilPlot.dateProfil(**kwargs)

    def plot(self):
        """
        The plot machinery
        """
        # Stop défilement avec bouton droit
        def button_press(event):
            if event.button > 1:
                self.stop_right_click = not self.stop_right_click
                return

        # Graphe du profil animé à droite en fonction du mouvement de la souris
        def motion(event):
            if self.stop_right_click:
                return
            if event.inaxes == self.master.main.ax1:
                if trace_hauteur is None:
                    self.hauteur = None
                else:
                    self.hauteur = event.ydata

                self.master.main.ax2.clear()
                self.master.main.ax3.clear()

                dico = self.give_react_args(event.xdata)
                self.reactfig(**dico)

                self.master.main.first_profil = False
                self.master.main.update()

        if self.master.fileobj is None:
            return

        var_info = self.check_var_info()
        if var_info is None:
            return

        point = self.get_choice()
        if point is None:
            return

        additional_options = self.get_additional_choices()

        self.info_text_bar(point, additional_options=additional_options)
        self.get_data(point, additional_options=additional_options)
        self.master.main.clear()
        if not self.master.choices.variables_w.exists_snl:
            self.master.main.ready_to_plot(1)
            self.masterfig1d(**self.give_master1d_args())
        else:
            if self.vartoplot_master_desc['has_snl']:
                self.master.main.ready_to_plot(self.same_y, 2, *self.ratio)
                if not self.same_y:
                    self.master.main.ax2.set_ylim(0, self.ymax_react)
                self.masterfigsaison(**self.give_mastersaison_args())
                trace_hauteur = True
            else:
                self.master.main.ready_to_plot(False, 2, *self.ratio)
                self.master.main.ax2.set_ylim(0, self.ymax_react)
                self.masterfig1d(**self.give_master1d_args())
                trace_hauteur = None

            self.master.main.cid = self.master.main.Canevas.mpl_connect('motion_notify_event', motion)
            self.master.main.Canevas.mpl_connect('button_press_event', button_press)

        self.master.main.update()

    def reset(self):
        """
        Reset the selection (and plot)
        """
        for widget in self.master.choices.point_w.lselectors:
            widget.set('')
        self.master.choices.variables_w.choice_var_master.set('')
        self.master.choices.variables_w.choice_var_react.set('')
        self.master.main.clear()


class ProPlotterControllerStandard(ProPlotterController):
    pass


class ProPlotterControllerHeight(ProPlotterController):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.direction = None
        self.height = None
        self.same_y = False
        self.hauteur = None
        self.masterfigsaison = profilPlot.heightplot

    def info_text_bar(self, point):
        v_master = self.master.choices.variables_w.var_master
        if not self.master.fileobj.variable_desc(v_master)['has_snl']:
            text = 'WARNING: MISSING SNOW LAYER DIMENSION for HEIGHT GRAPH'
            self.master.controls.update_text(text)
        else:
            v_react = self.master.choices.variables_w.var_react
            text = 'HEIGHT GRAPH with VARS: {}/{}. POINT: {}'.format(v_master, v_react, point)
            self.master.controls.update_text(text)

    def check_var_info(self):
        """
        Check if there is a choice for variable(s) to plot
        """
        self.vartoplot_master_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_master)
        self.vartoplot_react_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_react)
        if self.vartoplot_master_desc is None:
            error_msg = 'Please choose a variable to plot'
            messagebox.showerror(title='Missing variable', message=error_msg)
            return None
        if self.master.choices.variables_w.exists_snl:
            if self.vartoplot_react_desc is None:
                error_msg = 'Please choose a variable for profil plot'
                messagebox.showerror(title='Missing variable', message=error_msg)
                return None
        return 'OK'

    def give_mastersaison_args(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        self.timeplot = self.master.fileobj.get_time()
        self.direction = self.master.choices.params_w.var_direction
        self.master.choices.params_w.update_height()
        self.height = self.master.choices.params_w.var_height
        return dict(ax=self.master.main.ax1, value=self.dataplot_master, time=self.timeplot,
                    value_ep=self.dztoplot, direction_cut=self.direction, height_cut=self.height)

    def reset(self):
        super().reset()
        self.master.choices.params_w.choice_direction.set('')
        self.master.choices.params_w.choice_height.delete(0, self.master.choices.WIDTH)


class ProPlotterControllerSlider(ProPlotterController):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.dateslice = None

    def update_slice_date(self, value):
        """
        Redraw the ax1 graph and clear ax2 and ax3
        """
        self.dateslice = self.master.choices.params_w.scale_date.get()
        self.master.main.first_master = False

        self.master.main.ax1.clear()
        self.master.main.ax2.clear()
        self.master.main.ax3.clear()

        if not self.master.choices.variables_w.exists_snl:
            self.masterfig1d(**self.give_master1d_args())
        else:
            if self.vartoplot_master_desc['has_snl']:
                self.masterfigsaison(**self.give_mastersaison_args())
            else:
                self.masterfig1d(**self.give_master1d_args())

        self.master.main.update()


class ProPlotterControllerMember(ProPlotterControllerSlider):
    def __init__(self, master):
        self.master = master
        super().__init__(master)

    def info_text_bar(self, point):
        v_master = self.master.choices.variables_w.var_master
        v_react = self.master.choices.variables_w.var_react
        text = 'MEMBER GRAPH with VARS: {}/{}. POINT: {}'.format(v_master, v_react, point)
        self.master.controls.update_text(text)

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        In this case shape = nb_membres, time, snowlayer
        """
        self.vartoplot_master_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_master)
        self.dataplot_master, list_members = self.master.fileobj.get_data(self.vartoplot_master_desc['name'],
                                                                          point, members='all',
                                                                          additional_options=additional_options)
        self.vartoplot_react_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_react)
        self.dataplot_react = self.master.fileobj.get_data(self.vartoplot_react_desc['name'], point,
                                                           members='all', additional_options=additional_options)[0]

        self.dztoplot = self.master.fileobj.get_data(self.master.fileobj.variable_dz, point, fillnan=0.,
                                                     members='all', additional_options=additional_options)[0]
        self.timeplot = self.master.fileobj.get_time()
        self.colormap = self.master.fileobj.colorbar_variable(self.vartoplot_master_desc['name'])

        # usefull for motion
        if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
            self.grain = self.master.fileobj.get_data(self.master.fileobj.variable_grain, point, fillnan=0.,
                                                      members='all', additional_options=additional_options)[0]
        if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
            self.ram = self.master.fileobj.get_data(self.master.fileobj.variable_grain, point, fillnan=0.,
                                                    members='all', additional_options=additional_options)[0]
        if self.vartoplot_react_desc['name'] in self.master.fileobj.variables_log:
            self.dataplot_react = np.where(self.dataplot_react > 10 ** (-10), self.dataplot_react, 10 ** (-10))
            self.dataplot_react = np.where(self.dataplot_react > 0, np.log10(self.dataplot_react), -10)

        self.dataplot_master = np.array(self.dataplot_master)
        self.dataplot_react = np.array(self.dataplot_react)
        self.dztoplot = np.array(self.dztoplot)
        self.grain = np.array(self.grain)
        self.ram = np.array(self.ram)
        self.ymax_react = np.max(np.nansum(self.dztoplot, axis=2))
        self.x_legend = list_members
        self.dateslice = self.master.choices.params_w.var_dateslice
        if self.dateslice is None:
            self.dateslice = 0

        self.master.choices.params_w.scale_date.config(from_=0, to=(len(self.master.fileobj.get_time()) - 1),
                                                       state='normal', showvalue=0, variable=tk.IntVar,
                                                       command=self.update_slice_date)

    def give_master1d_args(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        dataplot_master = self.dataplot_master[:, self.dateslice]

        if self.dztoplot is not None:
            limit_dz = np.max(np.cumsum(self.dztoplot, axis=2))
            limit_value = np.max(self.dataplot_master)
            ylimit = max(limit_dz, limit_value)
        else:
            ylimit = np.max(self.dataplot_master)

        return dict(ax=self.master.main.ax1, value=dataplot_master, time=self.x_legend,
                    title=self.timeplot[self.dateslice], ylimit=ylimit)

    def give_mastersaison_args(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        if np.all(np.isnan(self.dataplot_master)):
            max_value = 1
            min_value = 0.1
        else:
            max_value = np.nanmax(self.dataplot_master)
            min_value = np.nanmin(self.dataplot_master)

        dataplot_master = self.dataplot_master[:, self.dateslice, :]
        dztoplot = self.dztoplot[:, self.dateslice, :]
        ylimit = np.max(np.cumsum(self.dztoplot, axis=2))

        return dict(ax=self.master.main.ax1, value=dataplot_master, time=self.x_legend, dz=dztoplot,
                    colormap=self.colormap, title=self.timeplot[self.dateslice], value_max=max_value,
                    value_min=min_value, cbar_show=self.master.main.first_master, ylimit=ylimit)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        limitplot_react = self.master.fileobj.limits_variable(self.vartoplot_react_desc['name'])

        xindex = min(int(x_event), self.dztoplot.shape[0] - 1)
        legend_member = "member " + str(self.x_legend[xindex])
        data_member = self.dataplot_react[xindex, self.dateslice, :]
        dz_member = self.dztoplot[xindex, self.dateslice, :]
        if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
            grain_date = self.grain[xindex, self.dateslice, :]
        else:
            grain_date = None

        if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
            ram_date = self.ram[xindex, self.dateslice, :]
        else:
            ram_date = None

        return dict(axe=self.master.main.ax2, axe2=self.master.main.ax3, cbar_show=self.master.main.first_profil,
                    xlimit=limitplot_react, value=data_member, value_dz=dz_member, value_grain=grain_date,
                    value_ram=ram_date, legend=legend_member, hauteur=self.hauteur)


class ProPlotterControllerMemberProfil(ProPlotterControllerMember):
    def __init__(self, master):
        self.master = master
        super().__init__(master)


class ProPlotterControllerMemberSaison(ProPlotterControllerMember):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.ratio = [1, 1]

    def reactfig(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        xindex = min(int(x_event), self.dztoplot.shape[0] - 1)
        legend_member = "member " + str(self.x_legend[xindex])
        dataplot_react = self.dataplot_react[xindex, ...]
        dztoplot = self.dztoplot[xindex, ...]
        colormap = self.master.fileobj.colorbar_variable(self.vartoplot_react_desc['name'])

        return dict(ax=self.master.main.ax2, value=dataplot_react, time=self.timeplot, dz=dztoplot,
                    colormap=colormap, title=legend_member, cbar_show=self.master.main.first_profil)


class ProPlotterControllerMultiple(ProPlotterControllerSlider):
    def __init__(self, master):
        self.master = master
        super().__init__(master)

    def get_choice(self):
        selector = self.master.choices.point_w.get_selector()
        liste_points = self.master.fileobj.get_points(selector=selector)
        if len(liste_points) == 0:
            error_msg = 'No points with current selection.'
            messagebox.showerror(title='No point found', message=error_msg)
            return None
        return liste_points

    def info_text_bar(self, point):
        v_master = self.master.choices.variables_w.var_master
        v_react = self.master.choices.variables_w.var_react
        text = 'MULTIPLE GRAPH with VARS: {}/{}.'.format(v_master, v_react)
        self.master.controls.update_text(text)

    def get_data(self, liste_points, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        In this case shape = time, snowlayer, nb_points
        """
        self.vartoplot_master_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_master)
        self.dataplot_master = self.master.fileobj.get_data(self.vartoplot_master_desc['name'], list(liste_points),
                                                            additional_options=additional_options)
        self.vartoplot_react_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_react)
        self.dataplot_react = self.master.fileobj.get_data(self.vartoplot_react_desc['name'], list(liste_points),
                                                           additional_options=additional_options)

        self.dztoplot = self.master.fileobj.get_data(self.master.fileobj.variable_dz, list(liste_points), fillnan=0.,
                                                     additional_options=additional_options)
        self.timeplot = self.master.fileobj.get_time()
        self.colormap = self.master.fileobj.colorbar_variable(self.vartoplot_master_desc['name'])

        # usefull for motion
        if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
            self.grain = self.master.fileobj.get_data(self.master.fileobj.variable_grain, list(liste_points),
                                                      fillnan=0., additional_options=additional_options)
        if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
            self.ram = self.master.fileobj.get_data(self.master.fileobj.variable_grain, list(liste_points), fillnan=0.,
                                                    additional_options=additional_options)
        if self.vartoplot_react_desc['name'] in self.master.fileobj.variables_log:
            self.dataplot_react = np.where(self.dataplot_react > 10 ** (-10), self.dataplot_react, 10 ** (-10))
            self.dataplot_react = np.where(self.dataplot_react > 0, np.log10(self.dataplot_react), -10)

        self.ymax_react = np.max(np.nansum(self.dztoplot, axis=2))

        self.x_legend = list(liste_points)
        self.x_legend = [int(i) for i in self.x_legend]

        self.dateslice = self.master.choices.params_w.var_dateslice
        if self.dateslice is None:
            self.dateslice = 0

        self.master.choices.params_w.scale_date.config(from_=0, to=(len(self.master.fileobj.get_time()) - 1),
                                                       state='normal', showvalue=0, variable=tk.IntVar,
                                                       command=self.update_slice_date)

    def give_master1d_args(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        # Select a date (this is always the first dimension (no multiple member graph)
        dataplot_master = np.transpose(self.dataplot_master[self.dateslice, ...])
        if self.dztoplot is not None:
            ylimit = np.max(np.cumsum(self.dztoplot, axis=1))  # axis 1 is layers
            return dict(ax=self.master.main.ax1, value=dataplot_master, time=self.x_legend,
                        title=self.timeplot[self.dateslice], ylimit=ylimit)
        else:
            return dict(ax=self.master.main.ax1, value=dataplot_master, time=self.x_legend,
                        title=self.timeplot[self.dateslice])

    def give_mastersaison_args(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        max_value = np.nanmax(self.dataplot_master)
        min_value = np.nanmin(self.dataplot_master)
        dataplot_master = np.transpose(self.dataplot_master[self.dateslice, ...])
        dztoplot = np.transpose(self.dztoplot[self.dateslice, ...])
        ylimit = np.max(np.cumsum(self.dztoplot, axis=1))

        return dict(ax=self.master.main.ax1, value=dataplot_master, time=self.x_legend, dz=dztoplot,
                    colormap=self.colormap, title=self.timeplot[self.dateslice], value_max=max_value,
                    value_min=min_value, cbar_show=self.master.main.first_master, ylimit=ylimit)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        limitplot_react = self.master.fileobj.limits_variable(self.vartoplot_react_desc['name'])

        # here shape = time, snowlayer, nb_points
        xindex = min(int(x_event), self.dztoplot.shape[2] - 1)
        legend_mult = "multiple " + str(self.x_legend[xindex])
        data_mult = self.dataplot_react[self.dateslice, :, xindex]
        dz_mult = self.dztoplot[self.dateslice, :, xindex]
        if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
            grain_date = self.grain[self.dateslice, :, xindex]
        else:
            grain_date = None

        if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
            ram_date = self.ram[self.dateslice, :, xindex]
        else:
            ram_date = None

        # TODO: Check what happens in multiple 2d file ... something strange... #

        return dict(axe=self.master.main.ax2, axe2=self.master.main.ax3, cbar_show=self.master.main.first_profil,
                    xlimit=limitplot_react, value=data_mult, value_dz=dz_mult, value_grain=grain_date,
                    value_ram=ram_date, legend=legend_mult, hauteur=self.hauteur)


class ProPlotterControllerMultipleProfil(ProPlotterControllerMultiple):
    def __init__(self, master):
        self.master = master
        super().__init__(master)


class ProPlotterControllerMultipleSaison(ProPlotterControllerMultiple):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.ratio = [1, 1]

    def reactfig(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        # BEWARE: .shape[2] because here shape = time, snowlayer, nb_points
        xindex = min(int(x_event), self.dztoplot.shape[2] - 1)
        legend_multiple = "multiple " + str(self.x_legend[xindex])
        dataplot_react = self.dataplot_react[:, :, xindex]
        dztoplot = self.dztoplot[:, :, xindex]
        colormap = self.master.fileobj.colorbar_variable(self.vartoplot_react_desc['name'])

        return dict(ax=self.master.main.ax2, value=dataplot_react, time=self.timeplot, dz=dztoplot,
                    colormap=colormap, title=legend_multiple, cbar_show=self.master.main.first_profil)


class ProPlotterControllerCompare(ProPlotterController):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.dztoplot_react = None
        self.timeplot_react = None
        self.ratio = [1, 1]

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        """
        self.dataplot_master = self.master.fileobj.get_data(self.vartoplot_master_desc['name'], point,
                                                            additional_options=additional_options)
        self.dztoplot = self.master.fileobj.get_data(self.master.fileobj.variable_dz, point, fillnan=0.,
                                                     additional_options=additional_options)
        self.colormap = self.master.fileobj.colorbar_variable(self.vartoplot_master_desc['name'])

        self.dataplot_react = self.master.choices.params_w.fileobj2.get_data(self.vartoplot_react_desc['name'], point,
                                                                             additional_options=additional_options)
        self.dztoplot_react = self.master.choices.params_w.fileobj2.get_data(self.master.fileobj.variable_dz, point,
                                                                             fillnan=0.,
                                                                             additional_options=additional_options)
        self.colormap_react = self.master.fileobj.colorbar_variable(self.vartoplot_react_desc['name'])

        self.timeplot = self.master.fileobj.get_time()

    def reactfigsaison(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def reactfig1d(self, **kwargs: dict):
        return profilPlot.saison1d(**kwargs)

    def give_reactsaison_args(self):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        return dict(ax=self.master.main.ax2, value=self.dataplot_react, time=self.timeplot,
                    dz=self.dztoplot_react, colormap=self.colormap_react)

    def give_react1d_args(self):
        """
        Returns dictionary for the master figure, depending of the choice for the graph type.
        """
        return dict(ax=self.master.main.ax2, value=self.dataplot_react, time=self.timeplot)

    def plot(self):
        """
        The plot machinery
        """
        if self.master.fileobj is None:
            return

        if self.master.choices.params_w.fileobj2 is None:
            return

        var_info = self.check_var_info()
        if var_info is None:
            return

        point = self.get_choice()
        if point is None:
            return

        additional_options = self.get_additional_choices()

        self.info_text_bar(point)
        self.get_data(point, additional_options=additional_options)
        self.master.main.clear()

        self.master.main.ax1 = self.master.main.fig1.add_subplot(1, 2, 1)

        if self.vartoplot_master_desc['has_snl'] and self.vartoplot_react_desc['has_snl']:
            self.master.main.ax2 = self.master.main.fig1.add_subplot(1, 2, 2, sharex=self.master.main.ax1,
                                                                     sharey=self.master.main.ax1)
        elif self.vartoplot_master_desc['name'] == self.vartoplot_react_desc['name']:
            self.master.main.ax2 = self.master.main.fig1.add_subplot(1, 2, 2, sharex=self.master.main.ax1,
                                                                     sharey=self.master.main.ax1)
        else:
            self.master.main.ax2 = self.master.main.fig1.add_subplot(1, 2, 2, sharex=self.master.main.ax1)

        if self.vartoplot_master_desc['has_snl']:
            self.masterfigsaison(**self.give_mastersaison_args())
        else:
            self.masterfig1d(**self.give_master1d_args())

        if self.vartoplot_react_desc['has_snl']:
            self.reactfigsaison(**self.give_reactsaison_args())
        else:
            self.reactfig1d(**self.give_react1d_args())

        self.master.main.update()


def main(*args, **kwargs):
    NAME = 'GUI PROplotter CEN'
    root = tk.Tk(className=NAME)
    root.title(NAME)
    root.protocol("WM_DELETE_WINDOW", root.quit)
    root.geometry('1100x850')

    try:
        _here = os.path.dirname(os.path.realpath(__file__))
        img = tk.PhotoImage(file=os.path.join(_here, 'proplotter.png'))
        root.tk.call('wm', 'iconphoto', root._w, img)
    except:
        pass

    app = ProPlotterApplication(*args, master=root, **kwargs)
    app.mainloop()


if __name__ == "__main__":
    main()
