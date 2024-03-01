# -*- coding: utf-8 -*-

import textwrap

import tkinter as tk
import tkinter.filedialog
from tkinter import ttk
from tkinter import messagebox

import numpy as np

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk

from snowtools.plots.stratiprofile import proplotter_functions


class Application(tk.Frame):
    """
    The base frame for the GUI application
    """
    def __init__(self, master=None):
        super().__init__(master)
        self.master = master
        self.pack(fill=tk.BOTH, expand=True)

        # Keyboard shortcuts
        self.master.bind('<Escape>', self.close_window)
        self.master.bind('<Control-q>', self.close_window)

    def close_window(self, *args, **kwargs):
        """
        Just to close the application
        """
        self.quit()

    def open_credits(self):
        """
        Opening the help window with text inside
        """
        messagebox.showinfo('CREDITS', 'Software coded by Meteo France, team: CNRM/CEN/CENMOD. \n'
                                       'It benefits from numerous interns, doctoral students, etc. \n'
                                       'It benefits obviously also from our tremendous local geek \n'
                                       'The name of our geek is not given to avoid job offers far from CEN \n'
                                       'Thanks to you, Geek, once again.'
                                       '')


class ProPlotterChoicesBar(tk.Frame):
    """
    The lateral bar for choosing:
     * Parameters specific to representation
     * Variable selection
     * Point selection
    """

    WIDTH = 40
    WIDTH_TXT = WIDTH

    def __init__(self, master, n_variables=2):
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

        self.variables_w = ProPlotterChoicesBarVariables(self, self.variables, n_variables=n_variables)
        self.point_w = ProPlotterChoicesBarPoint(self, self.point)
        self.addparams_w = ProPlotterchoicesBarAdditionalParams(self, self.addparams)
        self.params_w = None


class ProPlotterChoicesBarVariables:
    """
    Choice of variables in the opened file
    """
    def __init__(self, master, frame, n_variables):
        self.master = master
        self.n_variables = n_variables
        self.frame = frame
        self._var_master = None
        self._var_react = None
        self.label = None
        self.label1 = None
        self.label2 = None
        self.variables_info = None
        self.choice_var_master = None
        self.choice_var_react = None
        self.update()

    def update(self):
        """Clean and fill the Combobox with choices for variables"""
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of variables', relief=tk.RAISED)
        self.label.pack()

        # choice of master and react variable. Vocabulary coming from the plotter application
        # plotter application: one master graph and possibly one servant graph which is reacting from master
        # compare application: used with only the master variable (several graph of same variable) 
        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_desc
            variables_list = [v['full_name'] if 'full_name' in v else k for k, v in self.variables_info.items()]
            list_bool = [v['has_snl'] if 'full_name' in v else k for k, v in self.variables_info.items()]
            variables_with_snl = [variables_list[i] for i in range(len(variables_list)) if list_bool[i]]

            self.label1 = tk.Label(self.frame, text='Variable:')
            self.label1.pack()
            self.choice_var_master = ttk.Combobox(self.frame, state='readonly', values=variables_list,
                                                  width=self.master.WIDTH, )
            self.choice_var_master.bind('<<ComboboxSelected>>', self.update_var_master)
            self.choice_var_master.pack()

            # only for plotter application
            if self.n_variables >= 2:
                self.label2 = tk.Label(self.frame, text='Variable profil:')
                self.label2.pack()
                self.choice_var_react = ttk.Combobox(self.frame, state='readonly', values=variables_with_snl,
                                                     width=self.master.WIDTH)
                self.choice_var_react.bind('<<ComboboxSelected>>', self.update_var_react)
                self.choice_var_react.pack()

    def set_var_master(self, var):
        """Set the value for the master graph which is the left graph"""
        if var in self.choice_var_master['values']:
            i = self.choice_var_master['values'].index(var)
            self.choice_var_master.current(i)
            self.update_var_master()
            return True
        return False

    def update_var_master(self, *args):
        """Update the value for the master graph which is the left graph"""
        value = self.choice_var_master.get()
        if value != self._var_master:
            self._var_master = value
            self.master.master.controls.plot_mark()

    def set_var_react(self, var):
        """
        For plotter application only. Not used for compare application.
        Set the value for the servant graph which is the graph on the right
        """
        if self.n_variables < 2:
            return False
        if var in self.choice_var_react['values']:
            i = self.choice_var_react['values'].index(var)
            self.choice_var_react.current(i)
            self.update_var_react()
            return True
        return False

    def update_var_react(self, *args):
        """
        For plotter application only. Not used for compare application.
        Update the value for the servant graph which is the graph on the right
        """
        if self.n_variables < 2:
            return
        value = self.choice_var_react.get()
        if value != self._var_react:
            self._var_react = value
            self.master.master.controls.plot_mark()

    @property
    def var_master(self):
        return self._var_master

    @property
    def var_react(self):
        if self.n_variables < 2:
            return None
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

        # choice of the point for which the plot is made
        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_selection_point
            for v, info in self.variables_info.items():
                label = tk.Label(self.frame, text=textwrap.fill(info['full_name'], width=self.master.WIDTH_TXT))
                label.pack()
                ii = len(self.llabels)
                # 'type' can be:
                #    - choices = the list of possible values
                #    - int for point number
                #    - float for point localization (PRO for 2D case)
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
    Example: direction of cut for height plot, date for Escroc plot
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
                # 'type' can be:
                #    - choices = the list of possible values
                #    - int for point number
                #    - float for point localization (PRO for 2D case)
                if info['type'] == 'choices':
                    choices = list(info['choices'])  # Tkinter knows nothing of numpy arrays...
                    selector = ttk.Combobox(self.frame, state='readonly', values=[''] + choices,
                                            width=self.master.WIDTH)
                    selector.bind('<<ComboboxSelected>>', lambda _, i=ii: self.update_var())
                elif info['type'] in ['int', 'float']:
                    choices = list(range(info['limits'][0], info['limits'][1] + 1))
                    sv = tk.StringVar()
                    selector = ttk.Spinbox(self.frame, values=choices, textvariable=sv,
                                           width=self.master.WIDTH,
                                           command=lambda *_, i=ii: self.update_var())
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

    def update_var(self):
        """
        run at each modification of a widget:
        """
        self.master.master.controls.plot_mark()

    def clean_frame(self):
        """Clean"""
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterMain(tk.Frame):
    """The Frame for plotting"""

    def __init__(self, master):
        """Initialize plotting area """
        self.master = master
        super().__init__(master, relief=tk.RAISED, borderwidth=1)
        self.pack(fill=tk.BOTH, expand=True)

        self.fig1 = plt.figure()
        self.ax = {'ax1': None, 'ax2': None, 'ax3': None}
        self.cid = {'motion': None, 'right_click': None}
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
        for key, value in self.cid.items():
            if value is not None:
                self.Canevas.mpl_disconnect(value)
                self.cid[key] = None
        self.ax = {'ax1': None, 'ax2': None, 'ax3': None}
        self.first_profil = True
        self.first_master = True
        self.first_graph = True
        self.update()

    def ready_to_plot(self, same_y, nb_graph, ratio=None, same_x=False, third_axis=True):
        """
        Prepare the main frame for a new figure.
        This function include refreshing of surrounding dependent bricks
        such as the matplotlib navigation toolbar.

        :param same_y: Boolean on whether master and servant graph have same y dimension or not
        :type same_y: bool
        :param nb_graph: 1 or 2 to have one or two graphs.
        :param ratio: ratios for the size of the two graphs (when there are two)
        :type ratio: list of integers
        :param same_x: Boolean on whether master and servant graph have same x dimension or not
        :type same_x: bool
        :param third_axis: Create a third axis on the second subplot.
        :type third_axis: bool
        
        .. note for developpers::
            * same_x and same_y are true when you want to compare two graph of same variable/different simulation
            * same_x false and same_y true when you want to plot on one simulation two variables with snowlayer dimension
            * same_x false and same_y false if you want to plot albedo and see what happen on temperature for example
        """
        self.clear()
        self.ax = proplotter_functions.create_axis_for_figure(self.fig1, nb_graph, same_y, ratio=ratio,
                                                              same_x=same_x, third_axis=third_axis)
        if nb_graph == 2:
            self.first_profil = True
        # Update the Matplotlib toolbar to take into account axes added to the figure.
        self.toolbar.update()

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


