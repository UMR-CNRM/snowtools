#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
GUI Application for comparing simulation output PRO files

The GUI is divided in different parts :

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
import os.path

import tkinter as tk
import tkinter.filedialog
from tkinter import messagebox

import numpy as np
import matplotlib
import pyproj

from snowtools.plots.stratiprofile import proplotter_functions

from snowtools.plots.stratiprofile import gui_elements
from snowtools.plots.maps import quicklookmap
from snowtools.plots.maps import mapplotter_fileobj

matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
import cartopy.crs as ccrs

logger = logging.getLogger()

# App constants:
FILETYPES = [('PRO files', '.nc'), ('all files', '.*')]
STD_MAP_ARGS = {
    'parallels': 'auto',
    'meridians': 'auto',
    'cartopy_features': []
}


class MapPlotterApplication(gui_elements.Application):
    """
    The base frame for the GUI application

    :param master: The Tk root (tk.Tk())
    :param fileobjs: file proreader objects if already open
    :param point: Select point, if any
    :param arguments: Other arguments provided as a dict.
                      Accepted keys are: type, begin, end, variable

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
                    safer not to link directly to an action of another one as it may have changed since callback
                    definition) which will call a controller action which will act on desired graphical elements.
    * fileobj:

    Note that only two elements are adapted to graph type: the first part of the choices Frame
    (``choices.params_w``) and the controller.

    .. warning::
        * Actions when clicking buttons or similar should only call functions in the same class
        * Class actions should only call functions in main Application class or fileobj or controller
        * controller can act on every element.
    """
    def __init__(self, master=None, fileobj=None, point=None, arguments: dict = {}):
        super().__init__(master)
        self.master = master
        self.fileobj = fileobj
        self.pack(fill=tk.BOTH, expand=True)
        self.parameters = {'show_depts': tkinter.IntVar()}

        # Controls
        self.controller = MapPlotterController(self)

        # Graphical elements
        self.menu = MapPlotterMenu(self)
        self.choices = gui_elements.ProPlotterChoicesBar(self, n_variables=1, point_selection=False)
        self.controls = gui_elements.ProPlotterControlsBar(self)
        self.openbar = gui_elements.ProPlotterOpenBar(self)
        self.main = MapPlotterMain(self)
        self.status = gui_elements.ProPlotterStatus(self)
        self.choices.params_w = gui_elements.ProPlotterChoicesBarParamsDateSlicer(self, self.choices.params)

        # Update to take into account an eventual open file
        # self.open_update()

        # Keyboard shortcuts
        self.master.bind('<Escape>', self.close_window)
        self.master.bind('<Control-q>', self.close_window)

        # Parse additional arguments
        if 'variable' in arguments:
            set_v1 = self.choices.variables_w.set_var_master(arguments['variable'])
        else:
            set_v1 = False

        if set_v1:
            self.plot()

        self.update_idletasks()

    def plot(self):
        self.controls.plot()

    def open(self, *args, **kwargs):
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
        fileobj = mapplotter_fileobj.read_file(selectedfilename)
        if fileobj is not None:
            self.fileobj = fileobj
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
        self.controls.plot_mark()
        self.choices.variables_w.update()
        self.choices.addparams_w.update()
        if self.choices.params_w is not None:
            self.choices.params_w.update()
        self.controller.open_update()


class MapPlotterMenu(tk.Menu):
    """ The app menu """
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        # Menu 0
        self.filemenu = tk.Menu(self, tearoff=0)
        self.filemenu.add_command(label='Quit', command=self.master.close_window,
                                  accelerator='ESC')
        self.add_cascade(label='File', menu=self.filemenu)

        self.optionsmenu = tk.Menu(self, tearoff=0)
        self.optionsmenu.add_checkbutton(label="Show French Departments", onvalue=1, offvalue=0,
                                         variable=self.master.parameters['show_depts'])
        self.add_cascade(label='Options', menu=self.optionsmenu)

        self.master.master.config(menu=self)


class MapPlotterController(abc.ABC):

    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.data = None
        self.vartoplot_master_desc = None
        self.master_xlim = None
        self.master_ylim = None

    def check_var_info(self):
        """
        Check if there is a choice for variable(s) to plot
        """
        self.vartoplot_master_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_master)
        if self.vartoplot_master_desc is None:
            error_msg = 'Please choose a variable to plot'
            messagebox.showerror(title='Missing variable', message=error_msg)
            return None
        return True

    def info_text_bar(self, additional_options=None):
        """
        Fill the information bar above the graph
        """
        v_master = self.master.choices.variables_w.var_master
        text = 'VARIABLE: {}.\n'.format(v_master)
        if additional_options is not None:
            for key, value in additional_options.items():
                text += ' {}: {}'.format(key, value)
        self.master.controls.update_text(text)

    def get_additional_choices(self):
        selector = self.master.choices.addparams_w.get_selector()
        return selector

    def plot(self):
        """
        The plot machinery
        """
        if self.master.fileobj is None:
            return

        # In order to keep in memory the xlim and ylim after a zoom on master figure
        def change_xlim(event):
            self.master_xlim = event.get_xlim()

        def change_ylim(event):
            self.master_ylim = event.get_ylim()

        # Get Variable
        var_info = self.check_var_info()
        if var_info is None:
            return
        varinfo = self.vartoplot_master_desc
        varname = self.vartoplot_master_desc['name']

        # Get additional options
        additional_options = self.get_additional_choices()

        # Get time
        date = self.master.choices.params_w.get_date()

        # Get data
        only = {}
        infos = {}
        for k, v in additional_options.items():
            if k in varinfo['dimensions']:
                only[k] = v
                infos[k] = v
        infos['date'] = date
        self.info_text_bar(additional_options=infos)
        self.get_data(varname, date, additional_options=only)
        self.master.main.clear()

        # Prepare fig and axis
        self.master.main.ready_to_plot(nb_graph=1, same_y=True, same_x=True, third_axis=False)
        fig = self.master.main.fig1
        ax = self.master.main.ax1

        # Get colormap
        colormap = self.master.fileobj.colorbar_variable(varname)
        # Epygram does not allow to use user-defined colormaps
        colormap = 'viridis'

        # Plot
        quicklookmap.scalar_map(self.data, plot_kwargs={
            'ax': ax,
            'fig': fig,
            'colormap': colormap,
            'plot_method': 'pcolormesh',
            'epygram_departments': self.master.parameters['show_depts'].get() == 1,
            **STD_MAP_ARGS
        })

        # Apply previous zoom if relevant and store zoom information
        if self.master_xlim is not None and self.master_ylim is not None:
            self.master.main.toolbar.push_current()
            self.master.main.ax1.set_xlim(self.master_xlim)
            self.master.main.ax1.set_ylim(self.master_ylim)

        self.master.main.ax1.callbacks.connect('xlim_changed', change_xlim)
        self.master.main.ax1.callbacks.connect('ylim_changed', change_ylim)

        # Transfer to GUI interface
        self.master.main.update()

    def get_data(self, varname, date, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        """
        self.data = None
        if self.master.fileobj is None:
            return
        self.data = quicklookmap.read_and_preprocess(
            self.master.fileobj.resource,
            varname,
            date,
            additional_selection_options=additional_options)

    def give_master_args(self):
        """
         Returns dictionary for the master figure, depending on the choice for the graph type.
        """
        return proplotter_functions.give_master_args_std(self.master.main.ax1, self.data)

    def reset(self):
        """
        Reset the selection (and plot)
        """
        self.master.choices.variables_w.choice_var_master.set('')
        self.master.main.clear()

    def open_update(self):
        """
        Update to do to specific components when a new file is opened.
        """
        self.master_xlim = None
        self.master_ylim = None


class MapPlotterMain(tk.Frame):
    """The Frame for plotting"""

    def __init__(self, master):
        """Initialize plotting area """
        self.master = master
        super().__init__(master, relief=tk.RAISED, borderwidth=1)
        self.pack(fill=tk.BOTH, expand=True)

        self.fig1 = plt.figure()
        self.ax1 = None
        self.cid = {'motion': None, 'right_click': None}
        self.Canevas = FigureCanvasTkAgg(self.fig1, self)
        self.toolbar = NavigationToolbar2Tk(self.Canevas, self)
        self.toolbar.update()
        self.Canevas.mpl_connect('button_press_event', self.button_press)

        self.Canevas.get_tk_widget().pack(fill=tk.BOTH, expand=True)
        self.update()

        self.update_idletasks()

    def clear(self):
        """Clean main frame (figure)"""
        if self.ax1 is not None:
            self.fig1.delaxes(self.ax1.axes)
        for ax in self.fig1.get_axes():
            self.fig1.delaxes(ax.axes)
        for key, value in self.cid.items():
            if value is not None:
                self.Canevas.mpl_disconnect(value)
                self.cid[key] = None
        self.ax1 = None
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
        self.ax1 = plt.axes(projection=ccrs.PlateCarree())
        self.toolbar.update()

    def button_press(self, event):
        if event.button > 1:
            if event.xdata is None:
                return
            lat = event.ydata
            lon = event.xdata
            t = pyproj.Transformer.from_crs('EPSG:4326', 'EPSG:2154')
            xlambert, ylambert = t.transform(lat, lon)
            xx = None
            yy = None
            xxyy = None
            if self.master.fileobj is not None:
                try:
                    g = self.master.fileobj.resource.readfield('DSN_T_ISBA').geometry.get_lonlat_grid()
                    _d = (g[0] - lon) ** 2 + (g[1] - lat) ** 2
                    xxyy = np.argmin(_d)
                    xx, yy = xxyy // g[0].shape[1], xxyy % g[0].shape[1]
                except Exception:
                    pass
            messagebox.showinfo(
                title = 'Coordinates',
                message = f"""lat: {lat}\nlon: {lon}\nxlambert: {xlambert}\nylambert: {ylambert}\n"""
                f"""#xx: {xx}\n#yy: {yy}\n##: {xxyy}""")

    def update(self):
        """
        Update the figure after a change in plot.

        NB: If you do not call this function, the changes on the graph are not shown.

        :return: None
        """
        self.Canevas.draw()


def main(*args, **kwargs):
    NAME = 'GUI MapPlotter'
    root = tk.Tk(className=NAME)
    root.title(NAME)
    root.protocol("WM_DELETE_WINDOW", root.quit)
    root.geometry('1100x850')

    try:
        _here = os.path.dirname(os.path.realpath(__file__))
        img = tk.PhotoImage(file=os.path.join(_here, '..', 'stratiprofile', 'proplotter.png'))
        root.tk.call('wm', 'iconphoto', root._w, img)
    except Exception:
        pass

    app = MapPlotterApplication(*args, master=root, **kwargs)
    app.mainloop()
