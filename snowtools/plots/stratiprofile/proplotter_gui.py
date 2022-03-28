# -*- coding: utf-8 -*-

import abc
import tkinter as tk
import tkinter.filedialog
from tkinter import ttk
from tkinter import messagebox
import logging

import numpy as np
import pdb

from snowtools.plots.stratiprofile import proreader
from snowtools.plots.stratiprofile import profilPlot
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

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
                      direction, height (only for type=height)

    Maintain links to the different parts of the interface:
     * menu: The Menu (File, graph type, change language)
     * openbar: The Tk Frame containing the Open button and the information on currently opened file
     * choices: The Tk lateral frame for selecting:
                 * Parameters specific to representation
                 * Variable selection (specific to opened file)
                 * Point selection (specific to opened file)
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
        self.to_graph()

        # Keyboard shortcuts
        self.master.bind('<Control-o>', self.open)
        self.master.bind('<Escape>', self.close_window)
        self.master.bind('<Control-q>', self.close_window)

        self.update_idletasks()

    def close_window(self, *args, **kwargs):
        self.master.destroy()

    def to_graph(self, typ=None):
        if typ is None:
            typ = self.type

        if typ == 'standard':
            self.to_graph_standard()
        elif typ == 'massif':
            self.to_graph_massif()
        elif typ == 'height':
            self.to_graph_height()
        elif typ == 'member':
            self.to_graph_member()
        else:
            raise ValueError('Unknown graph type')
        self.type = typ

    def to_graph_standard(self):
        self.controller = ProPlotterController_Standard(self)
        self.choices.params_w = ProPlotterChoicesBar_Params_Standard(self)

    def to_graph_massif(self):
        self.controller = ProPlotterController_Massif(self)
        self.choices.params_w = ProPlotterChoicesBar_Params_Massif(self)

    def to_graph_height(self):
        self.var_height = tk.Frame(self.choices.canvas)
        self.var_height.pack(fill=tk.X)
        self.controller = ProPlotterController_Height(self)
        self.choices.params_w = ProPlotterChoicesBar_Params_Height(self, self.var_height)

    def to_graph_member(self):
        self.controller = ProPlotterController_Member(self)
        self.choices.params_w = ProPlotterChoicesBar_Params_Member(self)

    def open(self, *args):
        self.status.set_status('Open file...')
        selectedfilename = tkinter.filedialog.askopenfilename(title='Open filename', filetypes=FILETYPES)
        if len(selectedfilename) == 0:
            logger.info('No file selected. Ignore.')
            self.status.set_status('Open file... No file selected.')
            return None
        self.openbar.update_filename(selectedfilename)
        self.status.set_status('Opening file {}'.format(selectedfilename))
        self.fileobj = proreader.read_file(selectedfilename)
        if self.fileobj is not None:
            self.status.set_status('Successfully opened file {}'.format(selectedfilename))
        self.choices.variables_w.update()
        self.choices.point_w.update()
        if self.choices.params_w.use:
            self.choices.params_w.update()


class ProPlotterMenu(tk.Menu):
    """ The app menu """
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        # Menu 0
        self.filemenu = tk.Menu(self, tearoff=0)
        self.filemenu.add_command(label='Open', command=self.master.open)
        self.filemenu.add_command(label='Quit', command=self.master.quit)
        self.filemenu.add_separator()
        self.filemenu.add_command(label='Save graph', command=lambda x: None)
        self.add_cascade(label='File', menu=self.filemenu)
        # Menu 1
        self.typemenu = tk.Menu(self, tearoff=0)
        self.typemenu.add_command(label='Standard graph', command=self.master.to_graph_standard)
        self.typemenu.add_command(label='Massif graph', command=self.master.to_graph_massif)
        self.typemenu.add_command(label='Member graph', command=self.master.to_graph_member)
        self.typemenu.add_command(label='Height graph', command=self.master.to_graph_height)
        self.add_cascade(label='Graph type', menu=self.typemenu)
        # Menu 2
        self.langmenu = tk.Menu(self, tearoff=0)
        self.langmenu.add_command(label='French', command=self.to_French)
        self.langmenu.add_command(label='English', command=self.to_English)
        self.add_cascade(label='Change Language', menu=self.langmenu)
        # Config
        self.master.master.config(menu=self)

    def to_French():
        pass

    def to_English():
        pass


class ProPlotterOpenBar(tk.Frame):
    """The Frame for open buton and file path"""

    def __init__(self, master):
        """Initialize open bar """
        self.master = master
        super().__init__(master, height=100)
        self.pack(fill=tk.X)

        self.openbutton = tk.Button(self, text="Open File", command=self.open)
        self.openbutton.pack(side=tk.LEFT, padx=5)

        filename = self.master.fileobj.filename if self.master.fileobj is not None else '--'
        self.filename = tk.Label(self, text=filename)
        self.filename.pack(side=tk.LEFT)

    def open(self):
        self.master.open()

    def update_filename(self, filename):
        self.filename.configure(text=filename)


class ProPlotterChoicesBar(tk.Frame):
    """
    The lateral bar for choosing:
     * Parameters specific to representation
     * Variable selection
     * Point selection
    """

    WIDTH = 40

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

        self.params = tk.Frame(self.canvas)
        self.params.pack(fill=tk.X)
        self.variables = tk.Frame(self.canvas)
        self.variables.pack(fill=tk.X)
        self.point = tk.Frame(self.canvas)
        self.point.pack(fill=tk.X)

        self.params_w = None
        self.variables_w = ProPlotterChoicesBar_Variables(self, self.variables)
        self.point_w = ProPlotterChoicesBar_Point(self, self.point)


class ProPlotterChoicesBar_Variables:
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
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of variables', relief=tk.RAISED)
        self.label.pack()

        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_desc
            variables_list = [v['full_name'] if 'full_name' in v else k for k, v in self.variables_info.items()]

            variables_with_snl = [v['full_name'] if 'full_name' in v else k for k, v in
                                  self.master.master.fileobj.variables_snl.items()]

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

    def update_var_master(self, *args):
        value = self.choice_var_master.get()
        if value != self._var_master:
            self._var_master = value
            self.master.master.controls.plot_mark()

    def update_var_react(self, *args):
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
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterChoicesBar_Point:
    """
    Choice of points in the opened file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self.label = None
        self.variables_info = None
        self.lselectors = []
        self.llabels = []
        self.lvariables = []
        self.lf = []
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of point selectors\n(fill from top to bottom)', relief=tk.RAISED)
        self.label.pack(pady=5)

        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_selection_point
            for v, info in self.variables_info.items():
                label = tk.Label(self.frame, text=info['full_name'])
                label.pack()
                choices = list(info['choices'])  # Tkinter knows nothing of numpy arrays...
                ii = len(self.llabels)
                if info['type'] == 'choices':
                    selector = ttk.Combobox(self.frame, state='readonly', values=[''] + choices,
                                            width=self.master.WIDTH)
                    selector.bind('<<ComboboxSelected>>', lambda _, i=ii: self.update_var(i))
                elif info['type'] in ['int', 'float']:
                    sv = tk.StringVar()
                    selector = ttk.Spinbox(self.frame, values=choices, textvariable=sv,
                                           width=self.master.WIDTH, validate='focusout',
                                           validatecommand=lambda *_, i=ii: self.update_var_numeric(i))
                else:
                    selector = None  # TO BE CHECKED BY LEO
                selector.pack()
                self.llabels.append(label)
                self.lselectors.append(selector)
                self.lvariables.append(v)

    def get_selector(self):
        selector = {}
        for j in range(len(self.llabels)):
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
        for j in range(i+1):
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
        for j in range(i+1, len(self.llabels)):
            v = self.lvariables[j]
            ro = list(remaining_options[v])
            null_val = [''] if self.variables_info[v]['type'] == 'choices' else []
            self.lselectors[j].configure(values=null_val + ro)

    def update_var_numeric(self, i):
        # TODO: Check value or do it in previous function  <13-09-21, Léo Viallon-Galinier> #
        # We have to put the nearest value (or maybe not)
        self.update_var(i)

    def clean_frame(self):
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

        text = 'Some useful informations on what is currently plotted to be updated when relevant'
        self.infos = tk.Label(self, text=text)
        self.infos.pack(side=tk.LEFT)

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

        self.fig1, self.ax1 = plt.subplots(1, 1, sharex=True, sharey=True)
        self.ax2 = None
        self.ax3 = None
        self.first_profil = True
        self.Canevas = FigureCanvasTkAgg(self.fig1, self)

        self.update_idletasks()

        self.toberemoved = tk.Label(self, text='Plotting area')
        self.toberemoved.pack()

    def clear(self):
        self.toberemoved.destroy()
        self.Canevas.get_tk_widget().destroy()

    def ready_to_plot(self):
        self.fig1, self.ax1 = plt.subplots(1, 1, sharex=True, sharey=True)
        self.Canevas = FigureCanvasTkAgg(self.fig1, self)
        self.toberemoved = tk.Label(self, text='Plotting area')
        self.toberemoved.pack()

    def ready_to_plot_2_graphs(self, same_y=False):
        self.first_profil = True
        self.fig1, (self.ax1, self.ax2) = plt.subplots(ncols=2, gridspec_kw={'width_ratios': [2, 1]},
                                                       sharex=False, sharey=same_y)
        self.ax3 = self.ax2.twiny()
        self.Canevas = FigureCanvasTkAgg(self.fig1, self)
        self.toberemoved = tk.Label(self, text='Plotting area')
        self.toberemoved.pack()


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


class ProPlotterChoicesBar_Params_Standard:
    def __init__(self, master):
        self.use = False


class ProPlotterChoicesBar_Params_Height:
    """
     Specific choice for direction and height
     """

    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self.use = True
        self._direction = None
        self._height = None
        self.label = None
        self.label1 = None
        self.label2 = None
        self.choice_direction = None
        self.choice_height = None
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of direction and height', relief=tk.RAISED)
        self.label.pack()
        if self.master.fileobj is not None:
            direction_list = ['up', 'down']
            self.label1 = tk.Label(self.frame, text='Direction:')
            self.label1.pack()
            self.choice_direction = ttk.Combobox(self.frame, state='readonly', values=direction_list,
                                                 width=self.master.choices.WIDTH, )
            self.choice_direction.bind('<<ComboboxSelected>>', self.update_direction)
            self.choice_direction.pack()

            height_list = [i for i in range(5,100,5)]
            self.label2 = tk.Label(self.frame, text='Height:')
            self.label2.pack()
            self.choice_height = ttk.Combobox(self.frame, state='readonly', values=height_list,
                                              width=self.master.choices.WIDTH)
            self.choice_height.bind('<<ComboboxSelected>>', self.update_height)
            self.choice_height.pack()

    def update_direction(self, *args):
        value = self.choice_direction.get()
        if value != self._direction:
            self._direction = value

    def update_height(self, *args):
        value = self.choice_height.get()
        if value != self._height:
            self._height = value

    @property
    def var_direction(self):
        return self._direction

    @property
    def var_height(self):
        return self._height

    def clean_frame(self):
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterController(abc.ABC):
    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.plotting_function = None
        self.stop_right_click = False
        self.boolzoom = False
        self.width_rect = 0.01
        self.x_start_zoom = 0
        self.x_end_zoom = 0
        self.size_x = None
        self.rectangle_choix = None
        self.dataplot_master = None
        self.dztoplot = None
        self.topdz = None
        self.sizex = None
        self.timeplot = None
        self.dataplot_react = None
        self.grain = None
        self.ram = None

    def masterfig1d(self, **kwargs):
        return profilPlot.saison1d(**kwargs)

    def masterfigsaison(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def reactfig(self, **kwargs):
        return profilPlot.dateProfil(**kwargs)

    def get_data_master1d(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        return dict(ax=self.master.main.ax1, value=self.dataplot_master, list_date=self.timeplot)

    def get_data_mastersaison(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        return dict(ax=self.master.main.ax1, value=self.dataplot_master, list_date=self.timeplot, dz=self.dztoplot)

    def get_data_react(self):
        """
         Collecting datas for the reacting figure, depending of the choice for the graph type.
         """
        return dict(axe=self.master.main.ax2, axe2=self.master.main.ax3, ylimit=self.topdz,
                    cbar_show=self.master.main.first_profil)

    def plot(self):
        """
        The plot machinery
        """
        # ZOOM
        def button_press(event):
            if event.button > 1:
                self.stop_right_click = not self.stop_right_click
                return
            if event.inaxes == self.master.main.ax1:
                self.boolzoom = True
                self.x_start_zoom = int(event.xdata)
                bottom, top = self.master.main.ax1.get_ylim()
                self.rectangle_choix = self.master.main.ax1.add_patch(
                    matplotlib.patches.Rectangle((self.x_start_zoom, bottom), self.width_rect, top - bottom, alpha=0.1))
                self.master.main.Canevas.draw()

        def move_press(event):
            if self.stop_right_click:
                return
            if not self.boolzoom:
                return
            if event.inaxes == self.master.main.ax1:
                self.width_rect = abs(event.xdata - self.x_start_zoom)
                self.rectangle_choix.set_width(self.width_rect)
                if event.xdata - self.x_start_zoom < 0:
                    self.rectangle_choix.set_x(event.xdata)
                self.master.main.Canevas.draw()
                self.master.main.Canevas.flush_events()

        def button_release(event):
            if self.stop_right_click:
                return
            if event.button > 1:
                return
            if event.inaxes == self.master.main.ax1:
                self.width_rect = 0.01

                self.x_end_zoom = int(event.xdata)
                if self.x_start_zoom > self.x_end_zoom:
                    self.x_start_zoom, self.x_end_zoom = self.x_end_zoom, self.x_start_zoom

                self.dataplot_master = self.dataplot_master[self.x_start_zoom:self.x_end_zoom]
                self.dztoplot = self.dztoplot[self.x_start_zoom:self.x_end_zoom, :]
                self.size_x = self.dztoplot.shape[0]
                self.topdz = np.max(np.sum(self.dztoplot[:, :], 1))
                self.timeplot = self.timeplot[self.x_start_zoom:self.x_end_zoom]
                self.dataplot_react = self.dataplot_react[self.x_start_zoom:self.x_end_zoom, :]
                if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
                    self.grain = self.grain[self.x_start_zoom:self.x_end_zoom, :]
                if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
                    self.ram = self.ram[self.x_start_zoom:self.x_end_zoom, :]

                self.master.main.fig1.clear()
                self.master.main.clear()
                if not self.master.fileobj.variables_snl:
                    self.master.main.ready_to_plot()
                    self.master.main.toberemoved.destroy()
                    self.masterfig1d(**self.get_data_master1d())
                else:
                    if vartoplot_master['name'] in snl_names:
                        self.master.main.ready_to_plot_2_graphs(True)
                        self.master.main.toberemoved.destroy()
                        self.masterfigsaison(**self.get_data_mastersaison())
                    else:
                        self.master.main.ready_to_plot_2_graphs()
                        self.master.main.toberemoved.destroy()
                        self.masterfig1d(**self.get_data_master1d())

                self.master.main.Canevas.draw()
                self.master.main.Canevas.get_tk_widget().pack(fill=tk.BOTH, expand=True)
                self.boolzoom = False
                self.master.main.Canevas.mpl_connect('button_press_event', button_press)
                self.master.main.Canevas.mpl_connect('motion_notify_event', move_press)
                self.master.main.Canevas.mpl_connect('button_release_event', button_release)
                self.master.main.Canevas.mpl_connect('motion_notify_event', motion)

        # Graphe du profil animé à droite en fonction du mouvement de la souris
        def motion(event):
            if self.stop_right_click | self.boolzoom:
                return
            if event.inaxes == self.master.main.ax1:
                # print(event.xdata)
                # print(event.ydata)

                xindex = min(int(event.xdata), self.size_x-1)
                date = self.timeplot[xindex]
                data_date = self.dataplot_react[xindex, :]
                dz_date = self.dztoplot[xindex, :]
                if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
                    grain_date = self.grain[xindex, :]
                else:
                    grain_date = None

                if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
                    ram_date = self.ram[xindex, :]
                else:
                    ram_date = None

                if trace_hauteur is None:
                    hauteur = None
                else:
                    hauteur = event.ydata

                '''test_dz_date = self.master.fileobj.get_data(self.master.fileobj.variable_dz, point, fillnan=0.,
                                                            begin = int(event.xdata), end = int(event.xdata))'''

                self.master.main.ax2.clear()
                self.master.main.ax3.clear()

                dico = self.get_data_react()
                dico.update(dict(value=data_date, value_dz=dz_date, value_grain=grain_date, value_ram=ram_date,
                                 xlimit=limitplot_react, hauteur=hauteur, date=date))
                self.reactfig(**dico)

                self.master.main.first_profil = False
                self.master.main.Canevas.draw()
                self.master.main.Canevas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

        if self.master.fileobj is None:
            return
        var_master = self.master.choices.variables_w.var_master
        var_react = self.master.choices.variables_w.var_react
        selector = self.master.choices.point_w.get_selector()
        points = self.master.fileobj.get_points(selector=selector)

        if len(points) > 1:
            error_msg = "Too much points, please refine you selection"
            messagebox.showerror(title='Too much points', message=error_msg)
            return
        elif len(points) == 0:
            error_msg = "No points with current selection."
            messagebox.showerror(title='No point found', message=error_msg)
            return

        point = points[0]
        text = "Vars: {}/{}. Point: {}".format(var_master, var_react, point)
        self.master.controls.update_text(text)

        # TODO: Actually do the plot here <13-09-21, Léo Viallon-Galinier> #
        vartoplot_master = self.master.fileobj.variable_desc(var_master)
        self.dataplot_master = self.master.fileobj.get_data(vartoplot_master['name'], point)
        snl_names = [v['name'] if 'name' in v else k for k, v in self.master.fileobj.variables_snl.items()]
        self.timeplot = self.master.fileobj.get_time()

        if not self.master.fileobj.variables_snl:
            self.master.main.ready_to_plot()
            self.master.main.toberemoved.destroy()
            self.masterfig1d(**self.get_data_master1d())
        else:
            self.dztoplot = self.master.fileobj.get_data(self.master.fileobj.variable_dz, point, fillnan=0.)
            if vartoplot_master['name'] in snl_names:
                self.master.main.ready_to_plot_2_graphs(True)
                self.master.main.toberemoved.destroy()
                self.masterfigsaison(**self.get_data_mastersaison())
                trace_hauteur = 0
            else:
                self.master.main.ready_to_plot_2_graphs()
                self.master.main.toberemoved.destroy()
                self.masterfig1d(**self.get_data_master1d())
                trace_hauteur = None

            # usefull for motion
            vartoplot_react = self.master.fileobj.variable_desc(var_react)
            self.dataplot_react = self.master.fileobj.get_data(vartoplot_react['name'], point)
            limitplot_react = self.master.fileobj.limits_variable(vartoplot_react['name'])
            self.size_x = self.dztoplot.shape[0]
            self.topdz = np.max(np.sum(self.dztoplot[:, :], 1))
            if self.master.fileobj.variable_grain in self.master.fileobj.variables_t:
                self.grain = self.master.fileobj.get_data(self.master.fileobj.variable_grain, point, fillnan=0.)
            if self.master.fileobj.variable_ram in self.master.fileobj.variables_t:
                self.ram = self.master.fileobj.get_data(self.master.fileobj.variable_grain, point, fillnan=0.)
            if vartoplot_react['name'] in self.master.fileobj.variables_log:
                self.dataplot_react = np.where(self.dataplot_react > 10 ** (-10), self.dataplot_react, 10 ** (-10))
                self.dataplot_react = np.where(self.dataplot_react > 0, np.log10(self.dataplot_react), -10)
            self.master.main.Canevas.mpl_connect('motion_notify_event', motion)
            self.master.main.Canevas.mpl_connect('button_press_event', button_press)
            self.master.main.Canevas.mpl_connect('motion_notify_event', move_press)
            self.master.main.Canevas.mpl_connect('button_release_event', button_release)

        self.master.main.Canevas.draw()
        self.master.main.Canevas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

    def reset(self):
        """
        Reset the selection (and plot ?)
        """
        # TODO: tbd  <13-09-21, Léo Viallon-Galinier> #


class ProPlotterController_Standard(ProPlotterController):
    # idée: mettre en appel ProPlotterController(graphe_A, graphe_B, graphe_C) -> ce sont les graphes/champs d'entrées
    # qui changent...
    pass


class ProPlotterController_Height(ProPlotterController):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.direction = None
        self.height = None

    def masterfigsaison(self, **kwargs: dict):
        return profilPlot.heightplot(**kwargs)

    def get_data_mastersaison(self):
        """
        Collecting datas for the master figure, depending of the choice for the graph type.
        """
        self.direction = self.master.choices.params_w.var_direction
        self.height = self.master.choices.params_w.var_height
        return dict(ax=self.master.main.ax1, value=self.dataplot_master, list_date=self.timeplot, value_ep=self.dztoplot,
                    direction_cut=self.direction, height_cut=self.height)



def main(*args, **kwargs):
    root = tk.Tk()
    root.title('GUI PROreader CEN')
    root.geometry('1100x850')
    app = ProPlotterApplication(*args, master=root, **kwargs)
    app.mainloop()


if __name__ == "__main__":
    main()
