# -*- coding: utf-8 -*-

"""
GUI Application for proplotter

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
from tkinter import ttk
from tkinter import messagebox

from snowtools.plots.stratiprofile import proreader
from snowtools.plots.stratiprofile import profilPlot
from snowtools.plots.stratiprofile import proplotter_functions

from snowtools.plots.stratiprofile import gui_elements

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
    def __init__(self, master=None, fileobj: proreader.reader = None, point=None, arguments: dict = {}):
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
        self.choices = gui_elements.ProPlotterChoicesBar(self)
        self.controls = gui_elements.ProPlotterControlsBar(self)
        self.main = gui_elements.ProPlotterMain(self)
        self.status = gui_elements.ProPlotterStatus(self)

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
        elif typ == 'escroc profil':
            self.to_graph_escroc_profil()
        elif typ == 'escroc saison':
            self.to_graph_escroc_saison()
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
        self.choices.params_w = ProPlotterChoicesBarParamsStandard(self, self.choices.params)
        self.controller = ProPlotterControllerStandard(self)

    def to_graph_multiple_profil(self):
        """
        Reset then launch Controller and Choices Bar Parameter for multiple graph with profil on the right
        """
        self.to_graph_reset()
        self.choices.params_w = ProPlotterChoicesBarParamsMultiple(self, self.choices.params)
        self.controller = ProPlotterControllerMultipleProfil(self)

    def to_graph_multiple_saison(self):
        """
        Reset then launch Controller and Choices Bar Parameter for multiple graph with seasonal graph on the right
        """
        self.to_graph_reset()
        self.choices.params_w = ProPlotterChoicesBarParamsMultiple(self, self.choices.params)
        self.controller = ProPlotterControllerMultipleSaison(self)

    def to_graph_height(self):
        """
        Reset then launch Controller and Choices Bar Parameter for height graph
        """
        self.to_graph_reset()
        self.choices.params_w = ProPlotterChoicesBarParamsHeight(self, self.choices.params)
        self.controller = ProPlotterControllerHeight(self)

    def to_graph_escroc_profil(self):
        """
        Reset then launch Controller and Choices Bar Parameter for Meteo France ensemble graph with profil on the right
        """
        self.to_graph_reset()
        self.choices.params_w = ProPlotterChoicesBarParamsEscroc(self, self.choices.params)
        self.controller = ProPlotterControllerEscrocProfil(self)

    def to_graph_escroc_saison(self):
        """
        Reset then launch Controller and Choices Bar Parameter for Meteo France ensemble graph with seasonal graph on
        the right
        """
        self.to_graph_reset()
        self.choices.params_w = ProPlotterChoicesBarParamsEscroc(self, self.choices.params)
        self.controller = ProPlotterControllerEscrocSaison(self)

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
        self.choices.addparams_w.update()
        self.choices.params_w.update()
        self.controller.open_update()

    def open_user_guide(self):
        """
        Opening the help window with text inside
        """
        messagebox.showinfo('User Guide', 'This software is a visualisation tool for PRO.nc files coming'
                                          'from our snow model CROCUS embedded in the SURFEX model. \n \n'
                                          'There are several types of graphs:\n'
                                          '- usual graph: click the Open File button. Select a PRO file. Choose the '
                                          'variables you want and then the point. \n \n '
                                          '- the height graph is made to follow a parameter in a specified place. For '
                                          'example, 5 cm below the snow surface. \n \n'
                                          '- if you want to compare several points in your simulation, try the multiple'
                                          ' plot. Just have let free a part of the choices for point selection. \n \n'
                                          '\n \n'
                                          '- in the Meteo France ensemble simulation ESCROC, there is the possibility'
                                          'to have several members for a simulation. In that case, choose escroc plot.'
                                          '\n'
                                          'Feel free to contact us at crocus@meteo.fr if you have some questions.\n'
                                          '\n'
                                          'More informations are available on \n'
                                          'https://opensource.umr-cnrm.fr/projects/snowtools_git/wiki'

                            )

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
        self.typemenu.add_command(label='Escroc Profil', command=self.master.to_graph_escroc_profil)
        self.typemenu.add_command(label='Escroc Saison', command=self.master.to_graph_escroc_saison)
        self.typemenu.add_command(label='Height', command=self.master.to_graph_height)
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

    def update_height(self):
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
     Specific choice via adding a Date Slicer (for Multiple Plots or Meteo France ensemble Plots for example)
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


class ProPlotterChoicesBarParamsEscroc(ProPlotterChoicesBarParamsDateSlicer):
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self.update()


class ProPlotterChoicesBarParamsMultiple(ProPlotterChoicesBarParamsDateSlicer):
    def __init__(self, master, frame):
        super().__init__(master, frame)
        self.update()


class ProPlotterController(abc.ABC):
    GRAPH_NAME = 'Graph'

    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.data = {}
        self.vartoplot_master_desc = None
        self.vartoplot_react_desc = None
        self.stop_right_click = False
        self.x_legend = None
        self.hauteur = None
        self.master_xlim = None
        self.master_ylim = None
        self.ratio = [2, 1]
        """Ratio of the different figures to plot (when several subplots on the graph)"""

        self.open_update()

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
        text = '{}\nVARIABLES: {}/{}.\nPOINT: {}'.format(self.GRAPH_NAME, v_master, v_react, point)
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
        if self.vartoplot_react_desc is None:
            error_msg = 'Please choose a variable for profil plot'
            messagebox.showerror(title='Missing variable', message=error_msg)
            return None

        return 'OK'

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        """
        self.data = proplotter_functions.get_data(self.master.fileobj, point,
                                                  var_master=self.vartoplot_master_desc['name'],
                                                  var_react=self.vartoplot_react_desc['name'] if
                                                  self.vartoplot_react_desc is not None else None,
                                                  additional_options=additional_options)

    def give_master_args(self):
        """
         Returns dictionary for the master figure, depending on the choice for the graph type.
        """
        return proplotter_functions.give_master_args_std(self.master.main.ax['ax1'], self.data)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending on the choice for the graph type.
         """
        xindex = max(min(int(x_event), self.data['timeplot'].size - 1), 0)
        return proplotter_functions.give_react_args_std(self.master.main.ax['ax2'], self.master.main.ax['ax3'],
                                                        self.data, xindex, hauteur=self.hauteur,
                                                        plot_colorbar=self.master.main.first_profil)

    def masterfig(self, **kwargs):
        return proplotter_functions.masterfig(**kwargs)

    def reactfig(self, **kwargs):
        return profilPlot.dateProfil(**kwargs)

    def plot(self):
        """
        The plot machinery
        """
        # Stop the servant graph with right click on the mouse
        def button_press(event):
            if event.button > 1:
                self.stop_right_click = not self.stop_right_click
                return

        # Animation (or not) of the servant graph depending on the position of
        # the mouse in master graph
        def motion(event):
            if self.stop_right_click:
                return
            if event.inaxes == self.master.main.ax['ax1']:
                if self.data['var_master_has_snl']:
                    self.hauteur = event.ydata
                else:
                    self.hauteur = None

                plot_react(event.xdata)
                self.master.main.update()

        # Plot the servant graph
        def plot_react(i):
            self.master.main.ax['ax2'].clear()
            self.master.main.ax['ax3'].clear()
            dico = self.give_react_args(i)
            self.reactfig(**dico)
            self.master.main.first_profil = False

        # In order to keep in memory the xlim and ylim after a zoom on master figure
        def change_xlim(event):
            self.master_xlim = event.get_xlim()

        def change_ylim(event):
            self.master_ylim = event.get_ylim()

        # Checking existence of a selection, otherwise get out
        if self.master.fileobj is None:
            return

        var_info = self.check_var_info()
        if var_info is None:
            return

        point = self.get_choice()
        if point is None:
            return

        # Get infos and prepare graph
        additional_options = self.get_additional_choices()
        self.info_text_bar(point, additional_options=additional_options)
        self.get_data(point, additional_options=additional_options)
        self.master.main.clear()

        if self.vartoplot_react_desc is None:
            self.master.main.ready_to_plot(same_y=False, nb_graph=1, ratio=None,
                                           same_x=False, third_axis=False)
        else:
            self.master.main.ready_to_plot(same_y=self.data['var_master_has_snl'],
                                           nb_graph=2, ratio=self.ratio,
                                           same_x=False, third_axis=True)
            if not self.data['var_master_has_snl']:
                self.master.main.ax['ax2'].set_ylim(0, self.data['ymax_react'])

        # Make graph
        self.masterfig(**self.give_master_args())
        if self.vartoplot_react_desc is not None:
            # Not optimal -> usefull for standard right plot (ie date profile) but not for season
            plot_react(0)
        if self.master_xlim is not None and self.master_ylim is not None:
            self.master.main.toolbar.push_current()
            self.master.main.ax['ax1'].set_xlim(self.master_xlim)
            self.master.main.ax['ax1'].set_ylim(self.master_ylim)

        # Management of graph interaction: moving servant graph, stopping, keeping zoom limits
        self.master.main.cid['motion'] = self.master.main.Canevas.mpl_connect('motion_notify_event', motion)
        self.master.main.cid['right_click'] = self.master.main.Canevas.mpl_connect('button_press_event',
                                                                                   button_press)
        self.master.main.ax['ax1'].callbacks.connect('xlim_changed', change_xlim)
        self.master.main.ax['ax1'].callbacks.connect('ylim_changed', change_ylim)

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

    def open_update(self):
        """
        Update to do to specific components when a new file is opened.
        """
        self.master_xlim = None
        self.master_ylim = None


class ProPlotterControllerStandard(ProPlotterController):
    pass


class ProPlotterControllerHeight(ProPlotterController):
    GRAPH_NAME = 'Height graph'

    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.hauteur = None

    def check_var_info(self):
        """
        Check if there is a choice for variable(s) to plot
        """
        v_master = self.master.choices.variables_w.var_master
        self.vartoplot_master_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_master)
        self.vartoplot_react_desc = self.master.fileobj.variable_desc(self.master.choices.variables_w.var_react)
        if self.vartoplot_master_desc is None:
            error_msg = 'Please choose a variable to plot'
            messagebox.showerror(title='Missing variable', message=error_msg)
            return None
        if self.vartoplot_react_desc is None:
            error_msg = 'Please choose a variable for profil plot'
            messagebox.showerror(title='Missing variable', message=error_msg)
            return None
        if not self.master.fileobj.variable_desc(v_master)['has_snl']:
            error_msg = 'Please choose a variable with Snow Layer Dimension for Height Graph'
            messagebox.showerror(title='Wrong variable', message=error_msg)
            return None

        return 'OK'

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        """
        self.master.choices.params_w.update_direction()
        self.master.choices.params_w.update_height()
        self.data = proplotter_functions.get_data(self.master.fileobj, point,
                                                  var_master=self.vartoplot_master_desc['name'],
                                                  var_react=self.vartoplot_react_desc['name'] if
                                                  self.vartoplot_react_desc is not None else None,
                                                  direction_cut=self.master.choices.params_w.var_direction,
                                                  height_cut=self.master.choices.params_w.var_height,
                                                  additional_options=additional_options)

    def give_master_args(self):
        """
         Returns dictionary for the master figure, depending on the choice for the graph type.
        """
        return proplotter_functions.give_master_args_height(self.master.main.ax['ax1'], self.data)

    def masterfig(self, **kwargs):
        return proplotter_functions.masterfig_height(**kwargs)

    def reset(self):
        super().reset()
        self.master.choices.params_w.choice_direction.set('')
        self.master.choices.params_w.choice_height.delete(0, self.master.choices.WIDTH)


class ProPlotterControllerSlider(ProPlotterController):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        # initialisation dateslice to None is giving a problem for the first title
        self.dateslice = 0

    def update_slice_date(self, *args):
        """
        Redraw the ax1 graph and clear ax2 and ax3
        """
        self.dateslice = self.master.choices.params_w.scale_date.get()
        self.master.main.first_master = False

        if 'ax1' in self.master.main.ax and self.master.main.ax['ax1'] is not None:
            self.master.main.ax['ax1'].clear()
        if 'ax2' in self.master.main.ax and self.master.main.ax['ax2'] is not None:
            self.master.main.ax['ax2'].clear()
        if 'ax3' in self.master.main.ax and self.master.main.ax['ax3'] is not None:
            self.master.main.ax['ax3'].clear()

        proplotter_functions.masterfig(**self.give_master_args())

        self.master.main.update()


class ProPlotterControllerEscroc(ProPlotterControllerSlider):
    def __init__(self, master):
        self.master = master
        super().__init__(master)

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        In this case shape = nb_membres, time, snowlayer
        """
        self.data = proplotter_functions.get_data_escroc(self.master.fileobj, point,
                                                         var_master=self.vartoplot_master_desc['name'],
                                                         var_react=self.vartoplot_react_desc['name'] if
                                                         self.vartoplot_react_desc is not None else None,
                                                         additional_options=additional_options)

    def open_update(self):
        """
        Update the date scaler after opening a file.
        """
        if self.master.fileobj is not None:
            self.master.choices.params_w.scale_date.config(from_=0, to=(len(self.master.fileobj.get_time()) - 1),
                                                           state='normal', showvalue=0, variable=tk.IntVar,
                                                           command=self.update_slice_date)

    def give_master_args(self):
        """
         Returns dictionary for the master figure, depending on the choice for the graph type.
        """

        return proplotter_functions.give_master_args_escroc(self.master.main.ax['ax1'], self.data, self.dateslice,
                                                            self.master.main.first_master)


class ProPlotterControllerEscrocProfil(ProPlotterControllerEscroc):
    def __init__(self, master):
        self.master = master
        super().__init__(master)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending on the choice for the graph type.
         """
        xindex = max(min(int(x_event), self.data['dztoplot'].shape[0] - 1), 0)

        return proplotter_functions.give_react_args_escroc_profil(self.master.main.ax['ax2'],
                                                                  self.master.main.ax['ax3'],
                                                                  self.data, xindex, self.dateslice,
                                                                  hauteur=self.hauteur,
                                                                  plot_colorbar=self.master.main.first_profil)


class ProPlotterControllerEscrocSaison(ProPlotterControllerEscroc):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.ratio = [1, 1]

    def reactfig(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending on the choice for the graph type.
         """
        # .shape[0] because shape = nb_membres, time, snowlayer
        xindex = max(min(int(x_event), self.data['dztoplot'].shape[0] - 1), 0)

        return proplotter_functions.give_react_args_escroc_saison(self.master.main.ax['ax2'], self.data, xindex,
                                                                  plot_colorbar=self.master.main.first_profil)


class ProPlotterControllerMultiple(ProPlotterControllerSlider):
    GRAPH_NAME = "Multiple Graph"

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

    def get_data(self, liste_points, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        In this case shape = time, snowlayer, nb_points
        """
        self.data = proplotter_functions.get_data_multiple(self.master.fileobj, liste_points,
                                                           var_master=self.vartoplot_master_desc['name'],
                                                           var_react=self.vartoplot_react_desc['name'] if
                                                           self.vartoplot_react_desc is not None else None,
                                                           additional_options=additional_options)

    def open_update(self):
        """
        Update the date scaler after opening a file.
        """
        if self.master.fileobj is not None:
            self.master.choices.params_w.scale_date.config(from_=0, to=(len(self.master.fileobj.get_time()) - 1),
                                                           state='normal', showvalue=0, variable=tk.IntVar,
                                                           command=self.update_slice_date)

    def give_master_args(self):
        """
         Returns dictionary for the master figure, depending on the choice for the graph type.
        """

        return proplotter_functions.give_master_args_multiple(self.master.main.ax['ax1'], self.data, self.dateslice,
                                                              self.master.main.first_master)


class ProPlotterControllerMultipleProfil(ProPlotterControllerMultiple):
    def __init__(self, master):
        self.master = master
        super().__init__(master)

    def give_react_args(self, x_event):
        """
        Collecting datas for the reacting figure, depending on the choice for the graph type.
        """
        # .shape[2] because shape = time, snowlayer, nb_points
        xindex = max(min(int(x_event), self.data['dztoplot'].shape[2] - 1), 0)

        return proplotter_functions.give_react_args_multiple_profil(self.master.main.ax['ax2'],
                                                                    self.master.main.ax['ax3'],
                                                                    self.data, xindex, self.dateslice,
                                                                    hauteur=self.hauteur,
                                                                    plot_colorbar=self.master.main.first_profil)


class ProPlotterControllerMultipleSaison(ProPlotterControllerMultiple):
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        self.ratio = [1, 1]

    def reactfig(self, **kwargs: dict):
        return profilPlot.saisonProfil(**kwargs)

    def give_react_args(self, x_event):
        """
         Collecting datas for the reacting figure, depending on the choice for the graph type.
         """
        # .shape[2] because shape = time, snowlayer, nb_points
        xindex = max(min(int(x_event), self.data['dztoplot'].shape[2] - 1), 0)

        return proplotter_functions.give_react_args_multiple_saison(self.master.main.ax['ax2'], self.data, xindex,
                                                                    plot_colorbar=self.master.main.first_profil)


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
    except Exception:
        pass

    app = ProPlotterApplication(*args, master=root, **kwargs)
    app.mainloop()


if __name__ == "__main__":
    main()
