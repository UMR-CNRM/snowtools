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

from snowtools.plots.stratiprofile import proreader
from snowtools.plots.stratiprofile import proplotter_functions

from snowtools.plots.stratiprofile import gui_elements

logger = logging.getLogger()

# App constants:
FILETYPES = [('PRO files', '.nc'), ('all files', '.*')]


class ProCompareApplication(gui_elements.Application):
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
    def __init__(self, master=None, fileobjs=[], point=None, arguments: dict = {}):
        super().__init__(master)
        self.master = master
        self.fileobjs = fileobjs
        self.point = point
        self.n_files = max(2, len(self.fileobjs))
        self.pack(fill=tk.BOTH, expand=True)

        # Controls
        self.controller = ProCompareController(self)

        # Graphical elements
        self.menu = ProCompareMenu(self)
        self.choices = gui_elements.ProPlotterChoicesBar(self, n_variables=1)
        self.controls = gui_elements.ProPlotterControlsBar(self)
        self.openbar = ProCompareOpenBar(self)
        self.main = gui_elements.ProPlotterMain(self)
        self.status = gui_elements.ProPlotterStatus(self)

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
        if point is not None:
            set_v3 = self.choices.point_w.set_point(point)
        else:
            set_v3 = False

        if set_v1 and set_v3:
            self.plot()

        self.update_idletasks()

    def plot(self):
        self.controls.plot()

    def open(self, i, *args, **kwargs):
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
        fileobj = proreader.read_file(selectedfilename)
        if i < len(self.fileobjs):
            self.fileobjs[i] = fileobj
        else:
            i = len(self.fileobjs)
            self.fileobjs.append(fileobj)
        if fileobj is not None:
            self.status.set_status('Successfully opened file {}'.format(selectedfilename))
            self.open_update(i)

    def open_update(self, i):
        """
        Do the different update after successfully open a new file
        (i.e. once self.fileobj correctly set).
        """
        if self.fileobj is None:
            return
        self.openbar.update_filename(self.fileobjs[i].get_filename(), i)
        self.controls.plot_mark()
        if i == 0:
            self.choices.variables_w.update()
            self.choices.point_w.update()
            self.choices.addparams_w.update()
            if self.choices.params_w is not None:
                self.choices.params_w.update()
            self.controller.open_update()

    @property
    def fileobj(self):
        """
        Get one fileobject to initialize GUI elements that require access to some properties of the file objects.
        e.g. selection of point and variables.
        """
        for e in self.fileobjs:
            if e is not None:
                return e
        return None


class ProCompareMenu(tk.Menu):
    """ The app menu """
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        # Menu 0
        self.filemenu = tk.Menu(self, tearoff=0)
        self.filemenu.add_command(label='Quit', command=self.master.close_window)
        self.add_cascade(label='File', menu=self.filemenu)


class ProCompareOpenBar(tk.Frame):
    """The Frame for open button and file path"""

    def __init__(self, master):
        """Initialize open bar """
        super().__init__(master, height=50)
        self.master = master
        self.pack(fill=tk.X)

        self.frames = []
        self.init_bar()

    def init_bar(self):
        for i in range(self.master.n_files):
            frame = tk.Frame(self, width=400, height=50)
            frame.pack(side=tk.LEFT)
            frame.pack_propagate(False)

            def command(*args, **kwargs):
                return self.open(i)

            openbutton = tk.Button(frame, text="Open File", command=lambda i=i: self.open(i))
            openbutton.pack(side=tk.LEFT, padx=5)

            filename_ = self.master.fileobjs[i].get_filename() if len(self.master.fileobjs) > i and \
                self.master.fileobjs[i] is not None else '--'
            filename = tk.Label(frame, text=filename_)
            filename.pack(side=tk.LEFT, fill=tk.X)
            self.frames.append({'frame': frame, 'openbutton': openbutton, 'filename': filename, 'command': command})
        self.bind('<Configure>', self.update_wraplength)

    def update_wraplength(self, *args, **kwargs):
        for i in range(self.master.n_files):
            frame = self.frames[i]['frame']
            openbutton = self.frames[i]['openbutton']
            filename = self.frames[i]['filename']
            frame.configure(width=self.winfo_width() // 2)
            wraplength = self.winfo_width() // 2 - openbutton.winfo_width() - 15
            if wraplength > 0:
                filename.configure(wraplength=wraplength)

    def update_bar(self):
        self.destroy_bar()
        self.init_bar()

    def destroy_bar(self):
        for e in self.frames:
            e['frame'].pack_forget()
            e['frame'].destroy()
        self.frames = []

    def open(self, i):
        """
        If the Open button is clicked, launch the Open application
        """
        self.master.open(i)

    def update_filename(self, filename, i):
        """Get the name of opened file"""
        self.frames[i]['filename'].configure(text=filename)


class ProCompareController(abc.ABC):

    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.data = []
        self.vartoplot_master_desc = None
        self.master_xlim = None
        self.master_ylim = None

    def get_choice(self):
        """
        Check if there is only one point which is selected
        """
        selector = self.master.choices.point_w.get_selector()
        pointslist = []
        for i in range(self.master.n_files):
            if len(self.master.fileobjs) <= i or self.master.fileobjs[i] is None:
                pointslist.append(None)
                continue
            points = self.master.fileobjs[i].get_points(selector=selector)
            if len(points) > 1:
                error_msg = 'Too much points, please refine you selection'
                messagebox.showerror(title='Too much points', message=error_msg)
                return None
            elif len(points) == 0:
                error_msg = 'No points with current selection.'
                messagebox.showerror(title='No point found', message=error_msg)
                return None
            pointslist.append(points[0])
        return pointslist

    def get_additional_choices(self):
        selector = self.master.choices.addparams_w.get_selector()
        return selector

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

    def info_text_bar(self, point, additional_options=None):
        """
        Fill the information bar above the graph
        :param point: the number of the point chosen by the user
        """
        v_master = self.master.choices.variables_w.var_master
        text = 'VARIABLE: {}.'.format(v_master)
        if additional_options is not None:
            for key, value in additional_options.items():
                text += ' {}: {}'.format(key, value)
        self.master.controls.update_text(text)

    def plot(self):
        """
        The plot machinery
        """
        # In order to keep in memory the xlim and ylim after a zoom on master figure
        def change_xlim(event):
            self.master_xlim = event.get_xlim()

        def change_ylim(event):
            self.master_ylim = event.get_ylim()

        if self.master.fileobj is None:
            return

        var_info = self.check_var_info()
        if var_info is None:
            return

        # Point information
        # Note : possible different point for each file. Here, point is a list !
        point = self.get_choice()
        if point is None:
            return

        additional_options = self.get_additional_choices()

        self.info_text_bar(point, additional_options=additional_options)
        self.get_data(point, additional_options=additional_options)
        self.master.main.clear()
        
        # From matplotlib.pyplot.colorbar documentation:
        # the parameter fraction is the Fraction of original axes to use for colorbar
        # By default this is 0.15
        # So, a ratio of 85 for firsts figures and 100 for last one allows figure of same size after colorbar
        # Finally, in order to get some space between the graphs, ratio of 5 and 6 is OK. 
        ratio = int(self.master.n_files - 1) * [5] + [6]
        self.master.main.ready_to_plot(nb_graph=self.master.n_files, same_y=True, ratio=ratio, same_x=True, third_axis=False)

        # To ensure that the two graphs have the same colorbar, the value_min and value_max parameters are passed to
        # the plotting function
        add_params = {}
        value_min = None
        value_max = None
        for e in self.data:
            if e is not None:
                if not e['var_master_has_snl']:
                    break
                max_ = np.nanmax(e['dataplot_master'])
                min_ = np.nanmin(e['dataplot_master'])
                value_min = min(value_min, min_) if value_min is not None else min_
                value_max = max(value_max, max_) if value_max is not None else max_
        else:
            add_params['value_min'] = value_min
            add_params['value_max'] = value_max

        # Do the plots one by one from left to right
        for i in range(self.master.n_files):
            if self.data[i] is not None and self.data[i]['var_master_has_snl']:
                proplotter_functions.masterfig(**self.give_master_args(i),
                                               cbar_show=False if i < self.master.n_files - 1 else True)
            elif self.data[i] is not None:
                proplotter_functions.masterfig(**self.give_master_args(i))

        # Apply previous zoom if relevant and store zoom information
        if self.master_xlim is not None and self.master_ylim is not None:
            self.master.main.toolbar.push_current()
            self.master.main.ax['ax1'].set_xlim(self.master_xlim)
            self.master.main.ax['ax1'].set_ylim(self.master_ylim)

        self.master.main.ax['ax1'].callbacks.connect('xlim_changed', change_xlim)
        self.master.main.ax['ax1'].callbacks.connect('ylim_changed', change_ylim)

        # Transfer to GUI interface
        self.master.main.update()

    def get_data(self, point, additional_options=None):
        """
        Collecting datas for figures, before subsetting with motions
        """
        self.data = []
        for i in range(self.master.n_files):
            if len(self.master.fileobjs) <= i or self.master.fileobjs[i] is None:
                self.data.append(None)
                continue
            data = proplotter_functions.get_data(self.master.fileobjs[i], point[i],
                                                 var_master=self.vartoplot_master_desc['name'],
                                                 additional_options=additional_options)
            self.data.append(data)

    def give_master_args(self, i):
        """
         Returns dictionary for the master figure, depending on the choice for the graph type.
        """
        return proplotter_functions.give_master_args_std(self.master.main.ax['ax{}'.format(i + 1)], self.data[i])

    def reset(self):
        """
        Reset the selection (and plot)
        """
        for widget in self.master.choices.point_w.lselectors:
            widget.set('')
        self.master.choices.variables_w.choice_var_master.set('')
        self.master.main.clear()

    def open_update(self):
        """
        Update to do to specific components when a new file is opened.
        """
        self.master_xlim = None
        self.master_ylim = None


def main(*args, **kwargs):
    NAME = 'GUI PROcompare'
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

    app = ProCompareApplication(*args, master=root, **kwargs)
    app.mainloop()


if __name__ == "__main__":
    main()
