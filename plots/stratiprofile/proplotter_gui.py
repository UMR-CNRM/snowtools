# -*- coding: utf-8 -*-

import abc
import tkinter as tk
import tkinter.filedialog
from tkinter import ttk
import logging

import numpy as np

from snowtools.plots.stratiprofile import proreader

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
        self.controller = ProPlotterController_Height(self)
        self.choices.params_w = ProPlotterChoicesBar_Params_Height(self)

    def to_graph_member(self):
        self.controller = ProPlotterController_Member(self)
        self.choices.params_w = ProPlotterChoicesBar_Params_Member(self)

    def open(self, *args):
        selectedfilename = tkinter.filedialog.askopenfilename(title='Open filename', filetypes=FILETYPES)
        if len(selectedfilename) == 0:
            logger.info('No file selected. Ignore.')
            return None
        self.openbar.update_filename(selectedfilename)
        self.fileobj = proreader.read_file(selectedfilename)
        self.choices.variables_w.update()
        self.choices.point_w.update()


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


class ProPlotterChoicesBar_Variables():
    """
    Choice of variables in the opened file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self._var_1 = None
        self._var_2 = None
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of variables', relief=tk.RAISED)
        self.label.pack()

        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_desc
            variables_list = [v['full_name'] if 'full_name' in v else k for k, v in self.variables_info.items()]

            self.label1 = tk.Label(self.frame, text='Variable:')
            self.label1.pack()
            self.choice_var_1 = ttk.Combobox(self.frame, state='readonly', values=variables_list,
                                             width=self.master.WIDTH, )
            self.choice_var_1.bind('<<ComboboxSelected>>', self.update_var_1)
            self.choice_var_1.pack()
            self.label2 = tk.Label(self.frame, text='Variable profil:')
            self.label2.pack()
            self.choice_var_2 = ttk.Combobox(self.frame, state='readonly', values=variables_list,
                                             width=self.master.WIDTH)
            self.choice_var_2.bind('<<ComboboxSelected>>', self.update_var_2)
            self.choice_var_2.pack()

    def update_var_1(self):
        value = self.choice_var_1.get()
        if value != self._var_1:
            self._var_1 = value
            self.master.master.controls.plot_mark()

    def update_var_2(self):
        value = self.choice_var_2.get()
        if value != self._var_2:
            self._var_2 = value
            self.master.master.controls.plot_mark()

    @property
    def var_1(self):
        return self._var_1

    @property
    def var_2(self):
        return self._var_2

    def clean_frame(self):
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterChoicesBar_Point():
    """
    Choice of points in the opened file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of point selectors\n(fill from top to bottom)', relief=tk.RAISED)
        self.label.pack(pady=5)
        self.lselectors = []
        self.llabels = []
        self.lvariables = []
        self.lf = []
        if self.master.master.fileobj is not None:
            self.variables_info = self.master.master.fileobj.variables_selection_point
            for v, info in self.variables_info.items():
                label = tk.Label(self.frame, text=info['full_name'])
                label.pack()
                choices = list(info['choices'])  # Tkinter knows nothing of numpy arrays...
                ii = len(self.llabels)
                if info['type'] == 'choices':
                    selector = ttk.Combobox(self.frame, state='readonly', values=choices,
                                            width=self.master.WIDTH)
                    selector.bind('<<ComboboxSelected>>', lambda _, i=ii: self.update_var(i))
                elif info['type'] in ['int', 'float']:
                    sv = tk.StringVar()
                    selector = ttk.Spinbox(self.frame, values=choices, textvariable=sv,
                                           width=self.master.WIDTH, validate='focusout',
                                           validatecommand=lambda *_, i=ii: self.update_var_numeric(i))
                    # Trace + check do not execute if string is vide.
                    # validate='focusout', validatecommand=lambda *_: self
                    # sv.bind('w', lambda *_: self.update_var(len(self.llabels)))
                selector.pack()
                self.llabels.append(label)
                self.lselectors.append(selector)
                self.lvariables.append(v)

    def update_var(self, i):
        """
        run at each modification of a widget:

        * Set 1st acceptable value for previous widgets if they have not
          already an acceptable value
        * Reduce the choices for following widgets
        """
        self.master.master.controls.plot_mark()
        selector = {}
        for j in range(i):
            v = self.lvariables[j]
            val = self.lselectors[j].get()
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
            self.lselectors[j].configure(values=ro)
            if v in selector and selector[v] in ro:
                continue
            elif v != 'point' and (len(remaining_options[v]) == 1 or
                                   len(remaining_options[v]) > 0 and v not in selector):
                self.lselectors[j].set(remaining_options[v][0])

    def update_var_numeric(self, i):
        # TODO: Check value or do it in previous function  <13-09-21, Léo Viallon-Galinier> #
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
        pass


class ProPlotterController(abc.ABC):
    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.plotting_function = None

    def plot(self):
        """
        The plot machinery
        """
        # TODO: tbd  <13-09-21, Léo Viallon-Galinier> #

    def reset(self):
        """
        Reset the selection (and plot ?)
        """
        # TODO: tbd  <13-09-21, Léo Viallon-Galinier> #


class ProPlotterController_Standard(ProPlotterController):
    pass


def main(*args, **kwargs):
    root = tk.Tk()
    root.title('GUI PROreader CEN')
    root.geometry('900x700')
    app = ProPlotterApplication(*args, master=root, **kwargs)
    app.mainloop()


if __name__ == "__main__":
    main()
