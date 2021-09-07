# -*- coding: utf-8 -*-

import abc
import tkinter as tk
import tkinter.filedialog
import logging

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

        # TODO: call to_graph_something depending on self.type  <07-09-21, Léo Viallon-Galinier> #

        # Keyboard shortcuts
        self.master.bind('<Control-o>', self.open)
        self.master.bind('<Escape>', self.close_window)
        self.master.bind('<Control-q>', self.close_window)

    def close_window(self, *args, **kwargs):
        self.master.destroy()

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
        # TODO: To be adjusted:  <07-09-21, Léo Viallon-Galinier> #
        # * If filename is none ask, else do not open dialog
        # * Reload self.fileobj
        # * self.choices.variables_w.update()
        # * self.choices.point_w.update()
        raise NotImplementedError('NYI')


class ProPlotterMenu(tk.Menu):
    """ The app menu """
    def __init__(self, master):
        self.master = master
        super().__init__(master)
        # Menu 0
        self.filemenu = tk.Menu(self, tearoff=0)
        self.filemenu.add_command(label='Open', command=self.master.open)
        self.filemenu.add_command(label='Quit', command=self.master.quit)
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

        filename = self.master.fileobj.filename if self.master.fileobj is not None else 'No file open'
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
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of variables')
        self.label.pack()
        # TODO: Use self.master.master.fileobj to create list of variables  <07-09-21, Léo Viallon-Galinier> #

    def clean_frame(self):
        for widgets in self.frame.winfo_children():
            widgets.destroy()


class ProPlotterChoicesBar_Point():
    """
    Choice of variables in the opened file
    """
    def __init__(self, master, frame):
        self.master = master
        self.frame = frame
        self.update()

    def update(self):
        self.clean_frame()
        self.label = tk.Label(self.frame, text='Choice of point selectors')
        self.label.pack()
        # TODO: Use self.master.master.fileobj to create list of point selector  <07-09-21, Léo Viallon-Galinier> #

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
        # TODO: Call controller plotting function  <07-09-21, Léo Viallon-Galinier> #
        pass

    def reset(self):
        # TODO: Call controller reset function  <07-09-21, Léo Viallon-Galinier> #
        pass

    # TODO: Add functions to change plotbutton color and update text (called by controller)  <07-09-21, LVG> #


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


class ProPlotterController(abc.ABC):
    def __init__(self, master):
        """
        Abstract method that define the applicative logic
        """
        self.master = master
        self.plotting_function = None


def main(*args, **kwargs):
    root = tk.Tk()
    root.title('GUI PROreader CEN')
    root.geometry('900x700')
    app = ProPlotterApplication(*args, master=root, **kwargs)
    app.mainloop()


if __name__ == "__main__":
    main()
