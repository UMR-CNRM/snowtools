# -*- coding: utf-8 -*-

"""
Created on 4 déc. 2018

This script contains classes allowing to make matplotlib graphs with an automatic formatting of the temporal axis in
order to adapt to simulation temporal extent.

@author: lafaysse
"""

from matplotlib import pyplot as plt
from matplotlib.dates import HourLocator, DayLocator, MonthLocator, YearLocator, DateFormatter

from snowtools.plots.abstracts.figures import Mplfigure


class temporalplot_abstract(Mplfigure):

    def __init__(self, **kwargs):
        """Constructor for temporalplot_abstract class, redefinied for the child class"""
        super(temporalplot_abstract, self).__init__(**kwargs)
        self.plot = None
        #self.draw = None

    def finalize(self, timeOut, **kwargs):

        self.set_xaxis(timeOut)
        self.set_yaxis(**kwargs)
        kwargs.setdefault("fontsize", "x-small")
        self.plot.legend(loc="upper left", fontsize=kwargs["fontsize"])

    def set_yaxis(self, **kwargs):

        if 'forcemin' in kwargs.keys() and 'forcemax' in kwargs.keys():
            self.plot.set_ylim([kwargs['forcemin'], kwargs['forcemax']])
        self.plot.set_ylim(top=self.plot.get_ylim()[1] * 1.05)

        if 'ylabel' in kwargs.keys():
            label = kwargs.pop('ylabel')
            if 'ycolor' in kwargs.keys():
                color = kwargs['ycolor']
            else:
                color = 'black'
            self.plot.set_ylabel(label, color=color)

#    def draw(self, timeOut, *args, **kwargs):
#        self.finalize(timeOut, **kwargs)

    def add_line(self, timeOut, varOut, **kwargs):

        linesargs = {}
        for key, value in kwargs.items():
            if key not in ['forcemin', 'forcemax', 'ylabel', 'fillcolor']:
                linesargs[key] = value

        self.set_default("fmt", "-", linesargs)
        self.set_default("color", "red", linesargs)
        self.plot.plot_date(timeOut, varOut, **linesargs)

    def add_points(self, timeOut, varOut, **kwargs):
        self.set_default("fmt", "s", kwargs)
        self.set_default("color", "black", kwargs)
        self.set_default("markersize", 3, kwargs)
        self.add_line(timeOut, varOut, **kwargs)

    def set_default(self, varname, value, kwargs):
        if varname not in kwargs.keys():
            kwargs[varname] = value

    def set_xaxis(self, timeOut):

        duration = timeOut[-1] - timeOut[0]
        ndays = duration.days

        if ndays <= 2:
            self.plot.xaxis.set_major_locator(HourLocator([0, 6, 12, 18]))
            formatDate = '%d/%m\n%Hh'
        if ndays <= 4:
            self.plot.xaxis.set_major_locator(HourLocator([0, 12]))
            formatDate = '%d/%m\n%Hh'
        elif ndays <= 10:
            self.plot.xaxis.set_major_locator(DayLocator(range(1, 31, 1)))
            formatDate = '%d %b\n%Y'
        elif ndays <= 30:
            self.plot.xaxis.set_major_locator(DayLocator(range(1, 31, 3)))
            formatDate = '%d %b\n%Y'
        elif ndays <= 90:
            self.plot.xaxis.set_major_locator(DayLocator([1, 11, 21]))
            formatDate = '%d %b\n%Y'
        elif ndays <= 366:
            self.plot.xaxis.set_major_locator(MonthLocator(range(1, 13, 2), 1))
            formatDate = '%d %b'
        elif ndays <= (5 * 365):
            interval = int(ndays / 365) + 1
            self.plot.xaxis.set_major_locator(MonthLocator(range(1, 13, interval), 1))
            formatDate = '%d %b\n%Y'
        elif ndays >= (30 * 365):
            interval = int(ndays / 365) + 1
            self.plot.xaxis.set_major_locator(YearLocator(interval))
            formatDate = '%Y'
        else:
            interval = int(ndays / 365) + 1
            self.plot.xaxis.set_major_locator(YearLocator(interval))
            formatDate = '%Y'

        self.plot.xaxis.set_major_formatter(DateFormatter(formatDate))

    def save(self, *args, **kwargs):
        super(temporalplot_abstract, self).save(*args, **kwargs)
        self.plot.cla()


class temporalplot(temporalplot_abstract):

    figsize = (5, 4)

    def __init__(self,  *args, **kwargs):
        super(temporalplot, self).__init__(**kwargs)
        self.fig = plt.figure(figsize=self.figsize)
        self.plot = plt.subplot(111)


class temporalsubplot(temporalplot_abstract):
    def __init__(self, subplots, i, j, **kwargs):
        super(temporalsubplot, self).__init__(**kwargs)
        if hasattr(subplots, "shape"):
            if len(subplots.shape) == 2:
                self.plot = subplots[i, j]
            elif len(subplots.shape) == 1:
                self.plot = subplots[i * j]
            else:
                raise Exception
        else:
            self.plot = subplots


class temporalplotSim(temporalplot):
    """
    Class for 1D variable temporal plot

    Example :

    .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.temporal.chrono import temporalplotSim

        # read data
        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_LaPlagne_2000-2001.nc') as p:
            sd = p.read('DSN_T_ISBA')
            sd2 = p.read('SNOWTEMP')
            time = p.readtime()
        plot = temporalplotSim()
        plot.draw(time, sd,label='prout')
        plot.finalize(time) # pour gérer les dates correctement
        plot.save('EX-temporalplotSim.png')

    .. figure:: /images/EX-temporalplotSim.png
        :align: center

    """
    figsize=(5, 4)

    def draw(self, timeSim, varSim, *args, **kwargs):
        """
        Method of temporalplotSim class. Used for generating a 1D plot with time formatting.

        :param timeSim: Array of simulation duration usually given by prosimu.readtime()
        :param varSim: Array of simulation data usially given by prosimu.read()
        :param args: (optional)
        :param kwargs: optional ( matplotlib.pyplot key arguments )
        :return:
        """
        if 'label' in kwargs.keys():
            label = kwargs.pop('label')
        else:
            label = 'S2M'
        self.add_line(timeSim, varSim, label=label, **kwargs)
        self.finalize(timeSim, **kwargs)


class temporalplotObsSim(temporalplot):
    """
    Class for 1D variable temporal plot comparaison with observation

    Example :

    .. code-block:: python


        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.temporal.chrono import temporalplotObsSim
        # read data
        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_LaPlagne_2000-2001.nc') as p:
            sd = p.read('DSN_T_ISBA')
            sd2 = p.read('SNOWTEMP')
            time = p.readtime()
        plot = temporalplotObsSim()
        plot.draw(time, sd,time,sd,label='prout') # comparaison of sd with sd during the same time
        plot.finalize(time) # pour gérer les dates correctement
        plot.save('EX-temporalplotObsSim.png')

    .. figure:: /images/EX-temporalplotObsSim.png
        :align: center
    """

    def draw(self, timeObs, varObs, timeSim, varSim, *args, **kwargs):
        """
        Method of temporalplotObsSim class. Used for generating comparative 1D plot with time formatting.

        :param timeObs: Array of Observation duration usually given by prosimu.readtime()
        :param varObs: Array of Observation data usially given by prosimu.read()
        :param timeSim: Array of simulation duration usually given by prosimu.readtime()
        :param varSim: Array of simulation data usially given by prosimu.read()
        :param args: (optional)
        :param kwargs: optional ( matplotlib.pyplot key arguments )
        :return:
        """
        self.add_points(timeObs, varObs, label="Observations")
        if 'label' in kwargs.keys():
            label = kwargs['label']
        else:
            label = 'S2M'
        self.add_line(timeSim, varSim, label=label)
        self.finalize(timeSim, **kwargs)


class temporalplotObsMultipleSims(temporalplot):
    """ Kept for bakward compatibility , please refer to temporalplotObsSim
        Similar to temporalplotObsSim but UGLIER
    """
    def draw(self, timeObs, varObs, timeSim, varSim, *args, **kwargs):
        """Method of temporalplotObsMultipleSims class. Used for generating comparative 1D plot with time formatting.
        Similar to temporalplotObsSim but UGLIER

        :param timeObs: Array of Observation duration usually given by prosimu.readtime()
        :param varObs: Array of Observation data usially given by prosimu.read()
        :param timeSim: Array of simulation duration usually given by prosimu.readtime()
        :param varSim: Array of simulation data usially given by prosimu.read()
        :param args: (optional)
        :param kwargs: optional ( matplotlib.pyplot key arguments )
        :return:
        """
        self.add_points(timeObs, varObs, label="Observations")
        self.set_default("label", "S2M", kwargs)
        self.add_line(timeSim, varSim, **kwargs)


class temporalplot2Axes(temporalplot):
    """ Kept for bakward compatibility
    LM2B = Looks Broken to Me
    """
    def addAx2(self, labelAx2, color='black', **kw):
        """

        :param labelAx2: str of label
        :param color:
        :param kw:
        :return:
        """
        self.ax2 = self.plot.twinx()
        if 'forcemin' in kw.keys() and 'forcemax' in kw.keys():
            self.ax2.set_ylim(kw['forcemin'], kw['forcemax'])
        self.ax2.tick_params('y', colors=color)
        self.ax2.set_ylabel(labelAx2, color=color)

    def addVarAx2(self, timeOut, varAx2, label, color='red', linestyle="-", **kw):
        self.ax2.plot_date(timeOut, varAx2, linestyle, label=label, color=color, **kw)

    def addLegendAx2(self, location="upper right"):
        self.ax2.legend(loc=location, fontsize="x-small")


class prettyensemble(temporalplot):
    figsize = (15, 4.5)

    def __init__(self, *args, **kwargs):
        super(prettyensemble, self).__init__(*args, **kwargs)
        #         self.fig.subplots_adjust(top=0.85)
        self.fig.subplots_adjust()

    def draw(self, timeSim, qmin, qmed, qmax, *args, **kwargs):

        if 'colorquantiles' not in kwargs.keys():
            kwargs['colorquantiles'] = 'red'
        if 'colormembers' not in kwargs.keys():
            kwargs['colormembers'] = 'blue'

        medianlabel = u"Median"
        quantileslabel = u"Q10-Q90"
        if 'commonlabel' in kwargs.keys():
            medianlabel += " " + kwargs['commonlabel']
            quantileslabel += " " + kwargs['commonlabel']

        self.plot.plot_date(timeSim, qmed, "-", color=kwargs['colorquantiles'], linewidth=kwargs['linewidth'],
                            label=medianlabel)
        self.plot.fill_between(timeSim, qmin, qmax, color=kwargs['colorquantiles'], alpha=kwargs['alpha'],
                               label=quantileslabel)
        self.finalize(timeSim, **kwargs)


class spaghettis(temporalplot):
    figsize = (10, 3.5)

    def __init__(self, *args, **kwargs):
        super(spaghettis, self).__init__( *args, **kwargs)
        self.fig.subplots_adjust(top=0.85)

    def draw(self, timeSim, ensemble, qmin, qmed, qmax, *args, **kwargs):

        if 'colorquantiles' not in kwargs.keys():
            kwargs['colorquantiles'] = 'red'
        if 'colormembers' not in kwargs.keys():
            kwargs['colormembers'] = 'blue'

        medianlabel = u"Médiane"
        quantileslabel = u"Q20-Q80"
        if 'commonlabel' in kwargs.keys():
            medianlabel += " " + kwargs['commonlabel']
            quantileslabel += " " + kwargs['commonlabel']

        self.add_line(timeSim, ensemble, color=kwargs['colormembers'], linewidth=0.5)
        self.plot.plot(timeSim, qmed, "-", color=kwargs['colorquantiles'], linewidth=2, label=medianlabel)
        self.plot.fill_between(timeSim, qmin, qmax, color=kwargs['colorquantiles'], alpha=0.25, label=quantileslabel)
        self.finalize(timeSim, **kwargs)


class spaghettis_with_det(spaghettis):
    def draw(self, timeSim, ensemble, qmin, qmed, qmax, deterministic=None, *args, **kwargs):
        # super(spaghettis_with_det, self).draw(timeSim, ensemble, qmin, qmed, qmax, deterministic, *args, **kwargs)
        if 'commonlabel' in kwargs.keys():
            detlabel = u"Dét." + " " + kwargs['commonlabel']
        else:
            detlabel = u"Déterministe"

        self.add_line(timeSim, deterministic, color="black", linewidth=2, label=detlabel, zorder=100)
        super(spaghettis_with_det, self).draw(timeSim, ensemble, qmin, qmed, qmax, **kwargs)
