# -*- coding: utf-8 -*-

"""
Functions for proplotter

Pool of functions in order to separate: proplotter (main) / proreader (just to read files) /
proplotter_GUI (just for interaction) / profilPlot (plot to be used) / proplotter_functions
(link between properties from file and type of plot)
"""
import numpy as np
from snowtools.plots.stratiprofile import profilPlot


def create_axis_for_figure(fig, nb_graph, same_y=False, ratio=None, same_x=False, third_axis=True):
    """
    Create axis for figure depending on snow-layers presence or not, use of ratios if 2 graphs in same figure.

    :param fig: matplotlib figure
    :param nb_graph: number of graph for the figure
    :type nb_graph: int
    :param same_y: share y-axis if set to true. Usually set to true where ax1 is used to plot variable with snow-layers
    :type same_y: bool
    :param ratio: ratios for the size of the two graphs (when there are two)
    :type ratio: list of integers
    :param same_x: share x-axis if set to true. Usually set to true if it is used for compare
    :type same_x: bool
    :param third_axis: Create a third axis on the second subplot.
    :type third_axis: bool
    """
    if nb_graph == 1:
        ax1 = fig.add_subplot(1, 1, 1)
        return {'ax1': ax1}
    else:
        # give equal ratio to all graphs if ratio is not defined
        # Check that len(ratio) == nb_graph ? 
        if ratio is None:
            ratio = [1] * nb_graph
        ax1 = fig.add_subplot(1, sum(ratio), (1, ratio[0]))
        axs = {'ax1': ax1}
        add_params = {}
        if same_y:
            add_params['sharey'] = ax1
        if same_x:
            add_params['sharex'] = ax1
        
        # In add_subplot: use of (1, sum(ratio), (index_begin, index_end))
        # where index_begin is 1, 1+ratio1, 1+ratio1+ratio2, 1+ratio1+ratio2+ratio3,...
        # where index_end is ratio1, ratio1+ratio2, ratio1+ratio2+ratio3, ratio1+ratio2+ratio3+ratio4,...
        # in add_subplot, we want (1, ratio1 + ratio2, (1, ratio1)) then (1, ratio1 + ratio2, (ratio1 + 1, ratio1+ratio2))
        ind_begin = [1] + (np.cumsum(ratio)+1).tolist()[:-1]
        ind_end = np.cumsum(ratio).tolist()
        for i in range(1, nb_graph):
            ii = i + 1
            axs['ax{}'.format(ii)] = fig.add_subplot(1, sum(ratio), (ind_begin[i], ind_end[i]), **add_params)
            if same_y:
                axs['ax{}'.format(ii)].tick_params('y', labelleft=False)
            
        # Because figure was initially thought for two graph with two axes on second graph
        if third_axis:
            # instantiate a third axes that shares the same y-axis
            # For profil plot: ax3 is the Resistance-Snowgrain part of the plot
            axs['ax3'] = axs['ax2'].twiny()
        return axs
        

def get_data(fileobj, point, var_master, var_react=None, direction_cut=None, height_cut=None, additional_options=None):
    """
    Collecting datas for figures, before returning them
    """
    data = {}
    data['dataplot_master'] = fileobj.get_data(var_master, point, additional_options=additional_options)
    if var_react is not None:
        data['dataplot_react'] = fileobj.get_data(var_react, point, additional_options=additional_options)
        data['limitplot_react'] = fileobj.limits_variable(var_react)

    else:
        data['dataplot_react'] = None
        data['limitplot_react'] = None
    data['dztoplot'] = fileobj.get_data(fileobj.variable_dz, point, fillnan=0., additional_options=additional_options)
    data['timeplot'] = fileobj.get_time()
    data['colormap'] = fileobj.colorbar_variable(var_master)
    data['ymax_react'] = np.max(np.nansum(data['dztoplot'], axis=1))
    data['var_master_has_snl'] = fileobj.variable_desc(var_master)['has_snl']

    # useful for motion
    if fileobj.variable_grain in fileobj.variables_t:
        data['grain'] = fileobj.get_data(fileobj.variable_grain, point, fillnan=0.,
                                         additional_options=additional_options)
    else:
        data['grain'] = None
    if fileobj.variable_ram in fileobj.variables_t:
        data['ram'] = fileobj.get_data(fileobj.variable_ram, point, fillnan=0.,
                                       additional_options=additional_options)
    else:
        data['ram'] = None
    if var_react is not None and var_react in fileobj.variables_log:
        data['dataplot_react'] = np.where(data['dataplot_react'] > 10 ** (-10), data['dataplot_react'], 10 ** (-10))
        data['dataplot_react'] = np.where(data['dataplot_react'] > 0, np.log10(data['dataplot_react']), -10)

    if direction_cut is not None and height_cut is not None:
        data['direction_cut'] = direction_cut
        data['height_cut'] = height_cut

        # A VOIR AVEC LEO (height plot -> on doit découpler les hauteurs des graphes master et profil
        data['var_master_has_snl'] = False
    return data


def masterfig(var_master_has_snl, **kwargs):
    if var_master_has_snl:
        return profilPlot.saisonProfil(**kwargs)
    else:
        return profilPlot.saison1d(**kwargs)


#########################################
# FUNCTIONS FOR STANDARD GRAPHS
#########################################
def give_master_args_std(ax1, data):
    """
    Returns dictionary for the master figure, depending on the choice for the graph type.
    """
    if data['var_master_has_snl']:
        return dict(ax=ax1, value=data['dataplot_master'], time=data['timeplot'],
                    dz=data['dztoplot'], colormap=data['colormap'],
                    var_master_has_snl=data['var_master_has_snl'])
    else:
        return dict(ax=ax1, value=data['dataplot_master'], time=data['timeplot'],
                    var_master_has_snl=data['var_master_has_snl'])


def give_react_args_std(ax2, ax3, data, xindex, hauteur=None, plot_colorbar=False):
    """
    Collecting datas for the reacting figure, depending on the choice for the graph type.

    :param ax2: matplotlib axis
    :param ax3: second matplotlib axis sharing the same y-axis with ax2
    :param data: the data dictionary provided by function get_data_std
    :type data: dict
    :param xindex: index of time axis in order to select date for plot
    :type xindex: int
    :param hauteur: if provided, draw a horizontal line at provided height
    :type hauteur: float
    :param plot_colorbar: plot or not the colorbar
    :type plot_colorbar: bool
    """
    date = data['timeplot'][xindex]
    data_date = data['dataplot_react'][xindex, ...]
    dz_date = data['dztoplot'][xindex, ...]
    if data['grain'] is not None:
        grain_date = data['grain'][xindex, ...]
    else:
        grain_date = None

    if data['ram'] is not None:
        ram_date = data['ram'][xindex, ...]
    else:
        ram_date = None

    if data['var_master_has_snl']:
        return dict(axe=ax2, axe2=ax3, cbar_show=plot_colorbar, xlimit=data['limitplot_react'], value=data_date,
                    value_dz=dz_date, value_grain=grain_date, value_ram=ram_date, legend=date, hauteur=hauteur)
    else:
        return dict(axe=ax2, axe2=ax3, cbar_show=plot_colorbar, xlimit=data['limitplot_react'], value=data_date,
                    value_dz=dz_date, value_grain=grain_date, value_ram=ram_date, legend=date, hauteur=hauteur,
                    ylimit=data['ymax_react'])


#########################################
# FUNCTIONS FOR HEIGHT GRAPHS
#########################################
def give_master_args_height(ax1, data):
    """
    Returns dictionary for the master figure, depending on the choice for the graph type.
    """
    return dict(ax=ax1, value=data['dataplot_master'], time=data['timeplot'], value_ep=data['dztoplot'],
                direction_cut=data['direction_cut'], height_cut=data['height_cut'])


def masterfig_height(**kwargs):
    return profilPlot.heightplot(**kwargs)


#########################################
# FUNCTIONS FOR ESCROC GRAPHS
#########################################
def get_data_escroc(fileobj, point, var_master, var_react=None, additional_options=None):
    """
    Collecting datas for figures,  before returning them and subsetting with motions
    In this case shape = nb_membres, time, snowlayer
    """

    data = {}
    # x_legend is the list of members
    data['dataplot_master'], data['x_legend'] = fileobj.get_data(var_master, point, members='all',
                                                                 additional_options=additional_options)
    if var_react is not None:
        data['dataplot_react'] = fileobj.get_data(var_react, point, members='all',
                                                  additional_options=additional_options)[0]
        data['limitplot_react'] = fileobj.limits_variable(var_react)
    else:
        data['dataplot_react'] = None
        data['limitplot_react'] = None
    data['dztoplot'] = fileobj.get_data(fileobj.variable_dz, point, fillnan=0., members='all',
                                        additional_options=additional_options)[0]
    data['timeplot'] = fileobj.get_time()
    data['colormap'] = fileobj.colorbar_variable(var_master)
    data['ymax_react'] = np.max(np.nansum(data['dztoplot'], axis=2))  # axis=2 instead of axis=1
    data['var_master_has_snl'] = fileobj.variable_desc(var_master)['has_snl']

    # useful for motion
    if fileobj.variable_grain in fileobj.variables_t:
        data['grain'] = fileobj.get_data(fileobj.variable_grain, point, fillnan=0., members='all',
                                         additional_options=additional_options)[0]
        data['grain'] = np.array(data['grain'])
    else:
        data['grain'] = None
    if fileobj.variable_ram in fileobj.variables_t:
        data['ram'] = fileobj.get_data(fileobj.variable_ram, point, fillnan=0., members='all',
                                       additional_options=additional_options)[0]
        data['ram'] = np.array(data['ram'])
    else:
        data['ram'] = None
    if var_react is not None and var_react in fileobj.variables_log:
        data['dataplot_react'] = np.where(data['dataplot_react'] > 10 ** (-10), data['dataplot_react'], 10 ** (-10))
        data['dataplot_react'] = np.where(data['dataplot_react'] > 0, np.log10(data['dataplot_react']), -10)

    data['dataplot_master'] = np.array(data['dataplot_master'])
    data['dataplot_react'] = np.array(data['dataplot_react'])
    data['dztoplot'] = np.array(data['dztoplot'])

    return data


def give_master_args_escroc(ax1, data, dateslice, plot_colorbar=False):
    if data['var_master_has_snl']:
        if np.all(np.isnan(data['dataplot_master'])):
            max_value = 1
            min_value = 0.1
        else:
            max_value = np.nanmax(data['dataplot_master'])
            min_value = np.nanmin(data['dataplot_master'])

        dataplot_master = data['dataplot_master'][:, dateslice, :]
        dztoplot = data['dztoplot'][:, dateslice, :]
        ylimit = np.max(np.cumsum(data['dztoplot'], axis=2))

        return dict(ax=ax1, value=dataplot_master, time=data['x_legend'], dz=dztoplot, colormap=data['colormap'],
                    title=data['timeplot'][dateslice], value_max=max_value, value_min=min_value,
                    cbar_show=plot_colorbar, ylimit=ylimit, var_master_has_snl=data['var_master_has_snl'])
    else:
        dataplot_master = data['dataplot_master'][:, dateslice]
        if data['dztoplot'] is not None:
            limit_dz = np.max(np.cumsum(data['dztoplot'], axis=2))
            limit_value = np.max(data['dataplot_master'])
            ylimit = max(limit_dz, limit_value)
        else:
            ylimit = np.max(data['dataplot_master'])

        return dict(ax=ax1, value=dataplot_master, time=data['x_legend'], title=data['timeplot'][dateslice],
                    ylimit=ylimit, var_master_has_snl=data['var_master_has_snl'])


def give_react_args_escroc_profil(ax2, ax3, data, xindex, dateslice, hauteur=None, plot_colorbar=False):
    legend_escroc = "member " + str(data['x_legend'][xindex])
    data_escroc = data['dataplot_react'][xindex, dateslice, :]
    dz_escroc = data['dztoplot'][xindex, dateslice, :]

    if data['grain'] is not None:
        grain_date = data['grain'][xindex, dateslice, :]
    else:
        grain_date = None

    if data['ram'] is not None:
        ram_date = data['grain'][xindex, dateslice, :]
    else:
        ram_date = None

    return dict(axe=ax2, axe2=ax3, cbar_show=plot_colorbar, xlimit=data['limitplot_react'], value=data_escroc,
                value_dz=dz_escroc, value_grain=grain_date, value_ram=ram_date, legend=legend_escroc, hauteur=hauteur)


def give_react_args_escroc_saison(ax2, data, xindex, plot_colorbar=False):
    legend_escroc = "member " + str(data['x_legend'][xindex])
    dataplot_react = data['dataplot_react'][xindex, ...]
    dztoplot = data['dztoplot'][xindex, ...]

    return dict(ax=ax2, value=dataplot_react, time=data['timeplot'], dz=dztoplot, colormap=data['colormap'],
                title=legend_escroc, cbar_show=plot_colorbar)


#########################################
# FUNCTIONS FOR MULTIPLE GRAPHS
#########################################
def get_data_multiple(fileobj, liste_points, var_master, var_react=None, additional_options=None):
    """
    Collecting datas for figures, before subsetting with motions
    In this case shape = time, snowlayer, nb_points
    """
    data = {}
    data['dataplot_master'] = fileobj.get_data(var_master, list(liste_points), additional_options=additional_options)
    if var_react is not None:
        data['dataplot_react'] = fileobj.get_data(var_react, list(liste_points),
                                                  additional_options=additional_options)
        data['limitplot_react'] = fileobj.limits_variable(var_react)
    else:
        data['dataplot_react'] = None
        data['limitplot_react'] = None

    data['dztoplot'] = fileobj.get_data(fileobj.variable_dz, list(liste_points), fillnan=0.,
                                        additional_options=additional_options)

    data['timeplot'] = fileobj.get_time()
    data['colormap'] = fileobj.colorbar_variable(var_master)
    data['ymax_react'] = np.max(np.nansum(data['dztoplot'], axis=1))  # axis=1 because snowlayer is second dimension
    data['var_master_has_snl'] = fileobj.variable_desc(var_master)['has_snl']
    data['x_legend'] = [int(i) for i in list(liste_points)]

    # useful for motion
    if fileobj.variable_grain in fileobj.variables_t:
        data['grain'] = fileobj.get_data(fileobj.variable_grain, list(liste_points), fillnan=0.,
                                         additional_options=additional_options)
        data['grain'] = np.array(data['grain'])
    else:
        data['grain'] = None
    if fileobj.variable_ram in fileobj.variables_t:
        data['ram'] = fileobj.get_data(fileobj.variable_ram, list(liste_points), fillnan=0.,
                                       additional_options=additional_options)
        data['ram'] = np.array(data['ram'])
    else:
        data['ram'] = None
    if var_react is not None and var_react in fileobj.variables_log:
        data['dataplot_react'] = np.where(data['dataplot_react'] > 10 ** (-10), data['dataplot_react'], 10 ** (-10))
        data['dataplot_react'] = np.where(data['dataplot_react'] > 0, np.log10(data['dataplot_react']), -10)

    data['dataplot_master'] = np.array(data['dataplot_master'])
    data['dataplot_react'] = np.array(data['dataplot_react'])
    data['dztoplot'] = np.array(data['dztoplot'])

    return data


def give_master_args_multiple(ax1, data, dateslice, plot_colorbar=False):
    """
    Collecting datas for the master figure, depending on the choice for the graph type.
    """
    if data['var_master_has_snl']:
        if np.all(np.isnan(data['dataplot_master'])):
            max_value = 1
            min_value = 0.1
        else:
            max_value = np.nanmax(data['dataplot_master'])
            min_value = np.nanmin(data['dataplot_master'])
        dataplot_master = np.transpose(data['dataplot_master'][dateslice, ...])
        dztoplot = np.transpose(data['dztoplot'][dateslice, ...])
        ylimit = np.max(np.cumsum(data['dztoplot'], axis=1))

        return dict(ax=ax1, value=dataplot_master, time=data['x_legend'], dz=dztoplot, colormap=data['colormap'],
                    title=data['timeplot'][dateslice], value_max=max_value, value_min=min_value,
                    cbar_show=plot_colorbar, ylimit=ylimit, var_master_has_snl=data['var_master_has_snl'])
    else:
        # Select a date (this is always the first dimension)
        dataplot_master = np.transpose(data['dataplot_master'][dateslice, ...])
        if data['dztoplot'] is not None:
            ylimit = np.max(np.cumsum(data['dztoplot'], axis=1))
            return dict(ax=ax1, value=dataplot_master, time=data['x_legend'], title=data['timeplot'][dateslice],
                        ylimit=ylimit, var_master_has_snl=data['var_master_has_snl'])
        else:
            return dict(ax=ax1, value=dataplot_master, time=data['x_legend'], title=data['timeplot'][dateslice],
                        var_master_has_snl=data['var_master_has_snl'])


def give_react_args_multiple_profil(ax2, ax3, data, xindex, dateslice, hauteur=None, plot_colorbar=False):
    legend_mult = "multiple " + str(data['x_legend'][xindex])
    data_mult = data['dataplot_react'][dateslice, :, xindex]
    dz_mult = data['dztoplot'][dateslice, :, xindex]

    if data['grain'] is not None:
        grain_date = data['grain'][dateslice, :, xindex]
    else:
        grain_date = None

    if data['ram'] is not None:
        ram_date = data['grain'][dateslice, :, xindex]
    else:
        ram_date = None

    # TODO: Check what happens in multiple 2d file ... something strange... #
    return dict(axe=ax2, axe2=ax3, cbar_show=plot_colorbar, xlimit=data['limitplot_react'], value=data_mult,
                value_dz=dz_mult, value_grain=grain_date, value_ram=ram_date, legend=legend_mult, hauteur=hauteur)


def give_react_args_multiple_saison(ax2, data, xindex, plot_colorbar=False):
    """
    Collecting datas for the reacting figure, depending on the choice for the graph type.
    """
    legend_multiple = "multiple " + str(data['x_legend'][xindex])
    dataplot_react = data['dataplot_react'][:, :, xindex]
    dztoplot = data['dztoplot'][:, :, xindex]

    return dict(ax=ax2, value=dataplot_react, time=data['timeplot'], dz=dztoplot, colormap=data['colormap'],
                title=legend_multiple, cbar_show=plot_colorbar)
