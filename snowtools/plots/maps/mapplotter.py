#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 17 June 2025

:Authors:
    radanovics

    largely copied from epygram epy_cartoplot.py

"""
import argparse
from bronx.syntax.parsing import str2dict
from bronx.stdtypes.date import Date
import epygram
from epygram import epylog, epygramError
from epygram.cli.args_catalog import (add_arg_to_parser,
                                      files_args, fields_args,
                                      misc_args, output_args,
                                      runtime_args, graphical_args)
import matplotlib.pyplot as plt
import cartopy.feature as cf
from snowtools.plots.maps.quicklookmap import read_and_preprocess, wind_map, difference_map, scalar_map

CFEATURES = [f for f in dir(cf) if all([c.isupper() for c in f])]
"""Cartopy features"""

def main(filename,
         fid=None,
         Ufid=None,
         Vfid=None,
         refname=None,
         diffmode=False,
         date=None,
         # pre-processing
         operation=None,
         global_shift_center=None,
         zoom=None,
         # figure
         title=None,
         # graphical settings
         plot_method='pcolormesh',
         minmax=None,
         mask_threshold=None,
         colorsnumber=50,
         colormap='plasma',
         center_cmap_on_0=False,
         scatter_kw=None,
         # cartography
         parallels='auto',
         meridians='auto',
         french_depts=False,
         cartopy_features=[],
         # wind/vectors
         vectors_subsampling=20,
         vector_plot_method='quiver',
         wind_components_are_projected_on=None,
         quiverkey=None,
         map_factor_correction=False,
         # output
         savefig=False,
         outputfilename=None,
         figures_dpi=epygram.config.default_figures_dpi):
    """
    Plot fields.

    :param filename: name of the file to be processed.
    :param fid: field identifier.
    :param Ufid: U-component of wind field identifier.
    :param Vfid: V-component of wind field identifier.
    :param refname: name of the reference file to be compared to.
    :param diffmode: if True, plots the difference field between the field in filename and refname.
    :param date: date and time to choose from the file

    Pre-processing:

    :param operation: makes the requested operation
        (e.g. {'operation':'-','operand':273.15} or
        {'operation':'exp'}) on the field before plot.
    :param global_shift_center: for global lon/lat grids, shift the center by the
        requested angle (in degrees). Enables a [0,360] grid
        to be shifted to a [-180,180] grid, for instance (with -180 argument).
    :param zoom: a dict(lonmin, lonmax, latmin, latmax) on which to build the plot.

    Figure:

    :param title: title to be written over plot.

    Graphical settings:

    :param plot_method: matplotlib plotting method to be used, among
        ('pcolormesh', 'contourf', 'contour', 'scatter').
    :param minmax: tuple giving (or not) min and max fields values to be plotted.
    :param colorsnumber: number of color discretization/isolines for fields plots.
    :param colormap: name of the colormap for fields plots.
    :param center_cmap_on_0: to center the colormap on 0.
    :param mask_threshold: dict with min and/or max value(s) to mask outside.
    :param scatter_kw: kwargs to be passed to matplotlib's ax.scatter().
        Only for plot_method = 'scatter'.

    Cartography:

    :param meridians and parallels: enable to fine-tune the choice of lines to
        plot, with either:
        - 'auto': automatic scaling to the map extents
        - 'default': range(0,360,10) and range(-90,90,10)
        - a list of values
        - a grid step, e.g. 5 to plot each 5 degree.
        - None: no one is plot
    :param french_depts: draws french departments instead of countries boundaries.
    :param cartopy_features: list of cartopy.feature.??? features.

    Vector plots:

    :param vectors_subsampling: subsampling ratio of vectors plots.
    :param vector_plot_method: among ('quiver', 'barbs', 'streamplot') for vector plots.
    :param wind_components_are_projected_on: inform the plot on which axes the
        vector components are projected on ('grid' or 'lonlat').
        If None (default), look for information in the field, or raise error.
    :param quiverkey: options to be passed to plotfield to activate a quiver key
        (cf. pyplot.quiverkey).
    :param map_factor_correction: if True, applies a correction of magnitude
        to vector due to map factor.

    Output:

    :param savefig: save figures to file, instead of interactive plot
    :param outputfilename: specify an output filename for the plot,
        including format as extension.
    :param figures_dpi: quality of saved figures.
    """

    # 0/ checks, determine mode, initializations
    # checks
    assert not all([f is None for f in (fid, Ufid, Vfid)]), "Mandatory arguments: *fid* OR *Ufid/Vfid*."
    if fid is not None:
        assert Ufid is Vfid is None, "Exclusive arguments: *fid* OR *Ufid/Vfid*."
    if Ufid is not None:
        assert Vfid is not None, "Arguments Ufid/Vfid got by pair."
    # mode
    windmode = fid is None
    assert not (diffmode and windmode), "Exclusive options: diff to reference file and wind plot."
    # plot options
    plot_kwargs = dict(
        # graphical settings
        plot_method=plot_method,
        minmax=minmax,
        mask_threshold=mask_threshold,
        scatter_kw=scatter_kw,
        # cartography
        parallels=parallels,
        meridians=meridians,
        epygram_departments=french_depts,
        cartopy_features=cartopy_features,
        # colormapping
        colormap=colormap,
        colorsnumber=colorsnumber,
        center_cmap_on_0=center_cmap_on_0)
    # pre-processing options
    preprocess_options = dict(
        operation=operation,
        global_shift_center=global_shift_center,
        zoom=zoom)

    # 1/ resource(s)
    resource = epygram.formats.resource(filename, openmode='r')
    if resource.format == 'DDHLFA':
        raise epygramError('use ddhlfa_plot.py tool for DDHLFA files.')
    if diffmode:
        reference = epygram.formats.resource(refname, openmode='r')

    if windmode:
        # 2.1/ wind
        u = read_and_preprocess(resource, Ufid, date,
                                **preprocess_options)
        v = read_and_preprocess(resource, Vfid, date,
                                **preprocess_options)
        field = epygram.fields.make_vector_field(u, v)
        if title is None:
            title = "\n".join([str(fid), str(field.validity.get())])
        takeover = wind_map(field, title, map_factor_correction, vectors_subsampling, wind_components_are_projected_on,
                            vector_plot_method, quiverkey, plot_kwargs)
        fig = takeover['fig']


    else:
        # 2.2/ scalar field
        if diffmode:
            # 2.2.1/ diff of scalar fields
            # read field and ref, compute diff
            field = read_and_preprocess(resource, fid, date,
                                        **preprocess_options)
            ref_field = read_and_preprocess(reference, fid, date,
                                            **preprocess_options)
            if title is None:
                print(resource.container.basename)
                title = "\n".join([resource.container.basename + ' - ' +
                                       reference.container.basename,
                                       str(fid), str(field.validity.get())])
            takeover = difference_map(field, ref_field, title, plot_kwargs)
            fig = takeover['fig']
        else:
            # 2.2.2/ plot single scalar fields
            field = read_and_preprocess(resource, fid, date,
                                        **preprocess_options)
            if title is None:
                title = "\n".join([str(fid), str(field.validity.get())])
            takeover = scalar_map(field, title, plot_kwargs)
            fig = takeover['fig']
            
    # 3/ output
    if savefig:
        epylog.info("save plot...")
        save_kwargs = dict(bbox_inches='tight', dpi=figures_dpi)
        fig.savefig(outputfilename, **save_kwargs)
    else:
        plt.show()
# end of main() ###############################################################


if __name__ == '__main__':

    # 1. Parse arguments
    ####################
    parser = argparse.ArgumentParser(description='An EPyGrAM based tool for simple plots\
                                                  of meteorological or snow cover related 2D'
                                                 ' fields from a resource file.',
                                     epilog='End of help for: %(prog)s (EPyGrAM v' + epygram.__version__ + ')')

    add_arg_to_parser(parser, files_args['principal_file'])
    add_arg_to_parser(parser, fields_args['field'])
    add_arg_to_parser(parser, fields_args['windfieldU'])
    add_arg_to_parser(parser, fields_args['windfieldV'])
    add_arg_to_parser(parser, ['--date', dict(type=str, dest='date',
                                             help="Selected date in a bronx.Date compatible string format",
                                             required=True)])

    diffmodes = parser.add_mutually_exclusive_group()
    add_arg_to_parser(diffmodes, files_args['file_to_refer_in_diff'])

    add_arg_to_parser(parser, misc_args['operation_on_field'])
    add_arg_to_parser(parser, misc_args['mask_threshold'])
    add_arg_to_parser(parser, misc_args['wind_components_are_projected_on'])
    add_arg_to_parser(parser, misc_args['map_factor_correction'])

    # graphics
    add_arg_to_parser(parser, graphical_args['plot_method'])
    add_arg_to_parser(parser, graphical_args['minmax'])
    add_arg_to_parser(parser, graphical_args['levels_number'])
    add_arg_to_parser(parser, graphical_args['colormap'], default='YlGnBu')
    add_arg_to_parser(parser, graphical_args['center_cmap_on_0'])
    add_arg_to_parser(parser, graphical_args['title'])
    add_arg_to_parser(parser, graphical_args['cartopy_features'],
                      help="cartopy features (cartopy.feature.*), separated by comma " +
                      str(CFEATURES))
    add_arg_to_parser(parser, graphical_args['french_departments'])
    add_arg_to_parser(parser, graphical_args['parallels'])
    add_arg_to_parser(parser, graphical_args['meridians'])
    add_arg_to_parser(parser, graphical_args['vectors_subsampling'])
    add_arg_to_parser(parser, graphical_args['scatter_kw'])
    add_arg_to_parser(parser, graphical_args['lonlat_zoom'])
    add_arg_to_parser(parser, graphical_args['vector_plot_method'])
    add_arg_to_parser(parser, graphical_args['quiverkey'])
    add_arg_to_parser(parser, graphical_args['figures_dpi'])
    add_arg_to_parser(parser, graphical_args['global_shift_center'])
    # diff
    add_arg_to_parser(parser, graphical_args['diffcolormap'], default='RdBu_r')
    add_arg_to_parser(parser, graphical_args['diff_center_cmap_on_0'])
    # output
    add_arg_to_parser(parser, output_args['outputfilename'], default=None)
    add_arg_to_parser(parser, runtime_args['verbose'])

    args = parser.parse_args()

    # 2. Initializations
    ####################
    epygram.init_env()
    # 2.0 logs
    epylog.setLevel('WARNING')
    if args.verbose:
        epylog.setLevel('INFO')

    # 2.1 options
    refname = args.refname
    if args.refname is not None:
        diffmode = True
        center_cmap_on_0 = args.diffcenter_cmap_on_0
        colormap = args.diffcolormap
    else:
        diffmode = False
        center_cmap_on_0 = args.center_cmap_on_0
        colormap = args.colormap
    if args.minmax is not None:
        minmax = args.minmax.split(',')
    else:
        minmax = None
    if args.zoom is not None:
        zoom = str2dict(args.zoom, float)
    else:
        zoom = None
    if args.operation is not None:
        _operation = args.operation.split(',')
        operation = {'operation':_operation.pop(0).strip()}
        if len(_operation) > 0:
            operation['operand'] = float(_operation.pop(0).strip())
    else:
        operation = None

    if args.parallels == 'None':
        parallels = None
    elif ',' in args.parallels:
        parallels = [float(p.strip()) for p in args.parallels.split(',')]
    else:
        try:
            parallels = float(args.parallels)
        except ValueError:
            parallels = args.parallels
    if args.meridians == 'None':
        meridians = None
    elif ',' in args.meridians:
            meridians = [float(m.strip()) for m in args.meridians.split(',')]
    else:
        try:
            meridians = float(args.meridians)
        except ValueError:
            meridians = args.meridians
    if args.mask_threshold is not None:
        mask_threshold = str2dict(args.mask_threshold, float)
    else:
        mask_threshold = None
    if args.quiverkey is None or args.quiverkey == '':
        quiverkey = None
    else:
        quiverkey = str2dict(args.quiverkey, float)
    if args.scatter_kw is not None:
        scatter_kw = str2dict(args.scatter_kw, int)
    else:
        scatter_kw = None
    if args.cartopy_features is not None:
        cartopy_features = args.cartopy_features.split(',')
    else:
        cartopy_features = []
    if args.outputfilename:
        savefig = True
    else:
        savefig = False

    # 2.2 field to be processed
    if args.Ucomponentofwind is not None or args.Vcomponentofwind is not None:
        if None in (args.Ucomponentofwind, args.Vcomponentofwind):
            raise epygramError("wind mode: both U & V components of wind must be supplied")
        if diffmode:
            raise NotImplementedError("diffmode (-d/D) AND wind mode (--wU/wV) options together.")
    elif args.field is None:
        raise epygramError("Need to specify a field (-f) or two wind fields (--wU/--wV).")

    if args.date is None:
        raise ValueError("Need to specify a date (--date).")
    else:
        date = Date(args.date)


    # 3. Main
    #########
    main(args.filename,
         fid=args.field,
         Ufid=args.Ucomponentofwind,
         Vfid=args.Vcomponentofwind,
         refname=refname,
         diffmode=diffmode,
         date=date,
         # pre-processing
         operation=operation,
         global_shift_center=args.global_shift_center,
         zoom=zoom,
         # figure
         title=args.title,
         # graphical settings
         plot_method=args.plot_method,
         minmax=minmax,
         mask_threshold=mask_threshold,
         colorsnumber=args.levelsnumber,
         colormap=colormap,
         center_cmap_on_0=center_cmap_on_0,
         scatter_kw=scatter_kw,
         # cartography
         parallels=parallels,
         meridians=meridians,
         french_depts=args.depts,
         cartopy_features=cartopy_features,
         # wind/vectors
         vectors_subsampling=args.vectors_subsampling,
         vector_plot_method=args.vector_plot_method,
         wind_components_are_projected_on=args.wind_components_are_projected_on,
         quiverkey=quiverkey,
         map_factor_correction=args.map_factor_correction,
         # output
         savefig=savefig,
         outputfilename=args.outputfilename)


