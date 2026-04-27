# -*- coding: utf-8 -*-

"""
A collection of functions to help pimping your cartopy maps !

Use case example, I wanto to plot a total snow depth of a simulation with
``snowtools.plots.maps.quicklookmap`` and add a hillshade, some POIs, and a layer
representing the hydrography.

.. code-block:: python

   import datetime

   import matplotlib.pyplot as plt
   import cartopy.crs as ccrs
   import epygram

   from snowtools.plots.maps import quicklookmap, map_helpers

   projection = ccrs.epsg(9794)  # French Lambert projection
   fig, ax = plt.subplots(1, 1, subplot_kw={'projection': projection}, figsize=(13, 6))

   # Plot a scalar map of total snow depth
   filename = '...'
   date = datetime.datetime(2018, 2, 18)
   resource = epygram.formats.resource(filename, openmode='r')
   field = quicklookmap.read_and_preprocess(resource, 'DSN_T_ISBA', date)
   quicklookmap.scalar_map(
       field,
       title=f"Hauteur de neige totale (m)",
       plot_kwargs={
           'fig': fig, 'ax': ax,
           'minmax': [0, 4],
           'colormap': 'viridis',
           'parallels': 'auto',
           'meridians': 'auto',
           'epygram_departments': True,
           'cartopy_features': []
       })

   # Add points of interset
   POI = [
       {'lat': 45.15449, 'lon': 6.143825, 'name': 'Pic de l\'Étendard'},
       {'lat': 45.004942, 'lon': 6.309428, 'name': 'La Meije'},
       {'lat': 45.127064, 'lon': 6.336827, 'name': 'Aiguilles d\'Arves'},
   ]

   map_helpers.add_poi(ax, POIs=POI)

   # Add a layer from IGN for rivers and hydrography
   map_helpers.add_IGN_rivers(ax)

   # Add an hillshade from IGN
   map_helpers.add_IGN_hillshade(ax)

   # Add road layer from IGN
   map_helpers.add_IGN_feature(ax, layer='IGNF_TRANSPORTNETWORKS.ROADS')

   plt.show()


.. figure:: /images/map_helpers_example.png
   :align: center

"""

import pyproj


def add_poi(ax, POIs, crs_ref='EPSG:4326', **kwargs):
    """
    Add point of interests on a map.
    POI is a list of points consisting of dictionnaries with key lat, lon
    (or x, y) and an optional 'name' to be plotted on the map.

    :param ax: A maptpltlib Axis, with a projection defined
    :param POIs: The list of the Point of interests
    :type POIs: list of dict
    :param crs_ref: The CRS of the coordinates describing the POI (lat/lon or x, y). Defaults to lat/lon.
    :type crs_ref: str
    :param kwarks: Arguments to be passes to ax.scatter
                   (nb: marker, color and s are set resp. to o, k and 15 but can be overwritten).
    """
    if len(POIs) == 0:
        return

    if 'lat' in POIs[0] and 'lon' in POIs[0]:
        xn = 'lat'
        yn = 'lon'
    else:
        xn = 'x'
        yn = 'y'

    if 'marker' not in kwargs:
        kwargs['marker'] = 'o'
    if 'color' not in kwargs:
        kwargs['color'] = 'k'
    if 's' not in kwargs:
        kwargs['s'] = 15

    # Converison of coordinates
    t = pyproj.Transformer.from_crs(crs_ref, ax.projection)

    # POI plotting
    px, py = [], []
    for e in POIs:
        x, y = t.transform(e[xn], e[yn])
        px.append(x)
        py.append(y)
    ax.scatter(px, py, color="k", marker="o", s = 15)

    # Labels
    for elem in POIs:
        if 'name' in elem:
            x, y = t.transform(elem[xn] - .002, elem[yn] + .002)
            ax.text(x, y,
                    elem['name'],
                    va='top',
                    ha='left', transform=ax.projection)


def add_IGN_feature(ax, layer, zorder=10, **kwargs):
    """
    Add a layer from the IGN map service on the cartopy map
    provided on axis ax.

    For the list of layers available, see IGN documentation :

    - https://data.geopf.fr/wms-r?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities
    - https://cartes.gouv.fr/aide/fr/guides-utilisateur/utiliser-les-services-de-la-geoplateforme/tutoriels-api/qgis/

    :param ax: A maptpltlib Axis, with a projection defined
    :param layer: Layer of the IGN index (str descriptor. e.g. : ``ELEVATION.ELEVATIONGRIDCOVERAGE.SHADOW``)
    :type layer: str
    :param zorder: Layer orderning (matplotlib index)
    :type zorder: int
    :param kwargs: Arguments passed to matplotlib ``ax.imshow``.
    """
    url = 'https://data.geopf.fr/wms-r/wms'
    from owslib.wms import WebMapService
    service = WebMapService(url, version='1.3.0')
    ax.add_wms(wms=service,
               layers=[layer], zorder=zorder, **kwargs)


def add_IGN_hillshade(ax):
    """
    Add an Hillshade for relief visualization (from IGN data).
    """
    return add_IGN_feature(ax, 'ELEVATION.ELEVATIONGRIDCOVERAGE.SHADOW', zorder=10)


def add_IGN_rivers(ax):
    """
    Add a layer representing hydrography (from IGN data).
    """
    return add_IGN_feature(ax, 'IGNF_HYDROGRAPHY.HYDROGRAPHY', zorder=9)

