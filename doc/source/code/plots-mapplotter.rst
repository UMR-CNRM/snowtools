The mapplotter script
=====================

GUI to visualize maps of simulation outputs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Launching the ``mapplotter`` script without any argument (or with only a filename) opens a GUI that alow browsing the content of a PRO file (browsing across variables, dates and potential other dimensions such as tiles, layers if needed).


.. figure:: /images/mapplotter.png
   :align: center

Command line use
^^^^^^^^^^^^^^^^

.. include:: ./autoscripts/plots--maps--mapplotter.rst

Example:

     .. code-block:: bash

        python snowtools/plots/maps/mapplotter.py -f DSN_T_ISBA --date 20190513T10
                    --parallels 0.1 --meridians 0.1
                    <testbasedir>/PRO/PRO_2019051300_2019051400.nc
                    -O 20190513T10_snowheight.png


    .. figure:: /images/20190513T10_snowheight.png
       :align: center

    .. code-block:: bash

        python snowtools/plots/maps/mapplotter.py -f DSN_T_ISBA --date 20150301T06
                --diff <testbasedir>/PRO/PRO_first_2014080106_2015080106.nc
                <testbasedir>/PRO/PRO_second_2014080106_2015080106.nc
                -O 20150301T06_snowheight_diff.png

    .. figure:: /images/20150301T06_snowheight_diff.png
       :align: center

    .. code-block:: bash

        python snowtools/plots/maps/mapplotter.py --wU WSN_T_ISBA --wV SNOMLT_ISBA
                --date 20150405T06 --wpo grid --vectors_subsampling 1
                <testbasedir>/PRO/PRO_first_2014080106_2015080106.nc
                -O 20150405T06_pseudo_wind_map.png

    .. figure:: /images/20150405T06_pseudo_wind_map.png
       :align: center


Main plot function
^^^^^^^^^^^^^^^^^^

.. automodule:: plots.maps.mapplotter
   :members:
