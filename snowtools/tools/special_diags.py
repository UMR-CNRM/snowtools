# -*- coding: utf-8 -*-

'''
Created on 28 juin 2018

@author: lafaysse

Template for Jesus developments (special SURFEX post-processing)
Note that in the general case, we should avoid to add new diagnostics here,
all diagnostics shoud be implemented in SURFEX when possible

'''

import numpy as np
from snowtools.utils.prosimu import prosimu


class diagprosimu(prosimu):

    # Define variable names
    varname_dz = "SNOWDZ"
    varname_snowdepth = "SNOWDEPTH"
    varname_impcontent = "XXXXXXXXXXXXXXXXXXXX"

    def __init__(self, *args, **kwargs):
        # Get dimensions
        super(diagprosimu, self).__init__(self, *args, **kwargs)
        self.dimensions = self.getdimvar(self.varname_snowdepth)

    def add_surface_impcontent(self):

        # Read snow layers thicknesses
        snowdz = self.read(self.varname_dz)

        # Read impurities content from SURFEX file
        impcontent_bylayer = self.read(self.varname_impcontent)

        # Compute the mean impurity content for the first 5 mm of SWE from the surface
        # Write here the correct algorithm
        impcontent_5mm = np.mean(impcontent_bylayer * snowdz)  # Incorrect formula

        # Create the netcdf variable
        var = self.dataset.createVariable("Impcontent5mm", np.float, self.dimensions)

        # Write the results
        var[:] = impcontent_5mm


if __name__ == "__main__":
    import sys
    profile = sys.argv[1]

    mypro = diagprosimu(profile, openmode="a")
    mypro.add_surface_impcontent()
    mypro.close()
