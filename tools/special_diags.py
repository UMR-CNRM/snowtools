'''
Created on 28 juin 2018

@author: lafaysse

Template for Jesus developments (special SURFEX post-processing)

'''

import numpy as np
from utils.prosimu import prosimu


class diagprosimu(prosimu):

    def __init__(self, *args, **kwargs):
        # Get dimensions
        super(diagprosimu, self).__init__(self, *args, **kwargs)
        self.dimensions = self.getdimvar("SNOWDEPTH")

    def add_surface_impcontent(self):

        # Read snow layers thicknesses
        snowdz = self.read("SNOWDZ")

        # Read impurities content from SURFEX file
        impcontent_bylayer = self.read("XXXXXXXXXXXXXXXXXXXX")

        # Compute the mean impurity content for the first 5 mm of SWE from the surface
        # Write here the correct algorithm
        impcontent_5mm = np.mean(impcontent_bylayer * snowdz)  # Incorrect formula

        # Create the netcdf variable
        var = self.dataset.createVariable("Impcontent5mm", np.float, self.dimensions)

        # Write the results
        var[:] = impcontent_5mm


if __name__ == "__main__":
    mypro = diagprosimu("PRO.nc", openmode="a")
    mypro.add_surface_impcontent()
    mypro.close()
