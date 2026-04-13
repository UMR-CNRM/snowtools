# -*- coding: utf-8 -*-

import vortex


class SurfexParamsMixin:
    """
    Mixin class that provides methods to get constant land cover and Crocus metamorphism parameter files.
    """

    def get_ecoclimap(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Binary ECOCLIMAP I files are mandatory to run OFFLINE and taken from the uenv
        Binary ECOCLIMAP II files are mandatory to run OFFLINE and taken from the uenv
        """
        # Binary ECOCLIMAP I files are mandatory to run OFFLINE and taken from the uenv
        self.sh.title('Input ecoclimap1')
        ecoclimap1_tbi = vortex.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap1 =', ecoclimap1_tbi)
        print()

        # Binary ECOCLIMAP II files are mandatory to run OFFLINE and taken from the uenv
        self.sh.title('Input ecoclimap2')
        ecoclimap2_tbi = vortex.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap2 =', ecoclimap2_tbi)
        print()

    def get_drdt_bst_fit(self):
        """
        Get drdt_bst_fit_60.nc from uenv
        Crocus metamorphism parameters mandatory to run OFFLINE, PREP or PGD
        """
        self.sh.title('Input drdt_bst_fit_60')
        drdt_bst_fit_tbi = vortex.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv            = self.conf.genv,
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model           = 'surfex',
        )
        print(self.ticket.prompt, 'drdt_bst_fit_60 =', drdt_bst_fit_tbi)
        print()