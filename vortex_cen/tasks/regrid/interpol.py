# -*- coding: utf-8 -*-
"""
Created the 14 January 2026
@author: Radanovics S.
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class InterpolateS2MForcing(_CenResearchTask):
    """
    Interpolate a forcing file in "massif" geometry onto a 2D grid, or 1D grid, that is a list of points.

    Inputs:
    --------
    - FORCING file in the "massif" geometry.
    - GRID file containing the desired output grid.
    - interpolation binary

    Outputs:
    ---------
    - FORCING file on the new grid.

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "forcing_datebegin|datebegin",
            "forcing_dateend|dateend",
            "forcing_xpid",
            "xpid",
            "forcing_geometry+help=A SAFRAN massif geometry",
            "forcing_block",
            "geometry",
            "uenv+help=Name of the UEnv containing the DEM file and interpolator executable",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "member",
        ]
        overwrite = [
            "datebegin",
            "dateend",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES,
                overwrite=overwrite)

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        # Target grid file for interpolation
        # the path must be provided in the configuration file
        self.sh.title('Toolbox input output grid definition')
        grid_tbi = vortex.input(
            role='gridout',
            kind='interpolgrid',
            model='surfex',
            # TODO : ne pas utiliser de "remote" dans les tâches unitaires de base, utiliser l'héritage
            # pour créer une tâche spécifique identifiée comme "non reproductible"
            # --> pour cela il faut pouvoir surcharger cette "toolbox input" spécifiquement et donc
            # la mettre dans une méthode spécifique de "Mixin"
            remote=self.conf.gridout,
            # genv=self.conf.uenv,
            # gvar='DEM',
            local='GRID.nc',
        )
        print(self.ticket.prompt, 'toolbox input grid definition file =', grid_tbi)
        print()

        # take the interpolation binary from the uenv
        bin_interpol_tbx = vortex.executable(
            role='Binary',
            kind='offline',
            local='INTERPOL',
            model='surfex',
            genv=self.conf.uenv,
            gvar='master_interpol_mpi',
        )

        print(self.ticket.prompt, 'interpolation binary =', bin_interpol_tbx)
        print()

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Algo component for interpolation of the forcing on a regular grid
        """
        self.sh.title('Toolbox algo interpolation')
        interpolation_tba = vortex.task(
            engine='parallel',
            binary='INTERPOL',
            kind='deterministic',
            reprod_info=dict(genv=self.conf.uenv),
        )
        print(self.ticket.prompt, 'interpolation algo component =', interpolation_tba)
        print()

        return interpolation_tba

    def launch_algo(self, algo):
        """
        launch the algo component.

        :param algo: Algorithm to be launched.
        :type algo: AlgoComponent
        """
        # mpiopts = dict(nnodes=self.conf.nnodes, nprocs=self.conf.nprocs, ntasks=self.conf.ntasks)
        # self.launch_MPI_executable(algo, mpiopts=mpiopts)
        self.launch_executable(algo=algo)

    def put_outputs(self):
        """

        """
        if self.conf.forcing_geometry == self.conf.geometry:
            print(self.conf.forcing_geometry, self.conf.geometry)
            raise ValueError("The output 'geometry' can not be the same as the input one.\n"
                             "Please provide two different 'geometry' and 'forcing_geometry' configuration variables")
        else:
            self.sh.title('Toolbox output interpolated forcing file')
            forcing_tbo = vortex.output(
                local       = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment  = self.conf.xpid,
                geometry    = self.conf.geometry,
                datebegin   = self.list_dates_begin,
                dateend     = self.dict_dates_end,
                nativefmt   = 'netcdf',
                kind        = 'MeteorologicalForcing',
                model       = 's2m',
                namespace   = self.namespace_out,
                namebuild   = 'flat@cen',
                block       = 'meteo',
                member      = self.conf.get('member', None),
            ),
            print(self.ticket.prompt, 'interpolated forcing file toolbox =', forcing_tbo)
            print()

class InterpolateS2MRemoteForcing(InterpolateS2MForcing):
    """
    Interpolate a forcing file in "massif" geometry onto a 2D grid, or 1D grid, that is a list of points.

    Inputs:
    --------
    - remote FORCING file in the "massif" geometry
    - GRID file containing the desired output grid.
    - interpolation binary

    Outputs:
    ---------
    - FORCING file on the new grid.

    """

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        super().get_remote_inputs()
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')


class InterpolateS2MLocalForcing(InterpolateS2MForcing):
    """
    Interpolate a forcing file in "massif" geometry onto a 2D grid, or 1D grid, that is a list of points.

    Inputs:
    --------
    - Local FORCING file in the "massif" geometry.
    - GRID file containing the desired output grid.
    - interpolation binary

    Outputs:
    ---------
    - FORCING file on the new grid.

    """

    def get_local_inputs(self):
        """
        FORCING can come from local cache because if just a subpart of yearly forcing is used.
        """
        # FORCING coming from cache after extraction of a sub-period
        self.sh.title('Input sub-forcing file')
        forcing_tbi = vortex.input(
            local       = 'FORCING_' + str(self.conf.datebegin.strftime("%Y%m%d%H")) + '_' + str(self.conf.dateend.strftime("%Y%m%d%H")) + '.nc',
            experiment  = self.conf.xpid,
            # MV : il faut forcer la géométrie de sortie à la géométrie d'entrée puisqu'il n'y a
            # pas de changement de géométrie (--> sortir du répertoire "regrid" pour clarifier).
            # TODO : trouver une façon plus standardisée de faire ça.
            geometry    = self.conf.get('forcing_geometry', self.conf.geometry),
            datebegin   = self.conf.datebegin,
            dateend     = self.conf.dateend,
            nativefmt   = 'netcdf',
            kind        = 'MeteorologicalForcing',
            model       = 's2m',
            # MV : archivage sur cache uniquement par défaut pour ne pas dupliquer de la donnée existante
            namespace   = self.conf.get('namespace_out', 'vortex.cache.fr'),
            namebuild   = 'flat@cen',
            # MV : archivage dans le même block que le forcing d'origine
            block       = self.conf.get('forcing_block', 'meteo'),
            member      = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'Sub-forcing =', forcing_tbi)
        print()
