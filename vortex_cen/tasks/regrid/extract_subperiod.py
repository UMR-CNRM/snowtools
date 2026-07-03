# -*- coding: utf-8 -*-
"""
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ExtractSubPeriod(_CenResearchTask):
    """
    Extract a sub period in a Forcing file

    Inputs:
    --------
    - FORCING file

    Outputs:
    ---------
    - FORCING file on a shorter period

    Configuration variables:

    :param datebegin: begin date(s) of files
    :param dateend: end date(s) of files

    """

    # TODO : à voir ensemble pour ajouter la doc "dynamique pour documenter les variables de configuration

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        self.get_forcing(localname='FORCING_before_time_cut.nc')

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Direct algo for extraction of period
        """
        import xarray as xr

        ds = xr.open_dataset('FORCING_before_time_cut.nc')
        shorter_forcing = ds.sel(time=slice(self.conf.datebegin, self.conf.dateend))
        shorter_forcing.to_netcdf('FORCING.nc', format='NETCDF4_CLASSIC')

        return None

    def launch_algo(self, algo):
        # MV : cette classe doit être implémentée sinon la méthode "mère" de "research_task_base" raise
        # une "NotImplementedError".
        # TODO : revoir le comportement par défaut pour gérer ce genre de tâche (à condition d'accepter de ne pas
        # forcmément passer par un algo component dans des cas "simples")
        pass

    def put_outputs(self):

        self.sh.title('Output sub-forcing file')
        forcing_tbo = vortex.output(
            local       = 'FORCING.nc',
            experiment  = self.conf.xpid,
            # MV : il faut forcer la géométrie de sortie à la géométrie d'entrée puisqu'il n'y a
            # pas de changement de géométrie (--> sortir du répertoire "regrid" pour clarifier).
            # TODO : trouver une façon plus standardisée de faire ça.
            geometry    = self.conf.get('forcing_geometry'),
            datebegin   = self.conf.datebegin,
            dateend     = self.conf.dateend,
            nativefmt   = 'netcdf',
            kind        = 'MeteorologicalForcing',
            model       = 's2m',
            # MV : archivage sur cache uniquement par défaut pour ne pas dupliquer de la donnée existante
            namespace   = self.conf.get('namespace_out', 'vortex.cache.fr'),
            namebuild   = 'flat@cen',
            # MV : archivage dans le même block que le forcing d'origine
            block       = 'meteo',
            member      = self.conf.get('member', None),
            role        = 'Forcing',
        ),
        print(self.ticket.prompt, 'Sub-forcing =', forcing_tbo)
        print()
