# -*- coding: utf-8 -*-
""" """

import vortex

from vortex_cen.tasks.research_task_base import _CenResearchTask


class ExtractSubPeriod(_CenResearchTask):
    """
    Extract a sub period in a Forcing file

    Inputs:
    -------
    - FORCING file

    Outputs:
    --------
    - FORCING file on a shorter period

    Configuration variables:
    ------------------------

    * ``datebegin`` begin date(s) of files
    * ``dateend`` end date(s) of files
    * ``forcing_geometry`` geometry of the forcing file which is going to be time cut

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin+help=Time cut forcing file will start at this date included",
            "dateend+help=Time cut forcing file will end at this date included",
            "forcing_geometry+help=Geometry of the forcing file which is going to be time cut",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "namespace_out+help=Path to keep the time cut forcing. Othewise, the file is put on cache"
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        self.get_forcing(localname="FORCING_before_time_cut.nc")

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Direct algo for extraction of period
        """
        import xarray as xr

        ds = xr.open_dataset("FORCING_before_time_cut.nc", engine="snowtools")
        shorter_forcing = ds.sel(time=slice(self.conf.datebegin, self.conf.dateend))
        shorter_forcing.to_netcdf("FORCING.nc", format="NETCDF4_CLASSIC")
        return None

    def launch_algo(self, algo):
        pass

    def put_outputs(self):
        self.sh.title("Output sub-forcing file")
        forcing_tbo = (
            vortex.output(
                local="FORCING.nc",
                experiment=self.conf.xpid,
                geometry=self.conf.get("forcing_geometry"),
                datebegin=self.conf.datebegin,
                dateend=self.conf.dateend,
                nativefmt="netcdf",
                kind="MeteorologicalForcing",
                model="s2m",
                namespace=self.conf.get("namespace_out", "vortex.cache.fr"),
                namebuild="flat@cen",
                block="subperiod",
                member=self.conf.get("member", None),
                role="Forcing",
            ),
        )
        print(self.ticket.prompt, "Sub-forcing =", forcing_tbo)
        print()
