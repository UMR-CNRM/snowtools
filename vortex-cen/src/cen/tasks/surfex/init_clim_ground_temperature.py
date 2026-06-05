# from vortex import toolbox
import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class InitClimGroundTemperature(SurfexCommonsMixin, _CenResearchTask):
    """
    Initialize Surfex ground temperature (GT) by taking the climatological mean of the input forcing air temperature.

    Inputs :
    --------
    - FORCING file(s) on simulation grid points
    - Init_TG file (initial values of ground temperature)

    Outputs :
    ---------
    - Ground Temperature initialization file on simulation grid points
    """

    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING_[datebegin:ymdh]_[dateend:ymdh].nc" in the different working sub-directories.
        """

        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Return an InitClimGroundTemperatureAlgo with the appropriate arguments.


        Working tree :
        rootdir
        |-- FORCING_datebegin1_dateend1.nc
        |-- FORCING_datebegin2_dateend2.nc

        """

        self.sh.title("Toolbox algo")
        algo = vortex.task(
            engine="s2m",
            kind="clim",
        )
        print(self.ticket.prompt, "algo =", algo)
        print()
        return algo

    def launch_algo(self, algo):
        """
        Launch an algo component.
        :param algo: algo component
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """
        Save the output Ground temperature (GT) initialization based on the climatological mean file in the simulation
        geometry.

        Configuration parameters used:
        ------------------------------
        * ``geometry`` simulation geometry
        * ``xpid`` Experiment identifier
        """

        self.sh.title("Toolbox output for initial values of ground temperature")
        init_ground_temperature_out = vortex.output(
            role       = "InitialValuesOfGroundTemperature",
            kind       = "climTG",
            nativefmt  = "netcdf",
            local      = "init_TG.nc",
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            model      = "surfex",
            namespace  = "vortex.multi.fr",
            namebuild  = "flat@cen",
            block      = "prep",
        )
        print(self.ticket.prompt, "Output init ground temperature =", init_ground_temperature_out)
        print()


class GetClimGroundTemperature(InitClimGroundTemperature):
    """
    If InitTG is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    Configuration Parameters:
    -------------------------

    * ``tg_xpid`` experiment id the init_TG.nc file should be fetched from.
    * ``tg_user`` name of the user that produced the target the init_TG.nc file.
    * ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation

    Optional Configuration Parameters:
    ----------------------------------

    * ``tg_uenv`` uenv to look for the init_TG.nc file in case the file should come from an uenv.
    * ``tg_gvar`` key to look up the init_TG.nc file in the uenv if the file should come from there.
    * ``forcing_source_app`` in case the init_TG needs to be calculated
        and the forcing comes from the S2M reanalysis
        (example: arpege)
    * ``forcing_source_conf`` in case the init_TG needs to be calculated
        and the forcing comes from the S2M reanalysis
        (example: 4dvarfr)
    * ``forcing_localname`` in case the init_TG needs to be calculated
        and the forcing comes from the S2M reanalysis
        (example: [datebegin:ymdh]_[dateend:ymdh]/FORCING_IN.nc)
    """

    def get_remote_inputs(self):
        # First try to get an init_TG file from the local cache or the archive
        self.init_tg = self.get_init_TG_from_cache_or_archive(fatal=False)
        # then try to get init_TG from uenv
        if not self.init_tg[0]:
            self.get_init_TG_from_uenv(fatal=False)

        # If no init_TG file was found, launch the actual init_TG task
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            super().get_remote_inputs()

    def algo(self):
        # If no init_TG file was found, launch the actual init_TG task
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            myalgo = super().algo()
            return myalgo
        else:
            pass

    def launch_algo(self, algo):
        # If no init_TG file was found, launch the actual init_TG task
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            super().launch_algo(algo)
        else:
            pass
