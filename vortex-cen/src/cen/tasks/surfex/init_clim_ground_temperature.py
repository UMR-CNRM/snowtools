# from vortex import toolbox
import vortex
from vortex.algo.components import AlgoComponent
from vortex_cen.tasks.research_task_base import _CenResearchTask


class InitClimGroundTemperature(_CenResearchTask):
    """
    Initialize Surfex ground temperature (GT) by taking the climatological mean of the input forcing air temperature.

    Inputs :
    --------
    - FORCING file(s) on simulation grid points

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
        Save the output Ground temperature (GT) initialization based on the climatological mean file in the simulation geometry.
        Arguments:
        :param geometry:
        type geometry: simulation geometry
        :param xpid: Experiment identifier (format "experiment_name@user")
        :type xpid: str
        """

        self.sh.title("Toolbox output for initial values of ground temperature")
        init_ground_temperature_out = vortex.output(
                role="InitialValuesOfGroundTemperature",
                kind="climTG",
                nativefmt="netcdf",
                local="init_TG.nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                model="surfex",
                namespace="vortex.multi.fr",
                namebuild="flat@cen",
                block="prep",
            )
        print(self.ticket.prompt, "Output init ground temperature =", init_ground_temperature_out)
        print()

    def unittest(self):
        """
        Reproductibility test : compare output to reference.
        """
        self.sh.title("Toolbox Reference File")
        forcing_diff = vortex.diff(
                role="InitialValuesOfGroundTemperature",
                kind="climTG",
                nativefmt="netcdf",
                local="init_TG.nc",
                experiment="reference",
                username="vernaym",
                geometry=self.conf.geometry,
                model="surfex",
                namespace="vortex.multi.fr",
                namebuild="flat@cen",
                block="prep",
            )
        print(self.ticket.prompt, "diff forcing =", forcing_diff)
        print()


class GetClimGroundTemperature(InitClimGroundTemperature):
    """
    If InitTG is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    Configuration Parameters:
    -------------------------

    * ``xpid_tg`` experiment id the init_TG.nc file should be fetched from.
    * ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation

    Optional Configuration Parameters:
    ----------------------------------

    * ``genv_tg`` uenv to look for the init_TG.nc file in case the file should come from an uenv.
    * ``gvar_tg`` key to look up the init_TG.nc file in the uenv if the file should come from there.
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
        # try to get init_TG from cache or archive
        self.sh.title('Toolbox input init_TG from cache')
        initTG_cache_tbi = vortex.input(
            role="InitialValuesOfGroundTemperature",
            kind='climTG',
            nativefmt='netcdf',
            local='init_TG.nc',
            experiment=self.conf.xpid_tg,
            geometry=self.conf.geometry,
            model='surfex',
            namespace='vortex.multi.fr',
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block='prep',
            fatal=False,
        ),
        print(self.ticket.prompt, 'initTG_cache_tbi =', initTG_cache_tbi)
        print()

        # try to get init_TG from uenv
        if not initTG_cache_tbi[0] and hasattr(self.conf, 'genv_tg'):
            self.sh.title('Toolbox input init_TG from uenv')
            initTG_uenv_tbi = vortex.input(
                role="InitialValuesOfGroundTemperature",
                kind='climTG',
                nativefmt='netcdf',
                local='init_TG.nc',
                geometry=self.conf.geometry,
                genv=self.conf.genv_tg,
                gvar=self.conf.get('gvar_tg', 'climtg_[geometry::area]'), # TODO: I'm not sure about the "area". It used to be "tag"
                # but "tag" does not exist in geometries_vortex2.ini @vernaym: should it be area, tag or nothing?
                model='surfex',
                fatal=False,
            ),
            print(self.ticket.prompt, 'initTG_uenv_tbi =', initTG_uenv_tbi)
            print()

        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
                super().get_remote_inputs()

    def algo(self):
        print(self.ctx.sequence.effective_inputs()[0].role)
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) > 0:
            pass
        else:
            myalgo = super().algo()
            return myalgo

    def launch_algo(self, algo):
        print('input sequence length: ',
              len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")))
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) > 0:
            pass
        else:
            super().launch_algo(algo)

    def put_outputs(self):
        """
        Put the init_TG.nc file in the vortex cache.
        """

        print('effective inputs:', self.ctx.sequence.effective_inputs())
        print('input sequence length: ', len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")))

        self.sh.title("Toolbox output for initial values of ground temperature")
        init_ground_temperature_out = vortex.output(
            role="InitialValuesOfGroundTemperature",
            kind="climTG",
            nativefmt="netcdf",
            local="init_TG.nc",
            experiment=self.conf.xpid_tg,
            geometry=self.conf.geometry,
            model="surfex",
            namespace="vortex.cache.fr",
            namebuild="flat@cen",
            block="prep",
        )
        print(self.ticket.prompt, "Output init ground temperature =", init_ground_temperature_out)
        print()

        print('effective outputs:', self.ctx.sequence.effective_outputs()[0].role)


    def unittest(self):
        pass



