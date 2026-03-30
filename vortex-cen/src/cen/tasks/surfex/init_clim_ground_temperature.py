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

        if 'test' in self.conf and 'localtest' not in self.conf:

            self.sh.title("Toolbox Reference File")
            reference_file = vortex.input(
                role="initial values of ground temperature",
                kind="climTG",
                nativefmt="netcdf",
                local="init_TG_ref.nc",
                experiment="reference",
                username="vernaym",
                geometry=self.conf.geometry,
                model="surfex",
                namespace="vortex.multi.fr",
                namebuild="flat@cen",
                block="prep",
            ),
            print(self.ticket.prompt, "reference file =", reference_file)
            print()

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
                role="initial values of ground temperature",
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
                role="initial values of ground temperature",
                kind="climTG",
                nativefmt="netcdf",
                local="init_TG_reference.nc",
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