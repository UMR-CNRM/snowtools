from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask


class InitClimGroundTemperature(_CenResearchTask):
    """
    Initialize Surfex ground temperature by taking the climatological (i.e. the mean on all time steps) of the input forcing.

        Inputs :
    --------
    -

    Outputs :
    ---------
    -
    """

    def get_remote_inputs(self):
        """ """

        self.get_forcing(localname="[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")

    def algo(self):
        """



        Working tree :
        rootdir


        Arguments:

        """

        self.sh.title("Toolbox algo")
        algo = toolbox.algo(
            engine="s2m",
            kind="clim",
        )
        print(self.ticket.prompt, "algo =", algo)
        print()
        return algo

    def put_remote_outputs(self):
        """

        Arguments:
        :param out_geometry:

        """

        self.sh.title("Toolbox output for initial values of ground temperature")
        init_ground_temperature_out = (
            toolbox.output(
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
            ),
        )
        print(self.ticket.prompt, "Output init ground temperature =", init_ground_temperature_out)
        print()

    def unittest(self):
        """
        Reproductibility test : compare output to reference.
        """

        self.sh.title("Toolbox ")
        forcing_diff = (
            toolbox.diff(
                role="initial values of ground temperature",
                kind="climTG",
                nativefmt="netcdf",
                local="init_TG.nc",
                experiment="reference@vernaym",
                geometry=self.conf.geometry,
                model="surfex",
                namespace="vortex.multi.fr",
                namebuild="flat@cen",
                block="prep",
            ),
        )
        print(self.ticket.prompt, "diff forcing =", forcing_diff)
        print()
