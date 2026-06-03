import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.init_clim_ground_temperature import \
    InitClimGroundTemperature


def setup(t, **kw):
    # Manually set the "iniconf" argument for local test
    # Otherwise, this is done by the JobAssistant
    if 'iniconf' in kw:
        iniconf = kw.pop('iniconf')
    else:
        iniconf = None
    return Driver(
        tag='initClimGT',
        ticket=t,
        nodes=[
            TestInitClimGroundTemperature(tag='initClimGroundTemperature', ticket=t, **kw),
        ],
        options=kw,
        iniconf = iniconf
    )


class TestInitClimGroundTemperature(InitClimGroundTemperature):

    def unittest(self):
        """
        Reproductibility test : compare output to reference.
        """
        self.sh.title("Reference File")
        forcing_diff = vortex.diff(
            role       = "InitialValuesOfGroundTemperature",
            kind       = "climTG",
            nativefmt  = "netcdf",
            local      = "init_TG.nc",
            experiment = "reference",
            username   = "vernaym",
            geometry   = self.conf.geometry,
            model      = "surfex",
            namespace  = "vortex.multi.fr",
            namebuild  = "flat@cen",
            block      = "prep",
        )
        print(self.ticket.prompt, "diff forcing =", forcing_diff)
        print()
