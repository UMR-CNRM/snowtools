import vortex

from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.soda import Soda, SodaCommonsMixin


class PythonParticleFilter(SodaCommonsMixin, _CenResearchTask):
    """
    **Task: Soda**

    Python Particle Filter assimilation task.

    **Input:**

    - options (how?)
    - Ensemble of snowpack initial conditions ("PREP.nc") refered to as "background"

    **Outputs:**

    - Modified ensemble of snowpack initial conditions ("PREP.nc") referred to as "analysis"
    - Assimilation disgnostics file

    **Mandatory configuration variables**

    * ``date`` or ``assimdate`` Analysis date
      type: str, Date
    * ``geometry`` *geometry* of the PREP files
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``uenv`` User Environment in which the following resources are to be retrieved:
      Format : uenv:{uenv_name}@{user}
      type: str
    * ``observation_block`` By convention, the sensor used for the observation (ex: MODIS, PLEIADES, VIIRS)
      type: str
    * ``members`` Ensemble members
      type: footprints.stdtypes.FPList

    **Optional configuration variables**

    * ``scope`` Scope of the observation
      type: str
    * ``observation_xpid`` Experiment identifier of the observation file
      type: str
    * ``observation_user`` User who produced / owns the observation file
      type: str
    * ``observation_vapp`` *vapp* of the observation file
      type: str
    * ``observation_vconf`` *vconf* of the observation file
      type: str
    * ``prep_vapp`` *vapp* of the PREP files
      type: str
    * ``prep_vconf`` *vconf* of the PREP files
      type: str
    * ``prep_xpid`` Exepriment Identifier of the PREP files
      type: str
    * ``prep_user`` User who produced the PREP files
      type: str
    * ``prep_vortex1`` Whether or not the PREP files have been produced with Vortex-v1
      type: bool
    * ``pgd_vapp`` *vapp* of the PGD file
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file
      type: str
    * ``pgd_xpid`` Exepriment Identifier of the PGD file
      type: str
    * ``pgd_user`` User who produced the PGD file
      type: str
    * ``pgd_vortex1`` Whether or not the PGD files have been produced with Vortex-v1
      type: bool

    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "xpid",
            "geometry",
            "surfex_uenv|uenv",
            "members",
            "assimdate|date",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "namelist_source",
            "observation_vapp+help=*vapp* of the snow observation to assimilate;type=str;default=*vapp*",
            "observation_vconf+help=*vconf* of the snow observation to assimilate;type=str;default=*vconf*",
            "observation_xpid+help=Experiment identifier of the snow observation to assimilate;type=str;default=*xpid*",
            "observation_user+help=Name of the user who owns the snow observation file to assimilate;"
            + "type=str;default=$USER",
            "observation_block+help=Sensor used for the snow observation to assimilate "
            + "(ex MODIS, PLEIADES, VIIRS,...);type=str",
            "scope+help=Type of the snow observation to assimilate;type=str",
            "soda_gvar",
            "prep",
            "prep_namespace+help=Where to look for the PREP files ('vortex.cache.fr' if part of the 'assim' task);"
            + "type=str;choices=vortex.cache.fr (local cache), vortex.archive.fr (Hendrix), "
            + "vortex.multi.fr (Hendrix and local cache)",
            "pgd",
            "diff_xpid",
            "diff_user",
        ]

        overwrite = [
            "datebegin",
            "dateend",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=overwrite)

    @property
    def nmembers(self):
        return len(self.get_list_members())

    def get_remote_inputs(self):

        self.get_snow_observation()
        # self.get_namelist()

    def get_local_inputs(self):

        self.sh.title("Input SODA background PREPs")
        prep = vortex.input(
            role="SnowpackInit",
            member=self.get_list_members(),
            vapp=self.conf.get("prep_vapp", self.conf.vapp),
            vconf=self.conf.get("prep_vconf", self.conf.vconf),
            local="mb[member%04d]/background/PREP_[datevalidity:ymdh].nc",
            experiment=self.conf.get("prep_xpid", self.conf.xpid),
            username=self.conf.get("prep_user", None),
            geometry=self.conf.geometry,
            datevalidity=self.conf.get("assimdate", self.conf.get("date", None)),
            nativefmt="netcdf",
            kind="PREP",
            model="surfex",
            namespace=self.conf.get("prep_namespace", "vortex.multi.fr"),
            namebuild="flat@cen",
            block=self.conf.get("prep_block", "offline"),
            vortex1=self.conf.get("prep_vortex1", None),
            fatal=True,
        )
        print(self.ticket.prompt, "Background PREP =", prep)
        print()

    def algo(self):

        self.sh.title("Algo Python particle filter")
        algo = vortex.task(
            engine="algo",
            kind="python_particle_filter",
            dateassim=self.conf.get("assimdate", self.conf.get("date", None)),
            role_members="SnowpackSimulation",
        )
        print(self.ticket.prompt, "algo =", algo)
        print()

        return algo

    def launch_algo(self, algo, **kwargs):
        # Launch without MPI parallelisation
        self.launch_python_algo(algo)

    def put_outputs(self):

        t = self.ticket

        self.sh.title("Output PREP (analysis)")
        prep = (
            vortex.output(
                local="mb[member%04d]/analysis/PREP_[datevalidity:ymdh].nc",
                role="SnowpackInit",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datevalidity=self.conf.get("assimdate", self.conf.get("date", None)),
                member=self.get_list_members(),
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",
                block="soda",
                fatal=True,
            ),
        )
        print(t.prompt, "SODA analysis =", prep)
        print()

        self.sh.title("Output SODA diagnostics")
        diags = vortex.output(
            # @NI Take out IMASK temporarily because it's 1GB each assimilation in distributed simulations
            kind=["PART", "ALPHA"],
            model="soda",
            block="soda",
            namebuild="flat@cen",
            geometry=self.conf.geometry,
            namespace="vortex.multi.fr",
            dateassim=self.conf.get("assimdate", self.conf.get("date", None)),
            experiment=self.conf.xpid,
            local="[kind]",
            fatal=True,  # TODO : cela pourrait dépendre du "kind" pour plus de flexibilité
        )
        print(t.prompt, "SODA diags =", diags)
        print()

    def diff(self):
        # The "random" selection of particles in SODA makes reproducibility tests pointless
        pass
