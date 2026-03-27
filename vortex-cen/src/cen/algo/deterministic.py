"""
Algo Components for deterministic S2M simulations.
"""

from bronx.fancies import loggers
from bronx.stdtypes.date import Date, tomorrow
from bronx.syntax.externalcode import ExternalCodeImportChecker
import footprints

from vortex.algo.components import Parallel, AlgoComponent
from vortex.nwp.tools.drhook import DrHookDecoMixin

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.tools.change_prep import prep_tomodify
    from snowtools.utils.resources import get_file_period, save_file_period, save_file_date
    from snowtools.tools.update_namelist import update_surfex_namelist_object, update_namelist_var
    from snowtools.tools.initTG import generate_clim
    from snowtools.tools.massif_diags import massif_simu


@echecker.disabled_if_unavailable
class Surfex_PreProcess(AlgoComponent):
    """Algo component to apply the required preprocessing before a SURFEX run (e.g. namelists adjustements)"""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['surfex_preprocess']),
            engine = dict(
                optional     = True,
                default   = 'algo'
            ),
            datebegin = dict(
                info = "Date in the namelist to run PREP.",
                type = Date,
            ),
            dateend = dict(
                info = "Date in the namelist to stop OFFLINE.",
                type = Date,
                optional = True,
                default = None
            ),
            forcingname = dict(
                info = "Name of the first forcing file",
                type = str,
            ),
        )
    )

    def find_namelists(self, opts=None):
        """Find any namelists candidates in actual context inputs."""
        namcandidates = [x.rh for x in self.context.sequence.effective_inputs(kind='namelist')]
        self.system.subtitle('Namelist candidates')
        for nam in namcandidates:
            nam.quickview()

        return namcandidates

    def execute(self, rh, opts):

        # Add forcing preparation

        # Modification of the namelist
        for namelist in self.find_namelists():
            # Update the contents of the namelist (date and location)
            # Location taken in the FORCING file.
            newcontent = update_surfex_namelist_object(
                namelist.contents,
                self.datebegin,
                forcing=self.forcingname,
                dateend=self.dateend
            )
            newnam = footprints.proxy.container(filename=namelist.container.basename)
            newcontent.rewrite(newnam)
            newnam.close()


@echecker.disabled_if_unavailable
class Generate_Clim_TG(AlgoComponent):
    """
    Algocomponent that generates a file of initial temperature to initialize the soil
    from surfex meteorological forcing files
    """

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['clim']),
            engine = dict(
                optional     = True,
                default   = 's2m',
                values = ['s2m']
            ),
        )
    )

    def execute(self, rh, opts):

        avail_forcing = self.context.sequence.effective_inputs(role="Forcing")
        listforcing = list({self.system.path.basename(am.rh.container.filename) for am in avail_forcing})

        generate_clim(listforcing)


class Pgd_Parallel_from_Forcing(Parallel):
    """
    This algo component is designed to run PGD with MPI parallelization and using
    a FORCING.nc as input for topography.
    """
    _footprint = dict(
        info = 'This algo component is designed to run PGD with MPI parallelization '
               'and using a FORCING.nc as input for topography.',
        attr = dict(
            kind = dict(values = ['pgd_from_forcing']),
            forcingname = dict(
                info = "Name of the first forcing file",
                type = str,
            ),
            engine = dict(
                optional = True,
                default = 'parallel'
            )
        )
    )

    def execute(self, rh, opts):
        self.system.symlink(self.forcingname, "FORCING.nc")
        super().execute(rh, opts)
        self.system.remove("FORCING.nc")


@echecker.disabled_if_unavailable
class Surfex_Parallel(Parallel, DrHookDecoMixin):
    """
    This algo component is designed to run SURFEX experiments over large domains
    with MPI parallelization.
    """

    _footprint = dict(
        info = 'AlgoComponent designed to run SURFEX experiments over large domains '
               'with MPI parallelization.',
        attr = dict(
            binary = dict(
                values = ['OFFLINE'],
            ),

            datebegin   = dict(
                info = "The first date of the simulation.",
                type = Date,
                optional = False
            ),

            dateend = dict(
                info = "The final date of the simulation.",
                type = Date,
                optional = False
            ),

            dateinit = dict(
                info = "The initialization date if different from the starting date.",
                type = Date,
                optional = True,
                default = '[datebegin]'
            ),

            threshold = dict(
                info = "Threshold on snow water equivalent on August 1st.",
                type = int,
                optional = True,
                default = -999
            ),
            daily = dict(
                info = "If True, split simulations in daily runs",
                type = bool,
                optional = True,
                default = False,
            ),

            reprod_info = dict(
                info = "Informations that must be stored in output files for reproductibility",
                type = dict,
                optional = True,
                default = dict(),
            )
        )
    )

    def execute(self, rh, opts):

        need_other_run = True
        need_other_forcing = True
        datebegin_this_run = self.datebegin

        while need_other_run:

            # Modification of the PREP file
            self.modify_prep(datebegin_this_run)

            if need_other_forcing:
                # Get the first file covering part of the whole simulation period
                dateforcbegin, dateforcend = get_file_period("FORCING", ".", datebegin_this_run, self.dateend)

            if self.daily:
                dateend_this_run = min(tomorrow(base=datebegin_this_run), min(self.dateend, dateforcend))
                need_other_forcing = dateend_this_run == dateforcend
                self.modify_namelist(datebegin_this_run, dateend_this_run)
            else:
                dateend_this_run = min(self.dateend, dateforcend)

            # Run surfex offline
            self.execute_single(rh, opts)

            # Copy the SURFOUT file for next iteration
            self.system.cp("SURFOUT.nc", "PREP.nc")

            # Rename outputs with the dates
            save_file_date(".", "SURFOUT", dateend_this_run, newprefix="PREP")

            # Post-process
            pro = massif_simu("ISBA_PROGNOSTIC.OUT.nc", openmode='a')
            pro.massif_natural_risk()
            pro.dataset.GlobalAttributes(**self.reprod_info)
            pro.dataset.add_standard_names()
            pro.close()

            save_file_period(".", "ISBA_PROGNOSTIC.OUT", datebegin_this_run, dateend_this_run, newprefix="PRO")

            if self.system.path.isfile("ISBA_DIAGNOSTICS.OUT.nc"):
                save_file_period(".", "ISBA_DIAGNOSTICS.OUT", datebegin_this_run, dateend_this_run, newprefix="DIAG")
            if self.system.path.isfile("ISBA_DIAG_CUMUL.OUT.nc"):
                save_file_period(".", "ISBA_DIAG_CUMUL.OUT", datebegin_this_run, dateend_this_run, newprefix="CUMUL")

            if need_other_forcing:
                # Remove the symbolic link for next iteration
                self.system.remove("FORCING.nc")

            # Prepare next iteration if needed
            datebegin_this_run = dateend_this_run
            need_other_run = dateend_this_run < self.dateend

    def find_namelists(self, opts=None):
        """Find any namelists candidates in actual context inputs."""
        namcandidates = [x.rh for x in self.context.sequence.effective_inputs(kind='namelist')]
        self.system.subtitle('Namelist candidates')
        for nam in namcandidates:
            nam.quickview()

        return namcandidates

    def modify_namelist(self, datebegin, dateend):

        # Modification of the namelist
        for namelist in self.find_namelists():
            # Update the contents of the namelist (date and location)
            # Location taken in the FORCING file.
            newcontent = update_surfex_namelist_object(
                namelist.contents,
                datebegin,
                dateend=dateend,
                updateloc=False
            )
            newnam = footprints.proxy.container(filename=namelist.container.basename)
            newcontent.rewrite(newnam)
            newnam.close()

    def modify_prep(self, datebegin_this_run):
        """
        The PREP file needs to be modified if the init date differs from the
        starting date or if a threshold needs to be applied on snow water equivalent.
        """

        modif_swe = self.threshold > 0 and datebegin_this_run.month == 8 and datebegin_this_run.day == 1
        modif_date = datebegin_this_run == self.datebegin and self.datebegin != self.dateinit
        modif = modif_swe or modif_date

        if modif:
            prep = prep_tomodify("PREP.nc")

            if modif_swe:
                print("APPLY THRESHOLD ON SWE.")
                prep.apply_swe_threshold(self.threshold)

            if modif_date:
                print("CHANGE DATE OF THE PREP FILE.")
                prep.change_date(self.datebegin)

            prep.close()
        else:
            print("DO NOT CHANGE THE PREP FILE.")


class Interpol_Forcing(Parallel):
    """
    This algo component is designed to interpolate SAFRAN forcings on regular grid
    with MPI parallelization.
    """

    _footprint = dict(
        info = 'AlgoComponent designed to interpolate SAFRAN forcings on regular grid '
               'with MPI parallelization.',
        attr = dict(
            binary = dict(
                values = ['INTERPOL'],
            ),

            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            )
        )
    )

    def execute(self, rh, opts):

        list_forcings = [x.rh for x in self.context.sequence.effective_inputs(role='Forcing')]

        self.algoassert(len(list_forcings) >= 1)
        print([forcing.container.filename for forcing in list_forcings])

        for forcing in list_forcings:
            self.system.mv(forcing.container.filename, 'input.nc')
            super().execute(rh, opts)
            self.system.mv('output.nc', forcing.container.filename)


@echecker.disabled_if_unavailable
class Prosnow_Parallel(Surfex_Parallel):
    """
    It adds snow management specificities by ski resorts to standard SURFEX-Crocus algo components.

    This class was implemented by C. Carmagnola in April 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'AlgoComponent designed to run SURFEX experiments over large domains with MPI parallelization.',
        attr = dict(
            insert_data = dict(
                values = ['prosnow_insert_data', ],
                type = str,
            )
        )
    )

    def prosnow_modify_namelist(self):
        new_nam = update_namelist_var("OPTIONS_unmodified.nam", "water.txt")
        return new_nam

    def prosnow_modify_prep(self):
        dateend_str = self.dateend.strftime('%Y%m%d%H')
        my_name_OBS = 'OBS_' + dateend_str + '.nc'
        my_name_PREP = 'PREP_' + dateend_str + '.nc'

        old_prep = prep_tomodify(my_name_PREP)
        new_prep = old_prep.insert_snow_depth('SRU.txt', 'snow.txt', my_name_OBS, 'prep_fillup_50.nc',
                                              'prep_fillup_5.nc', 'variables', my_name_PREP)

        return new_prep

    def execute(self, rh, opts):

        # Insert water consumption in namelist (before running surfex)
        self.prosnow_modify_namelist()

        # Call execute of Surfex_Parallel
        # Note that modify_namelist and modify_prep methods of the mother class
        # still have to be called in the following instruction
        super().execute(rh, opts)

        # Insert snow height in prep (after running surfex)
        self.prosnow_modify_prep()
