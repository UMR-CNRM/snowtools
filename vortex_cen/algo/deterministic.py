# -*- coding: utf-8 -*-
"""
deterministic.py
----------------

Algo Components for deterministic Surfex simulations.

.. inheritance-diagram:: vortex_cen.algo.deterministic
   :top-classes: vortex.algo.components.Parallel, vortex.nwp.tools.drhook.DrHookDecoMixin,
                 vortex.algo.components.AlgoComponent
   :private-bases:


.. autoclass:: Surfex_PreProcess
   :no-members:
   :show-inheritance:

.. autoclass:: Generate_Clim_TG
   :no-members:
   :show-inheritance:

.. autoclass:: Pgd_Parallel_from_Forcing
   :no-members:
   :show-inheritance:

.. autoclass:: Surfex_Parallel
   :no-members:
   :show-inheritance:

.. autoclass:: Surfex_Xios_Parallel
   :no-members:
   :show-inheritance:

.. autoclass:: Interpol_Forcing
   :no-members:
   :show-inheritance:

"""

import numpy as np

import footprints
from bronx.fancies import loggers
from bronx.stdtypes.date import Date, tomorrow
from bronx.syntax.externalcode import ExternalCodeImportChecker
from vortex.algo.components import AlgoComponent, Parallel, ParallelIoServerMixin
from vortex.nwp.tools.drhook import DrHookDecoMixin
from vortex_cen.algo.components import _CenMixIn

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker("snowtools")
with echecker:
    from snowtools.tools.change_prep import prep_tomodify
    from snowtools.tools.initTG import generate_clim
    from snowtools.tools.massif_diags import massif_simu
    from snowtools.tools.update_namelist import update_surfex_namelist_object
    from snowtools.utils.resources import save_file_date, save_file_period
    from snowtools.utils.FileException import MultipleValueException


@echecker.disabled_if_unavailable
class Surfex_PreProcess(AlgoComponent):
    """
    Algo component to apply the required preprocessing before a SURFEX run (e.g. namelists adjustements)
    WARNING : this algo should now be used only in the operationnal S2M chain
    """

    _footprint = {  # noqa: RUF012
        "attr": {
            "kind": {"values": ["surfex_preprocess"]},
            "engine": {"optional": True, "default": "algo"},
            "datebegin": {
                "info": "Date in the namelist to run PREP.",
                "type": Date,
            },
            "dateend": {
                "info": "Date in the namelist to stop OFFLINE.",
                "type": Date,
                "optional": True,
                "default": None
            },
            "forcingname": {
                "info": "Name of the first forcing file",
                "type": str,
            },
            # "forcingname": {
            #     "info": "Name of the first forcing file",
            #     "type": str,
            # },
        }
    }

    def find_namelists(self, opts=None):
        """Find any namelists candidates in actual context inputs."""
        namcandidates = [x.rh for x in self.context.sequence.effective_inputs(kind="namelist")]
        self.system.subtitle("Namelist candidates")
        for nam in namcandidates:
            nam.quickview()

        return namcandidates

    def execute(self, rh, opts):

        print('WARNING : this algo should not be used anymore')

        # Add forcing preparation

        # Modification of the namelist
        for namelist in self.find_namelists():
            # Update the contents of the namelist (date and location)
            # Location taken in the FORCING file.
            first_forcing = self.context.sequence.effective_inputs(kind="FORCING")[0].rh
            forcingname = first_forcing.container.localpath()
            newcontent = update_surfex_namelist_object(
                namelist.contents,
                self.datebegin,
                forcing=self.forcingname,
                dateend=self.dateend
            )
            # Save input namelist for comparison
            self.system.cp(namelist.container.basename, namelist.container.basename.rstrip(".nam") + "_IN.nam")
            newnam = footprints.proxy.container(filename=namelist.container.basename)
            newcontent.rewrite(newnam)
            newnam.close()


@echecker.disabled_if_unavailable
class Generate_Clim_TG(AlgoComponent):
    """
    Algocomponent that generates a file of initial temperature to initialize the soil
    from surfex meteorological forcing files
    """

    _footprint = {  # noqa: RUF012
        "attr": {
            "kind": {"values": ["clim"]},
        }
    }

    def execute(self, rh, opts):

        avail_forcing = self.context.sequence.effective_inputs(role="Forcing")
        listforcing = list({self.system.path.basename(am.rh.container.filename) for am in avail_forcing})

        generate_clim(listforcing)


class SurfexMixIn(_CenMixIn):

    _footprint = dict(
        info="AlgoComponent designed to run SURFEX experiments over large domains with MPI parallelization.",
        attr=dict(
            # Unused ?
            # binary = dict(
            #     values = ['OFFLINE'],
            # ),
            datebegin=dict(info="The first date of the simulation.", type=Date, optional=False),
            dateend=dict(info="The final date of the simulation.", type=Date, optional=False),
            dateinit=dict(
                info="The initialization date if different from the starting date.",
                type=Date,
                optional=True,
                default="[datebegin]",
            ),
            threshold=dict(info="Threshold on snow water equivalent on August 1st.", type=int,
                           optional=True, default=-999),
            daily=dict(
                info="If True, split simulations in daily runs",
                type=bool,
                optional=True,
                default=False,
            ),
            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            ),
        ),
    )

    def execute(self, rh, opts):

        need_other_run = True
        need_other_forcing = True
        datebegin_this_run = self.datebegin

        while need_other_run:
            # Modification of the PREP file
            self.modify_prep(datebegin_this_run)

            if need_other_forcing:
                dateforcbegin, dateforcend, forcingname = self.find_forcing(datebegin_this_run, self.dateend)
                self.link_in(forcingname, 'FORCING.nc')
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

            self.surfex_postprocess(datebegin_this_run, dateend_this_run)

            if need_other_forcing:
                # Remove the symbolic link for next iteration
                self.system.remove("FORCING.nc")

            # Prepare next iteration if needed
            datebegin_this_run = dateend_this_run
            need_other_run = dateend_this_run < self.dateend

    def sort_forcings(self, avail_forcings, list_datebegin, list_dateend):
        """
        Sort available forcing files with ascending *datebegin*
        """
        list_datebegin = np.asarray(list_datebegin)
        list_dateend   = np.asarray(list_dateend)
        avail_forcings = np.asarray(avail_forcings)
        idx = np.argsort(list_datebegin)
        return avail_forcings[idx], list_datebegin[idx], list_dateend[idx]

    def find_forcing(self, datebegin, dateend):
        """
        This method is designed to find a forcing file covering the next simulation period
        (starting at *datebegin*) among available forcing files.
        """
        # First retrieve the list of available forcing files and the associated lists of datebegin/dateend
        if hasattr(self, 'avail_forcings'):
            # In Surfex algo components deriving from Taylorism, the list of available forcings
            # must be retieved before each worker goes into its own context where the effective
            # inputs are no longer available
            avail_forcings = self.avail_forcings
        else:
            avail_forcings = [x.rh for x in self.context.sequence.effective_inputs(role='Forcing')]
        list_datebegin = [forcing.resource.datebegin for forcing in avail_forcings]
        list_dateend = [forcing.resource.dateend for forcing in avail_forcings]

        # Sort lists with ascending *datebegin*
        avail_forcings, list_datebegin, list_dateend = self.sort_forcings(avail_forcings, list_datebegin, list_dateend)

        # Find the index correponding to the last element in the ordered list of forcing's *datebegin*
        # smaller or equal to the begining of the next iteration
        idx_max = np.searchsorted(list_datebegin, datebegin, side='right')

        # Among forcing files starting before the begining of the next iteration, get the list of indices
        # of those ending after the begining of the next iteration
        valid_indices = np.where(list_dateend[:idx_max] > datebegin)[0]

        if len(valid_indices) == 0:
            # raise an error because there is a time period not covered by any forcing file
            next_date = dateend if idx_max == len(list_datebegin) else list_datebegin[idx_max]
            raise FileNotFoundError(f'No forcing file was found for the period {datebegin} - {next_date}')
        elif len(valid_indices) > 1:
            # raise an error because there are several files covering the same period of time
            print("Forcing files found : \n" +
                "\n".join([fic.container.basename for fic in avail_forcings[valid_indices]]))
            raise MultipleValueException
        else:
            # A single forcing file has been identified to run the next simulation's interation
            idx = valid_indices[0]

        # Return the target forcing file's name and the period covered
        forcingname = avail_forcings[idx].container.basename
        logger.info(f'Next FORCING file : {forcingname}')
        return list_datebegin[idx], min(list_dateend[idx], dateend), forcingname

    def find_namelists(self, opts=None):
        """Find any namelists candidates in actual context inputs."""
        namcandidates = [x.rh for x in self.context.sequence.effective_inputs(kind="namelist")]
        self.system.subtitle("Namelist candidates")
        for nam in namcandidates:
            nam.quickview()

        return namcandidates

    def modify_namelist(self, datebegin, dateend, forcingname="FORCING.nc"):

        # Modification of the namelist
        for namelist in self.find_namelists():
            # Update the contents of the namelist (date and location)
            # Location taken in the FORCING file.
            newcontent = update_surfex_namelist_object(namelist.contents, datebegin, forcing=forcingname,
                    dateend=dateend, updateloc=False)
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


class Pgd_Parallel_from_Forcing(Parallel, SurfexMixIn):
    """
    This algo component is designed to run PGD with MPI parallelization and using
    a FORCING.nc as input for topography.
    """

    _footprint = dict(
        info="This algo component is designed to run PGD with MPI parallelization "
        "and using a FORCING.nc as input for topography.",
        attr=dict(
            kind=dict(values=["pgd_from_forcing"]),
            engine=dict(optional=True, default="parallel"),
        ),
    )

    def execute(self, rh, opts):
        firstforcing = self.context.sequence.effective_inputs(role='Forcing')[0]
        # retrieve FORCING information for namelist pre-processing
        datebegin = firstforcing.rh.resource.datebegin
        dateend = firstforcing.rh.resource.dateend
        forcingname = firstforcing.rh.container.basename
        self.system.symlink(forcingname, "FORCING.nc")
        self.modify_namelist(datebegin, dateend, forcingname)
        super().execute(rh, opts)


@echecker.disabled_if_unavailable
class Surfex_Parallel(Parallel, DrHookDecoMixin, SurfexMixIn):
    """
    This algo component is designed to run SURFEX experiments over large domains
    with MPI parallelization.
    """

    _footprint = dict(
        info="AlgoComponent designed to run SURFEX experiments over large domains with MPI parallelization.",
        attr=dict(
            # Unused ?
            # binary = dict(
            #     values = ['OFFLINE'],
            # ),
            datebegin=dict(info="The first date of the simulation.", type=Date, optional=False),
            dateend=dict(info="The final date of the simulation.", type=Date, optional=False),
            dateinit=dict(
                info="The initialization date if different from the starting date.",
                type=Date,
                optional=True,
                default="[datebegin]",
            ),
            threshold=dict(
                info="Threshold on snow water equivalent on August 1st.",
                type=int,
                optional=True,
                default=-999
            ),
            daily=dict(
                info="If True, split simulations in daily runs",
                type=bool,
                optional=True,
                default=False,
            ),
            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            ),
        ),
    )

    def surfex_postprocess(self, datebegin_this_run, dateend_this_run):
        # Post-process
        pro = massif_simu("ISBA_PROGNOSTIC.OUT.nc", openmode="a")
        pro.massif_natural_risk()
        pro.dataset.GlobalAttributes(**self.reprod_info)
        pro.dataset.add_standard_names()
        pro.close()

        save_file_period(".", "ISBA_PROGNOSTIC.OUT", datebegin_this_run, dateend_this_run, newprefix="PRO")

        if self.system.path.isfile("ISBA_DIAGNOSTICS.OUT.nc"):
            save_file_period(".", "ISBA_DIAGNOSTICS.OUT", datebegin_this_run, dateend_this_run, newprefix="DIAG")
        if self.system.path.isfile("ISBA_DIAG_CUMUL.OUT.nc"):
            save_file_period(".", "ISBA_DIAG_CUMUL.OUT", datebegin_this_run, dateend_this_run, newprefix="CUMUL")

    def execute(self, rh, opts):
        self.execute_offline(rh, opts)


class Surfex_Xios_Parallel(Parallel, ParallelIoServerMixin, SurfexMixIn, DrHookDecoMixin):
    """
    This algo component is designed to run SURFEX experiments over large domains
    with MPI parallelization and IO server XIOS
    """

    _footprint = {  # noqa: RUF012
        "info": "AlgoComponent designed to run SURFEX experiments over large domains with MPI parallelization "
                "and IO server XIOS",
        "attr": {
            # Unused ?
            # binary = dict(
            #     values = ['OFFLINE'],
            # ),
            "kind": {"values": ["xios"]},
            "datebegin": {"info": "The first date of the simulation.", "type": Date, "optional": False},
            "dateend": {"info": "The final date of the simulation.", "type": Date, "optional": False},
            "dateinit": {
                "info": "The initialization date if different from the starting date.",
                "type": Date,
                "optional": True,
                "default": "[datebegin]",
            },
            "threshold": {
                "info": "Threshold on snow water equivalent on August 1st.",
                "type": int,
                "optional": True,
                "default": -999,
            },
            "daily": {
                "info": "If True, split simulations in daily runs",
                "type": bool,
                "optional": True,
                "default": False,
            },
            "reprod_info": {
                "info": "Informations that must be stored in output files for reproductibility",
                "type": dict,
                "optional": True,
                "default": {},
            },
        },
    }

    def surfex_postprocess(self, datebegin_this_run, dateend_this_run):

        save_file_period(".", "PRO_nosl.nc", datebegin_this_run, dateend_this_run, newprefix="PRO_nosl")
        save_file_period(".", "PRO_sl1.nc", datebegin_this_run, dateend_this_run, newprefix="PRO_sl1")
        save_file_period(".", "PRO_sl2.nc", datebegin_this_run, dateend_this_run, newprefix="PRO_sl2")

        if self.system.path.isfile("ISBA_DIAGNOSTICS.OUT.nc"):
            save_file_period(".", "ISBA_DIAGNOSTICS.OUT", datebegin_this_run, dateend_this_run, newprefix="DIAG")
        if self.system.path.isfile("ISBA_DIAG_CUMUL.OUT.nc"):
            save_file_period(".", "ISBA_DIAG_CUMUL.OUT", datebegin_this_run, dateend_this_run, newprefix="CUMUL")

    def execute(self, rh, opts):
        self.execute_offline(rh, opts)


class Interpol_Forcing(Parallel):
    """
    This algo component is designed to interpolate SAFRAN forcings on regular grid
    with MPI parallelization.
    """

    _footprint = {  # noqa: RUF012
        "info": "AlgoComponent designed to interpolate SAFRAN forcings on regular grid with MPI parallelization.",
        "attr": {
            # Unused ?
            # binary = dict(
            #     values = ['INTERPOL'],
            # ),
            "reprod_info": {
                "info": "Informations that must be stored in output files for reproductibility",
                "type": dict,
                "optional": True,
                "default": {},
            }
        },
    }

    def execute(self, rh, opts):

        list_forcings = [x.rh for x in self.context.sequence.effective_inputs(role="Forcing")]

        self.algoassert(len(list_forcings) >= 1)
        print([forcing.container.filename for forcing in list_forcings])

        for forcing in list_forcings:
            self.system.mv(forcing.container.filename, "input.nc")
            super().execute(rh, opts)
            self.system.mv("output.nc", forcing.container.filename)
