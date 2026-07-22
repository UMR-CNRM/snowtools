# -*- coding:Utf-8 -*-
"""
reanalysis.py
--------------

.. autoclass:: SafranReanalysis
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

__all__ = []

import vortex
from vortex.syntax.stdattrs import nativefmt

from vortex_cen.tasks.research_task_base import _CenResearchTask


class SafranReanalysis(_CenResearchTask):
    """
    **Task : SafranReanalysis**

    SAFRAN reanalysis.

    Reference : https://essd.copernicus.org/articles/14/1707/2022/
    Associated opensource dataset : https://doi.org/10.25326/37#v2020.2

    **Input:**

    - Guess : daily packed files containing all lead time of a given 0H run of ARPEGE / PERAP
    - Observations : Packed SAFRAN-readable surface observation files (R, S and T files)
    - listem : List of SAFRAN massifs
    - listeml : List of coordinates of the SAFRAN massifs
    - Listeo : list of potential observation sites to assimilate
    - NORELmt : Monthly mean precipitation value
    - rsclim / icrccm : Climatological values
    - ADAPT/ANALYSE/EBAUCHE/IMPRESS/MELANGE/SORTIES : Safran namelists
    - carpost.tar : Files describing the output "postes"
    - safrane : Safran executable for synoptic interpolation of the guess on the Safran geometry
    - syrpluie / syrmRR : Safran executables for precipitation spatio-temporal precipitation interpolation
    - sytist : Safran executable for hourly interpolation and the creation of FORCING files

    **Output:**

    - FORCING_massifs.nc : Ensemble of forcing files on the "flat" massif geometry
    - FORCING_postes.nc : Ensemble of forcing files on the "postes" geometry
    - listings_safran : output safran execution listings
    - liste_obs : List of assimilated observations

    Mandatory Configuration Variables:
    ----------------------------------

    * ``datebegin`` First rundate of the guess (hour must be '00')
    * ``dateend`` Last run date of the guess (hour must be '00')
    * ``xpid`` Experiment id. Do not use experiment ids with 4 letters.
    * ``geometry`` Geometry of the simulation. This must be a valid geometry in your
      '$HOME/.vortexrc/geometries.ini' file.
    * ``guess_geometry`` Geometry of SAFRAN guess files. type: dict
    * ``obs_geometry`` geometry of the observation files. type: dict
    * ``uenv`` Name of the UEnv containing all SAFRAN constant input files and executables
    * ``ntasks`` Number of parallel tasks to allocate to the execution.  type: int or dict[geometry]
    * ``nnodes`` Number of nodes to allocate to the execution. type: int or dict[geometry]
    * ``execution`` Type of SAFRAN execution. type: str, choices: analysis, forecast, reanalysis, reforecast
    * ``assim`` Allow assimilation of observations. type: bool

    Optional Configuration Variables:
    ---------------------------------

    * ``obs_xpid`` Experiment identifier of the observation files. type: str, default: *xpid*
    * ``obs_vapp`` *vapp* level of the observation files. type: str, default: *vapp*
    * ``obs_vconf`` *vconf* level of the observation files. type: str, default: *vconf*
    * ``obs_user`` Name of the producer of the observation files. type: str
    * ``diff_xpid`` Experiment identifier of the reference files for reproducibility check. type: str
    * ``diff_user`` Name of the producer of the reference files for reproducibility check. type: str
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin",
            "dateend",
            "xpid",
            "geometry",
            "guess_geometry+help=Geometry of SAFRAN guess files;type=dict",
            "obs_geometry+help=geometry of the observation files;type=dict",
            "uenv+help=Name of the UEnv containing all SAFRAN constant input files and executables",
            "ntasks",
            "nnodes",
            "execution+help=Type of SAFRAN execution;type=str;choices=analysis, forecast, reanalysis, reforecast",
            "assim+help=Allow assimilation of observations;type=bool",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "obs_xpid+help=Experiment identifier of the observation files;type=str;default=*xpid*",
            "obs_vapp+help=*vapp* level of the observation files;type=str;default=*vapp*",
            "obs_vconf+help=*vconf* level of the observation files;type=str;default=*vconf*",
            "obs_user+help=Name of the producer of the observation files;type=str",
            "diff_xpid+help=Experiment identifier of the reference files for reproducibility check;type=str",
            "diff_user+help=Name of the producer of the reference files for reproducibility check;type=str",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_local_inputs(self):
        pass

    def launch_algo(self, algo):
        pass

    def get_remote_inputs(self):

        def untar_hook(t, rh):
            sh = t.sh
            target = rh.container.localpath()
            with sh.cdcontext(sh.path.dirname(target)):
                tarname = sh.path.basename(target)
                if sh.is_tarfile(tarname):
                    sh.untar(tarname)

        t = self.ticket

        rundate = self.conf.datebegin
        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
        for rundate in list_dates:
            datebegin = rundate
            dateend = rundate.replace(year = rundate.year + 1)
#                dateend = rundate + Period(years=1)
            season = datebegin.nivologyseason
            y1 = datebegin.year
            y2 = dateend.year

            self.sh.title(f'Input Guess - {y1:d}/{y2:d}')
            tb01 = vortex.input(
                role           = 'Ebauche',
                kind           = 'packedguess',
                local          = '{0:s}_{1:s}/guess{2:s}.tar'.format(datebegin.ymd6h, dateend.ymd6h, season),
                namespace      = 's2m.archive.fr',
                geometry       = self.conf.guess_geometry[self.conf.geometry.area],
                cumul          = self.conf.get('cumul', 6),
                nativefmt      = 'tar',
                model          = 'safran',
                source         = 'era5',
                date           = dateend.ymdh,
                datebegin      = datebegin.ymdh,
                dateend        = dateend.ymdh,
                now            = True,
                fatal          = True,
            ),
            print(t.prompt, 'tb01 =', tb01)
            print()

            if self.conf.assim:

                self.sh.title(f'Input Observations - {y1:d}/{y2:d}')
                tb02 = vortex.input(
                    role           = 'Observations',
                    # part           = 'all',
                    vapp          = self.conf.get("obs_vapp", self.conf.vapp),
                    vconf          = self.conf.get("obs_vconf", self.conf.vconf),
                    geometry       = self.conf.obs_geometry[self.conf.geometry.area],
                    kind           = 'packedobs',
                    local          = '{0:s}_{1:s}/rs{2:s}.tar'.format(datebegin.ymd6h, dateend.ymd6h, season),
                    experiment     = self.conf.get('obs_xpid', self.conf.xpid),
                    username       = self.conf.get('obs_user', None),
                    namespace      = 'vortex.multi.fr',
                    date           = dateend.ymdh,
                    datebegin      = datebegin.ymdh,
                    dateend        = dateend.ymdh,
                    model          = 'safran',
                    source         = 'surfaceobs',
                    namebuild      = 'flat@cen',
                    block          = 'observations',
                    nativefmt      = 'tar',
                    now            = True,
                    fatal          = True,
                    # Untar is not automatic for resrouces from the "ArchiveStore", unlike
                    # for resources coming from the "Finder" store
                    hook_autohook1 = (untar_hook, ),
                )
                print(t.prompt, 'tb02 =', tb02)
                print()

        self.sh.title('Input listem')
        tb07 = vortex.input(
            role            = 'ListeMassif',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'listem',
            nativefmt = "ascii",
            model           = 'safran',
            local           = 'listem',
        )
        print(t.prompt, 'tb07 =', tb07)
        print()

        self.sh.title('Input listeml')
        tb08 = vortex.input(
            role            = 'ListeLimitesMassif',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'listeml',
            nativefmt="ascii",
            model           = 'safran',
            local           = 'listeml',
        )
        print(t.prompt, 'tb08 =', tb08)
        print()

        self.sh.title('Input listeo')
        tb09 = vortex.input(
            role            = 'ListePost',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'listeo',
            nativefmt="ascii",
            model           = 'safran',
            local           = 'listeo',
        )
        print(t.prompt, 'tb09 =', tb09)
        print()

        self.sh.title('Input NORELot')
        tb09 = vortex.input(
            role            = 'MoyennesMensuellesRR',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'NORELot',
            nativefmt="ascii",
            model           = 'safran',
            local           = 'NORELot',
            fatal           = False,
        )
        print(t.prompt, 'tb09 =', tb09)
        print()

        self.sh.title('Input surfz')
        tb09 = vortex.input(
            role            = 'SurfZ',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'surfz',
            model           = 'safran',
            local           = 'surfz',
            nativefmt       = 'ascii',
        )
        print(t.prompt, 'tb09 =', tb09)
        print()

        self.sh.title('Input CARPOST')
        tb09 = vortex.input(
            role            = 'carac_post',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'carpost',
            model           = 'safran',
            nativefmt='ascii',
            local           = 'carpost.tar',
        )
        print(t.prompt, 'tb09 =', tb09)
        print()

        self.sh.title('Input Blacklist')
        tb12 = vortex.input(
            role            = 'BlackList',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'blacklist',
            model           = 'safran',
            local           = 'BLACK',
            nativefmt='ascii',
            fatal           = False,
        )
        print(t.prompt, 'tb12 =', tb12)
        print()

        self.sh.title('Input NORELmt')
        tb08 = vortex.input(
            role            = 'NormalesClimTT',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'NORELmt',
            model           = 'safran',
            nativefmt='ascii',
            local           = 'NORELmt',
            fatal           = False,
        )
        print(t.prompt, 'tb08 =', tb08)
        print()

        self.sh.title('Input rsclim')
        tb11 = vortex.input(
            role            = 'Clim',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'rsclim',
            model           = 'safran',
            nativefmt='ascii',
            local           = 'rsclim.don',
            fatal           = False,
        )
        print(t.prompt, 'tb11 =', tb11)
        print()

        self.sh.title('Input icrccm')
        tb12 = vortex.input(
            role            = 'Clim',
            genv            = self.conf.uenv,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry,
            kind            = 'icrccm',
            model           = 'safran',
            nativefmt='ascii',
            local           = 'icrccm.don',
            fatal           = False,
        )
        print(t.prompt, 'tb12 =', tb12)
        print()

        self.sh.title('Input Namelists')
        tb13 = vortex.input(
            source          = 'namelist_[local::lower]_[geometry:domain]',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = ['SORTIES', 'ANALYSE', 'ADAPT', 'MELANGE', 'OBSERVR', 'OBSERVA', 'EBAUCHE'],
            fatal           = False,
        )
        print(t.prompt, 'tb13 =', tb13)
        print()

        self.sh.title('Executable safrane')
        self.safrane = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'safrane',
            local          = 'safrane',
            model          = 'safran',
        )
        print(t.prompt, 'tb17 =', self.safrane)
        print()

        self.sh.title('Executable syrpluie')
        self.syrpluie = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'syrpluie',
            local          = 'syrpluie',
            model          = 'safran',
        )
        print(t.prompt, 'tb18 =', self.syrpluie)
        print()

        if self.conf.assim:

            self.sh.title('Executable sypluie')
            self.sypluie = vortex.executable(
                role           = 'Binary',
                genv           = self.conf.uenv,
                kind           = 'sypluie',
                local          = 'sypluie',
                model          = 'safran',
            )
            print(t.prompt, 'tb18_b =', self.sypluie)
            print()

            self.sh.title('Executable syvapr')
            self.syvapr = vortex.executable(
                role           = 'Binary',
                genv           = self.conf.uenv,
                kind           = 'syvapr',
                local          = 'syvapr',
                model          = 'safran',
            )
            print(t.prompt, 'tb19 =', self.syvapr)
            print()

            self.sh.title('Executable syvafi')
            self.syvafi = vortex.executable(
                role           = 'Binary',
                genv           = self.conf.uenv,
                kind           = 'syvafi',
                local          = 'syvafi',
                model          = 'safran',
            )
            print(t.prompt, 'tb20 =', self.syvafi)
            print()

        else:

            self.sh.title('Executable syrmRR')
            self.syrmrr = vortex.executable(
                role           = 'Binary',
                genv           = self.conf.uenv,
                kind           = 'syrmrr',
                local          = 'syrmRR',
                model          = 'safran',
            )
            print(t.prompt, 'tb13 =', self.syrmrr)
            print()

        self.sh.title('Executable sytist')
        self.sytist = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'sytist',
            local          = 'sytist',
            model          = 'safran',
        )
        print(t.prompt, 'tb21 =', self.sytist)
        print()

    def algo(self):

        t = self.ticket

        # NB : La date des executions est fixée à J-1 car l'analyse SAFRAN va de J-1 6h à J 6H
        self.sh.title('Algo SAFRANE')
        tb22 = tbalgo1 = vortex.task(
            engine         = 'blind',  # vortex2
            # engine         = 's2m',  # vortex1
            kind           = 'safrane',
            datebegin      = self.conf.datebegin.ymd6h,
            dateend        = self.conf.dateend.ymd6h,
            ntasks         = self.conf.ntasks,
            execution      = self.conf.execution,
        )
        print(t.prompt, 'tb22 =', tb22)
        print()

        self.component_runner(tbalgo1, self.safrane)

#            if self.conf.execution == 'analysis':

        # Cas d'une execution où l'on veut utiliser les rr ARPEGE comme guess
        # A lancer avec un job_name=with_rr_arpege (cf fichier de conf)
        # WARNING : si execution="analysis" l'algo component définit les
        # observations comme des fichiers "communs", ce qui n'est pas le
        # cas.
        # Les obs sont néanmoins bien assimilées car si les fichiers d'obs ne sont pas
        # au chemin indiqué dans les fichiers OP* (ce qui dépend de la variable "execution"),
        # SAFRAN les cherche dans le répertoire courrant.
        self.sh.title('Algo SYRPLUIE')
        tb23 = tbalgo2 = vortex.task(
            engine         = 'blind',  # vortex2
            # engine         = 's2m',  # vortex1
            kind           = 'syrpluie',
            datebegin      = self.conf.datebegin.ymd6h,
            dateend        = self.conf.dateend.ymd6h,
            ntasks         = self.conf.ntasks,
            execution      = self.conf.execution,
        )
        print(t.prompt, 'tb23 =', tb23)
        print()
        self.component_runner(tbalgo2, self.syrpluie)

        if self.conf.assim:

            self.sh.title('Algo SYPLUIE')
            tb23 = tbalgo3 = vortex.task(
                engine         = 'blind',  # vortex2
                # engine         = 's2m',  # vortex1
                kind           = 'sypluie',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb23 =', tb23)
            print()
            self.component_runner(tbalgo3, self.sypluie)

            self.sh.title('Algo SYVAPR')
            tb24 = tbalgo4 = vortex.task(
                engine         = 'blind',  # vortex2
                # engine         = 's2m',  # vortex1
                kind           = 'syvapr',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb24 =', tb24)
            print()
            self.component_runner(tbalgo4, self.syvapr)

            self.sh.title('Algo SYVAFI')
            tb25 = tbalgo5 = vortex.task(
                engine         = 'blind',  # vortex2
                # engine         = 's2m',  # vortex1
                kind           = 'syvafi',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb25 =', tb25)
            print()
            self.component_runner(tbalgo5, self.syvafi)

        else:

            self.sh.title('Algo SYRMRR')
            tb17 = tbalgo3 = vortex.task(
                engine         = 'blind',
                kind           = 'syrmrr',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb17 =', tb17)
            print()
            self.component_runner(tbalgo3, self.syrmrr)

        self.sh.title('Algo  SYTIST')
        tb26 = tbalgo6 = vortex.task(
            engine         = 'blind',  # vortex2
            # engine         = 's2m',  # vortex1
            kind           = 'sytist',
            datebegin      = self.conf.datebegin.ymd6h,
            dateend        = self.conf.dateend.ymd6h,
            ntasks         = self.conf.ntasks,
            execution      = self.conf.execution,
            metadata       = 'StandardSAFRAN',
        )
        print(t.prompt, 'tb26 =', tb26)
        print()
        self.component_runner(tbalgo6, self.sytist)

    def put_outputs(self):

        t = self.ticket

        rundate = self.conf.datebegin
        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
        for rundate in list_dates:
            datebegin = rundate
            dateend = min(datebegin.replace(year=datebegin.year + 1), self.conf.dateend)

            self.sh.title('Output forcing massifs')
            tb27 = vortex.output(
                role           = 'Ana_massifs',
                kind           = 'MeteorologicalForcing',
                cutoff         = 'assimilation',
                local          = '[datebegin::ymd6h]_[dateend::ymd6h]/'
                                 'FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                experiment     = self.conf.xpid,
                block          = 'safran/massifs',
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                date           = dateend.ymd6h,
                namespace      = self.conf.namespace,
                namebuild      = 'flat@cen',
            ),
            print(t.prompt, 'tb27 =', tb27)
            print()

            self.sh.title('Output forcing postes')
            tb27 = vortex.output(
                role           = 'Ana_postes',
                kind           = 'MeteorologicalForcing',
                cutoff         = 'assimilation',
                local          = '[datebegin::ymd6h]_[dateend::ymd6h]/'
                                 'FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                experiment     = self.conf.xpid,
                block          = 'safran/postes',
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                date           = dateend.ymd6h,
                namespace      = self.conf.namespace,
                namebuild      = 'flat@cen',
            ),
            print(t.prompt, 'tb28 =', tb27)
            print()

            self.sh.title('Output liste observations')
            tb29 = vortex.output(
                role           = 'Liste_obs',
                block          = 'safran/liste_obs',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                cutoff         = 'assimilation',
                nativefmt      = 'tar',
                model          = 'safran',
                kind           = 'listobs',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                date           = dateend.ymd6h,
                local          = '[datebegin::ymd6h]_[dateend::ymd6h]/'
                                 'liste_obs_[datebegin::ymd6h]_[dateend::ymd6h].tar.gz',
                namespace      = self.conf.namespace,
                namebuild      = 'flat@cen',
            )
            print(t.prompt, 'tb32 =', tb29)
            print()

            self.sh.title('Output listings')
            tb31 = vortex.output(
                role           = 'Listing',
                block          = 'safran/listing',
                experiment     = self.conf.xpid,
                cutoff         = 'assimilation',
                geometry        = self.conf.geometry,
                kind           = 'packedlisting',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                date           = dateend.ymd6h,
                local          = '[datebegin::ymd6h]_[dateend::ymd6h]/'
                                 'listings_safran_[datebegin::ymdh]_[dateend::ymdh].tar.gz',
                nativefmt      = 'tar',
                model          = 'safran',
                namespace      = self.conf.namespace,
                namebuild      = 'flat@cen',
            )
            print(t.prompt, 'tb31 =', tb31)
            print()

    def diff(self):

        output = self.ticket.context.sequence.effective_outputs(role='Ana_massifs')
        for out in output:
            filename = out.rh.container.filename
            datebegin = out.rh.resource.datebegin
            dateend = out.rh.resource.dateend
            geometry = out.rh.resource.geometry
            self.sh.title('Output diff')
            diff = vortex.diff(
                kind           = 'MeteorologicalForcing',
                datebegin      = datebegin,
                dateend        = dateend,
                geometry       = geometry,
                experiment     = self.conf.diff_xpid,
                username       = self.conf.get('diff_username', None),
                block          = 'safran/massifs',
                namebuild      = 'flat@cen',
                local          = filename,
            ),
            print(self.ticket.prompt, 'diff =', diff)
            print()
