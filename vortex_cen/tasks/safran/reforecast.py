# -*- coding:Utf-8 -*-

import footprints
from bronx.stdtypes.date import Period
import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask


class SafranReforecast(_CenResearchTask):
    """
    Task : SafranReforecast
    =======================

    Safran ensemble re-forecast task (daily run covering J 6H --> J+4 6H).
    SAFRAN guess files come from both the PEARP ensemble and ARPEGE (as member 'N+1') from the 0 UTC run.

    Inputs
    ------
    - Guess : daily packed files containing all lead time of a given 0H run of ARPEGE / PERAP
    - listem : List of SAFRAN massifs
    - listeml : List of coordinates of the SAFRAN massifs
    - Listeo : list of potential observation sites to assimilate (mandatory but unused)
    - NORELmt : Monthly mean precipitation value
    - rsclim / icrccm : Climatological values
    - ADAPT/ANALYSE/EBAUCHE/IMPRESS/MELANGE/SORTIES : Safran namelists
    - carpost.tar : Files describing the output "postes"
    - safrane : Safran executable for synoptic interpolation of the guess on the Safran geometry
    - syrpluie / syrmRR : Safran executables for precipitation spatio-temporal precipitation interpolation
    - sytist : Safran executable for hourly interpolation and the creation of FORCING files

    Outputs
    -------
    - FORCING_massifs.nc : Ensemble of forcing files on the "flat" massif geometry
    - FORCING_postes.nc : Ensemble of forcing files on the "postes" geometry
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin+help=First rundate of the guess (hour must be '00')",
            "dateend+help=Last run date of the guess (hour must be '00')",
            "xpid",
            "geometry",
            "uenv+help=Name of the UEnv containing all SAFRAN constant input files and executables",
            "prv_terms",
            "uenv",
            "ntasks",
            "nnodes",
            "members",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "guess_xpid",
            "guess_user",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        t = self.ticket

        rundate = self.conf.datebegin
        while rundate <= self.conf.dateend:

            self.sh.title('Input Guess')
            guess = vortex.input(
                role            = 'Ebauche',
                local           = f'{rundate.ymdh}/ebauches_[geometry:domain]_[datebegin:ymdh]_[dateend:ymdh].tar',
                kind            = 'packedguess',
                experiment      = self.conf.get('guess_xpid', self.conf.xpid),
                username        = self.conf.get('guess_user', None),
                block           = 'guess',
                geometry        = self.conf.geometry,
                nativefmt       = 'tar',
                namespace       = 'vortex.multi.fr',
                namebuild       = 'flat@cen',
                datebegin       = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[0]),
                dateend         = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[-1]),
                model           = 'safran',
                auto_tarextract = True,
            ),
            print(t.prompt, 'Guess =', guess)
            print()

            rundate = rundate + Period(days=1)

        self.sh.title('Input listem')
        listem = vortex.input(
            role            = 'ListeMassif',
            genv            = self.conf.uenv,
            kind            = 'listem',
            model           = 'safran',
            local           = 'listem',
            geometry        = self.conf.geometry,
            gdomain         = '[geometry:domain]',
        )
        print(t.prompt, 'listem =', listem)
        print()

        self.sh.title('Input listeml')
        listeml = vortex.input(
            role            = 'ListeLimitesMassif',
            genv            = self.conf.uenv,
            kind            = 'listeml',
            model           = 'safran',
            local           = 'listeml',
            geometry        = self.conf.geometry,
            gdomain         = '[geometry:domain]',
        )
        print(t.prompt, 'listeml =', listeml)
        print()

        self.sh.title('Input listeo')
        listeo = vortex.input(
            role            = 'ListeObs',
            genv            = self.conf.uenv,
            kind            = 'listeo',
            model           = 'safran',
            local           = 'listeo',
            geometry        = self.conf.geometry,
            gdomain         = '[geometry:domain]',
        )
        print(t.prompt, 'listeo=', listeo)
        print()

        if self.conf.geometry.area in ['alp', 'pyr']:

            self.sh.title('Input norelmt')
            norelmt = vortex.input(
                role            = 'MoyRRmensuelles',
                genv            = self.conf.uenv,
                kind            = 'NORELmt',
                model           = 'safran',
                local           = 'NORELmt',
                geometry        = self.conf.geometry,
                gdomain         = '[geometry:domain]',
            )
            print(t.prompt, 'norelmt =', norelmt)
            print()

        self.sh.title('Input rsclim')
        rsclim = vortex.input(
            role            = 'Clim',
            genv            = self.conf.uenv,
            kind            = 'rsclim',
            model           = 'safran',
            local           = 'rsclim.don',
            geometry        = self.conf.geometry,
            gdomain         = '[geometry:domain]',
        )
        print(t.prompt, 'rsclim =', rsclim)
        print()

        self.sh.title('Input icrccm')
        icrccm = vortex.input(
            role            = 'Clim',
            genv            = self.conf.uenv,
            kind            = 'icrccm',
            model           = 'safran',
            local           = 'icrccm.don',
            geometry        = self.conf.geometry,
            gdomain         = '[geometry:domain]',
        )
        print(t.prompt, 'icrccm =', icrccm)
        print()

        self.sh.title('Input SORTIES')
        sorties = vortex.input(
            role            = 'Nam_sorties',
            source          = 'namelist_sorties_[geometry:domain]',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'SORTIES',
        )
        print(t.prompt, 'sorties =', sorties)
        print()

        self.sh.title('Input adapt')
        adapt = vortex.input(
            role            = 'Nam_adapt',
            source          = 'namelist_adapt',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'ADAPT',
        )
        print(t.prompt, 'adapt =', adapt)
        print()

        self.sh.title('Input melange')
        melange = vortex.input(
            role            = 'Nam_melange',
            source          = 'namelist_melange_[geometry:domain]',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'MELANGE',
        )
        print(t.prompt, 'melange =', melange)
        print()

        self.sh.title('Input carpost')
        carpost = vortex.input(
            role            = 'carac_post',
            genv            = self.conf.uenv,
            geometry        = self.conf.geometry,
            kind            = 'carpost',
            model           = 'safran',
            local           = 'carpost.tar',
            gdomain         = '[geometry:domain]',
        )
        print(t.prompt, 'carpost =', carpost)
        print()

        self.sh.title('Input impress')
        impress = vortex.input(
            role            = 'Nam_impress',
            source          = 'namelist_impress_[geometry:domain]',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'IMPRESS',
        )
        print(t.prompt, 'impress =', impress)
        print()

        self.sh.title('Input analyse')
        analyse = vortex.input(
            role            = 'Nam_analyse',
            source          = 'namelist_analyse_[geometry:domain]',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'ANALYSE',
            fatal           = False,
        )
        print(t.prompt, 'analyse =', analyse)
        print()

        self.sh.title('Input ebauche')
        ebauche = vortex.input(
            role            = 'Nam_ebauche',
            source          = 'namelist_ebauche_[geometry:domain]',
            geometry        = self.conf.geometry,
            genv            = self.conf.uenv,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'EBAUCHE',
            fatal           = False,
        )
        print(t.prompt, 'ebauche =', ebauche)
        print()

        self.sh.title('Executable safrane')
        self.safrane = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'safrane',
            local          = 'safrane',
            model          = 'safran',
        )
        print(t.prompt, 'safrane =', self.safrane)
        print()

        self.sh.title('Executable syrpluie')
        self.syrpluie = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'syrpluie',
            local          = 'syrpluie',
            model          = 'safran',
        )
        print(t.prompt, 'syrpluie =', self.syrpluie)
        print()

        self.sh.title('Executable syrmRR')
        self.syrmrr = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'syrmrr',
            local          = 'syrmRR',
            model          = 'safran',
        )
        print(t.prompt, 'syrmrr =', self.syrmrr)
        print()

        self.sh.title('Executable sytist')
        self.sytist = vortex.executable(
            role           = 'Binary',
            genv           = self.conf.uenv,
            kind           = 'sytist',
            local          = 'sytist',
            model          = 'safran',
        )
        print(t.prompt, 'sytist =', self.sytist)
        print()

    def get_local_inputs(self):
        pass

    def algo(self):

        t = self.ticket

        self.sh.title('Algo SAFRANE')
        safrane = vortex.task(
            engine         = 's2m',
            kind           = 'safrane',
            execution      = 'reforecast',
            ntasks         = int(self.conf.ntasks) * int(self.conf.nnodes),
        )
        print(t.prompt, 'safrane =', safrane)
        print()

        self.component_runner(safrane, self.safrane)

        self.sh.title('Algo SYRPLUIE')
        syrpluie = vortex.task(
            engine         = 's2m',
            kind           = 'syrpluie',
            execution      = 'reforecast',
            ntasks         = int(self.conf.ntasks) * int(self.conf.nnodes),
        )
        print(t.prompt, 'syrpluie =', syrpluie)
        print()

        self.component_runner(syrpluie, self.syrpluie)

        self.sh.title('Algo SYRMRR')
        syrmrr = vortex.task(
            engine         = 's2m',
            kind           = 'syrmrr',
            execution      = 'reforecast',
            ntasks         = int(self.conf.ntasks) * int(self.conf.nnodes),
        )
        print(t.prompt, 'syrmrr =', syrmrr)
        print()

        self.component_runner(syrmrr, self.syrmrr)

        self.sh.title('Toolbox algo tb18 = SYTIST')
        sytist = vortex.task(
            engine         = 's2m',
            kind           = 'sytist',
            execution      = 'reforecast',
            ntasks         = int(self.conf.ntasks) * int(self.conf.nnodes),
        )
        print(t.prompt, 'sytist =', sytist)
        print()

        self.component_runner(sytist, self.sytist)

    def launch_algo(self, algo):
        pass

    def put_outputs(self):

        t = self.ticket

        rundate = self.conf.datebegin
        while rundate <= self.conf.dateend:

            self.sh.title('Output FRORCING massifs')
            massifs = vortex.output(
                role           = 'Prv_massifs',
                kind           = 'MeteorologicalForcing',
                local          = f'{rundate.ymdh}/mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                experiment     = self.conf.xpid,
                block          = 'massifs',
                geometry        = self.conf.geometry,
                nativefmt      = 'netcdf',
                model          = 'safran',
                datebegin      = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[0]),
                dateend        = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[-1]),
                namespace      = 'vortex.multi.fr',
                member         = footprints.util.rangex(self.conf.members),
                namebuild      = 'flat@cen',
            ),
            print(t.prompt, 'massifs =', massifs)
            print()

            self.sh.title('Output FORCING postes')
            postes = vortex.output(
                role           = 'Prv_postes',
                kind           = 'MeteorologicalForcing',
                local          = f'{rundate.ymdh}/mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                experiment     = self.conf.xpid,
                block          = 'postes',
                geometry        = self.conf.geometry,
                nativefmt      = 'netcdf',
                model          = 'safran',
                datebegin      = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[0]),
                dateend        = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[-1]),
                namespace      = 'vortex.multi.fr',
                member         = footprints.util.rangex(self.conf.members),
                namebuild      = 'flat@cen',
            ),
            print(t.prompt, 'postes =', postes)
            print()

            rundate = rundate + Period(days=1)
