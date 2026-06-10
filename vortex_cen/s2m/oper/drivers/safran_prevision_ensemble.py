# -*- coding:Utf-8 -*-
"""
SAFRAN forecast
"""

__all__ = []

from vortex_cen.layout.nodes import S2MTaskMixIn
import footprints
import vortex
from mkjob.nodes import Driver

from mkjob.nodes import Task

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag    = 'safran',
        ticket = t,
        nodes  = [
            Safran(tag='prvsaf', ticket=t, delay_component_errors=True, on_error='delayed_fail', **kw),
        ],
        options = kw,
    )


class Safran(Task, S2MTaskMixIn):
    """
    Task : Safran
    =============

    Safran forecast.

    Inputs
    ------
    - Guess : SAFRAN guess ("P" files)
        * 3H run : individual files covering J-5, 6H -> J, 6H
        * 6H run : individual files covering J-1, 6H -> J, 6H
        * 9H run : individual files covering J-1, 6H -> J, 6H
        * 12H run (monthly) : archive containing all files back to the previous 01/08
    - listem : List of SAFRAN massifs
    - listeml : List of coordinates of the SAFRAN massifs
    - Listeo : list of potential observation sites to assimilate
    - NORELmt : Monthly mean precipitation value
    - rsclim / icrccm : Climatological values
    - ADAPT/ANALYSE/EBAUCHE/IMPRESS/MELANGE/SORTIES : Safran namelists
    - carpost.tar : Files describing the output "postes"
    - safrane : Safran executable for the IO assimilation of synoptic surface observations
    - syrpluie / syrmRR : Safran executables for precipitation spatio-temporal precipitation interpolation
    - sypluie : Safran executable for precipitation analysis
    - syvapr / syvafi : Safran executables for the assimilation of hourly observations
    - sytist : Safran executable for hourly interpolation and the creation of FORCING files

    Outputs
    -------
    - FORCING_massifs.nc : Ensemble of forcing files on the "flat" massif geometry
    - FORCING_postes.nc : Ensemble of forcing files on the "postes" geometry
    - listings_safran : output safran execution listings
    - liste_obs : List of assimilated observations
    """

    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "rundate+help=Date of run;choices=YYYYMMDD03;type=str or Date",  # used in the "get_period" method
        "previ+help=Activate forecast mode;typr=bool",  # used in the "get_period" method
        "cumul+help=Output guess files frequency;type=int",
        "source_app+help=NWP files *vapp*;type=str",
        "deterministic_conf+help=ARPEGE *vconf*;type=str",
        "namespace_in+help=Where to look for nwp files;type=str",
        "namespace_out+help=Where to store output guess files;type=str",
        "geometry+help=Geometry covered by the guess files;type=dict[vconf]",
        "cycle+help=Alias for uenv;type=str",
        "xpid_guess+help=*xpid* of the SAFRAN guess files;type=str",
        "guess_block+help=*block* of the SAFRAN guess files;type=str",

    ]
    OPTIONAL_CONFIGURATION_VARIABLES = [
        "diff_xpid+help=*xpid* of the refence FORCING files;type=str",
    ]

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    # Report execution warnings with CEN's method
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    # Report execution errors with CEN's method
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error  # TO MODIFY for operationnal transfer

    def process(self):
        """Safran"""

        t = self.ticket
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            if True:  # To match IGA indentation

                # I- ARPEGE
                # ---------

                # I.1- Pseudo-prevision de (J-1) 6h à J 6h
                # A6 des réseaux 0, 6, 12, 18 (J-1)
                self.sh.title('Toolbox input guess arpege J-1 -> J')
                tb01a = vortex.input(
                    role           = 'Ebauche_Deterministic',
                    # On est obligé d'avoir un "local" précisant le réseau et le cumul
                    # car on a 2 fichiers valides à J 6h (une A6 et une P6)
                    # RQ : on pourrait utiliser la même dans le cas d'ARPEGE, mais
                    # pas pour la PEARP (cf commentaire tb02)
                    local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                    experiment     = self.conf.xpid_guess,
                    block          = self.conf.guess_block,
                    geometry       = self.conf.geometry[self.conf.vconf],
                    cutoff         = 'assimilation',
                    date           = ['{0:s}/+PT{1:s}H/-PT6H'.format(datebegin.ymd6h,
									  str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    namespace      = self.conf.namespace_in,
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    fatal          = False,
                ),
                print(t.prompt, 'tb01a =', tb01a)
                print()

                # L'A6 du réseau 0h J n'est génaralement pas encore là pour le run de 3h, SAFRAN utilisera alors la P6
                # du réseau 0h J récupérée dans la TB suivante car également utilisée pour la prévision de J à J+1.
                # En l'état même si l'A6 du réseau 0h est présente, elle sera écrasée par la P6 qui porte le même nom...
                # RQ : il est fondamental de prendre une P6 pour avoir un cumul des RR sur 6h homogène avec le cumul
                # dans les fichiers d'assimilation
                # P6 du réseau 0h (J)

                # I.2- Prevision de J 6h à J+4 6h

                # P6 à P 102 du réseau 0h J
                self.sh.title('Toolbox input guess arpege J -> J+4')
                tb01b = vortex.input(
                    role           = 'Ebauche_Deterministic',
                    local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                    experiment     = self.conf.xpid_guess,
                    block          = self.conf.guess_block,
                    geometry       = self.conf.geometry[self.conf.vconf],
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    namespace      = self.conf.namespace_in,
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    fatal          = False,
                ),
                print(t.prompt, 'tb01b =', tb01b)
                print()

                # TODO : Pas de mode secours pour le déterministe ?
                # On ne peut pas faire mieux que la prévision jusqu'à J+3 issue
                # du réseau 0h de J-1 qui a tourné la veille...

                # II- PEARP
                # ---------

                # II.1- Prevision de (J-1) 6h à J 6h

                # P0/P6/P12/P18/P24 du réseau 6h (J-1)
                self.sh.title('Toolbox intput guess pearp J-1 -> J')
                tb02a = vortex.input(
                    role           = 'Ebauche',
                    local          = 'mb[member]/P[date::yymdh]_[cumul:hour]',
                    experiment     = self.conf.xpid_guess,
                    block          = self.conf.guess_block,
                    geometry       = self.conf.geometry[self.conf.vconf],
                    date           = '{0:s}'.format(datebegin.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.ana_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    namespace      = self.conf.namespace_in,
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    fatal          = False,
                ),
                print(t.prompt, 'tb02a =', tb02a)
                print()

                # P6 à P102 du réseau 0h (J)
                self.sh.title('Toolbox intput guess pearp J -> J+4')
                tb02b = vortex.input(
                    role           = 'Ebauche',
                    # coherentgroup  = 'pearp_forecast', # Supprime TOUTES les ressources présente dès lors qu'il en manque une
                    local          = 'mb[member]/P[date::yymdh]_[cumul:hour]',
                    experiment     = self.conf.xpid_guess,
                    block          = self.conf.guess_block,
                    geometry       = self.conf.geometry[self.conf.vconf],
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h), # Réseau 0h (J)
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    namespace      = self.conf.namespace_in,
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    fatal          = False,
                ),
                print(t.prompt, 'tb02b =', tb02b)
                print()

                self.sh.title('Toolbox input listem')
                tb03 = vortex.input(
                    role            = 'ListeMassif',
                    genv            = self.conf.cycle,
                    kind            = 'listem',
                    model           = 'safran',
                    local           = 'listem',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb03 =', tb03)
                print()

                self.sh.title('Toolbox input listeml')
                tb04 = vortex.input(
                    role            = 'ListeLimitesMassif',
                    genv            = self.conf.cycle,
                    kind            = 'listeml',
                    model           = 'safran',
                    local           = 'listeml',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb04 =', tb04)
                print()

                self.sh.title('Toolbox input listeo')
                tb05 = vortex.input(
                    role            = 'ListePost',
                    genv            = self.conf.cycle,
                    kind            = 'listeo',
                    model           = 'safran',
                    local           = 'listeo',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb05 =', tb05)
                print()

                # WARNING : Les ressoucre rsclim ne sert pas dans le cas nominal mais
                # constitue un mode secours pour SAFRAN si il rencontre un problème pour faire son guess
                # A partir des fichiers P
#                self.sh.title('Toolbox input rsclim')
#                tb07 = vortex.input(
#                    role            = 'Clim',
#                    genv            = self.conf.cycle,
#                    gvar            = '[kind]',
#                    kind            = 'rsclim',
#                    model           = 'safran',
#                    local           = 'rsclim.don',
#                    geometry        = self.conf.geometry[self.conf.vconf],
#                )
#                print(t.prompt, 'tb07 =', tb07)
#                print()

                self.sh.title('Toolbox input icrccm')
                tb08 = vortex.input(
                    role            = 'Clim',
                    genv            = self.conf.cycle,
                    gvar            = '[kind]',
                    kind            = 'icrccm',
                    model           = 'safran',
                    local           = 'icrccm.don',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb08 =', tb08)
                print()

                self.sh.title('Toolbox input namelist sorties')
                tb09 = vortex.input(
                    role            = 'Nam_sorties',
                    source          = 'namelist_sorties_[geometry:domain]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'safran',
                    local           = 'SORTIES',
                )
                print(t.prompt, 'tb09 =', tb09)
                print()

                self.sh.title('Toolbox input namelist adapt')
                tb14 = vortex.input(
                    role            = 'Nam_adapt',
                    source          = 'namelist_adapt',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'safran',
                    local           = 'ADAPT',
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

                self.sh.title('Toolbox input namelist melange')
                tb10 = vortex.input(
                    role            = 'Nam_melange',
                    source          = 'namelist_melange_[geometry:domain]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'safran',
                    local           = 'MELANGE',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()

                self.sh.title('Toolbox input carpost')
                tb11 = vortex.input(
                    role            = 'carac_post',
                    genv            = self.conf.cycle,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'carpost',
                    model           = 'safran',
                    local           = 'carpost.tar',
                )
                print(t.prompt, 'tb11 =', tb11)
                print()

                self.sh.title('Toolbox input namelist impress')
                tb12 = vortex.input(
                    role            = 'Nam_impress',
                    source          = 'namelist_impress_[geometry:domain]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'safran',
                    local           = 'IMPRESS',
                )
                print(t.prompt, 'tb12 =', tb12)
                print()

                if self.conf.vconf == 'pyr':

                    self.sh.title('Toolbox input namelist observr')
                    tb13 = vortex.input(
                        role            = 'Nam_observr',
                        source          = 'namelist_observr_[geometry:domain]',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        genv            = self.conf.cycle,
                        kind            = 'namelist',
                        model           = 'safran',
                        local           = 'OBSERVR',
                        fatal           = False,
                    )
                    print(t.prompt, 'tb13 =', tb13)
                    print()

                self.sh.title('Toolbox input namelist analyse')
                tb14 = vortex.input(
                    role            = 'Nam_analyse',
                    source          = 'namelist_analyse_[geometry:domain]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'safran',
                    local           = 'ANALYSE',
                    fatal           = False,
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

                self.sh.title('Toolbox input namelist ebauche')
                tb16 = vortex.input(
                    role            = 'Nam_ebauche',
                    source          = 'namelist_ebauche_[geometry:domain]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'safran',
                    local           = 'EBAUCHE',
                    fatal           = False,
                )
                print(t.prompt, 'tb16 =', tb16)
                print()

                self.sh.title('Toolbox executable tbx1 = safrane')
                tb11 = tbx1 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'safrane',
                    local          = 'safrane',
                    model          = 'safran',
                )
                print(t.prompt, 'tb11 =', tb11)
                print()

                self.sh.title('Toolbox executable tbx2 = syrpluie')
                tb12 = tbx2 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syrpluie',
                    local          = 'syrpluie',
                    model          = 'safran',
                )
                print(t.prompt, 'tb12 =', tb12)
                print()

                self.sh.title('Toolbox executable tbx3 = syrmrr')
                tb13 = tbx3 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syrmrr',
                    local          = 'syrmRR',
                    model          = 'safran',
                )
                print(t.prompt, 'tb13 =', tb13)
                print()

                self.sh.title('Toolbox executable tbx4 = sytist')
                tb14 = tbx4 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'sytist',
                    local          = 'sytist',
                    model          = 'safran',
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb15 = SAFRANE')
            tb15 = tbalgo1 = vortex.task(
                engine         = 's2m',
                kind           = 'safrane',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb15 =', tb15)
            print()

            self.component_runner(tbalgo1, tbx1)

            self.sh.title('Toolbox algo tb16 = SYRPLUIE')
            tb16 = tbalgo2 = vortex.task(
                engine         = 's2m',
                kind           = 'syrpluie',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

            self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb17 = SYRMRR')
            tb17 = tbalgo3 = vortex.task(
                engine         = 's2m',
                kind           = 'syrmrr',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb17 =', tb17)
            print()

            self.component_runner(tbalgo3, tbx3)

            self.sh.title('Toolbox algo tb18 = SYTIST')
            tb18 = tbalgo4 = vortex.task(
                engine         = 's2m',
                kind           = 'sytist',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb18 =', tb18)
            print()

            self.component_runner(tbalgo4, tbx4)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            if True:  # To match IGA identation

                self.sh.title('Toolbox output FORCING_massif deteministe')
                tb27 = vortex.output(
                    role           = 'Prv_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    local          = 'mb035/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

#            self.sh.title('Toolbox diff tb27')
#            tb27 = vortex.diff(
#                role           = 'Prv_massifs',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = '4dvarfr',
#                local          = 'mb035/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'massifs',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = 'safran',
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb27 =', tb27)
#            print()

                self.sh.title('Toolbox output FORCING_postes deterministe')
                tb28 = vortex.output(
                    role           = 'Prv_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    local          = 'mb035/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                ),
                print(t.prompt, 'tb28 =', tb28)
                print()

#            self.sh.title('Toolbox diff tb28')
#            tb28 = vortex.diff(
#                role           = 'Prv_postes',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = '4dvarfr',
#                local          = 'mb035/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'postes',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = 'safran',
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb28 =', tb28)
#            print()

                self.sh.title('Toolbox output FORCING_massif pearp')
                tb29 = vortex.output(
                    role           = 'Prv_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = 'pearp',
                    local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print(t.prompt, 'tb29 =', tb29)
                print()

#            self.sh.title('Toolbox diff tb29')
#            tb29 = vortex.diff(
#                role           = 'Prv_massifs',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = 'pearp',
#                local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'massifs',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = 'safran',
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                member         = footprints.util.rangex(self.conf.pearp_members),
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb29 =', tb29)
#            print()

                self.sh.title('Toolbox output FORCING_postes pearp')
                tb30 = vortex.output(
                    role           = 'Prv_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = 'pearp',
                    local          = 'mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print(t.prompt, 'tb30 =', tb30)
                print()

#            self.sh.title('Toolbox diff tb30')
#            tb30 = vortex.diff(
#                role           = 'Prv_postes',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = 'pearp',
#                local          = 'mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'postes',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = 'safran',
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                member         = footprints.util.rangex(self.conf.pearp_members),
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb30 =', tb30)
#            print()

                # TODO : Archiver tous les listings en une seule fois ?

                self.sh.title('Toolbox output listings execution')
                tb31 = vortex.output(
                    role           = 'Listing',
                    block          = 'listing',
                    experiment     = self.conf.xpid,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind           = 'packedlisting',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = 'mb035/listings_safran_[datebegin::ymdh]_[dateend::ymdh].tar.gz',
                    format         = 'tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace_out,
                )
                print(t.prompt, 'tb31 =', tb31)
                print()

                self.sh.title('Toolbox output tb32')
                tb32 = vortex.output(
                    role           = 'Listing',
                    block          = 'listing',
                    experiment     = self.conf.xpid,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind           = 'packedlisting',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = 'mb{glob:a:\d+}/listings_safran_[datebegin::ymdh]_[dateend::ymdh].tar.gz',
                    format         = 'tar',
                    seta           = '[glob:a]',
                    member         = '[seta]',
                    namespace      = self.conf.namespace_out,
                )
                print(t.prompt, 'tb32 =', tb32)
                print()

#            print('==================================================================================================')
#            print('==================================================================================================')
#            raise Exception('INFO :The execution went well, do not take into account the following error')
