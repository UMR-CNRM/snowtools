#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver

from vortex.layout.nodes import Task

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
                tb01a = toolbox.input(
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
                tb01b = toolbox.input(
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
                tb02a = toolbox.input(
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
                tb02b = toolbox.input(
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
                tb03 = toolbox.input(
                    role            = 'ListeMassif',
                    genv            = self.conf.cycle,
                    kind            = 'listem',
                    model           = self.conf.model,
                    local           = 'listem',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb03 =', tb03)
                print()

                self.sh.title('Toolbox input listeml')
                tb04 = toolbox.input(
                    role            = 'ListeLimitesMassif',
                    genv            = self.conf.cycle,
                    kind            = 'listeml',
                    model           = self.conf.model,
                    local           = 'listeml',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb04 =', tb04)
                print()

                self.sh.title('Toolbox input listeo')
                tb05 = toolbox.input(
                    role            = 'ListePost',
                    genv            = self.conf.cycle,
                    kind            = 'listeo',
                    model           = self.conf.model,
                    local           = 'listeo',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb05 =', tb05)
                print()

                # WARNING : Les ressoucre rsclim ne sert pas dans le cas nominal mais
                # constitue un mode secours pour SAFRAN si il rencontre un problème pour faire son guess
                # A partir des fichiers P
#                self.sh.title('Toolbox input rsclim')
#                tb07 = toolbox.input(
#                    role            = 'Clim',
#                    genv            = self.conf.cycle,
#                    gvar            = '[kind]',
#                    kind            = 'rsclim',
#                    model           = self.conf.model,
#                    local           = 'rsclim.don',
#                    geometry        = self.conf.geometry[self.conf.vconf],
#                )
#                print(t.prompt, 'tb07 =', tb07)
#                print()

                self.sh.title('Toolbox input icrccm')
                tb08 = toolbox.input(
                    role            = 'Clim',
                    genv            = self.conf.cycle,
                    gvar            = '[kind]',
                    kind            = 'icrccm',
                    model           = self.conf.model,
                    local           = 'icrccm.don',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb08 =', tb08)
                print()

                self.sh.title('Toolbox input namelist sorties')
                tb09 = toolbox.input(
                    role            = 'Nam_sorties',
                    source          = 'namelist_sorties_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'SORTIES',
                )
                print(t.prompt, 'tb09 =', tb09)
                print()

                self.sh.title('Toolbox input namelist adapt')
                tb14 = toolbox.input(
                    role            = 'Nam_adapt',
                    source          = 'namelist_adapt',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'ADAPT',
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

                self.sh.title('Toolbox input namelist melange')
                tb10 = toolbox.input(
                    role            = 'Nam_melange',
                    source          = 'namelist_melange_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'MELANGE',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()

                self.sh.title('Toolbox input carpost')
                tb11 = toolbox.input(
                    role            = 'carac_post',
                    genv            = self.conf.cycle,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'carpost',
                    model           = self.conf.model,
                    local           = 'carpost.tar',
                )
                print(t.prompt, 'tb11 =', tb11)
                print()

                self.sh.title('Toolbox input namelist impress')
                tb12 = toolbox.input(
                    role            = 'Nam_impress',
                    source          = 'namelist_impress_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'IMPRESS',
                )
                print(t.prompt, 'tb12 =', tb12)
                print()

                if self.conf.vconf == 'pyr':

                    self.sh.title('Toolbox input namelist observr')
                    tb13 = toolbox.input(
                        role            = 'Nam_observr',
                        source          = 'namelist_observr_[geometry:area]',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        genv            = self.conf.cycle,
                        kind            = 'namelist',
                        model           = self.conf.model,
                        local           = 'OBSERVR',
                        fatal           = False,
                    )
                    print(t.prompt, 'tb13 =', tb13)
                    print()

                self.sh.title('Toolbox input namelist analyse')
                tb14 = toolbox.input(
                    role            = 'Nam_analyse',
                    source          = 'namelist_analyse_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'ANALYSE',
                    fatal           = False,
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

                self.sh.title('Toolbox input namelist ebauche')
                tb16 = toolbox.input(
                    role            = 'Nam_ebauche',
                    source          = 'namelist_ebauche_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'EBAUCHE',
                    fatal           = False,
                )
                print(t.prompt, 'tb16 =', tb16)
                print()

                self.sh.title('Toolbox executable tbx1 = safrane')
                tb11 = tbx1 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'safrane',
                    local          = 'safrane',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb11 =', tb11)
                print()

                self.sh.title('Toolbox executable tbx2 = syrpluie')
                tb12 = tbx2 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syrpluie',
                    local          = 'syrpluie',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb12 =', tb12)
                print()

                self.sh.title('Toolbox executable tbx3 = syrmrr')
                tb13 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syrmrr',
                    local          = 'syrmRR',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb13 =', tb13)
                print()

                self.sh.title('Toolbox executable tbx4 = sytist')
                tb14 = tbx4 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'sytist',
                    local          = 'sytist',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb15 = SAFRANE')
            tb15 = tbalgo1 = toolbox.algo(
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
            tb16 = tbalgo2 = toolbox.algo(
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
            tb17 = tbalgo3 = toolbox.algo(
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
            tb18 = tbalgo4 = toolbox.algo(
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
                tb27 = toolbox.output(
                    role           = 'Prv_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    local          = 'mb035/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

#            self.sh.title('Toolbox diff tb27')
#            tb27 = toolbox.diff(
#                role           = 'Prv_massifs',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = '4dvarfr',
#                local          = 'mb035/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'massifs',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = self.conf.model,
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb27 =', tb27)
#            print()

                self.sh.title('Toolbox output FORCING_postes deterministe')
                tb28 = toolbox.output(
                    role           = 'Prv_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    local          = 'mb035/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                ),
                print(t.prompt, 'tb28 =', tb28)
                print()

#            self.sh.title('Toolbox diff tb28')
#            tb28 = toolbox.diff(
#                role           = 'Prv_postes',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = '4dvarfr',
#                local          = 'mb035/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'postes',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = self.conf.model,
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb28 =', tb28)
#            print()

                self.sh.title('Toolbox output FORCING_massif pearp')
                tb29 = toolbox.output(
                    role           = 'Prv_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = 'pearp',
                    local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print(t.prompt, 'tb29 =', tb29)
                print()

#            self.sh.title('Toolbox diff tb29')
#            tb29 = toolbox.diff(
#                role           = 'Prv_massifs',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = 'pearp',
#                local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'massifs',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = self.conf.model,
#                datebegin      = datebegin.ymd6h,
#                dateend        = dateend.ymd6h,
#                namespace      = self.conf.namespace_in,
#                member         = footprints.util.rangex(self.conf.pearp_members),
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb29 =', tb29)
#            print()

                self.sh.title('Toolbox output FORCING_postes pearp')
                tb30 = toolbox.output(
                    role           = 'Prv_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = 'pearp',
                    local          = 'mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print(t.prompt, 'tb30 =', tb30)
                print()

#            self.sh.title('Toolbox diff tb30')
#            tb30 = toolbox.diff(
#                role           = 'Prv_postes',
#                kind           = 'MeteorologicalForcing',
#                source_app     = 'arpege',
#                source_conf    = 'pearp',
#                local          = 'mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
#                experiment     = self.conf.diff_xpid,
#                block          = 'postes',
#                geometry        = self.conf.geometry[self.conf.vconf],
#                nativefmt      = 'netcdf',
#                model          = self.conf.model,
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
                tb31 = toolbox.output(
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
                tb32 = toolbox.output(
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
