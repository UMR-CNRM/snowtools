#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints

from vortex import toolbox
from cen.layout.nodes import S2MTaskMixIn
from vortex.layout.nodes import Driver
import iga.tools.op as op
from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad

logger = footprints.loggers.getLogger(__name__)

def setup(t, **kw):
    return Driver(
        tag    = 'safran',
        ticket = t,
        nodes  = [
            Safran(tag='anasaf', ticket=t, delay_component_errors=True, on_error='delayed_fail', **kw),
        ],
        options = kw,
    )


class Safran(OpTask, S2MTaskMixIn):

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    #report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    # Report execution errors with the operationnal method
    report_execution_error = OpTask.s2moper_report_execution_error


    def refill(self):
        """Safran analysis"""

        t = self.ticket

        datebegin, dateend = self.get_period()

        if 'refill' in self.steps:  # Unused in the dev task

            with op.InputReportContext(self, t):

                if self.conf.rundate.hour == 12:

                    self.sh.title('Toolbox input tb01wi')
                    tb01wi = toolbox.input(
                        role           = 'Observations',
                        block          = 'observations',
                        suite          = self.conf.xpid,
                        vapp           = 's2m',
                        geometry       = self.conf.geometry_safran[self.conf.vconf],  # Distinction entre géométrie SAFRAN et Surfex spécifique aux taches oper
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                        dateend        = dateend.ymd6h,
                        local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        fatal          = False,
                        namespace      = 'bdpe.archive.fr',
                        bdpeid         = self.conf.bdpe_id[self.conf.vconf],
                        cutoff         = 'assimilation',
                    )
                    print((t.prompt, 'tb01wi =', tb01wi))
                    print()

                    self.sh.title('Toolbox output tb01wo')
                    tb01wo = toolbox.output(
                        role           = 'Observations',
                        block          = 'observations',
                        experiment     = self.conf.xpid,
                        vapp           = 's2m',
                        fatal          = False,
                        geometry       = self.conf.geometry_safran[self.conf.vconf],   # Distinction entre géométrie SAFRAN et Surfex spécifique aux taches oper
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                        dateend        = dateend.ymd6h,
                        local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        cutoff         = 'assimilation',
                        namespace      = self.conf.namespace_out,
                    )
                    print((t.prompt, 'tb01wo =', tb01wo))
                    print()

                else:

                    self.sh.title('Toolbox input observations')
                    tb01wi = toolbox.input(
                        role             = 'Observations',
                        vapp             = 's2m',
                        geometry         = self.conf.geometry_safran[self.conf.vconf],   # Distinction entre géométrie SAFRAN et Surfex spécifique aux taches oper
                        kind             = 'packedobs',
                        date             = self.conf.rundate.ymdh,
                        datebegin        = datebegin.ymd6h,
                        dateend          = dateend.ymd6h,
                        local            = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:area].tar',
                        model            = 'safran',
                        namespace        = 'bdpe.archive.fr',
                        cutoff           = 'assim',
                        bdpeid           = self.conf.bdpe_id[self.conf.vconf],
                    )
                    print(t.prompt, 'tb01wi =', tb01wi)
                    print()

                    self.sh.title('Toolbox output refill observations')
                    tb01wo = toolbox.output(
                        role           = 'Observations',
                        block          = 'observations',
                        experiment     = self.conf.xpid,
                        vapp           = 's2m',
                        geometry       = self.conf.geometry_safran[self.conf.vconf],   # Distinction entre géométrie SAFRAN et Surfex spécifique aux taches oper
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        cutoff         = 'assim',
                        delayed        = True,
                        namespace      = self.conf.namespace_out,
                    )
                    print(t.prompt, 'tb01wo =', tb01wo)
                    print()

    def process(self):
        """Safran analysis"""

        t = self.ticket

        def tb01_generic_hook1(t, rh):
            sh = t.sh
            tarname = sh.path.basename(rh.container.localpath())
            if sh.is_tarfile(tarname):
                sh.untar(tarname)

        datebegin, dateend = self.get_period()
        ndays = (dateend - datebegin).days
#       list_geometry = self.get_list_geometry()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            with op.InputReportContext(self, t):

                if self.conf.rundate.hour == 12:

					################################################
					##### Difference between dev and oper task #####

                    self.sh.title('Toolbox input observations')
                    tb01 = toolbox.input(
                        role           = 'Observations',
                        block          = 'observations',
                        experiment     = self.conf.xpid,
                        vapp           = 's2m',
                        geometry       = self.conf.geometry[self.conf.vconf],
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                        dateend        = dateend.ymd6h,
                        local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        suite          = self.conf.suite,
                        cutoff         = 'assimilation',
                        hook_autohook1 = (tb01_generic_hook1, ),
                    )
                    print(t.prompt, 'tb01 =', tb01)
                    print()

                else:

                    self.sh.title('Toolbox input observations')
                    tb01 = toolbox.input(
                        role           = 'Observations',
                        block          = 'observations',
                        experiment     = self.conf.xpid,
                        vapp           = 's2m',
                        geometry       = self.conf.geometry[self.conf.vconf],
                        suite          = self.conf.suite,
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        namespace      = self.conf.namespace_in,
                        cutoff         = 'assimilation',
                        hook_autohook1 = (tb01_generic_hook1, ),
                    )
                    print(t.prompt, 'tb01 =', tb01)
                    print()

                    ###########    End of differences    ###########
                    ################################################

                self.sh.title('Toolbox input listem')
                tb03 = toolbox.input(
                    role            = 'ListeMassif',
                    genv            = self.conf.cycle,
                    gdomain         = '[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'listem',
                    model           = self.conf.model,
                    local           = 'listem',
                )
                print(t.prompt, 'tb03 =', tb03)
                print()

                self.sh.title('Toolbox input listeml')
                tb04 = toolbox.input(
                    role            = 'ListeLimitesMassif',
                    genv            = self.conf.cycle,
                    gdomain         = '[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'listeml',
                    model           = self.conf.model,
                    local           = 'listeml',
                )
                print(t.prompt, 'tb04 =', tb04)
                print()

                self.sh.title('Toolbox input listeo')
                tb05 = toolbox.input(
                    role            = 'ListePost',
                    genv            = self.conf.cycle,
                    gdomain         = '[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'listeo',
                    model           = self.conf.model,
                    # local           = 'listeo' if self.conf.vconf == 'alp' else 'lysteo',
                    local           = 'listeo',
                )
                print(t.prompt, 'tb05 =', tb05)
                print()

                self.sh.title('Toolbox input carpost')
                tb06 = toolbox.input(
                    role            = 'carac_post',
                    genv            = self.conf.cycle,
                    gdomain         = '[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'carpost',
                    model           = self.conf.model,
                    local           = 'carpost.tar',
                )
                print(t.prompt, 'tb06 =', tb06)
                print()

                # WARNING : Le ressoucre rsclim  sert pas dans le cas nominal mais
                # constitue un mode secours pour SAFRAN si il rencontre un problème pour faire son guess
                # A partir des fichiers P
#                self.sh.title('Toolbox input rsclim')
#                tb09 = toolbox.input(
#                    role            = 'Clim',
#                    genv            = self.conf.cycle,
#                    gvar            = '[kind]',
#                    geometry        = self.conf.geometry[self.conf.vconf],
#                    kind            = 'rsclim',
#                    model           = self.conf.model,
#                    local           = 'rsclim.don',
#                )
#                print(t.prompt, 'tb09 =', tb09)
#                print()

                self.sh.title('Toolbox input icrccm')
                tb10 = toolbox.input(
                    role            = 'Clim',
                    genv            = self.conf.cycle,
                    gvar            = '[kind]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'icrccm',
                    model           = self.conf.model,
                    local           = 'icrccm.don',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()

                self.sh.title('Toolbox input namelist sorties')
                tb11 = toolbox.input(
                    role            = 'Nam_sorties',
                    source          = 'namelist_sorties_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'SORTIES',
                    fatal           = False,
                )
                print(t.prompt, 'tb11 =', tb11)
                print()

                self.sh.title('Toolbox input namelist analyse')
                tb12 = toolbox.input(
                    role            = 'Nam_analyse',
                    source          = 'namelist_analyse_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'ANALYSE',
                    fatal           = False,
                )
                print(t.prompt, 'tb12 =', tb12)
                print()

                self.sh.title('Toolbox input namelist melange')
                tb13 = toolbox.input(
                    role            = 'Nam_melange',
                    source          = 'namelist_melange_[geometry:area]',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = self.conf.model,
                    local           = 'MELANGE',
                    fatal           = False,
                )
                print(t.prompt, 'tb13 =', tb13)
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
                    fatal          = False,
                )
                print(t.prompt, 'tb14 =', tb14)
                print()

                if self.conf.vconf == 'pyr':

                    self.sh.title('Toolbox input namelist observr')
                    tb15 = toolbox.input(
                        role            = 'Nam_observr',
                        source          = 'namelist_observr_[geometry:area]',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        genv            = self.conf.cycle,
                        kind            = 'namelist',
                        model           = self.conf.model,
                        local           = 'OBSERVR',
                        fatal           = False,
                    )
                    print(t.prompt, 'tb15 =', tb15)
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

                if self.conf.rundate.hour == 12:

                    # Récupération de l'archive contenant tous les guess depuis le début de la saison
                    # TODO : a modifier en même temps que prepsaf_reana et refill_guess_safran
                    # pour que l'archive contenant les guess aille jusqu'à J, même si SAFRAN n'utilise pas
                    # les guess des 4 derniers jours (réanalyse mensuelle jusqu'à J-4)
                    # Cela permettrait d'avoir des modes secours
                    self.sh.title('Toolbox input guess')
                    tb17 = toolbox.input(
                        role           = 'Ebauche_Deterministic',
                        local          = 'guess.tar',
                        experiment     = self.conf.xpid_guess,
                        block          = 'guess',
                        nativefmt      = 'tar',
                        fatal          = False,
                        kind           = 'packedguess',
                        model          = 'safran',
                        datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                        dateend        = '{0:s}/+PT96H'.format(dateend.ymd6h),
                        date           = self.conf.rundate.ymdh,
                        vapp           = self.conf.vapp,
                        vconf          = self.conf.vconf,
                        geometry        = self.conf.geometry[self.conf.vconf],
                        intent         = 'inout',
                        hook_autohook1 = (tb01_generic_hook1, ),
                    )
                    print(t.prompt, 'tb17 =', tb17)
                    print()

                else:

                    # TODO : réorganiser les toolbox suivantes en 3 modes
                    #  - mode nominal (réseaux 3h et 9h) / mode nominal réseau 6h (mélange A6 et P6 ==> gérer le cutoff avec un dictionnaire en fonction de l'échéance)
                    #  - 1er mode secours (commun) ==> que des P6
                    #  - Second mode secours (commun) ==> utiliser un "coherentgroup")
                    #
                    # ==> 4 toolbox avec un "if" entre les 2 premières

                    # I- ARPEGE (J-5) -> J ou (J-1) -> J
                    # --------------------
                    # I.1- EBAUCHE issue des A6 des réseaux 0/6/12/18h (J-n) d'assimilation d'ARPEGE et l'A6 du réseau 0h J si présente pour couvrir (J-n) 6h -> J 6h
                    self.sh.title('Toolbox input tb17_a')
                    tb17_a = toolbox.input(
                        role           = 'Ebauche_Deterministic',
                        local          = 'mb035/P[date::addcumul_yymdh]',
                        experiment     = self.conf.xpid_guess,
                        block          = self.conf.guess_block,
                        geometry        = self.conf.geometry[self.conf.vconf],
                        cutoff         = 'assimilation',
                        date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d))
                                          for d in footprints.util.rangex(6, ndays * 24 + 6, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace_in,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb17_a =', tb17_a)
                    print()

                    # I.2- EBAUCHE issue de la P6 du réseau H-6 de production d'ARPEGE
                    # Si l'A6 du réseau H n'est pas là on prend la P6 du réseau H-6h
                    # RQ : il est fondamental de prendre une P6 pour avoir un cumul des RR sur 6h homogène avec le cumul dans les fichiers d'assimilation
                    self.sh.title('Toolbox input tb17_b')
                    tb17_b = toolbox.input(
                        alternate      = 'Ebauche_Deterministic',
                        local          = 'mb035/P[date::addcumul_yymdh]',
                        experiment     = self.conf.xpid_guess,
                        block          = self.conf.guess_block,
                        geometry        = self.conf.geometry[self.conf.vconf],
                        cutoff         = 'production',
                        date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d))
                                          for d in footprints.util.rangex(6, ndays * 24 + 6, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace_in,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb17_b =', tb17_b)
                    print()

                    # I.3- En dernier recours on essaye le réseau de production de 0h J-1
                    # PROBLEME : le nom dans 'local' change donc on passe dans l'alternate même si la ressource voulue
                    # est déjà présente
                    # TODO ==> SOLUTION : utiliser les "coherentgroup" (cf src/vortex/layout/dataflow.py)
                    # WARNING : L'utilisation de coherentgroup entraine la suppression de TOUTES les ressources présentes
                    # dès lors qu'il en manque une.

                    # II- PEARP (J-5) -> (J-1) ou (J-1) -> J
                    # --------------------
                    # II.1- EBAUCHE issue des prevision P0/P6/P12/P18/P24 du réseau 6h (J-n)
                    # de la PEARP pour couvrir (J-5) 6h -> (J-1) 6h
                    # RQ : on ne peut pas mélanger des resources issues de runs différents
                    # pour conserver des cumuls de précipitations cohérents
                    self.sh.title('Toolbox input guess pearp')
                    tb18_a = toolbox.input(
                        role           = 'Ebauche',
                        # local          = 'mb[member]/P[date:addcumul_yymdh]',
                        local          = 'mb[member]/P[date::yymdh]_[cumul:hour]',
                        term           = '[cumul]',
                        experiment     = self.conf.xpid_guess,
                        block          = self.conf.guess_block,
                        geometry        = self.conf.geometry[self.conf.vconf],
                        cutoff         = 'production',
                        date           = ['{0:s}/+PT{1:s}H'.format(datebegin.ymd6h, str(24 * i)) for i in range(ndays)],
                        cumul          = footprints.util.rangex(self.conf.ana_terms),
                        # cumul          = footprints.util.rangex('0-24-3'),
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.eps_conf,
                        namespace      = self.conf.namespace_in,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb18_a =', tb18_a)
                    print()

                    # II.2- Si le réseau de production de 6h n'est pas là, on utilise le réseau de 18h de la veille
                    # PROBLEME : le nom dans 'local' change donc on passe dans l'alternate même si la ressource voulue
                    # est déjà présente
        #             self.sh.title('Toolbox input guess pearp 18h j-1 (secours)')
        #             tb18_b = toolbox.input(
        #                 alternate      = 'Ebauche',
        #                 # local          = 'mb[member]/P[date:addcumul_yymdh]',
        #                 local          = 'mb[member]/P[date::yymdh]_[cumul:hour]',
        #                 term           = '[cumul]',
        #                 experiment     = self.conf.xpid,
        #                 block          = self.conf.guess_block,
        #                 geometry       = self.conf.geometry[self.conf.vconf],
        #                 cutoff         = 'production',
        #                 date           = ['{0:s}/+PT{1:s}H/-PT12H'.format(datebegin.ymd6h,
        #                                   str(24 * i)) for i in range(ndays)],
        #                 cumul          = footprints.util.rangex(self.conf.ana_terms, shift=12),
        #                 nativefmt      = 'ascii',
        #                 kind           = 'guess',
        #                 model          = 'safran',
        #                 source_app     = self.conf.source_app,
        #                 source_conf    = self.conf.eps_conf,
        #                 namespace      = self.conf.namespace_in,
        #                 member         = footprints.util.rangex(self.conf.pearp_members),
        #                 fatal          = False,
        #             ),
        #             print t.prompt, 'tb18_b =', tb18_b
        #             print

                self.sh.title('Toolbox executable safrane')
                tb17 = tbx1 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'safrane',
                    local          = 'safrane',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb17 =', tb17)
                print()

                self.sh.title('Toolbox executable syrpluie')
                tb18 = tbx2 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syrpluie',
                    local          = 'syrpluie',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb18 =', tb18)
                print()

                self.sh.title('Toolbox executable sypluie')
                tb18_b = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'sypluie',
                    local          = 'sypluie',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb18_b =', tb18_b)
                print()

                self.sh.title('Toolbox executable syvapr')
                tb19 = tbx4 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syvapr',
                    local          = 'syvapr',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb19 =', tb19)
                print()

                self.sh.title('Toolbox executable syvafi')
                tb20 = tbx5 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syvafi',
                    local          = 'syvafi',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb20 =', tb20)
                print()

                self.sh.title('Toolbox executable sytist')
                tb21 = tbx6 = toolbox.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'sytist',
                    local          = 'sytist',
                    model          = self.conf.model,
                )
                print(t.prompt, 'tb21 =', tb21)
                print()

        if 'fetch' in self.steps:
            pass

        if 'compute' in self.steps:

            # NB : La date des executions est fixée à J-1 car l'analyse SAFRAN va de J-1 6h à J 6H
            self.sh.title('Toolbox algo tb22 = SAFRANE')
            tb22 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind           = 'safrane',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'analysis',
            )
            print(t.prompt, 'tb22 =', tb22)
            print()

            self.component_runner(tbalgo1, tbx1)

            self.sh.title('Toolbox algo tb23 = SYRPLUIE')
            tb23 = tbalgo2 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syrpluie',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'analysis',
            )
            print(t.prompt, 'tb23 =', tb23)
            print()

            self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb23_b = SYPLUIE')
            tb23 = tbalgo3 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sypluie',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'analysis',
            )
            print(t.prompt, 'tb23 =', tb23)
            print()

            self.component_runner(tbalgo3, tbx3)

            self.sh.title('Toolbox algo tb24 = SYVAPR')
            tb24 = tbalgo4 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syvapr',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'analysis',
            )
            print(t.prompt, 'tb24 =', tb24)
            print()

            self.component_runner(tbalgo4, tbx4)

            self.sh.title('Toolbox algo tb25 = SYVAFI')
            tb25 = tbalgo5 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syvafi',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'analysis',
            )
            print(t.prompt, 'tb25 =', tb25)
            print()

            self.component_runner(tbalgo5, tbx5)

            self.sh.title('Toolbox algo tb26 = SYTIST')
            tb26 = tbalgo6 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sytist',
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'analysis',
            )
            print(t.prompt, 'tb26 =', tb26)
            print()

            self.component_runner(tbalgo6, tbx6)

        if 'late-backup' in self.steps:

            with op.OutputReportContext(self, t):

                if self.conf.rundate.hour == 12:
                    deterministicdir = ''
                else:
                    deterministicdir = 'mb035/'

                self.sh.title('Toolbox output FORCING_massif deterministe')
                tb27 = toolbox.output(
                    role           = 'Ana_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    cutoff         = 'assimilation',
                    local          = deterministicdir + 'FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                    delayed        = True,
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

                ad.phase(tb27)

                self.sh.title('Toolbox output FORCING postes deterministe')
                tb28 = toolbox.output(
                    role           = 'Ana_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    cutoff         = 'assimilation',
                    local          = deterministicdir + 'FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                    delayed        = True,
                ),
                print(t.prompt, 'tb28 =', tb28)
                print()

                ad.phase(tb28)

                if self.conf.rundate.hour != 12:

                    self.sh.title('Toolbox output FORCINGS massifs PEARP')
                    tb29 = toolbox.output(
                        role           = 'Ana_massifs',
                        kind           = 'MeteorologicalForcing',
                        source_app     = 'arpege',
                        source_conf    = 'pearp',
                        cutoff         = 'assimilation',
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
                        fatal          = False,
                        delayed        = True,
                    ),
                    print(t.prompt, 'tb29 =', tb29)
                    print()

                    ad.phase(tb29)

                    self.sh.title('Toolbox output FORCINGS postes PEARP')
                    tb30 = toolbox.output(
                        role           = 'Ana_postes',
                        kind           = 'MeteorologicalForcing',
                        source_app     = 'arpege',
                        source_conf    = 'pearp',
                        cutoff         = 'assimilation',
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
                        fatal          = False,
                        delayed        = True,
                    ),
                    print(t.prompt, 'tb30 =', tb30)
                    print()

                    ad.phase(tb30)

                self.sh.title('Toolbox output listings execution')
                tb31 = toolbox.output(
                    role           = 'Listing',
                    block          = 'listing',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind           = 'packedlisting',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = deterministicdir + 'listings_safran_[datebegin::ymdh]_[dateend::ymdh].tar.gz',
                    format         = 'tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace_out,
                    delayed        = True,
                )
                print(t.prompt, 'tb31 =', tb31)
                print()

                self.sh.title('Toolbox output listings observations')
                tb32 = toolbox.output(
                    role           = 'Liste_obs',
                    block          = 'listing',
                    experiment     = self.conf.xpid,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    cutoff         = 'assimilation',
                    kind           = 'listobs',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = deterministicdir + 'liste_obs_[datebegin::ymdh]_[dateend::ymdh].tar.gz',
                    format         = 'tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace_out,
                    delayed        = True,
                )
                print(t.prompt, 'tb32 =', tb32)
                print()

