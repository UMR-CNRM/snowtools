# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from cen.layout.nodes import S2MTaskMixIn
from vortex.layout.nodes import Driver, Task




def setup(t, **kw):
    return Driver(
        tag    = 'safran',
        ticket = t,
        nodes  = [
            Safran(tag='anasaf', ticket=t, **kw),
        ],
        options = kw,
    )


class Safran(Task, S2MTaskMixIn):

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error

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

            if True:

                if self.conf.rundate.hour == 12:

                    self.sh.title('Toolbox input observations')
                    tb01 = toolbox.input(
                        role           = 'Observations',
                        block          = 'observations',
                        experiment     = self.conf.xpid,
                        vapp           = 's2m',
                        geometry       = self.conf.vconf,
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        begindate      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                        enddate        = dateend.ymd6h,
                        local          = 'RST_[begindate::ymdh]_[enddate::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        namespace      = 'vortex.archive.fr',
                        now            = True,
                        cutoff         = 'assimilation',
                        hook_autohook1 = (tb01_generic_hook1, ),
                    )
                    print(t.prompt, 'tb01 =', tb01)
                    print()

                else:

                    self.sh.title('Toolbox input observations')
                    tb01a = toolbox.input(
                        role           = 'Observations',
                        geometry       = self.conf.vconf,
                        suite          = 'oper',
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        begindate      = datebegin.ymd6h,
                        enddate        = dateend.ymd6h,
                        local          = 'RST_[begindate::ymdh]_[enddate::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        hostname       = 'guppy.meteo.fr',
                        username       = 'vernaym',
                        tube           = 'ftp',
                        remote         = '/home/mrns/vernaym/extraction_obs/oper/ \
                        observations_safran_[vconf]_[date::ymdh].tar',
                        # namespace      = 'vortex.archive.fr',
                        cutoff         = 'assimilation',
                        now            = True,
                        hook_autohook1 = (tb01_generic_hook1, ),
                    )
                    print(t.prompt, 'tb01a =', tb01a)
                    print()

                    # Dans le cas d'une execution sur une date ancienne le cache de guppy est nettoyé,
                    # il faut donc aller chercher les obs sur hendrix
                    self.sh.title('Toolbox output observations (secours)')
                    tb01b = toolbox.output(
                        alternate      = 'Observations',
                        block          = 'observations',
                        experiment     = self.conf.xpid,
                        vapp           = 's2m',
                        geometry       = self.conf.vconf,
                        suite          = 'oper',
                        kind           = 'packedobs',
                        date           = self.conf.rundate.ymdh,
                        begindate      = datebegin.ymd6h,
                        enddate        = dateend.ymd6h,
                        local          = 'RST_[begindate::ymdh]_[enddate::ymdh]_[geometry:area].tar',
                        model          = 'safran',
                        namespace      = self.conf.namespace,
                        cutoff         = 'assimilation',
                    )
                    print(t.prompt, 'tb01b =', tb01b)
                    print()

    #           self.sh.title('Toolbox input tb02')
    #           tb02 = toolbox.input(
    #                   role           = 'ObsNeb',
    #                   part           = 'nebulosity',
    #                   block          = 'observations',
    #                   experiment     = self.conf.xpid,
    #                   geometry       = self.conf.vconf,
    #                   cutoff         = 'assim',
    #                   suite          = self.conf.xpid,
    #                   fatal          = False,
    #                   namespace      = 'vortex.cache.fr',
    #                   kind           = 'observations',
    #                   stage          = 'safrane',
    #                   nativefmt      = 'ascii',
    #                   date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(24 * i)) for i in range(ndays)],
    #                   local          = 'N[date:yymdh]',
    #                   model          = self.conf.model,
    #           )
    #           print t.prompt, 'tb02 =', tb02
    #           print

                self.sh.title('Toolbox input listem')
                tb03 = toolbox.input(
                    role            = 'ListeMassif',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
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
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
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
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
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
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
                    kind            = 'carpost',
                    model           = self.conf.model,
                    local           = 'carpost.tar',
                )
                print(t.prompt, 'tb06 =', tb06)
                print()

                if not self.conf.vconf == 'cor':

                    self.sh.title('Toolbox input norelmt')
                    tb07 = toolbox.input(
                        role            = 'MoyRRmensuelles',
                        genv            = self.conf.cycle,
                        gdomain         = self.conf.vconf,
                        geometry        = '[gdomain]',
                        kind            = 'NORELmt',
                        model           = self.conf.model,
                        local           = 'NORELmt',
                        fatal           = False,
                    )
                    print(t.prompt, 'tb07 =', tb07)
                    print()

                    self.sh.title('Toolbox input blacklist')
                    tb08 = toolbox.input(
                        role            = 'BlackList',
                        genv            = self.conf.cycle,
                        gdomain         = self.conf.vconf,
                        geometry        = '[gdomain]',
                        kind            = 'blacklist',
                        model           = self.conf.model,
                        local           = 'BLACK',
                        fatal           = False,
                    )
                    print(t.prompt, 'tb08 =', tb08)
                    print()

                # WARNING : Les ressoucres rsclim et icrccm ne servent pas dans le cas nominal mais
                # consituent un mode secours pour SAFRAN si il rencontre un problème pour faire son guess
                # A partir des fichiers P
                self.sh.title('Toolbox input rsclim')
                tb09 = toolbox.input(
                    role            = 'Clim',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
                    kind            = 'rsclim',
                    model           = self.conf.model,
                    local           = 'rsclim.don',
                )
                print(t.prompt, 'tb09 =', tb09)
                print()

                self.sh.title('Toolbox input icrccm')
                tb10 = toolbox.input(
                    role            = 'Clim',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
                    kind            = 'icrccm',
                    model           = self.conf.model,
                    local           = 'icrccm.don',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()

                self.sh.title('Toolbox input namelist sorties')
                tb11 = toolbox.input(
                    role            = 'Nam_sorties',
                    source          = 'namelist_sorties_[geometry]',
                    geometry        = self.conf.vconf,
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
                    source          = 'namelist_analyse_[geometry]',
                    geometry        = self.conf.vconf,
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
                    source          = 'namelist_melange_[geometry]',
                    geometry        = self.conf.vconf,
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
                    geometry        = self.conf.vconf,
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
                        source          = 'namelist_observr_[geometry]',
                        geometry        = self.conf.vconf,
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
                    source          = 'namelist_ebauche_[geometry]',
                    geometry        = self.conf.vconf,
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
                        role           = 'Ebauche',
                        local          = 'guess.tar',
                        experiment     = self.conf.xpid,
                        block          = 'guess',
                        nativefmt      = 'tar',
                        fatal          = False,
                        kind           = 'packedguess',
                        model          = 'safran',
                        hook_autohook1 = (tb01_generic_hook1, ),
                        date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymdh),
                        vapp           = self.conf.vapp,
                        vconf          = self.conf.vconf,
                        begindate      = datebegin.ymd6h,
                        enddate        = '{0:s}/-PT24H'.format(dateend.ymd6h),
                        geometry       = self.conf.vconf,
                        intent         = 'inout',
                    )
                    print(t.prompt, 'tb17 =', tb17)
                    print()

                else:

                    # I- ARPEGE (J-5) -> J ou (J-1) -> J
                    # --------------------

                    # I.1- EBAUCHE issue 
                    #     - de l'A6 du réseau 0h (J ou J-1) d'assimilation d'ARPEGE pour les réseaux 3h et 9h
                    #     - de la P6 du réseau 0h (J) de production d'AREPEG pour le réseau 6h (A6 pas encore disponible)
                    self.sh.title('Toolbox input guess arpege assim 0h J')
                    tb17 = toolbox.input(
                        role           = 'Ebauche_Deterministic',
                        local          = 'mb035/P[date::addcumul_yymdh]',
                        experiment     = self.conf.xpid_guess,
                        block          = self.conf.guess_block,
                        geometry       = self.conf.vconf,
                        cutoff         = 'production' if self.conf.rundate.hour == 6 else 'assimilation',
                        date           = '{0:s}/-PT{1:d}H'.format(dateend.ymd6h, 6),
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb17 =', tb17)
                    print()

                    if self.conf.rundate.hour != 6:

                        # Si l'A6 du réseau H n'est pas là on prend la P6 du réseau H-6h
                        # RQ : il est fondamental de prendre une P6 pour avoir un cumul des RR sur
                        # 6h homogène avec le cumul dans les fichiers d'assimilation
                        self.sh.title('Toolbox input guess arpege assim 0h J (secours)')
                        tb17 = toolbox.input(
                            role           = 'Ebauche_Deterministic',
                            local          = 'mb035/P[date::addcumul_yymdh]',
                            experiment     = self.conf.xpid_guess,
                            block          = self.conf.guess_block,
                            geometry       = self.conf.vconf,
                            cutoff         = 'production',
                            date           = '{0:s}/-PT{1:d}H'.format(dateend.ymd6h, 6),
                            cumul          = self.conf.cumul,
                            nativefmt      = 'ascii',
                            kind           = 'guess',
                            model          = 'safran',
                            source_app     = self.conf.source_app,
                            source_conf    = self.conf.deterministic_conf,
                            namespace      = self.conf.namespace,
                            fatal          = False,
                        ),
                        print(t.prompt, 'tb17 =', tb17)
                        print()

                    # I.1- EBAUCHE issue des A6 des réseaux 0/6/12/18h (J-n) d'assimilation d'ARPEGE et l'A6
                    # du réseau 0h J si présente pour couvrir (J-n) 6h -> J 0h
                    self.sh.title('Toolbox input guess arpege assim')
                    tb17_a = toolbox.input(
                        role           = 'Ebauche_Deterministic',
                        local          = 'mb035/P[date::addcumul_yymdh]',
                        experiment     = self.conf.xpid_guess,
                        block          = self.conf.guess_block,
                        geometry        = self.conf.vconf,
                        cutoff         = 'assimilation',
                        date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d))
                                          for d in footprints.util.rangex(12, ndays * 24 + 6, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb17_a =', tb17_a)
                    print()

                    # I.2- EBAUCHE issue de la P6 du réseau H-6 de production d'ARPEGE
                    # Si l'A6 du réseau H n'est pas là on prend la P6 du réseau H-6h
                    # RQ : il est fondamental de prendre une P6 pour avoir un cumul des RR sur
                    # 6h homogène avec le cumul dans les fichiers d'assimilation
                    self.sh.title('Toolbox input guess arpege prod (secours)')
                    tb17_b = toolbox.input(
                        alternate      = 'Ebauche_Deterministic',
                        local          = 'mb035/P[date::addcumul_yymdh]',
                        experiment     = self.conf.xpid_guess,
                        block          = self.conf.guess_block,
                        geometry       = self.conf.vconf,
                        cutoff         = 'production',
                        date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d))
                                          for d in footprints.util.rangex(12, ndays * 24 + 6, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb17_b =', tb17_b)
                    print()

                    # I.3- En dernier recours on essaye le réseau de production de 0h J-1
                    # PROBLEME : le nom dans 'local' change donc on passe dans l'alternate même si la ressource voulue
                    # est déjà présente
                    # TODO ==> SOLUTION : utiliser les "coherentgroup" (cf src/vortex/layout/dataflow.py)
                    #
    #                self.sh.title('Toolbox input guess arpege prod j-1 (secours bis)')
    #                 tb17_c = toolbox.input(
    #                     alternate      = 'Ebauche_Deterministic',
    #                     local          = 'mb035/P[date::addcumul_yymdh]',
    #                     experiment     = self.conf.xpid_guess,
    #                     block          = self.conf.guess_block,
    #                     geometry       = self.conf.vconf,
    #                     cutoff         = 'production',
    #                     date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d))
                    #                     for d in footprints.util.rangex(30, ndays * 24 + 6, 24)],
    #                     cumul          = footprints.util.rangex('6-30-6'),
    #                     nativefmt      = 'ascii',
    #                     kind           = 'guess',
    #                     model          = 'safran',
    #                     source_app     = self.conf.source_app,
    #                     source_conf    = self.conf.deterministic_conf,
    #                     namespace      = self.conf.namespace,
    #                     fatal          = False,
    #                 ),
    #                 print t.prompt, 'tb17_c =', tb17_c
    #                 print

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
                        geometry        = self.conf.vconf,
                        cutoff         = 'production',
                        date           = ['{0:s}/+PT{1:s}H'.format(datebegin.ymd6h, str(24 * i)) for i in range(ndays)],
                        cumul          = footprints.util.rangex(self.conf.ana_terms),
                        # cumul          = footprints.util.rangex('0-24-3'),
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.eps_conf,
                        namespace      = self.conf.namespace,
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
        #                 geometry        = self.conf.vconf,
        #                 cutoff         = 'production',
        #                 date           = ['{0:s}/+PT{1:s}H/-PT12H'.format( \
                #                 datebegin.ymd6h, str(24 * i)) for i in range(ndays)],
        #                 cumul          = footprints.util.rangex(self.conf.ana_terms, shift=12),
        #                 nativefmt      = 'ascii',
        #                 kind           = 'guess',
        #                 model          = 'safran',
        #                 source_app     = self.conf.source_app,
        #                 source_conf    = self.conf.eps_conf,
        #                 namespace      = self.conf.namespace,
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

            if True:

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
                    geometry       = self.conf.vconf,
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

                self.sh.title('Toolbox output tb27_diff')
                tb27_diff = toolbox.diff(
                    role           = 'Ana_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    cutoff         = 'assimilation',
                    local          = deterministicdir + 'FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.diff_xpid,
                    block          = 'massifs',
                    geometry       = self.conf.vconf,
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print(t.prompt, 'tb27_diff =', tb27_diff)
                print()

                self.sh.title('Toolbox output FORCING_postes deterministe')
                tb28 = toolbox.output(
                    role           = 'Ana_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    cutoff         = 'assimilation',
                    local          = deterministicdir + 'FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry       = self.conf.vconf,
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                ),
                print(t.prompt, 'tb28 =', tb28)
                print()

                if self.conf.rundate.hour != 12:

                    self.sh.title('Toolbox output FORCING_massif pearp')
                    tb29 = toolbox.output(
                        role           = 'Ana_massifs',
                        kind           = 'MeteorologicalForcing',
                        source_app     = 'arpege',
                        source_conf    = 'pearp',
                        cutoff         = 'assimilation',
                        local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.xpid,
                        block          = 'massifs',
                        geometry       = self.conf.vconf,
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        namespace      = self.conf.namespace,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb29 =', tb29)
                    print()

                    self.sh.title('Toolbox output tb29_diff')
                    tb29_diff = toolbox.diff(
                        role           = 'Ana_massifs',
                        kind           = 'MeteorologicalForcing',
                        source_app     = 'arpege',
                        source_conf    = 'pearp',
                        cutoff         = 'assimilation',
                        local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.diff_xpid,
                        block          = 'massifs',
                        geometry       = self.conf.vconf,
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        namespace      = self.conf.namespace,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb29_diff =', tb29_diff)
                    print()

                    self.sh.title('Toolbox output FORCING_postes pearp')
                    tb30 = toolbox.output(
                        role           = 'Ana_postes',
                        kind           = 'MeteorologicalForcing',
                        source_app     = 'arpege',
                        source_conf    = 'pearp',
                        cutoff         = 'assimilation',
                        local          = 'mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.xpid,
                        block          = 'postes',
                        geometry       = self.conf.vconf,
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        namespace      = self.conf.namespace,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb30 =', tb30)
                    print()

                self.sh.title('Toolbox output listings execution')
                tb31 = toolbox.output(
                    role           = 'Listing',
                    block          = 'listing',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry       = self.conf.vconf,
                    kind           = 'packedlisting',
                    begindate      = datebegin.ymd6h,
                    enddate        = dateend.ymd6h,
                    local          = deterministicdir + 'listings_safran_[begindate::ymdh]_[enddate::ymdh].tar.gz',
                    format         = 'tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace,
                )
                print(t.prompt, 'tb31 =', tb31)
                print()

                self.sh.title('Toolbox output listings observations')
                tb32 = toolbox.output(
                    role           = 'Liste_obs',
                    block          = 'listing',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.vconf,
                    cutoff         = 'assimilation',
                    kind           = 'listobs',
                    begindate      = datebegin.ymd6h,
                    enddate        = dateend.ymd6h,
                    local          = deterministicdir + 'liste_obs_[begindate::ymdh]_[enddate::ymdh].tar.gz',
                    format         = 'tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace,
                )
                print(t.prompt, 'tb32 =', tb32)
                print()

                self.sh.title('Toolbox output observations')
                tb33 = toolbox.output(
                    role           = 'Observations',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    vapp           = 's2m',
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'packedobs',
                    date           = self.conf.rundate.ymdh,
                    begindate      = datebegin.ymd6h,
                    enddate        = dateend.ymd6h,
                    local          = 'RST_[begindate::ymdh]_[enddate::ymdh]_[geometry:area].tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace,
                    cutoff         = 'assimilation',
                )
                print(t.prompt, 'tb33 =', tb33)
                print()

                print('==================================================================================================')
                print('INFO :The execution went well, do not take into account the following error')
                print('==================================================================================================')
                from vortex.tools.systems import ExecutionError
                raise ExecutionError('')