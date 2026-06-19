# -*- coding:Utf-8 -*-
"""
SAFRAN analysis
"""


__all__ = []

import footprints

import vortex
from vortex_cen.layout.nodes import S2MTaskMixIn
from vortex_cen.tasks.safran.common import SafranMixIn
from mkjob.nodes import Driver
from mkjob.nodes import Task

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


class Safran(Task, S2MTaskMixIn, SafranMixIn):
    """
    Task : Safran
    =============

    Safran analysis.

    Inputs
    ------
    - Observations : Packed SAFRAN-readable surface observation files (R, S and T files)
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
        "rundate+help=Date of run;choices=YYYYMMDD[03 06 09 12];type=str or Date",  # used in the "get_period" method
        "geometry+help=Geometry covered by the guess files;type=dict[vconf]",
        "previ+help=Activate forecast mode;type=bool",  # used in the "get_period" method
        "cumul+help=Output guess files frequency;type=int",
        "source_app+help=NWP files *vapp*;type=str",
        "deterministic_conf+help=ARPEGE *vconf*;type=str",
        "namespace_in+help=Where to look for nwp files;type=str",
        "namespace_out+help=Where to store output guess files;type=str",
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

    def refill(self):
        """Safran analysis"""

        t = self.ticket

        datebegin, dateend = self.get_period()

        if 'refill' in self.steps:  # Unused in the dev task

            if True:

                self.sh.title('Toolbox input tb01wi')
                tb01wi = vortex.input(
                    role           = 'Observations',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    vapp           = 's2m',
                    geometry       = self.conf.geometry[self.conf.vconf],  # Distinction entre géométrie SAFRAN et Surfex spécifique aux taches oper
                    kind           = 'packedobs',
                    date           = self.conf.rundate.ymdh,
                    datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h) if self.conf.rundate.hour == 12 else datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:domain].tar',
                    model          = 'safran',
                    fatal          = False,
                    namespace      = 'bdpe.archive.fr',
                    bdpeid         = self.conf.bdpe_id[self.conf.vconf],
                    cutoff         = 'assimilation',
                )
                print((t.prompt, 'tb01wi =', tb01wi))
                print()

                self.sh.title('Toolbox output tb01wo')
                tb01wo = vortex.output(
                    role           = 'Observations',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    vapp           = 's2m',
                    fatal          = False,
                    geometry       = self.conf.geometry[self.conf.vconf],   # Distinction entre géométrie SAFRAN et Surfex spécifique aux taches oper
                    kind           = 'packedobs',
                    date           = self.conf.rundate.ymdh,
                    datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                    dateend        = dateend.ymd6h,
                    local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:domain].tar',
                    model          = 'safran',
                    cutoff         = 'assimilation',
                    namespace      = self.conf.namespace_out,
                )
                print((t.prompt, 'tb01wo =', tb01wo))
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

            if True:  # To match IGA indentation

                ################################################
                ##### Difference between dev and oper task #####

                self.sh.title('Toolbox input tb01 (Observations)')
                tb01 = vortex.input(
                    role           = 'Observations',
                    geometry       = self.conf.geometry[self.conf.vconf],
                    kind           = 'packedobs',
                    date           = self.conf.rundate.ymdh,
                    datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h) if self.conf.rundate.hour == 12 else datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:domain].tar',
                    model          = 'safran',
                    hostname       = 'sotrtm35-sidev.meteo.fr',
                    username       = 'vernaym',
                    tube           = 'ftp',
                    remote         = '/home/mrns/vernaym/extraction_obs/oper/rep_[geometry:domain]/observations_safran_[vconf]_[date::ymdh].tar',
                    cutoff         = 'assimilation',
                    now            = True,
                    hook_autohook1 = (tb01_generic_hook1, ),
                )
                print((t.prompt, 'tb01 =', tb01))
                print()

                # Dans le cas d'une execution sur une date ancienne le cache de guppy est nettoyé,
                # il faut donc aller chercher les obs sur hendrix (cache oper)
                self.sh.title('Toolbox input tb01_b')
                tb01 = vortex.input(
                    alternate      = 'Observations',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    vapp           = 's2m',
                    geometry       = self.conf.geometry[self.conf.vconf],
                    kind           = 'packedobs',
                    date           = self.conf.rundate.ymdh,
                    datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h) if self.conf.rundate.hour == 12 else datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = 'RST_[datebegin::ymdh]_[dateend::ymdh]_[geometry:domain].tar',
                    model          = 'safran',
                    namespace      = self.conf.namespace_in,
                    cutoff         = 'assimilation',
                    hook_autohook1 = (tb01_generic_hook1, ),
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

                # ##########    End of differences    ###########
                # ###############################################

                if self.conf.rundate.hour == 12:

                    # Récupération de l'archive contenant tous les guess depuis le début de la saison
                    # TODO : a modifier en même temps que prepsaf_reana et refill_guess_safran
                    # pour que l'archive contenant les guess aille jusqu'à J, même si SAFRAN n'utilise pas
                    # les guess des 4 derniers jours (réanalyse mensuelle jusqu'à J-4)
                    # Cela permettrait d'avoir des modes secours
                    self.sh.title('Toolbox input guess')
                    tb17 = vortex.input(
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

                    # TODO : réorganiser les vortex.suivantes en 3 modes
                    #  - mode nominal (réseaux 3h et 9h) / mode nominal réseau 6h (mélange A6 et P6 ==> gérer le cutoff avec un dictionnaire en fonction de l'échéance)
                    #  - 1er mode secours (commun) ==> que des P6
                    #  - Second mode secours (commun) ==> utiliser un "coherentgroup")
                    #
                    # ==> 4 vortex.avec un "if" entre les 2 premières

                    # I- ARPEGE (J-5) -> J ou (J-1) -> J
                    # --------------------
                    # I.1- EBAUCHE issue des A6 des réseaux 0/6/12/18h (J-n) d'assimilation d'ARPEGE et l'A6 du réseau 0h J si présente pour couvrir (J-n) 6h -> J 6h
                    self.sh.title('Toolbox input tb17_a')
                    tb17_a = vortex.input(
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
                    tb17_b = vortex.input(
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
                    tb18_a = vortex.input(
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
        #             tb18_b = vortex.input(
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

                self.get_const_safran()

                self.sh.title('Toolbox executable safrane')
                tb17 = tbx1 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'safrane',
                    local          = 'safrane',
                    model          = 'safran',
                )
                print(t.prompt, 'tb17 =', tb17)
                print()

                self.sh.title('Toolbox executable syrpluie')
                tb18 = tbx2 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syrpluie',
                    local          = 'syrpluie',
                    model          = 'safran',
                )
                print(t.prompt, 'tb18 =', tb18)
                print()

                self.sh.title('Toolbox executable sypluie')
                tb18_b = tbx3 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'sypluie',
                    local          = 'sypluie',
                    model          = 'safran',
                )
                print(t.prompt, 'tb18_b =', tb18_b)
                print()

                self.sh.title('Toolbox executable syvapr')
                tb19 = tbx4 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syvapr',
                    local          = 'syvapr',
                    model          = 'safran',
                )
                print(t.prompt, 'tb19 =', tb19)
                print()

                self.sh.title('Toolbox executable syvafi')
                tb20 = tbx5 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'syvafi',
                    local          = 'syvafi',
                    model          = 'safran',
                )
                print(t.prompt, 'tb20 =', tb20)
                print()

                self.sh.title('Toolbox executable sytist')
                tb21 = tbx6 = vortex.executable(
                    role           = 'Binary',
                    genv           = self.conf.cycle,
                    kind           = 'sytist',
                    local          = 'sytist',
                    model          = 'safran',
                )
                print(t.prompt, 'tb21 =', tb21)
                print()

        if 'compute' in self.steps:

            # NB : La date des executions est fixée à J-1 car l'analyse SAFRAN va de J-1 6h à J 6H
            self.sh.title('Toolbox algo tb22 = SAFRANE')
            tb22 = tbalgo1 = vortex.task(
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
            tb23 = tbalgo2 = vortex.task(
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
            tb23 = tbalgo3 = vortex.task(
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
            tb24 = tbalgo4 = vortex.task(
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
            tb25 = tbalgo5 = vortex.task(
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
            tb26 = tbalgo6 = vortex.task(
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

        if 'backup' in self.steps or 'late-backup' in self.steps:

            if True:  # To match IGA indentation

                if self.conf.rundate.hour == 12:
                    deterministicdir = ''
                else:
                    deterministicdir = 'mb035/'

                self.sh.title('Toolbox output FORCING_massif deterministe')
                tb27 = vortex.output(
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
                    model          = 'safran',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

                if 'diff_xpid' in self.conf:
                    self.sh.title('Toolbox output tb27_diff')
                    tb27_diff = vortex.diff(
                        role           = 'Ana_massifs',
                        kind           = 'MeteorologicalForcing',
                        source_app     = 'arpege',
                        source_conf    = '4dvarfr',
                        cutoff         = 'assimilation',
                        local          = deterministicdir + 'FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.diff_xpid,
                        block          = 'massifs',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        nativefmt      = 'netcdf',
                        model          = 'safran',
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        namespace      = self.conf.namespace_out,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb27_diff =', tb27_diff)
                    print()

                self.sh.title('Toolbox output FORCING postes deterministe')
                tb28 = vortex.output(
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
                    model          = 'safran',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace_out,
                ),
                print(t.prompt, 'tb28 =', tb28)
                print()

                if self.conf.rundate.hour != 12:

                    self.sh.title('Toolbox output FORCINGS massifs PEARP')
                    tb29 = vortex.output(
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
                        model          = 'safran',
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        namespace      = self.conf.namespace_out,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb29 =', tb29)
                    print()

                    if 'diff_xpid' in self.conf:
                        self.sh.title('Toolbox output tb29_diff')
                        tb29_diff = vortex.diff(
                            role           = 'Ana_massifs',
                            kind           = 'MeteorologicalForcing',
                            source_app     = 'arpege',
                            source_conf    = 'pearp',
                            cutoff         = 'assimilation',
                            local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                            experiment     = self.conf.diff_xpid,
                            block          = 'massifs',
                            geometry        = self.conf.geometry[self.conf.vconf],
                            nativefmt      = 'netcdf',
                            model          = 'safran',
                            datebegin      = datebegin.ymd6h,
                            dateend        = dateend.ymd6h,
                            namespace      = self.conf.namespace_out,
                            member         = footprints.util.rangex(self.conf.pearp_members),
                            fatal          = False,
                        ),
                        print(t.prompt, 'tb29_diff =', tb29_diff)
                        print()

                    self.sh.title('Toolbox output FORCINGS postes PEARP')
                    tb30 = vortex.output(
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
                        model          = 'safran',
                        datebegin      = datebegin.ymd6h,
                        dateend        = dateend.ymd6h,
                        namespace      = self.conf.namespace_out,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb30 =', tb30)
                    print()

                self.sh.title('Toolbox output listings execution')
                tb31 = vortex.output(
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
                )
                print(t.prompt, 'tb31 =', tb31)
                print()

                self.sh.title('Toolbox output listings observations')
                tb32 = vortex.output(
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
                )
                print(t.prompt, 'tb32 =', tb32)
                print()

#            print('==================================================================================================')
#            print('==================================================================================================')
#            raise Exception('INFO :The execution went well, do not take into account the following error')
