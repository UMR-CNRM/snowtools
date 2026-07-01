# -*- coding:Utf-8 -*-
"""
Prepare analysis SAFRAN guess files
"""

__all__ = []

from vortex_cen.tasks.oper_research_mixin import CENTaskMixIn
from vortex_cen.tools.monitoring import InputReportContext, OutputReportContext
import footprints
import vortex
from mkjob.nodes import Driver, Task

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            PrepSafran(tag='prepsafana', ticket=t, delay_component_errors=True, on_error='delayed_fail', **kw),
        ],
        options = kw,
    )


class PrepSafran(Task, CENTaskMixIn):
    """
    Task : PrepSafran
    =================

    Generation of 6h guess files for the Safran oper ensemble analysis from ARPEGE and PEARP outptuts.
    1 execution for all domains.

    Inputs
    ------
    - METADATA : Description of the ARPEGE / PEARP grib files grid / geometry
    * 3H run :
        - ARPEGE +06h analysis lead time of 6H, 12H and 18H runs (J-1)
        - [Rescue mode] ARPEGE +06h forecast lead time of 6H, 12H and 18H runs (J-1)
        - PEARP +0h --> +24h forecast lead times of the 6H run (J-1)
    * 9H run:
        - ARPEGE +06h analysis lead time of 0H run (J)
    - massifs_safran.tar : shapefile describing the Safran massifs
    - makeP.py : script generating the Safran guess files from the ARPEGE / PEARP forecasts

    Outputs
    -------
    - PYYMMDDHH : Safran guess files (1 file per lead time and ensemble member)
    """

    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "cycle+help=Alias for uenv;type=str",
        "nwp_geometry+help=geometry of ARPEGE/PEARP files;type=str",
        "nwp_xpid+help=xpid of ARPEGE/PEARP files;type=str",
        "rundate+help=Date of run;choices=YYYMMDD[03 09];type=str or Date",
        "previ+help=Activate forecast mode;type=bool",
        "cumul+help=Output guess files frequency;type=int",
        "namespace_in+help=Where to look for nwp files;type=str",
        "source_app+help=NWP files *vapp*;type=str",
        "deterministic_conf+help=ARPEGE *vconf*;type=str",
        "ana_terms+help=Analysis lead times;type=FPList;format=first-last-step",
        "pearp_members+help=List of PEARP members;type=FPList",
        "eps_conf+help=PEARP *vconf*;type=str",
        "ntasks",
        "domains+help=List of output guess files geometries;type=list",
        "block+help=Output guess files *block*;type=str",
        "namespace_out+help=Where to store output guess files;type=str",
    ]
    OPTIONAL_CONFIGURATION_VARIABLES = []

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = CENTaskMixIn.s2moper_filter_execution_error
    # Report execution warnings with CEN's method
    report_execution_warning = CENTaskMixIn.s2moper_report_execution_warning
    # Report execution errors with CEN's method
    report_execution_error = CENTaskMixIn.s2moper_report_execution_error  # TO MODIFY for operationnal transfer

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            with InputReportContext(self, t):

                tbarp = list()

                ###########################
                #  I) FICHIER de METADONNES
                ###########################

                # On commence par récupérer un fichier à échéance 0h qui sert à lire le métédonnées (infos sur la grille en particulier)
                # Ce fichier supplémentaire est indispensable pour toujours travailler avec la bonne grille du modèle, même en cas d'évolution
                # de la géométrie ARPEGE.
                self.sh.title('Toolbox input metadata')
                tb01 = vortex.input(
                    role           = 'Metadata',
                    format         = 'grib',
                    genv            = self.conf.cycle,
                    geometry       = self.conf.nwp_geometry, #EURAT01
                    kind           = 'relief',
                    gdomain        = '[geometry:area]',
                    local          = 'METADATA.grib',
                    model          = 'safran',
                    intent         = 'in',  # Make a hard link rather than a copy
                    fatal          = True,
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

                if self.conf.rundate.hour == 3:

                    #####################################
                    # II- Guess ARPEGE (= "36eme membre")
                    # ###################################

                    # RUN 3h : Recuperation de A6 des réseaux 6H, 12H et 18H (J-1).
                    # On récupère aussi le réseau 0H (J-1) qui a normalement déjà été extrait par la tâche prepsaf de 9h de la veille par sécurité
                    # SAFRAN utilisera la P6 du réseau 0h J pour le dernier guess en attendant que l'analyse soit disponible (réseau 9h)
                    # On cherche d'abord sur le cache inline puis sur hendrix
                    # RQ : on ne peut pas utiliser le namespace multi car les fichiers sur hendrix n'ont pas de filtername...
                    self.sh.title('Toolbox input tbarp')
                    tbarp = vortex.input(
                        role           = 'Gridpoint',
                        format         = 'grib',
                        geometry       = self.conf.nwp_geometry,
                        kind           = 'gridpoint',
                        experiment     = self.conf.nwp_xpid,
                        local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                        date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d))
                                          for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                        # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                        term           = self.conf.cumul,
                        namespace      = self.conf.namespace_in,
                        block          = 'forecast',
                        nativefmt      = '[format]',
                        origin         = 'historic',
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.deterministic_conf,
                        intent         = 'in',  # Make a hard link rather than a copy
                        fatal          = False,
                    )
                    print(t.prompt, 'tbarp =', tbarp)
                    print()

                    # Mode secours : On récupère les prévisions 6h correspondantes
                    self.sh.title('Toolbox input tbarp secours')
                    tbarp.extend(vortex.input(
                        alternate      = 'Gridpoint',
                        format         = 'grib',
                        geometry       = self.conf.nwp_geometry,
                        kind           = 'gridpoint',
                        experiment     = self.conf.nwp_xpid,
                        cutoff         = 'production',
                        local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                        date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d))
                                          for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                        # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                        term           = self.conf.cumul,
                        namespace      = self.conf.namespace_in,
                        block          = 'forecast',
                        nativefmt      = '[format]',
                        origin         = 'historic',
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.deterministic_conf,
                        intent         = 'in',  # Make a hard link rather than a copy
                        fatal          = False,
                    ))
                    print(t.prompt, 'tbarp =', tbarp)
                    print()

                    ###################################
                    # III- Guess PEARP (membres 0 à 34)
                    ###################################

                    # Récupération des prévisions du réseau 6h (J-1) (utilisée seulement à partir du run de 6h)
                    # pour l'analyse (J-1) 6h -> J 6h
                    self.sh.title('Toolbox input pearp')
                    tbpearp = vortex.input(
                        role           = 'Gridpoint',
                        block          = 'forecast',
                        experiment     = self.conf.nwp_xpid,
                        cutoff         = 'production',
                        format         = 'grib',
                        geometry       = self.conf.nwp_geometry,
                        kind           = 'gridpoint',
                        local          = 'PEARP_[member]_[term:hour]/PEARP[date::addterm_ymdh]',
                        date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                        term           = footprints.util.rangex(self.conf.ana_terms),
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        namespace      = self.conf.namespace_in,
                        nativefmt      = '[format]',
                        origin         = 'historic',
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.eps_conf,
                        intent         = 'in',  # Make a hard link rather than a copy
                        fatal          = False,
                    )
                    print(t.prompt, 'tb04_a =', tbpearp)
                    print()

                    # Mode secours : les guess correspondant au prévisions du réseau 18h (J-2) ont déjà été extraites
                    # par la tâche prepsafran_analyse du réseau 3h

                else:

                    ##################
                    # II- Guess ARPEGE 
                    # ################

                    # RUN 9h : Récupération de A6 du réseau d'assimilation d'ARPEGE de 0h
                    self.sh.title('Toolbox input tb01')
                    tbarp = vortex.input(
                        role           = 'Gridpoint',
                        block          = 'forecast',
                        format         = 'grib',
                        geometry       = self.conf.nwp_geometry,
                        kind           = 'gridpoint',
                        experiment     = self.conf.nwp_xpid,
                        cutoff         = 'assimilation',
                        local          = 'ARPEGE[date::addterm_ymdh]',
                        date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                        # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                        term           = self.conf.cumul,
                        namespace      = self.conf.namespace_in,
                        nativefmt      = '[format]',
                        origin         = 'historic',
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.deterministic_conf,
                        intent         = 'in',  # Make a hard link rather than a copy
                        fatal          = True,
                    )
                    print(t.prompt, 'tb01 =', tbarp)
                    print()

                    tbpearp = list()

                ###########################
                #        SHAPEFILE 
                ###########################
                # Dans tous les cas de figure on aura besoin du shapefile des massifs SAFRAN
                self.sh.title('Toolbox input shapefile')
                tbshp = vortex.input(
                    role            = 'Shapefile',
                    genv            = self.conf.cycle,
                    gdomain         = 'all_massifs',
                    geometry        = '[gdomain]',
                    kind            = 'shapefile',
                    model           = 'safran',
                    local           = 'massifs_safran.tar',
                )
                print(t.prompt, 'tbshp =', tbshp)
                print()

                self.sh.title('Toolbox executable = PRE-TRAITEMENT FORCAGE script')
                tb03 = script = vortex.input(
                    role        = 'pretraitement',
                    local       = 'makeP.py',
                    genv        = self.conf.cycle,
                    kind        = 's2m_filtering_grib',
                    language    = 'python',
                    # En python 3 l'ordre des arguments a une importance pour que Vortex ne considère pas que les exécutables sont différents
                    # Pour éviter de complexifier le code ici, le script s2m_filtering_grib s'occupe désormais de supprimer les doublons.
                    # Ajouter l'option -p pour tracer les profils généré
                    #rawopts     = ' -o -p -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)])),
                    rawopts     = ' -o -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)])),
                )
                print(t.prompt, 'tb03 =', tb03)
                print()


        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = vortex.task(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                ntasks         = self.conf.ntasks,
                terms          = footprints.util.rangex(self.conf.ana_terms),
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            with OutputReportContext(self, t):

                if self.conf.rundate.hour == 3:

                    self.sh.title('Toolbox output tb05 = guess arpege assim')
                    tb05 = vortex.output(
                        role           = 'Ebauche',
                        #coherentgroup  = 'EbauchesDeterministes',
                        local          = 'ARP_[date:ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                        cutoff         = 'assimilation',
                        geometry       = self.conf.domains,
                        vconf          = '[geometry:domain]',
                        experiment     = self.conf.xpid,
                        block          = self.conf.block,
                        date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d))
                                          for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace_out,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb05 =', tb05)
                    print()

                    # Dans le mode secours, on est passé dans l'alternate qui consiste à prendre un fichier du cycle de production au lieu d'assimilation
                    # Le script qui génère les fichiers guess lit cette information dans le grib et la met dans le nom du fichier produit.
                    # On doit donc ajouter la vortex.suivante au cas où, qui doit renvoyer False dans le cas nominal.
                    # TODO : "l'alternate" ne fonctionne toujours pas car les noms des fichiers sont différents
#                self.sh.title('Toolbox output tb05_b')
#                tb05 = vortex.output(
#                    alternate      = 'Ebauche',
#                    coherentgroup  = 'EbauchesDeterministes',
#                    local          = 'ARP_[date:ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                    cutoff         = 'production',
#                    geometry       = self.conf.domains,
#                    vconf          = '[geometry:domain]',
#                    experiment     = self.conf.xpid,
#                    block          = self.conf.block,
#                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
#                    cumul          = self.conf.cumul,
#                    nativefmt      = 'ascii',
#                    kind           = 'guess',
#                    model          = 'safran',
#                    source_app     = self.conf.source_app,
#                    source_conf    = self.conf.deterministic_conf,
#                    namespace      = self.conf.namespace_out,
#                    fatal          = False,
#                ),
#                print(t.prompt, 'tb05_b =', tb05)
#                print()

                    self.sh.title('Toolbox output tb06 = guess pearp')
                    tb06 = vortex.output(
                        role           = 'Ebauche',
                        local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                        geometry       = self.conf.domains,
                        vconf          = '[geometry:domain]',
                        experiment     = self.conf.xpid,
                        block          = self.conf.block,
                        cutoff         = 'production',
                        date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                        cumul          = footprints.util.rangex(self.conf.ana_terms),
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.eps_conf,
                        namespace      = self.conf.namespace_out,
                        member         = footprints.util.rangex(self.conf.pearp_members),
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb06 =', tb06)
                    print()

                else:

                    self.sh.title('Toolbox output tb05 = guess arpege assim')
                    tb05 = vortex.output(
                        role           = 'Ebauche',
                        local          = 'P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                        geometry       = self.conf.domains,
                        vconf          = '[geometry:domain]',
                        experiment     = self.conf.xpid,
                        block          = self.conf.block,
                        date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace_out,
                        fatal          = True,
                    ),
                    print(t.prompt, 'tb05 =', tb05)
                    print()

#            print('==================================================================================================')
#            print('==================================================================================================')
#            raise Exception('INFO :The execution went well, do not take into account the following error')
