#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver, Task


logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            PrepSafran(tag='prepsafana', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            tbarp = list()

            if self.conf.rundate.hour == 3:

                ###########################
                #  I) FICHIER de METADONNES
                ###########################

                # On commence par récupérer un fichier à échéance 0h qui sert à lire le métédonnées (infos sur la grille en particulier)
                # Ce fichier supplémentaire est indispensable pour toujours travailler avec la bonne grille du modèle, même en cas d'évolution
                # de la géométrie ARPEGE.
                self.sh.title('Toolbox input metadata_inline')
                tb01a = toolbox.input(
                    role           = 'Metadata',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    local          = 'METADATA.grib',
                    date           = '{0:s}/-PT30H'.format(self.conf.rundate.ymd6h),
                    term           = 0,
                    namespace      = 'vortex.cache.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb01a =', tb01a)
                print()

                # Deuxième tentative sur hendrix
                self.sh.title('Toolbox input metadata_archive')
                tb01b = toolbox.input(
                    alternate      = 'Metadata',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = self.conf.suite,
                    local          = 'METADATA.grib',
                    date           = '{0:s}/-PT30H'.format(self.conf.rundate.ymd6h),
                    term           = 0,
                    namespace      = 'vortex.archive.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb01b =', tb01b)
                print()

                # Mode secours : On récupère les prévisions 6h correspondantes inline...
                self.sh.title('Toolbox input metadata_secours_inline')
                tb01c = toolbox.input(
                    alternate      = 'Metadata',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    local          = 'METADATA.grib',
                    date           = '{0:s}/-PT30H'.format(self.conf.rundate.ymd6h),
                    term           = 0,
                    namespace      = 'vortex.cache.fr',
                    cutoff         = 'production',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb01c =', tb01c)
                print()

                # Mode secours : On récupère les prévisions 6h correspondantes inline...
                self.sh.title('Toolbox input metadata_secours_inline')
                tb01d = toolbox.input(
                    alternate      = 'Metadata',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    local          = 'METADATA.grib',
                    date           = '{0:s}/-PT30H'.format(self.conf.rundate.ymd6h),
                    term           = 0,
                    namespace      = 'vortex.archive.fr',
                    cutoff         = 'production',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                )
                print(t.prompt, 'tb01d =', tb01d)
                print()

                #####################################
                # II- Guess ARPEGE (= "36eme membre")
                # ###################################

                # RUN 3h : Recuperation de A6 des réseaux 6H, 12H et 18H (J-1).
                # On récupère aussi le réseau 0H (J-1) qui a normalement déjà été extrait par la tâche prepsaf de 9h de la veille par sécurité
                # SAFRAN utilisera la P6 du réseau 0h J pour le dernier guess en attendant que l'analyse soit disponible (réseau 9h)
                # On cherche d'abord sur le cache inline puis sur hendrix
                # RQ : on ne peut pas utiliser le namespace multi car les fichiers sur hendrix n'ont pas de filtername...
                self.sh.title('Toolbox input tbarp_inline')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.cache.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # Deuxième tentative sur hendrix
                self.sh.title('Toolbox input tbarp_archive')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = self.conf.suite,
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.archive.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                ))
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # Mode secours : On récupère les prévisions 6h correspondantes inline...
                self.sh.title('Toolbox input tbarp_inline secours')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    cutoff         = 'production',
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.cache.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                ))
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # ... ou sur Hendrix
                self.sh.title('Toolbox input tbarp_archive secours')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = self.conf.suite,
                    cutoff         = 'production',
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.archive.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print(t.prompt, 'tbarp =', tbarp)
                print()

                ###################################
                # III- Guess PEARP (membres 0 à 34)
                ###################################

                # Récupération des prévisions du réseau 6h (J-1) (utilisée seulement à partir du run de 6h) pour l'analyse (J-1) 6h -> J 6h
                self.sh.title('Toolbox input tbpearp')
                tbpearp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    suite          = self.conf.suite,
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.pearp_geometry,
                    kind           = 'gridpoint',
                    local          = 'PEARP_[member]_[term:hour]/PEARP[date::addterm_ymdh]',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = 'vortex.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb04_a =', tbpearp)
                print()

                # Mode secours : les guess correspondant au prévisions du réseau 18h (J-2) ont déjà été extraites
                # par la tâche prepsafran_analyse du réseau 3h

            else:
                
                ###########################
                #  I) FICHIER de METADONNES
                ###########################

                # On commence par récupérer un fichier à échéance 0h qui sert à lire le métédonnées (infos sur la grille en particulier)
                # Ce fichier supplémentaire est indispensable pour toujours travailler avec la bonne grille du modèle, même en cas d'évolution
                # de la géométrie ARPEGE.
                self.sh.title('Toolbox input metadata_inline')
                tbarp = toolbox.input(
                    role           = 'Metadata',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    local          = 'METADATA.grib',
                    date           = '{0:s}/-PT30H'.format(self.conf.rundate.ymd6h),
                    term           = 0,
                    namespace      = 'vortex.cache.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # Deuxième tentative sur hendrix
                self.sh.title('Toolbox input metadata_archive')
                tbarp.extend(toolbox.input(
                    alternate      = 'Metadata',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = self.conf.suite,
                    local          = 'METADATA.grib',
                    date           = '{0:s}/-PT30H'.format(self.conf.rundate.ymd6h),
                    term           = 0,
                    namespace      = 'vortex.archive.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print(t.prompt, 'tbarp =', tbarp)
                print()

                ##################
                # II- Guess ARPEGE 
                # ################

                # RUN 9h : Récupération de A6 du réseau d'assimilation d'ARPEGE de 0h inline...
                self.sh.title('Toolbox input tb01')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    local          = 'ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.cache.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb01 =', tbarp)
                print()

                # ...ou sur hendrix
                self.sh.title('Toolbox input tb01')
                tbarp = toolbox.input(
                    alternate      = 'Gridpoint',
                    block          = 'forecast',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = self.conf.suite,
                    local          = 'ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.archive.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
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
            tbshp = toolbox.input(
                role            = 'Shapefile',
                genv            = self.conf.cycle,
                gdomain         = 'all_massifs',
                geometry        = '[gdomain]',
                kind            = 'shapefile',
                model           = self.conf.model,
                local           = 'massifs_safran.tar',
            )
            print(t.prompt, 'tbshp =', tbshp)
            print()

            self.sh.title('Toolbox executable = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.cycle,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
            )
            print(t.prompt, 'tb03 =', tb03)
            print()

        if 'fetch' in self.steps:

            pass

        if 'compute' in self.steps:

            elf.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                # Need to extend pythonpath to be independant of the user environment ($PYTHONPATH)
                # The vortex-build environment already set up the pythonpath (see jobassistant plugin) but the script is 
                # eventually launched in a 'user-defined' environment
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site', 'epygram/eccodes_python']],
                ntasks         = self.conf.ntasks,
                terms          = footprints.util.rangex(self.conf.ana_terms),
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            if self.conf.rundate.hour == 3:

                self.sh.title('Toolbox output tb05_a')
                tb05 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'ARP_[date:ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print(t.prompt, 'tb05_a =', tb05)
                print()

                # Dans le mode secours, on est passé dans l'alternate qui consiste à prendre un fichier du cycle de production au lieu d'assimilation
                # Le script qui génère les fichiers guess lit cette information dans le grib et la met dans le nom du fichier produit.
                # On doit donc ajouter la toolbox suivante au cas où, qui doit renvoyer False dans le cas nominal.
                self.sh.title('Toolbox output tb05_b')
                tb05 = toolbox.output(
                    alternate      = 'Ebauche',
                    local          = 'ARP_[date:ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    cutoff         = 'production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print(t.prompt, 'tb05_b =', tb05)
                print()

                self.sh.title('Toolbox output tb06')
                tb06 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
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
                    namespace      = self.conf.namespace,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    fatal          = False,
                ),
                print(t.prompt, 'tb06 =', tb06)
                print()

            else:

                self.sh.title('Toolbox output tb05')
                tb05 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = True,
                ),
                print(t.prompt, 'tb05 =', tb05)
                print()

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
