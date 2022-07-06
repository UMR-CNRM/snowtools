#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver
from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad
import iga.tools.op as op

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag='pearp2safran',
        ticket=t,
        nodes=[
            PrepSafran(tag='prepsafprv', ticket=t, **kw, delay_component_errors=True),
        ],
        options=kw,
    )


class PrepSafran(OpTask, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            with op.InputReportContext(self, t):

                ###########################
                #  I) FICHIER de METADONNES
                ###########################

                # On commence par récupérer un fichier à échéance 0h qui sert à lire le métédonnées (infos sur la grille en particulier)
                # Ce fichier supplémentaire est indispensable pour toujours travailler avec la bonne grille du modèle, même en cas d'évolution
                # de la géométrie ARPEGE.
                self.sh.title('Toolbox input metadata')
                tb01 = toolbox.input(
                    role           = 'Metadata',
                    format         = 'grib',
                    genv           = self.conf.cycle,
                    geometry       = self.conf.arpege_geometry,  # EURAT01
                    gdomain        = '[geometry:area]',
                    kind           = 'relief',
                    local          = 'METADATA.grib',
                    fatal          = True,
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

                # I- ARPEGE
                # Récupération des échéances de 6h à 102h du réseau 0h J d'ARPEGE
                # On traite les échéances en les considérant comme des membres distincts pour paralléliser les calculs
                self.sh.title('Toolbox input arpege')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = self.conf.suite,
                    cutoff         = 'production',
                    local          = 'ARP_[term:hour]/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    namespace      = self.conf.namespace_in,
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                )
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # II- PEARP
                # Récupération du réseau 0h (J) pour couvrir J 6h -> (J+4) 6h
                # On veut donc les échéances de 6h à 102h
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
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = self.conf.namespace_in,
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tbpearp =', tbpearp)
                print()

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

                self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
                tb03 = script = toolbox.input(
                    role        = 'pretraitement',
                    local       = 'makeP.py',
                    genv        = self.conf.cycle,
                    kind        = 's2m_filtering_grib',
                    language    = 'python',
                    # En python 3 l'ordre des arguments a une importance pour que Vortex ne considère pas que les exécutables sont différents
                    # Pour éviter de complexifier le code ici, le script s2m_filtering_grib s'occupe désormais de supprimer les doublons.
                    # ajouter l'option -p pour tracer les profils générés
                    #rawopts     = ' -o -p -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)])),
                    rawopts     = ' -o -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)])),
                )
                print(t.prompt, 'tb03 =', tb03)
                print()

        if 'fetch' in self.steps:
            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                # Need to extend pythonpath to be independant of the user environment
                # The vortex-build environment already set up the pythonpath (see jobassistant plugin) but the script is
                # eventually launched in a 'user-defined' environment
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d)
                                  for d in ['vortex/src', 'vortex/site', 'epygram',
                                            'epygram/site', 'epygram/eccodes_python']],
                terms          = footprints.util.rangex(self.conf.prv_terms),
                ntasks         = self.conf.ntasks,
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            with op.OutputReportContext(self, t):

                self.sh.title('Toolbox output guess arpege prod')
                tb05 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'ARP_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::area]',
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace_out,
                    fatal          = True,
                    delayed        = True,
                ),
                print(t.prompt, 'tb05 =', tb05)
                print()

                self.sh.title('Toolbox output guess pearp')
                tb06 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::area]',
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    namespace      = self.conf.namespace_out,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    fatal          = False,
                      delayed        = True,
                )
                print(t.prompt, 'tb06 =', tb06)
                print()

                ad.phase(tb05,tb06)

