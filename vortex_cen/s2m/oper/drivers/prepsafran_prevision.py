# -*- coding:Utf-8 -*-
"""
Prepare guess files for SAFRAN forecast
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
        tag='pearp2safran',
        ticket=t,
        nodes=[
            PrepSafran(tag='prepsafprv', ticket=t, delay_component_errors=True, on_error='delayed_fail', **kw),
        ],
        options=kw,
    )


class PrepSafran(Task, S2MTaskMixIn):
    """
    Task : PrepSafran
    =================

    Generation of guess files for the Safran oper ensemble foreacst task from ARPEGE and PEARP forecast.
    1 execution for all domains.

    Inputs
    ------
    - METADATA : Description of the ARPEGE / PEARP grib files grid / geometry
    - ARPEGE 3-hours forecasts from the 0H run (J) up to 102h lead time (6H J+4)
    - PEARP 3-hours forecasts from the 0H run (J) up to 102h lead time (6H J+4)
    - massifs_safran.tar : shapefile describing the Safran massifs
    - makeP.py : script generating the Safran guess files from the ARPEGE / PEARP forecasts

    Outputs
    -------
    - PYYMMDDHH : Safran guess files (1 file per lead time and ensemble member)
    """

    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "rundate+help=Date of run;choices=YYYYMMDD03;type=str or Date",  # used in the "get_period" method
        "previ+help=Activate forecast mode;typr=bool",  # used in the "get_period" method
        "cycle+help=Alias for uenv;type=str",
        "nwp_geometry+help=geometry of ARPEGE/PEARP files;type=str",
        "nwp_xpid+help=xpid of ARPEGE/PEARP files;type=str",
        "prv_terms+help=Analysis lead times;type=FPList;format=first-last-step",
        "namespace_in+help=Where to look for nwp files;type=str",
        "source_app+help=NWP files *vapp*;type=str",
        "deterministic_conf+help=ARPEGE *vconf*;type=str",
        "pearp_members+help=List of PEARP members;type=FPList",
        "eps_conf+help=PEARP *vconf*;type=str",
        "block+help=Output guess files *block*;type=str",
        "domains+help=List of output guess files geometries;type=list",
        "namespace_out+help=Where to store output guess files;type=str",
        "ntasks",
    ]
    OPTIONAL_CONFIGURATION_VARIABLES = []

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    # Report execution warnings with CEN's method
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    # Report execution errors with CEN's method
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error  # TO MODIFY for operationnal transfer

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            if True:  # To match IGA identation

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
                    genv           = self.conf.cycle,
                    geometry       = self.conf.nwp_geometry,  # EURAT01
                    gdomain        = '[geometry:area]',
                    kind           = 'relief',
                    local          = 'METADATA.grib',
                    model          = 'safran',
                    intent         = 'in',  # Make a hard link rather than a copy
                    fatal          = True,
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

                # I- ARPEGE
                # Récupération des échéances de 6h à 102h du réseau 0h J d'ARPEGE
                # On traite les échéances en les considérant comme des membres distincts pour paralléliser les calculs
                self.sh.title('Toolbox input arpege')
                tbarp = vortex.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.nwp_geometry,
                    kind           = 'gridpoint',
                    experiment     = self.conf.nwp_xpid,
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
                    intent         = 'in',  # Make a hard link rather than a copy
                    fatal          = True,
                )
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # II- PEARP
                # Récupération du réseau 0h (J) pour couvrir J 6h -> (J+4) 6h
                # On veut donc les échéances de 6h à 102h
                self.sh.title('Toolbox input tbpearp')
                tbpearp = vortex.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    experiment     = self.conf.nwp_xpid,
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.nwp_geometry,
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
                    intent         = 'in',  # Make a hard link rather than a copy
                    fatal          = False,
                )
                print(t.prompt, 'tbpearp =', tbpearp)
                print()

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

                self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
                tb03 = script = vortex.input(
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

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = vortex.task(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                terms          = footprints.util.rangex(self.conf.prv_terms),
                ntasks         = self.conf.ntasks,
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            if True:  # To match IGA identation

                self.sh.title('Toolbox output guess arpege prod')
                tb05 = vortex.output(
                    role           = 'Ebauche',
                    local          = 'ARP_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::domain]',
                    date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
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

                self.sh.title('Toolbox output guess pearp')
                tb06 = vortex.output(
                    role           = 'Ebauche',
                    local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::domain]',
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
                ),
                print(t.prompt, 'tb06 =', tb06)
                print()

#            self.sh.title('Toolbox output tb06b')
#            tb06b = vortex.output(
#                role           = 'Ebauche',
#                local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                experiment     = self.conf.xpid,
#                block          = self.conf.block,
#                geometry       = self.conf.domains,
#                vconf          = '[geometry::area]',
#                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
#                cumul          = footprints.util.rangex(self.conf.prv_terms)[20:38:2],
#                nativefmt      = 'ascii',
#                kind           = 'guess',
#                model          = 'safran',
#                source_app     = self.conf.source_app,
#                source_conf    = self.conf.eps_conf,
#                namespace      = self.conf.namespace_out,
#                member         = footprints.util.rangex(self.conf.pearp_members),
#                fatal          = False,
#            ),
#            print(t.prompt, 'tb06b =', tb06b)
#            print()

#            print('==================================================================================================')
#            print('==================================================================================================')
#            raise Exception('INFO :The execution went well, do not take into account the following error')
