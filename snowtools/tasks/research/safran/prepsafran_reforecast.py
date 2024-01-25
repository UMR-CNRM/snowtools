# -*- coding:Utf-8 -*-


__all__ = []

import os
import tarfile
import glob

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex.tools.systems import ExecutionError

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from bronx.stdtypes.date import Date, Period


def setup(t, **kw):
    return Driver(
        tag='pearp2safran',
        ticket=t,
        nodes=[
            PrepSafran(tag='prepsafprv', ticket=t, **kw),
        ],
        options=kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        datebegin = self.conf.datebegin.replace(hour=6)
        dateend = self.conf.dateend.replace(hour=6)
        t.env.setvar('DATADIR', '/scratch/mtool/vernaym/cache')

        ndays = (dateend - datebegin).days
#        if ndays > 366:
#            raise ExecutionError('Periode trop longue')

        #day_per_worker = ndays / 4

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            ###########################
            #  I) FICHIER de METADONNES
            ###########################

            # On commence par récupérer un fichier à échéance 0h qui sert à lire le métédonnées (infos sur la grille en particulier)
            # Ce fichier supplémentaire est indispensable pour toujours travailler avec la bonne grille du modèle, même en cas d'évolution
            # de la géométrie ARPEGE.
            self.sh.title('Toolbox input metadata')
            tbmeta = toolbox.input(
                role           = 'Metadata',
                format         = 'grib',
                genv            = self.conf.cycle,
                geometry       = self.conf.arpege_geometry, #EURAT01
                gdomain        = '[geometry:area]',
                kind           = 'relief',
                local          = 'METADATA.grib',
                fatal          = True,
            )
            print(t.prompt, 'tbmeta =', tbmeta)
            print()

            tbarp   = list()
            tbpearp = list()
            rundate = datebegin
            while rundate < dateend:

                if isinstance(self.conf.guess_xpid, dict): 
                    # Le reforecast PEARP produit par GMAP/RECYF en 2022 est dispo pour les réseaux 18h et 6h
                    # RQ : Le code suppose de passer comme datebegin une date avec un réseau de 18h disponible

                    # Récupération du réseau de 18:00 (J-1) pour couvrir J 6h -> (J+4) 6h
                    self.sh.title('Toolbox input pearp 18h')
                    tbpearp.extend(toolbox.input(
                        role           = 'Gridpoint',
                        kind           = 'gridpoint',
                        cutoff         = 'production',
                        format         = 'grib',
                        nativefmt      = '[format]',
                        experiment     = self.conf.guess_xpid['18'],
                        block          = 'forecast',
                        namespace      = 'vortex.multi.fr', # permet d'utiliser le cache inline pour les relances
                        geometry       = self.conf.pearp_geometry,
                        local          = '[date::ymdh]/mb[member%03]/[term:fmthour]/PEARP.grib',
                        origin         = 'historic',
                        date           = '{0:s}/+PT12H'.format(rundate.ymd6h),
                        term           = footprints.util.rangex(self.conf.prv_terms)[4:],
                        member         = footprints.util.rangex(self.conf.members),
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.eps_conf,
                        fatal          = False,
                    ))
                    print(t.prompt, 'tb18h')
                    print()

                    rundate = rundate + Period(days=3)

                    # Récupération du réseau 6:00 (J) 
                    self.sh.title(f'Toolbox input pearp {rundate.ymd} 6h')
                    tbpearp.extend(toolbox.input(
                        role           = 'Gridpoint',
                        kind           = 'gridpoint',
                        cutoff         = 'production',
                        format         = 'grib',
                        nativefmt      = '[format]',
                        experiment     = self.conf.guess_xpid['6'],
                        block          = 'forecast',
                        namespace      = 'vortex.multi.fr', # permet d'utiliser le cache inline pour les relances
                        geometry       = self.conf.pearp_geometry,
                        local          = '[date::ymdh]/mb[member%03]/[term:fmthour]/PEARP.grib',
                        origin         = 'historic',
                        date           = rundate.ymd6h,
                        term           = footprints.util.rangex(self.conf.prv_terms)[:33],
                        member         = footprints.util.rangex(self.conf.members),
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.eps_conf,
                        fatal          = False,
                    ))
                    print(t.prompt, 'tb6h')
                    print()

                    rundate = rundate + Period(days=2)

                else: # Reforecast chaine en double 2021

                    # Récupération du réseau ARPEGE de 0:00 (J) pour couvrir J 6h -> (J+4) 6h
                    self.sh.title('Toolbox input arpege 0h')
                    tbarp.extend(toolbox.input(
                        role           = 'Gridpoint',
                        kind           = 'gridpoint',
                        cutoff         = 'production',
                        format         = 'grib',
                        nativefmt      = '[format]',
                        experiment     = self.conf.guess_xpid,
                        block          = 'forecast',
                        namespace      = 'vortex.multi.fr', # permet d'utiliser le cache inline pour les relances
                        geometry       = self.conf.pearp_geometry,
                        local          = '[date::ymdh]/mb035/[term:fmthour]/ARPEGE.grib',
                        origin         = 'historic',
                        date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
                        term           = footprints.util.rangex(self.conf.prv_terms),
                        model          = '[vapp]',
                        vapp           = self.conf.source_app,
                        vconf          = self.conf.arpege_conf,
                    ))
                    print(t.prompt, 'tbarp')
                    print()

                    if self.conf.pearp:

                        # Récupération du réseau PEARP de 0:00 (J) pour couvrir J 6h -> (J+4) 6h
                        self.sh.title('Toolbox input pearp 0h')
                        tbpearp.extend(toolbox.input(
                            role           = 'Gridpoint',
                            kind           = 'gridpoint',
                            cutoff         = 'production',
                            format         = 'grib',
                            nativefmt      = '[format]',
                            experiment     = self.conf.guess_xpid,
                            block          = 'forecast',
                            namespace      = 'vortex.multi.fr', # permet d'utiliser le cache inline pour les relances
                            geometry       = self.conf.pearp_geometry,
                            local          = '[date::ymdh]/mb[member%03]/[term:fmthour]/PEARP.grib',
                            origin         = 'historic',
                            date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
                            term           = footprints.util.rangex(self.conf.prv_terms),
                            member         = footprints.util.rangex(self.conf.members),
                            model          = '[vapp]',
                            vapp           = self.conf.source_app,
                            vconf          = self.conf.eps_conf,
                        ))
                        print(t.prompt, 'tbpearp')
                        print()

                    rundate = rundate + Period(days=1)

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

            self.sh.title('Toolbox input tb02 = PRE-TRAITEMENT FORCAGE script')
            tb02 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.cycle,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp+tbpearp)]))),
            )
            print(t.prompt, 'tb02 =', tb02)
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb03')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                terms          = footprints.util.rangex(self.conf.prv_terms),
                interpreter    = 'current',
                # Need to extend pythonpath to be independant of the user environment
                # The vortex-build environment already set up the pythonpath (see jobassistant plugin) but the script is
                # eventually launched in a 'user-defined' environment
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d)
                                  for d in ['vortex/src', 'vortex/site', 'epygram',
                                            'epygram/site', 'epygram/eccodes_python']],
                ntasks         = self.conf.ntasks,
                reforecast     = True,
            )
            print(t.prompt, 'tb03 =', expresso)
            print()

            self.component_runner(expresso, script, fortran=False)

            for domain in self.conf.geometry.keys():

                # WARNING : Cette boucle est extrêmement lente (~1h soit 1/3 du temps total de calcul !!)
                #----------------------------------------------------------------------------------------

                tarname = 'ebauches_{0:s}_{1:s}_{2:s}.tar'.format(domain, datebegin.ymdh, dateend.ymdh)
                with tarfile.open(tarname, mode='w') as tarfic:
                    for f in glob.glob(f'*/*/*/P????????*{domain}*'):
                        # f = 'YYYYMMDD00/mbXXX/0ECH/PYYMMDDHH_E_dom_production'
                        basename = f.split('/')[-1] # PYYMMDDHH_E_dom_production
                        reseau = Date(f.split('/')[0]) # YYYYMMDDHH
                        member = f.split('/')[1] #mbXXX
                        ech = int(f.split('/')[2]) # ECH
                        # On veut organiser le tar pour qu'il soit directement exploitable par 
                        # l'algo SAFRAN arpès détarrage : toutes les échéances issues d'un même 
                        # réseau doivent être regroupées dans le même répertoire et le nom 
                        # du fichier guess de la forme PYYMMDDHH correspondant à la date
                        # de validité du guess
                        validity = reseau + Period(hours=ech)
                        arcname = os.path.join(f.split('/')[0], f.split('/')[1], f'P{validity.yymdh}')
                        tarfic.add(f, arcname=arcname)

        if 'late-backup' in self.steps:

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# WARNING : cette façon d'archiver les guess généré rend l'exécution
# de SAFRAN beaucoup moins flexible car il faut désormais impérativement
# le lancer sur exactement les même dates que le script de génération des
# ébauches.
# WARNING : The following output has not been tested yet !
# TODO : Modifier le tâche safran_reforecast en conséquence,
# ainsi que l'algo Vortex correspondant
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            for domain in self.conf.geometry.keys():

                tarname = 'ebauches_{0:s}_{1:s}_{2:s}.tar'.format(domain, datebegin.ymdh, dateend.ymdh)

                self.sh.title('Toolbox output tb04')
                tb04 = toolbox.output(
                    role           = 'Ebauche',
                    local          = tarname,
                    kind           = 'packedguess',
                    experiment     = self.conf.xpid,
                    vconf          = domain,
                    block          = 'guess',
                    geometry       = self.conf.geometry[domain],
                    nativefmt      = 'tar',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    date           = datebegin.ymdh,
                    datebegin      = datebegin.ymdh,
                    dateend        = dateend.ymdh,
                ),
                print(t.prompt, 'tb04 =', tb04)
                print()

            print('==================================================================================================')
            print('==================================================================================================')
            raise Exception('INFO :The execution went well, do not take into account the following error')

#            while rundate <= dateend:
#
#                if isinstance(self.conf.guess_xpid, dict): 
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# WARNING : Eviter d'archiver trop de petis fichiers sur HENDRIX.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#                    self.sh.title('Toolbox output tb6h')
#                    tb6h = toolbox.output(
#                        role           = 'Ebauche',
#                        local          = '[date::ymdh]/mb[member%03]/[cumul:fmthour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                        experiment     = self.conf.xpid,
#                        block          = self.conf.guess_block,
#                        geometry       = self.conf.domains,
#                        vconf          = '[geometry::area]',
#                        date           = rundate.ymd6h,
#                        cumul          = footprints.util.rangex(self.conf.prv_terms)[:33],
#                        nativefmt      = 'ascii',
#                        kind           = 'guess',
#                        model          = 'safran',
#                        source_app     = self.conf.source_app,
#                        source_conf    = self.conf.eps_conf,
#                        namespace      = 'vortex.multi.fr',
#                        member         = footprints.util.rangex(self.conf.members),
#                    ),
#                    print(t.prompt, 'tb6h =', tb6h)
#                    print()
#
#                    rundate = rundate + Period(days=3)
#
#                    self.sh.title('Toolbox output tb18h')
#                    tb18h = toolbox.output(
#                        role           = 'Ebauche',
#                        local          = '[date::ymdh]/mb[member%03]/[cumul:fmthour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                        experiment     = self.conf.xpid,
#                        block          = self.conf.guess_block,
#                        geometry       = self.conf.domains,
#                        vconf          = '[geometry::area]',
#                        date           = '{0:s}/-PT12H'.format(rundate.ymd6h),
#                        cumul          = footprints.util.rangex(self.conf.prv_terms)[4:],
#                        nativefmt      = 'ascii',
#                        kind           = 'guess',
#                        model          = 'safran',
#                        source_app     = self.conf.source_app,
#                        source_conf    = self.conf.eps_conf,
#                        namespace      = 'vortex.multi.fr',
#                        member         = footprints.util.rangex(self.conf.members),
#                    ),
#                    print(t.prompt, 'tb18h =', tb18h)
#                    print()
#
#                    rundate = rundate + Period(days=2)
#
#                else:
#
#                    self.sh.title('Toolbox output AREPEGE 0h')
#                    tbarp = toolbox.output(
#                        role           = 'Ebauche',
#                        local          = '[date::ymdh]/ARPEGE/[cumul:fmthour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                        experiment     = self.conf.xpid,
#                        block          = self.conf.guess_block,
#                        geometry       = self.conf.domains,
#                        vconf          = '[geometry::area]',
#                        date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
#                        cumul          = footprints.util.rangex(self.conf.prv_terms),
#                        nativefmt      = 'ascii',
#                        kind           = 'guess',
#                        model          = 'safran',
#                        source_app     = self.conf.source_app,
#                        source_conf    = self.conf.arpege_conf,
#                        namespace      = 'vortex.multi.fr',
#                    ),
#                    print(t.prompt, 'tbarp =', tbarp)
#                    print()
#
#                    if self.conf.pearp:
#
#                        self.sh.title('Toolbox output PEARP 0h')
#                        tbpearp = toolbox.output(
#                            role           = 'Ebauche',
#                            local          = '[date::ymdh]/mb[member%03]/[cumul:fmthour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                            experiment     = self.conf.xpid,
#                            block          = self.conf.guess_block,
#                            geometry       = self.conf.domains,
#                            vconf          = '[geometry::area]',
#                            date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
#                            cumul          = footprints.util.rangex(self.conf.prv_terms),
#                            nativefmt      = 'ascii',
#                            kind           = 'guess',
#                            model          = 'safran',
#                            source_app     = self.conf.source_app,
#                            source_conf    = self.conf.eps_conf,
#                            namespace      = 'vortex.multi.fr',
#                            member         = footprints.util.rangex(self.conf.members),
#                            fatal          = False,
#                        ),
#                        print(t.prompt, 'tbpearp =', tbpearp)
#                        print()
#
#                    rundate = rundate + Period(days=1)

